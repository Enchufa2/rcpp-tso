library(data.table)

flatten <- function(x) {
  data.table::rbindlist(lapply(x$items, function(i) {
    i$tags <- toString(i$tags)
    as.data.frame(i)
  }), fill=TRUE)
}

compress <- function(x) {
  div <- findInterval(x, c(0, 1e3, 1e6, 1e9, 1e12))
  paste0(round(x / 10^(3*(div-1)), 1), c("","k","M","B","T")[div])
}

# init
stackapi <- reticulate::import("stackapi")
so <- stackapi$StackAPI("stackoverflow")
so$page_size <- 100L
so$max_pages <- 1L

# fetch questions
res.q <- so$fetch("tags/{tags}/faq", tags="rcpp", filter="withbody")
res.q <- flatten(res.q)
res.q <- res.q[, !grepl("^migrated", names(res.q)), with=FALSE]

# remove stuff: OS-specific, arma/eigen-specific, errors...
f.os <- paste("macos", "osx", "windows", "linux", "ubuntu", sep="|")
f.pkg <- paste("armadillo", "eigen", sep="|")
f.err <- paste("crash", "error", "fail", "warning", sep="|")
f.tag <- paste(f.os, f.pkg, "inside", "inline", sep="|")
f.title <- paste(f.os, f.pkg, f.err, sep="|")
res.q <- res.q[!grepl(f.tag, tags) & !grepl(f.title, title, ignore.case=TRUE)]

# fetch answers
question.id <- split(res.q$question_id, ceiling(seq_along(res.q$question_id)/50))
so$max_pages <- 10000L
res.a <- lapply(question.id, function(ids)
  so$fetch("questions/{ids}/answers", ids=ids, filter="withbody"))
res.a <- rbindlist(lapply(res.a, flatten), fill=TRUE)
res.a <- res.a[res.a[, .I[which.max(score)], by=question_id]$V1]

# merge
names(res.q) <- paste0("question_", sub("^question_", "", names(res.q)))
names(res.a) <- paste0("answer_", sub("^question_", "", names(res.a)))
res.qa <- merge(res.q, res.a, by.x="question_id", by.y="answer_id", sort=FALSE)
res.qa <- res.qa[!duplicated(question_id)]

# some transformations
res.qa$question_tags <- sapply(strsplit(res.qa$question_tags, ", "), function(x) {
  paste0("<span>", x, "</span>", collapse="")
})
dateformat <- "%b %d '%y at %H:%M"
res.qa$question_creation_date <- format(as.POSIXct(
  res.qa$question_creation_date, origin="1970-01-01"), dateformat, tz="UTC")
res.qa$answer_creation_date <- format(as.POSIXct(
  res.qa$answer_creation_date, origin="1970-01-01"), dateformat, tz="UTC")
res.qa$question_view_count <- compress(res.qa$question_view_count)
res.qa$answer_view_count <- compress(res.qa$answer_view_count)

# output
res.qa <- res.qa[1:30]

toc <- paste0(
  "<li><a href='#qa-", res.qa$question_id, "'>",
  res.qa$question_title, "</a></li>", collapse="\n"
)

wcloud <- res.a[, .(n=.N), by=.(answer_owner.display_name, answer_owner.link)]
wcloud <- paste0(
  "<li><a data-weight='", wcloud$n, "' href='", wcloud$answer_owner.link, "'>",
  wcloud$answer_owner.display_name, "</a></li>", collapse="\n"
)

parts <- c("header", "footer", "qa")
parts <- lapply(setNames(parts, parts), function(x) {
  filename <- paste0("parts/", x, ".html")
  readChar(filename, file.info(filename)$size)
})
parts$header <- glue::glue(parts$header, .open="{% ", .close=" %}")
parts$qa <- glue::glue(parts$qa, .envir=res.qa, .open="{% ", .close=" %}")

dir.create("docs", showWarnings=FALSE)
cat(parts$header, file="docs/index.html", append=FALSE)
cat(parts$qa,     file="docs/index.html", append=TRUE)
cat(parts$footer, file="docs/index.html", append=TRUE)
