# Search and compare question text.
THR_ORIGINAL <- 0.95
THR_CHANGED <- 0.90
THR_SAME_TYPE <- 0.60
THR_LOW <- 0.40
CONTAINMENT_WEIGHT <- 0.92
CONTAINMENT_MIN_WORDS <- 6L
MAX_QUERY_CHARS <- 100000L
CONSOLE_WIDTH <- 80L
PDF_APP <- ""                 # Empty string uses the system default on macOS.
PDF_FALLBACK_APP <- "Preview" # Fallback PDF app on macOS.

# Wrap text by display width, including long paths and formulas.
.exam_print <- function(...) {
  value <- paste0(..., collapse = "")
  value <- gsub("\t", "  ", value, fixed = TRUE)
  for (line in strsplit(paste0(value, "\n"), "\n", fixed = TRUE)[[1]]) {
    chars <- strsplit(line, "", fixed = TRUE)[[1]]
    if (!length(chars)) { cat("\n"); next }
    widths <- nchar(chars, type = "width")
    start <- 1L
    while (start <= length(chars)) {
      fits <- which(cumsum(widths[start:length(chars)]) <= CONSOLE_WIDTH)
      end <- if (length(fits)) start + tail(fits, 1L) - 1L else start
      cat(paste0(chars[start:end], collapse = ""), "\n", sep = "")
      start <- end + 1L
    }
  }
  invisible(NULL)
}
.exam_error <- function(message) {
  lines <- capture.output(.exam_print(message))
  stop(paste0("\n", paste(lines, collapse = "\n")), call. = FALSE)
}
.exam_relative <- function(path) {
  is.character(path) && length(path) == 1L && !is.na(path) &&
    nzchar(path) && !grepl("^(/|~|[A-Za-z]:|\\\\)", path) &&
    !any(strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1]] == "..")
}
.exam_path <- function(path) {
  if (!.exam_relative(path)) .exam_error("Use a path relative to the bank directory.")
  file.path(BANK_ROOT, path)
}
# Compare numbers without assigning them to parameters or units.
.exam_num_pattern <- paste0(
  "[+−-]?(?:[0-9]+(?:,[0-9]{3})*(?:[.][0-9]+)?|[.][0-9]+)",
  "(?:[eE][+-]?[0-9]+)?(?:[ \\t]*[%％])?")
.exam_numbers <- function(text) {
  regmatches(text, gregexpr(.exam_num_pattern, text, perl = TRUE))[[1]]
}
.exam_num_keys <- function(x) {
  x <- gsub("−", "-", x, fixed = TRUE)
  x <- gsub("％", "%", x, fixed = TRUE)
  gsub("[ ,\\t]", "", x, perl = TRUE)
}
.exam_diff <- function(source, query) {
  sk <- .exam_num_keys(source); qk <- .exam_num_keys(query)
  used <- rep(FALSE, length(query)); source_only <- character()
  for (i in seq_along(source)) {
    match <- which(!used & qk == sk[i])
    if (length(match)) used[match[1]] <- TRUE else
      source_only <- c(source_only, source[i])
  }
  list(source_only = source_only, query_only = query[!used])
}
.exam_skeleton <- function(text) {
  text <- tolower(enc2utf8(text))
  text <- gsub(.exam_num_pattern, " <N> ", text, perl = TRUE)
  text <- gsub("[^[:alnum:]<>]+", " ", text, perl = TRUE)
  trimws(gsub("[[:space:]]+", " ", text))
}
.exam_words <- function(text) {
  text <- .exam_skeleton(text)
  if (!nzchar(text)) return(character())
  strsplit(text, " ", fixed = TRUE)[[1]]
}
.exam_index <- function(bank) {
  # Build the vocabulary and normalized TF-IDF matrix once per loaded bank.
  .exam_text <- bank$text
  normalized <- nzchar(trimws(bank$text_norm))
  .exam_text[normalized] <- bank$text_norm[normalized]
  .exam_tokens <- lapply(.exam_text, .exam_words)
  .exam_vocab <- sort(unique(unlist(.exam_tokens, use.names = FALSE)))
  if (!length(.exam_vocab)) .exam_error("The bank has no searchable text.")
  .exam_tf <- matrix(0, nrow(bank), length(.exam_vocab))
  for (.exam_i in seq_along(.exam_tokens)) {
    .exam_counts <- table(.exam_tokens[[.exam_i]])
    .exam_tf[.exam_i, match(names(.exam_counts), .exam_vocab)] <-
      as.numeric(.exam_counts)
  }
  .exam_idf <- log((1 + nrow(bank)) / (1 + colSums(.exam_tf > 0))) + 1
  .exam_matrix <- sweep(.exam_tf, 2L, .exam_idf, "*")
  .exam_norm <- sqrt(rowSums(.exam_matrix ^ 2))
  .exam_matrix <- .exam_matrix / pmax(.exam_norm, .Machine$double.eps)
  .exam_source_numbers <- lapply(.exam_text, .exam_numbers)
    index <- list(.exam_bank = bank, .exam_text = .exam_text, .exam_vocab = .exam_vocab,
      .exam_tokens = .exam_tokens, .exam_idf = .exam_idf, .exam_matrix = .exam_matrix,
      .exam_source_numbers = .exam_source_numbers,
      .exam_identity_bank = lapply(.exam_text, .exam_identity_parts))
  list2env(index, envir = environment(.exam_index))
  .exam_clear()
}
EXAM_LAST_QUERY <- ""
EXAM_LAST_TOPICS <- data.frame()
EXAM_LAST_RESULTS <- data.frame()
EXAM_LAST_OUTSIDE_COUNT <- 0L
EXAM_LAST_ALL <- FALSE
.exam_scope_note <- function() {
  .exam_print("There are also ", EXAM_LAST_OUTSIDE_COUNT,
    " out-of-scope matches above the minimum. Include them with all=TRUE.")
  invisible(NULL)
}
.exam_clear <- function() {
  EXAM_LAST_QUERY <<- ""
  EXAM_LAST_TOPICS <<- data.frame()
  EXAM_LAST_RESULTS <<- data.frame()
  EXAM_LAST_OUTSIDE_COUNT <<- 0L
  EXAM_LAST_ALL <<- FALSE
  invisible(NULL)
}
.exam_num_text <- function(x) if (length(x)) paste(x, collapse = ", ") else "none"
.exam_level <- function(score, same_numbers) {
  if (score >= THR_ORIGINAL && same_numbers) return("original")
  if (score >= THR_CHANGED && !same_numbers) return("changed numbers")
  if (score >= THR_SAME_TYPE) return("same type")
  if (score >= THR_LOW) return("low confidence")
  "below threshold"
}

.exam_public_params <- function(params) {
  gsub('"answer_locator"[[:space:]]*:[[:space:]]*"(?:\\\\.|[^"\\\\])*"',
       '"answer_locator":"[answer location hidden]"', params, perl = TRUE)
}

# Identity checks are separate from the unchanged scoring tokenizer above.
# Only format and numeric masking are normalized; no semantic rules apply.
.exam_identity_prepare <- function(text) {
  text <- enc2utf8(paste(text, collapse = " "))
  ligatures <- c("ﬁ" = "fi", "ﬂ" = "fl", "ﬀ" = "ff",
                 "ﬃ" = "ffi", "ﬄ" = "ffl")
  for (from in names(ligatures))
    text <- gsub(from, ligatures[[from]], text, fixed = TRUE)
  text <- tolower(text)
  text <- gsub("[‘’‚‛]", "'", text, perl = TRUE)
  text <- gsub('[“”„‟]', '"', text, perl = TRUE)
  text <- gsub("[‐‑‒–—―−﹘﹣－]", "-", text, perl = TRUE)
  text <- gsub("∼", "~", text, fixed = TRUE)
  text <- gsub("≤", "<=", text, fixed = TRUE)
  text <- gsub("≥", ">=", text, fixed = TRUE)
  text <- gsub("!=", "≠", text, fixed = TRUE)
  text <- gsub("\u00ad", "", text, fixed = TRUE)
  # Join broken words, retaining arithmetic minus between single letters.
  text <- gsub("([[:alpha:]]{2,})-[ \\t]*\\r?\\n[ \\t]*([[:alpha:]]{2,})",
               "\\1\\2", text, perl = TRUE)
  label <- paste0("(?m)^[ \\t]*(?:",
    "(?:exercise|example|problem|question)\\b[ \\t]*",
    "(?:[0-9]+(?:[.][0-9]+)*[a-z]?)?[ \\t]*[:.)-]?[ \\t]*|",
    "[0-9]+(?:[.][0-9]+)*[.)](?![0-9])[ \\t]*|",
    "\\([a-z]\\)(?=[ \\t]+[[:alpha:]]|[ \\t]*$)[ \\t]*)")
  repeat {
    next_text <- gsub(label, "", text, perl = TRUE)
    if (identical(next_text, text)) break
    text <- next_text
  }
  # Inline subquestion labels, including when line breaks are removed.
  text <- gsub("(^|[.!?:;][[:space:]]+)\\([a-z]\\)[[:space:]]+(?=[[:alpha:]])",
               "\\1", text, perl = TRUE)
  text
}
.exam_identity_atom <- "(?:[0-9]+(?:,[0-9]{3})*(?:[.][0-9]+)?|[.][0-9]+)"
.exam_identity_num_pattern <- paste0(.exam_identity_atom,
  "(?:[[:space:]]*/[[:space:]]*", .exam_identity_atom, ")?",
  "(?:[[:space:]]*[%％])?")
.exam_identity_parts <- function(text) {
  prepared <- .exam_identity_prepare(text)
  numbers <- regmatches(prepared,
    gregexpr(.exam_identity_num_pattern, prepared, perl = TRUE))[[1]]
  numbers <- gsub("[[:space:],]", "", numbers, perl = TRUE)
  numbers <- gsub("％", "%", numbers, fixed = TRUE)
  masked <- gsub(.exam_identity_num_pattern, " <N> ", prepared, perl = TRUE)
  pattern <- "<N>|>=|<=|[[:alpha:]_]+|[<>≠=∪∩|~+*/^−-]"
  tokens <- regmatches(masked, gregexpr(pattern, masked, perl = TRUE))[[1]]
  list(tokens = tokens, numbers = sort(numbers), number_sequence = numbers,
       number_pattern = match(numbers, unique(numbers)))
}
.exam_number_positions <- function(source, query) {
  positions <- which(source != query)
  if (!length(positions)) return(character())
  result <- paste0("Position ", head(positions, 6L), ": ",
    source[head(positions, 6L)], "→", query[head(positions, 6L)])
  if (length(positions) > 6L)
    result <- c(result, paste0("... total: ", length(positions), " positions differ"))
  result
}
.exam_number_pattern_text <- function(pattern) {
  if (!length(pattern)) return("none")
  values <- vapply(head(pattern, 12L), function(i) {
    if (i <= length(letters)) letters[i] else paste0("a", i)
  }, character(1))
  paste0(paste(values, collapse = " "),
    if (length(pattern) > 12L) paste0(" ... total: ", length(pattern), " items") else "")
}
.exam_identity_skeleton <- function(text) .exam_identity_parts(text)$tokens
.exam_contiguous_tokens <- function(long, short) {
  if (!length(short) || length(short) >= length(long)) return(FALSE)
  starts <- which(long == short[1L])
  starts <- starts[starts + length(short) - 1L <= length(long)]
  any(vapply(starts, function(i)
    identical(long[seq.int(i, length.out = length(short))], short), logical(1)))
}
.exam_subsequence_tokens <- function(long, short) {
  if (!length(short) || length(short) >= length(long)) return(FALSE)
  j <- 1L
  for (token in long) {
    if (identical(token, short[j])) j <- j + 1L
    if (j > length(short)) return(TRUE)
  }
  FALSE
}
# Sequence alignment preserves differences in order, e.g. P(A|B)/P(B|A).
.exam_identity_diff <- function(source, query) {
  if (identical(source, query))
    return(list(query_only = character(), source_only = character()))
  # Trim equal edges before alignment to keep the working strings short.
  prefix <- 0L; n <- min(length(source), length(query))
  while (prefix < n && identical(source[prefix + 1L], query[prefix + 1L]))
    prefix <- prefix + 1L
  if (prefix) {
    source <- source[-seq_len(prefix)]; query <- query[-seq_len(prefix)]
  }
  suffix <- 0L; n <- min(length(source), length(query))
  while (suffix < n && identical(source[length(source) - suffix],
                                 query[length(query) - suffix]))
    suffix <- suffix + 1L
  if (suffix) {
    source <- head(source, -suffix); query <- head(query, -suffix)
  }
  if (!length(source) || !length(query))
    return(list(query_only = query, source_only = source))
  vocab <- unique(c(source, query))
  encode <- function(tokens) intToUtf8(0xE000L + match(tokens, vocab))
  aligned <- adist(encode(source), encode(query),
    costs = c(insertions = 1, deletions = 1, substitutions = 2),
    counts = TRUE, useBytes = FALSE)
  operations <- strsplit(attr(aligned, "trafos")[[1]], "", fixed = TRUE)[[1]]
  si <- 1L; qi <- 1L; source_only <- query_only <- character()
  for (op in operations) {
    if (op %in% c("D", "S")) source_only <- c(source_only, source[si])
    if (op %in% c("I", "S")) query_only <- c(query_only, query[qi])
    if (op %in% c("M", "D", "S")) si <- si + 1L
    if (op %in% c("M", "I", "S")) qi <- qi + 1L
  }
  list(query_only = query_only, source_only = source_only)
}
.exam_word_preview <- function(tokens) {
  if (!length(tokens)) return("none")
  value <- paste(head(tokens, 10L), collapse = " ")
  if (length(tokens) > 10L)
    value <- paste0(value, " ... total: ", length(tokens), " words or symbols")
  value
}
.exam_identity_note <- function(row, show_diff = TRUE) {
  if (show_diff)
    .exam_print("  Text differences. Only in query: ",
      .exam_word_preview(row$query_only_words[[1]]), ". Only in source: ",
      .exam_word_preview(row$source_only_words[[1]]))
  if (row$is_fragment)
    .exam_print("  Partial match. The query may omit part of the question.")
  if (row$has_added_content)
    .exam_print("  The query has additional content.")
  if (nzchar(row$numeric_structure_note))
    .exam_print("  ", row$numeric_structure_note)
  if (length(row$numeric_position_differences[[1]]))
    .exam_print("  Changed number positions (source to query): ",
      paste(row$numeric_position_differences[[1]], collapse = "; "))
  previous <- if (row$numeric_previous_confidence != row$confidence)
    row$numeric_previous_confidence else row$base_confidence
  if (previous != row$confidence)
    .exam_print("  Previous label: ", previous, " → ", row$confidence,
                "(", row$confidence_reason, ")")
  invisible(NULL)
}

# Display only: the query and numeric preview never enter scoring or ranking.
.exam_query_header <- function(q) {
  text <- paste(as.character(q), collapse = " ")
  if (!nzchar(trimws(text))) return(invisible(NULL))
  preview <- gsub("[[:space:]]+", " ", text, perl = TRUE)
  numbers <- .exam_identity_parts(substr(text, 1L, MAX_QUERY_CHARS))$number_sequence
  .exam_print("Query: ", substr(preview, 1L, 100L),
    if (nchar(preview) > 100L) "…" else "", " (total: ", nchar(text), " characters)")
  .exam_print("Detected ", length(numbers), " numbers: ",
    if (length(numbers)) paste(head(numbers, 12L), collapse = ", ") else "none",
    if (length(numbers) > 12L) " …" else "")
  invisible(NULL)
}

exam_find <- function(q, top_n = 5, all = FALSE) {
  .exam_clear()
  if (missing(q) || is.null(q) || !length(q) ||
      all(is.na(q)) || !nzchar(trimws(paste(q, collapse = " ")))) {
    .exam_print("Enter a question to search."); return(invisible(EXAM_LAST_RESULTS))
  }
  if (!is.logical(all) || length(all) != 1L || is.na(all))
    .exam_error("all must be TRUE or FALSE.")
  EXAM_LAST_ALL <<- all
  q <- paste(as.character(q), collapse = " ")
  .exam_query_header(q)
  truncated <- nchar(q) > MAX_QUERY_CHARS
  if (truncated) {
    q <- substr(q, 1L, MAX_QUERY_CHARS)
  }
  if (length(top_n) != 1L || !is.numeric(top_n) || !is.finite(top_n))
    .exam_error("top_n must be a finite number. Result count is limited to 3 through 5.")
  top_n <- as.integer(max(3, min(5, floor(top_n))))
  if (any(!is.finite(c(THR_ORIGINAL, THR_CHANGED, THR_SAME_TYPE, THR_LOW))) ||
      !all(diff(c(THR_LOW, THR_SAME_TYPE, THR_CHANGED, THR_ORIGINAL)) >= 0) ||
      THR_LOW < 0 || THR_ORIGINAL > 1)
    .exam_error("Thresholds must satisfy 0 <= LOW <= SAME_TYPE <= CHANGED <= ORIGINAL <= 1.")
  EXAM_LAST_QUERY <<- q
  words <- .exam_words(q)
  counts <- table(words)
  known <- match(names(counts), .exam_vocab)
  good <- !is.na(known)
  query_vector <- numeric(length(.exam_vocab))
  query_vector[known[good]] <- as.numeric(counts[good]) * .exam_idf[known[good]]
  # Keep unseen terms in the query norm to avoid inflated scores.
  unseen_idf <- log(1 + nrow(.exam_bank)) + 1
  query_norm <- sqrt(sum(query_vector ^ 2) +
    sum((as.numeric(counts[!good]) * unseen_idf) ^ 2))
  cosine <- if (query_norm > 0) as.numeric(.exam_matrix %*%
    (query_vector / query_norm)) else rep(0, nrow(.exam_bank))
  unique_words <- unique(words)
  # Weight containment by query length to reduce scores for very short fragments.
  lexical <- setdiff(unique_words, "<N>")
  short_weight <- min(1, length(lexical) / CONTAINMENT_MIN_WORDS)
  unique_idf <- .exam_idf[match(unique_words, .exam_vocab)]
  unique_idf[is.na(unique_idf)] <- unseen_idf
  containment_raw <- vapply(.exam_tokens, function(tokens) {
    if (!length(unique_words)) return(0)
    mean(unique_words %in% tokens)
  }, numeric(1))
  containment <- vapply(.exam_tokens, function(tokens) {
    if (!length(unique_words)) return(0)
    hit <- unique_words %in% tokens
    # Adjust overlap by term specificity.
    ratio <- mean(hit)
    specificity <- if (any(hit)) mean(unique_idf[hit]) / mean(unique_idf) else 0
    ratio * specificity
  }, numeric(1)) * CONTAINMENT_WEIGHT * short_weight
  score <- pmin(1, pmax(cosine, containment))
  # Score every record using the unchanged full-bank vocabulary and IDF.
  # Scope only chooses visible candidates after scores have been computed.
  eligible <- if (all) rep(TRUE, nrow(.exam_bank)) else
    .exam_bank$scope %in% c("in_syllabus", "to_confirm")
  EXAM_LAST_OUTSIDE_COUNT <<- sum(.exam_bank$scope == "outside_syllabus" &
    score > THR_LOW)
  if (!length(score) || !any(eligible) || max(score[eligible]) < THR_LOW) {
    .exam_print("No reliable match found.")
    .exam_scope_note()
    .exam_print("Use exam_topic() for topic hints.")
    return(invisible(EXAM_LAST_RESULTS))
  }
  if (truncated) .exam_print("Query too long. Compared the first ", MAX_QUERY_CHARS, " characters.")
  query_numbers <- .exam_numbers(q)
  differences <- lapply(.exam_source_numbers, .exam_diff, query = query_numbers)
  same <- vapply(differences, function(d)
    !length(d$source_only) && !length(d$query_only), logical(1))
  order <- order(-score, -as.integer(same), -cosine, .exam_bank$id)
  order <- order[eligible[order]]
  picked <- head(order, min(top_n, length(order)))
  rows <- .exam_bank[picked, c("id", "source", "chapter", "qno", "pdf_file",
    "pdf_page_start", "pdf_page_end", "scope", "text_quality", "source_params")]
  rows$score <- score[picked]
  rows$cosine <- cosine[picked]
  rows$containment_raw <- containment_raw[picked]
  rows$weighted_containment <- containment[picked]
  rows$method <- ifelse(cosine[picked] >= containment[picked],
                       "TF-IDF cosine", "Weighted containment")
  rows$base_confidence <- mapply(.exam_level, rows$score, same[picked],
                                 USE.NAMES = FALSE)
  identity_query <- .exam_identity_parts(q)
  identity_source <- .exam_identity_bank[picked]
  rows$skeleton_equal <- vapply(identity_source, function(s)
    identical(s$tokens, identity_query$tokens), logical(1))
  rows$same_identity_numbers <- vapply(identity_source, function(s)
    identical(s$numbers, identity_query$numbers), logical(1))
  rows$same_number_sequence <- vapply(identity_source, function(s)
    identical(s$number_sequence, identity_query$number_sequence), logical(1))
  rows$same_number_pattern <- vapply(identity_source, function(s)
    identical(s$number_pattern, identity_query$number_pattern), logical(1))
  rows$is_fragment <- vapply(identity_source, function(s)
    .exam_contiguous_tokens(s$tokens, identity_query$tokens), logical(1))
  rows$has_added_content <- vapply(identity_source, function(s)
    .exam_subsequence_tokens(identity_query$tokens, s$tokens), logical(1))
  rows$confidence <- mapply(function(score, same_numbers, equal) {
    level <- .exam_level(score, same_numbers)
    if (!equal && level %in% c("original", "changed numbers")) "same type" else level
  }, rows$score, rows$same_identity_numbers, rows$skeleton_equal,
     USE.NAMES = FALSE)
  rows$confidence_reason <- ifelse(rows$skeleton_equal,
    "Normalized number multisets differ", "Text structure differs")
  rows$confidence_reason[rows$is_fragment] <- "Text structure differs; partial match"
  rows$confidence_reason[rows$has_added_content] <-
    "Text structure differs; the query has additional content"
  rows$confidence_reason[rows$skeleton_equal &
    rows$same_identity_numbers] <- "Ordered numbers match after removing question labels"
  swapped <- rows$skeleton_equal & rows$same_identity_numbers &
    !rows$same_number_sequence
  repeated <- rows$skeleton_equal & !rows$same_identity_numbers &
    !rows$same_number_pattern
  rows$confidence_reason[swapped] <- "Numbers changed positions, possibly swapping parameters"
  rows$confidence_reason[repeated] <- "Repeated-number pattern differs"
  numeric_cap <- (swapped | repeated) &
    rows$confidence %in% c("original", "changed numbers")
  rows$numeric_previous_confidence <- rows$confidence
  rows$confidence[numeric_cap] <- "same type"
  rows$numeric_structure_note <- rep("", nrow(rows))
  rows$numeric_structure_note[swapped] <- "Numbers changed positions, possibly swapping parameters"
  for (i in which(repeated))
    rows$numeric_structure_note[i] <- paste0("Repeated-number pattern differs. Source: ",
      .exam_number_pattern_text(identity_source[[i]]$number_pattern),
      ". Query: ", .exam_number_pattern_text(identity_query$number_pattern))
  rows$numeric_position_differences <- I(lapply(seq_along(identity_source),
    function(i) if (swapped[i]) .exam_number_positions(
      identity_source[[i]]$number_sequence, identity_query$number_sequence)
      else character()))
  word_differences <- lapply(identity_source, function(s)
    .exam_identity_diff(s$tokens, identity_query$tokens))
  rows$query_only_words <- I(lapply(word_differences, `[[`, "query_only"))
  rows$source_only_words <- I(lapply(word_differences, `[[`, "source_only"))
  rows$source_only_numbers <- I(lapply(differences[picked], `[[`, "source_only"))
  rows$query_only_numbers <- I(lapply(differences[picked], `[[`, "query_only"))
  EXAM_LAST_RESULTS <<- rows
  for (i in seq_len(nrow(rows))) {
    x <- rows[i, ]
    .exam_print(i, ". ", x$id, " | ", sprintf("%.2f%%", x$score * 100),
                " | ", x$confidence)
    .exam_print("  ", x$source, " | Chapter ", x$chapter, " | Question ", x$qno)
    .exam_print("  ", x$pdf_file, " | location: ", x$pdf_page_start)
    .exam_print("  Scoring method: ", x$method)
    .exam_print("  source-only numbers: ", .exam_num_text(x$source_only_numbers[[1]]),
      "; query-only numbers: ", .exam_num_text(x$query_only_numbers[[1]]),
      "; labels not inferred; source parameters: ", .exam_public_params(x$source_params))
    .exam_identity_note(x, show_diff = i <= 3L)
    .exam_print("  SCOPE: ", x$scope)
    .exam_print("  TEXT QUALITY: ", x$text_quality)
    if (x$confidence %in% c("low confidence", "below threshold"))
      .exam_print("  Use as a reference for further lookup.")
    if (x$confidence == "below threshold")
      .exam_print("  Score below the minimum threshold.")
  }
  .exam_scope_note()
  invisible(rows)
}

# A number indexes the last results. A string is an exact ID.
.exam_resolve <- function(id) {
  if (length(id) != 1L || is.na(id)) .exam_error("Enter a valid ID or result number.")
  if (is.numeric(id)) {
    if (!is.finite(id) || id != floor(id) || id < 1 ||
        id > nrow(EXAM_LAST_RESULTS))
      .exam_error("This number is outside the last results. Run exam_find() again.")
    id <- EXAM_LAST_RESULTS$id[id]
  } else if (!is.character(id)) .exam_error("Enter an exact ID or result number.")
  row <- match(id, .exam_bank$id)
  if (is.na(row)) .exam_error(paste("Unknown question ID: ", id))
  row
}
.exam_launch <- function(path) {
  platform <- Sys.info()[["sysname"]]
  if (identical(platform, "Windows")) {
    shell.exec(path)
    return(0L)
  }
  if (!identical(platform, "Darwin"))
    return(system2("xdg-open", shQuote(path), stdout = FALSE, stderr = FALSE))
  attempt <- function(app) {
    args <- if (nzchar(app)) c("-a", shQuote(app), shQuote(path)) else shQuote(path)
    tryCatch(system2("open", args, stdout = FALSE, stderr = FALSE),
             error = function(e) 1L)
  }
  status <- attempt(PDF_APP)
  if (status != 0L && nzchar(PDF_FALLBACK_APP) &&
      PDF_FALLBACK_APP != PDF_APP) status <- attempt(PDF_FALLBACK_APP)
  status
}
.exam_open_file <- function(path, manual, root = BANK_ROOT) {
  full <- if (.exam_relative(path)) file.path(root, path) else ""
  if (!nzchar(full) || !file.exists(full) || file.access(full, 4L) != 0L)
    .exam_error(paste("PDF is missing or unreadable. Source: ", manual))
  status <- tryCatch(.exam_launch(full), error = function(e) 1L)
  if (!identical(as.integer(status), 0L))
    .exam_error(paste0("The file is readable, but the PDF viewer did not start.\n",
      "On macOS you can set PDF_APP to Preview.\n",
      "Run this command in a local R session with a desktop viewer.\n",
      "Open manually: ", manual))
  invisible(path)
}
exam_open <- function(id) {
  if (missing(id)) .exam_error("Enter an exact ID or result number.")
  x <- .exam_bank[.exam_resolve(id), ]
  .exam_print("id: ", x$id)
  .exam_print("Source: ", x$pdf_file)
  .exam_print("Source PDF pages: ", x$pdf_page_start, "-", x$pdf_page_end)
  if (nzchar(x$question_pdf)) .exam_open_file(x$question_pdf, x$pdf_file)
  else .exam_print(x$text)
  invisible(x$text)
}
exam_answer <- function(id) {
  if (missing(id)) .exam_error("Enter an exact ID or result number.")
  x <- .exam_bank[.exam_resolve(id), ]
  .exam_print("id: ", x$id)
  recent <- match(x$id, EXAM_LAST_RESULTS$id)
  if (is.na(recent)) {
    .exam_print("This solution uses the source question's numbers. Run a search to compare the inputs.")
    .exam_print("source-only numbers: not compared; query-only numbers: not compared")
  } else if (EXAM_LAST_RESULTS$confidence[recent] != "original") {
    result <- EXAM_LAST_RESULTS[recent, ]
    if (result$is_fragment)
      .exam_print("The query matched a fragment. Check the full question before using this solution.")
    if (result$has_added_content)
      .exam_print("The query contains additional content. Check which parts this solution covers.")
    previous <- if (result$numeric_previous_confidence != result$confidence)
      result$numeric_previous_confidence else result$base_confidence
    if (previous != result$confidence)
      .exam_print("Previous label: ", previous, " → ", result$confidence,
                  "(", result$confidence_reason, ")")
    .exam_print("Current match: ", result$confidence,
                ". Check all conditions and parts of the question.")
    .exam_print("The solution uses source numbers (source-only numbers: ",
      .exam_num_text(EXAM_LAST_RESULTS$source_only_numbers[[recent]]), ").")
    .exam_print("Query number differences (query-only numbers: ",
      .exam_num_text(EXAM_LAST_RESULTS$query_only_numbers[[recent]]), ").")
  }
  if (x$answer_source == "self_prepared")
    .exam_print("Verify this independently prepared solution.")
  if (x$answer_source == "textbook_worked_example")
    .exam_print("This explanation is part of the original worked example.")
  if (!nzchar(x$answer_pdf) && !nzchar(x$answer_text)) {
    .exam_print("No solution is available. Use exam_open() to inspect the question.")
    return(invisible(NULL))
  }
  if (nzchar(x$answer_pdf)) {
    # Read the numeric end page without a JSON dependency.
    end <- regmatches(x$source_params,
      regexpr('"answer_page_end"[[:space:]]*:[[:space:]]*[0-9]+',
              x$source_params))
    end <- if (length(end) && nzchar(end)) sub(".*:", "", end) else x$answer_page
    pages <- paste0(x$answer_page, "-", trimws(end))
    .exam_print("Solution source: ", x$answer_file, " PDF pp.", pages)
  }
  # Show the solution text before launching a viewer.
  if (nzchar(x$answer_text)) .exam_print(x$answer_text)
  if (nzchar(x$answer_pdf))
    .exam_open_file(x$answer_pdf, paste(x$answer_file, "pages", pages))
  invisible(x$answer_text)
}

# Optional-use offline aids; loading does not calculate or print answers.
sys.source(file.path(WORKBENCH_ROOT, "R/v12/topics.R"), envir = environment())
sys.source(file.path(WORKBENCH_ROOT, "R/v12/calculators.R"), envir = environment())
