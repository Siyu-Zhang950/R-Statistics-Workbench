# Offline question search and source-linked solution retrieval. Base R only.
# Inputs: local CSV or RDS question and solution tables; output: ranked records.

exam_lang <- function(lang = c("en", "zh")) {
  lang <- match.arg(lang)
  options(exam.language = lang)
  invisible(lang)
}
.lang <- function() getOption("exam.language", "en")
.tr <- function(en, zh) if (.lang() == "zh") zh else en

.normalize <- function(text, mask_numbers = FALSE) {
  # Preserve mathematical case, e.g. X<x differs from x<X.
  text <- enc2utf8(text)
  text <- gsub("[\u2018\u2019]", "'", text, perl = TRUE)
  text <- gsub("[\u2010-\u2015]", "-", text, perl = TRUE)
  text <- gsub("\u2264", " <= ", text, fixed = TRUE)
  text <- gsub("\u2265", " >= ", text, fixed = TRUE)
  text <- gsub("\u03bb", " lambda ", text, fixed = TRUE)
  text <- gsub("\u03bc", " mu ", text, fixed = TRUE)
  text <- gsub("\u03c3", " sigma ", text, fixed = TRUE)
  text <- gsub("\u2212", "-", text, fixed = TRUE)
  text <- gsub("\u222a", " union ", text, fixed = TRUE)
  text <- gsub("\u2229", " intersection ", text, fixed = TRUE)
  text <- gsub("(?m)^\\s*(exercise|question)\\s+[0-9.]+\\s*", "",
               text, perl = TRUE)
  text <- gsub("^\\s*[0-9]+[.)]\\s+", "", text, perl = TRUE)
  if (mask_numbers) text <- gsub(
    "(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?", " NUM ",
    text, perl = TRUE)
  # Exact/changed-number matching retains punctuation: |, ', parentheses,
  # brackets, and arithmetic operators can change the mathematical meaning.
  trimws(gsub("\\s+", " ", text, perl = TRUE))
}
.tokens <- function(text) {
  lexical <- gsub("[^\\p{L}\\p{N}.%]+", " ", tolower(.normalize(text)), perl = TRUE)
  words <- strsplit(lexical, " ", fixed = TRUE)[[1]]
  stop_words <- c("the", "a", "an", "of", "and", "in", "to", "is",
                  "for", "that", "with", "it", "we", "you", "what",
                  "which", "this", "are", "be", "as", "by", "from",
                  "on", "or", "can", "using", "calculate", "find",
                  "questions", "question", "exercise", "provide")
  words <- words[nchar(words) > 1L & !words %in% stop_words]
  # Chinese character bigrams allow Chinese lookup without a segmentation package.
  han <- regmatches(text, gregexpr("[\\p{Han}]+", text, perl = TRUE))[[1]]
  grams <- unlist(lapply(han, function(s) {
    n <- nchar(s)
    if (n < 2) return(s)
    substring(s, seq_len(n - 1), seq_len(n - 1) + 1)
  }), use.names = FALSE)
  unique(c(words, grams))
}
.concepts <- function(text) {
  lex <- c(
    normal = "normal distribution|normally distributed|standard normal|(?<![A-Za-z])N\\s*\\(|\u6b63\u6001",
    binomial = "binomial|\u4e8c\u9879|nausea|\u6076\u5fc3|vaccinat|\u63a5\u79cd|successes in",
    poisson = "poisson|\u6cca\u677e", exponential = "exponential|\u6307\u6570\u5206\u5e03|memoryless|\u65e0\u8bb0\u5fc6",
    bayes = "bayes|infect|positive test|test positive|\u9633\u6027|\u611f\u67d3|\u6761\u4ef6\u6982\u7387|prevalence",
    testing = "test positive|testing positive|positive test|\u9633\u6027|sensitivity|specificity|\u68c0\u6d4b",
    infection = "infect|\u611f\u67d3|prevalence",
    point_mass = "singletons?|\u5355\u70b9|point probability|P\\s*\\(\\s*X\\s*=",
    event_count = "at most|more than|at least|most two|\u81f3\u591a|\u81f3\u5c11|\u8d85\u8fc7|\u4eba\u6570",
    continuous = "continuous|\u8fde\u7eed|singletons?|point probability|\u5355\u70b9",
    variance = "variance|\u65b9\u5dee|\\bVar\\s*\\(", expectation = "expectation|expected|\u5747\u503c|\u671f\u671b|arithmetic mean",
    quantile = "quantile|quartile|median|\u5206\u4f4d|\u4e2d\u4f4d",
    counting = "permutation|combination|lotto|\u6392\u5217|\u7ec4\u5408|\u653e\u56de|handshake|stars.and.bars",
    joint = "joint|covariance|correlation|\u8054\u5408|\u534f\u65b9\u5dee|\u76f8\u5173|two.dimensional",
    integration = "integral|integrat|\u79ef\u5206|\u222b",
    chebyshev = "cheb|tscheb|\u5207\u6bd4\u96ea\u592b", clt = "central limit|\u4e2d\u5fc3\u6781\u9650|clt",
    frequency = "frequenc|histogram|\u9891\u6570|\u76f4\u65b9\u56fe", kde = "kernel|\u6838\u5bc6\u5ea6",
    concentration = "lorenz|gini|\u6d1b\u4f26\u5179|\u57fa\u5c3c", uniform = "uniform|\u5747\u5300|(?<![A-Za-z])U\\s*\\(",
    hypergeometric = "hypergeometric|\u8d85\u51e0\u4f55", geometric = "geometric distribution|\u9996\u6b21\u6210\u529f|\u51e0\u4f55\u5206\u5e03",
    multinomial = "multinomial|dessert|\u591a\u9879\u5206\u5e03", sets = "de morgan|\u96c6\u5408|\u4ea4\u96c6|\u5e76\u96c6",
    cdf = "cdf|distribution function|ecdf|\u5206\u5e03\u51fd\u6570",
    scales = "qualitative|quantitative|measurement scale|\u5c3a\u5ea6|\u5b9a\u6027|\u5b9a\u91cf")
  names(lex)[vapply(lex, grepl, logical(1), x = text,
                    ignore.case = TRUE, perl = TRUE)]
}
.numbers <- function(text) {
  text <- gsub("Exercise [0-9.]+", "", text, perl = TRUE)
  z <- regmatches(text, gregexpr(
    "(?<![[:alpha:]0-9.])[+-]?(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?%?", text, perl = TRUE))[[1]]
  if (identical(z, character())) return(character())
  head(unique(z), 12)
}
.grams <- function(text) {
  s <- tolower(.normalize(text, TRUE))
  n <- nchar(s)
  if (n < 3) return(s)
  unique(substring(s, seq_len(n - 2), seq_len(n - 2) + 2))
}
.jaccard <- function(a, b) {
  if (!length(a) || !length(b)) return(0)
  length(intersect(a, b)) / length(union(a, b))
}

exam_bank_load <- function(path = "demo-bank") {
  paths <- file.path(path, c("questions.rds", "solutions.rds"))
  read_table <- readRDS
  if (!all(file.exists(paths))) {
    paths <- file.path(path, c("questions.csv", "solutions.csv"))
    read_table <- function(file) {
      read.csv(file, colClasses = "character", encoding = "UTF-8")
    }
  }
  if (!all(file.exists(paths))) {
    stop("Supply a folder with a complete questions/solutions CSV or RDS pair.",
         call. = FALSE)
  }
  bank <- read_table(paths[1])
  solutions <- read_table(paths[2])
  required <- c("id", "source", "chapter", "question_no", "full_text",
                "tags", "source_pdf", "source_pages", "scope",
                "recommended_command", "parameters", "text_quality")
  if (!is.data.frame(bank) || !all(required %in% names(bank)) ||
      !nrow(bank) || anyDuplicated(bank$id) || anyNA(bank[required])) {
    stop("Invalid question database schema", call. = FALSE)
  }
  if (!is.data.frame(solutions) ||
      !all(c("id", "text", "source_pdf", "source_pages", "provenance") %in% names(solutions)) ||
      anyDuplicated(solutions$id)) {
    stop("Invalid solution database schema", call. = FALSE)
  }
  .exam_bank$questions <- bank
  .exam_bank$solutions <- solutions
  .exam_bank$tokens <- lapply(bank$full_text, .tokens)
  .exam_bank$grams <- lapply(bank$full_text, .grams)
  .exam_bank$concepts <- lapply(seq_len(nrow(bank)), function(i) {
    unique(c(.concepts(bank$full_text[i]),
             strsplit(bank$tags[i], ";", fixed = TRUE)[[1]]))
  })
  .exam_bank$normalized <- .normalize(bank$full_text)
  .exam_bank$masked <- .normalize(bank$full_text, TRUE)
  vocab <- unique(unlist(.exam_bank$tokens, use.names = FALSE))
  df <- table(unlist(.exam_bank$tokens, use.names = FALSE))
  .exam_bank$idf <- setNames(log((nrow(bank) + 1) /
                                 (as.numeric(df[vocab]) + 1)) + 1, vocab)
  invisible(nrow(bank))
}

.parameter_diff <- function(question, candidate, parameters = NULL) {
  old <- .numbers(candidate$full_text)
  new <- .numbers(question)
  removed <- setdiff(old, new)
  added <- setdiff(new, old)
  if (is.null(parameters)) {
    return(paste0("source-only numbers: ", paste(removed, collapse = ","),
                  "; query-only numbers: ", paste(added, collapse = ","),
                  "; labels not inferred; source parameters: ",
                  candidate$parameters))
  }
  if (!is.list(parameters) || is.null(names(parameters))) {
    stop("parameters must be a named list", call. = FALSE)
  }
  paste0(paste(names(parameters), unlist(parameters), sep = "=",
               collapse = "; "), " | source: ", candidate$parameters,
         "; inspect event endpoints and units separately")
}

exam_search <- function(question, top_k = 5L, threshold = 0.58,
                        parameters = NULL) {
  if (!is.character(question) || length(question) != 1L ||
      is.na(question) || !nzchar(trimws(question))) {
    stop("question must be one nonempty text string", call. = FALSE)
  }
  if (nchar(question) > 30000L) stop("Question exceeds 30000 characters")
  if (!is.numeric(top_k) || length(top_k) != 1 ||
      !top_k %in% 3:5) stop("top_k must be 3, 4, or 5")
  if (!is.numeric(threshold) || length(threshold) != 1 ||
      is.na(threshold) || threshold < 0.5 || threshold > 0.95) {
    stop("threshold must be between 0.50 and 0.95")
  }
  if (is.null(.exam_bank$questions)) exam_bank_load()
  bank <- .exam_bank$questions
  qnorm <- .normalize(question)
  qmask <- .normalize(question, TRUE)
  qt <- .tokens(question)
  qg <- .grams(question)
  qc <- .concepts(question)
  scores <- numeric(nrow(bank))
  labels <- rep("low confidence", nrow(bank))
  for (i in seq_len(nrow(bank))) {
    if (identical(qnorm, .exam_bank$normalized[i])) {
      scores[i] <- 1
      labels[i] <- "original"
      next
    }
    if (identical(qmask, .exam_bank$masked[i]) && nchar(qmask) >= 35) {
      scores[i] <- 0.97
      labels[i] <- "changed numbers"
      next
    }
    bt <- .exam_bank$tokens[[i]]
    common <- intersect(qt, bt)
    idf <- .exam_bank$idf
    qweights <- idf[qt]
    qweights[is.na(qweights)] <- max(c(1, idf)) + 1
    # Binary TF-IDF cosine needs squared weights in the dot product.
    lexical <- if (length(common)) {
      sum(idf[common]^2) / sqrt(sum(qweights^2) * sum(idf[bt]^2))
    } else 0
    concept <- .jaccard(qc, .exam_bank$concepts[[i]])
    coverage <- if (length(qc)) {
      length(intersect(qc, .exam_bank$concepts[[i]])) / length(qc)
    } else 0
    chars <- .jaccard(qg, .exam_bank$grams[[i]])
    score <- 0.40 * lexical + 0.18 * chars + 0.27 * concept +
      0.15 * coverage
    # Chinese/English paraphrases rely on multiple shared topic concepts.
    if (length(qc) >= 2 && coverage == 1 && concept >= 0.2) {
      score <- max(score, 0.58 + 0.12 * concept + 0.08 * lexical)
    }
    # A matching distribution name alone never implies a reliable match.
    if (length(intersect(qc, .exam_bank$concepts[[i]])) < 2) {
      score <- min(score, 0.54)
    }
    if ("point_mass" %in% qc &&
        !"point_mass" %in% .exam_bank$concepts[[i]]) {
      score <- min(score, 0.54)
    }
    scores[i] <- min(score, 0.94)
    if (scores[i] >= threshold) labels[i] <- "same type"
  }
  ii <- order(-scores, bank$id)[seq_len(min(top_k, nrow(bank)))]
  reliable <- scores[ii] >= threshold
  result <- bank[ii, c("id", "source", "chapter", "question_no",
                       "source_pdf", "source_pages", "scope"), drop = FALSE]
  result$similarity <- round(scores[ii], 4)
  result$confidence <- labels[ii]
  result$reliable <- reliable
  result$parameter_difference <- vapply(ii, function(i) {
    .parameter_diff(question, bank[i, ], parameters)
  }, character(1))
  result$recommended_command <- ifelse(reliable,
                                       bank$recommended_command[ii], "")
  result$text_quality <- bank$text_quality[ii]
  result$command_status <- if ("command_status" %in% names(bank))
    bank$command_status[ii] else "Inspect model and source before calculation"
  structure(list(matches = result, threshold = threshold,
                 reliable = any(reliable), query = question,
                 language = .lang()), class = "exam_matches")
}

print.exam_matches <- function(x, ...) {
  if (!x$reliable) cat(.tr("No reliable match found.\n",
                            "\u672a\u627e\u5230\u53ef\u9760\u5339\u914d\u3002\n"))
  cat(.tr("Similarity is a heuristic, not a probability of correctness.\n",
          "\u76f8\u4f3c\u5ea6\u662f\u68c0\u7d22\u5206\u6570\uff0c\u4e0d\u662f\u7b54\u6848\u6b63\u786e\u6982\u7387\u3002\n"))
  for (i in seq_len(nrow(x$matches))) {
    r <- x$matches[i, ]
    label <- r$confidence
    if (.lang() == "zh") label <- c(
      original = "\u539f\u9898", `changed numbers` = "\u6539\u6570\u5b57",
      `same type` = "\u540c\u7c7b\u578b", `low confidence` = "\u4f4e\u53ef\u4fe1")[[label]]
    cat(sprintf("\n%d. %s | %.1f%% | %s\n", i, r$id,
                100 * r$similarity, label))
    cat("  ", r$source, " | ", r$chapter, " | ", r$question_no,
        "\n  ", r$source_pdf, " | location: ", r$source_pages, "\n", sep = "")
    cat("  ", r$parameter_difference, "\n", sep = "")
    cat("  SCOPE: ", r$scope, "\n  TEXT QUALITY: ", r$text_quality,
        "\n", sep = "")
    if (r$reliable && nzchar(r$recommended_command)) cat("  ", r$recommended_command, "\n", sep = "")
    if (r$reliable) cat("  ROUTE: ", r$command_status, "\n", sep = "")
  }
  cat(.tr("\nCommands are templates. Replace uppercase placeholders; no command is executed.\n",
          "\n\u547d\u4ee4\u4e3a\u6a21\u677f\uff0c\u987b\u66ff\u6362\u5927\u5199\u5360\u4f4d\u53c2\u6570\uff1b\u68c0\u7d22\u4e0d\u4f1a\u6267\u884c\u547d\u4ee4\u3002\n"))
  invisible(x)
}

exam_question <- function(id) {
  if (is.null(.exam_bank$questions)) exam_bank_load()
  bank <- .exam_bank$questions
  i <- match(id, bank$id)
  if (length(i) != 1 || is.na(i)) stop("Unknown question id")
  r <- bank[i, ]
  cat(r$id, " | ", r$source, " | ", r$chapter, "\n", sep = "")
  cat(r$source_pdf, " | location: ", r$source_pages, "\n")
  cat(r$full_text, "\n")
  cat("TEXT QUALITY: ", r$text_quality, "\n", sep = "")
  invisible(r)
}

exam_solution <- function(id) {
  if (is.null(.exam_bank$questions)) exam_bank_load()
  s <- .exam_bank$solutions
  i <- match(id, s$id)
  if (length(i) != 1 || is.na(i)) {
    cat(.tr("No source-linked solution available.\n",
            "\u672a\u63d0\u4f9b\u53ef\u8ffd\u6eaf\u7684\u89e3\u7b54\u3002\n"))
    return(invisible(NULL))
  }
  r <- s[i, ]
  cat("SOURCE: ", r$provenance, "\n", r$source_pdf,
      " | location: ", r$source_pages, "\n", sep = "")
  cat(r$text, "\n")
  cat(.tr("Consult the source for full context.\n",
          "PDF\u6587\u5b57\u63d0\u53d6\u53ef\u80fd\u9057\u6f0f\u516c\u5f0f\u56fe\u7247\uff1b\u6b64\u5904\u4e3a\u5e26\u6765\u6e90\u7684\u53c2\u8003\u6587\u672c\uff0c\u4e0d\u662f\u81ea\u52a8\u8ba4\u8bc1\u7684\u89e3\u7b54\u3002\n"))
  invisible(r)
}

.v3_search <- exam_search
exam_search <- function(question, top_k=5L, threshold=.58, parameters=NULL,
                        context=NULL) {
  if (!is.null(context) && (!is.list(context) || is.null(names(context))))
    stop('context must be a named list: test, variable_type')
  out <- .v3_search(question,top_k,threshold,parameters)
  m <- out$matches
  q <- tolower(question)
  wanted_test <- if (!is.null(context$test)) match.arg(context$test,c('positive','negative')) else
    if (grepl('testing negative|tested negative|test negative|\u9634\u6027',q)) 'negative' else
    if (grepl('testing positive|tested positive|test positive|\u9633\u6027',q)) 'positive' else NULL
  continuous <- identical(context$variable_type,'continuous') ||
    grepl('normal variables|continuous random|\u8fde\u7eed\u968f\u673a|\u6b63\u6001\u53d8\u91cf',q)
  m$route_status <- 'REFERENCE_ONLY'
  for (i in seq_len(nrow(m))) {
    cmd <- m$recommended_command[i]
    if (!nzchar(cmd)) next
    conflict <- !is.null(wanted_test) && grepl('exam_test',cmd,fixed=TRUE) &&
      !grepl(paste0('test = "',wanted_test,'"'),cmd,fixed=TRUE)
    conflict <- conflict || (continuous && grepl('JOINT_MATRIX',cmd,fixed=TRUE))
    if (conflict) {
      m$recommended_command[i] <- ''
      m$route_status[i] <- 'BLOCKED_CONDITION_CONFLICT'
    } else if (m$confidence[i] != 'original') {
      # Modified questions require a new parameterized calculation. Never carry
      # a fixed original worked helper or stored answer into that calculation.
      m$recommended_command[i] <- ''
      m$route_status[i] <- 'REFERENCE_ONLY_REBUILD_MODEL'
    } else m$route_status[i] <- 'ORIGINAL_REFERENCE_CHECK_ASSUMPTIONS'
    m$command_status[i] <- paste(m$route_status[i],m$command_status[i],sep=' | ')
  }
  out$matches <- m
  out
}

