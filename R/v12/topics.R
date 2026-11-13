# Topic hints and weak-label example lists; retrieval engine remains unchanged.
.exam_topic_cards <- read.csv(file.path(WORKBENCH_ROOT,"demo-bank/topic-cards.csv"),
  stringsAsFactors=FALSE,na.strings=NULL,fileEncoding="UTF-8")
.exam_topic_fit <- function(text, width=80L) {
  text <- gsub("[[:space:]]+"," ",text,perl=TRUE)
  chars <- strsplit(text,"",fixed=TRUE)[[1]]
  sizes <- nchar(chars,type="width")
  if(sum(sizes)<=width)return(text)
  fits <- which(cumsum(sizes)<=width-2L)
  paste0(paste(head(chars,if(length(fits))tail(fits,1L)else 0L),collapse=""),"…")
}
.exam_topic_score <- function(q) {
  q <- gsub("[[:space:]]+"," ",q,perl=TRUE)
  rows <- lapply(seq_len(nrow(.exam_topic_cards)),function(i) {
    card <- .exam_topic_cards[i,];score<-0;snippets<-patterns<-character()
    for(clue in strsplit(card$clues,"\n",fixed=TRUE)[[1]]) {
      pieces<-strsplit(clue,"::",fixed=TRUE)[[1]]
      weight<-as.numeric(pieces[1]);pattern<-paste(pieces[-1],collapse="::")
      hit<-regexpr(pattern,q,ignore.case=TRUE,perl=TRUE)
      if(hit[1]>0L && weight>0) {
        score<-score+weight
        snippets<-c(snippets,regmatches(q,hit));patterns<-c(patterns,pattern)
      }
    }
    card$score<-score
    card$matched_snippets<-paste(unique(snippets),collapse="\n")
    card$matched_patterns<-paste(unique(patterns),collapse="\n")
    card
  })
  do.call(rbind,rows)
}
.exam_topic_distinction <- function(ids, q, scores) {
  pair<-function(a,b)all(c(a,b)%in%ids)
  if(pair("BINOMIAL","POISSON"))
    return("Binomial: fixed independent trials with the same p. Poisson: counts in an interval. For large n and small p, use lambda=n*p.")
  if(pair("NORMAL","UNIFORM"))
    return("Normal: bell-shaped and unbounded. Uniform: flat on a finite interval. Both have zero probability at a single point.")
  if(pair("BAYES","CONDITIONAL"))
    return("Check which event is conditioned on. Bayes uses the prior and likelihood to reverse the conditioning.")
  if(pair("COMB","BINOMIAL"))
    return("Counting depends on order and replacement. Binomial probability needs fixed independent trials with the same p.")
  if("CLT"%in%ids && any(c("NORMAL","UNIFORM")%in%ids))
    return("The central limit theorem approximates the distribution of a sum or mean under its large-sample conditions.")
  focuses<-.exam_topic_cards$focus[match(ids,.exam_topic_cards$id)]
  prefix<-if(min(scores)/max(scores)>=0.75)"Similar clue scores. Compare "else"Compare "
  paste0(prefix,focuses[1]," with ",focuses[2],".")
}
# Execute the EXACT scoring expressions from exam_find, without its thresholds,
# grade assignment, prints or cache writes. No duplicated scoring formula.
.exam_topic_raw_scores <- local({
  expressions <- as.list(body(exam_find))
  text <- vapply(expressions,function(x)paste(deparse(x),collapse=" "),character(1))
  first <- which(startsWith(text,"words <- .exam_words(q)"))
  last <- which(startsWith(text,"score <- pmin("))
  if(length(first)!=1L || length(last)!=1L || first>=last)
    .exam_error("Could not locate the search scoring expressions.")
  f <- function(q) NULL
  body(f) <- as.call(c(list(as.name("{")),expressions[first:last],
    list(quote(list(score=score,cosine=cosine)))))
  environment(f) <- environment(exam_find)
  f
})
.exam_topic_bank_order <- function(q) {
  scored <- .exam_topic_raw_scores(q)
  diffs <- lapply(.exam_source_numbers,.exam_diff,query=.exam_numbers(q))
  same <- vapply(diffs,function(d)!length(d$source_only)&&
    !length(d$query_only),logical(1))
  list(score=scored$score,order=order(-scored$score,-as.integer(same),
    -scored$cosine,.exam_bank$id))
}
.exam_topic_examples <- function(topic,scored) {
  indices <- scored$order[.exam_bank$scope[scored$order] == "in_syllabus" &
    vapply(.exam_bank$tags[scored$order], function(tags)
      topic %in% strsplit(toupper(tags), ";", fixed = TRUE)[[1]], logical(1))]
  tiers <- rep("confirmed", length(indices))
  keep <- seq_len(min(3L,length(indices)));indices<-indices[keep]
  data.frame(id=.exam_bank$id[indices],score=scored$score[indices],
    source=.exam_bank$source[indices],tier=tiers[keep],stringsAsFactors=FALSE)
}
.exam_topic_example_lines <- function(example,prefix="") {
  marker <- switch(example$tier,weak=" (weak example)",cue_only=" (topic cue)","")
  c(paste0(prefix,example$id,marker," | ",sprintf("%.6f",example$score),
      " | ",example$source),paste0('View: exam_open("',example$id,'")'))
}
# Presentation-only groups: topic IDs/scores/clues are never changed.
.exam_topic_groups <- function(rows,excluded=character()) {
  if(!nrow(rows))return(list())
  paired <- nrow(rows)==2L && setequal(rows$id,c("BINOMIAL","POISSON")) &&
    nrow(rows$examples[[1]])>0L &&
    setequal(rows$examples[[1]]$id,rows$examples[[2]]$id)
  if(paired) {
    ex<-rows$examples[[1]];other<-rows$examples[[2]]
    ex$tier[other$tier[match(ex$id,other$id)]=="weak"]<-"weak"
    groups<-list(list(name="Binomial / Poisson",examples=ex,indices=1:2))
  } else groups<-lapply(seq_len(nrow(rows)),function(i)
    list(name=rows$name_en[i],examples=rows$examples[[i]],indices=i))
  seen<-excluded
  for(i in seq_along(groups)) {
    ex<-groups[[i]]$examples;ex<-ex[!ex$id%in%seen,,drop=FALSE]
    groups[[i]]$examples<-ex;seen<-c(seen,ex$id)
  }
  groups
}
exam_topic <- function(q=NULL) {
  if(is.null(q))q<-EXAM_LAST_QUERY else {
    if(!is.character(q)||anyNA(q)) .exam_error("Supply nonmissing question text.")
    q<-paste(q,collapse=" ")
    if(!identical(q,EXAM_LAST_QUERY)) {
      .exam_clear();EXAM_LAST_QUERY<<-q
    }
  }
  q<-substr(q,1L,MAX_QUERY_CHARS)
  EXAM_LAST_TOPICS<<-data.frame()
  ranked<-.exam_topic_score(q)
  ranked<-ranked[ranked$score>0,,drop=FALSE]
  .exam_print("Use these hints to identify a possible model. Examples illustrate related methods.")
  if(!nrow(ranked)) {
    .exam_print("No topic identified. Choose a model based on the question.")
    return(invisible(EXAM_LAST_TOPICS))
  }
  ranked<-ranked[order(-ranked$score,match(ranked$id,.exam_topic_cards$id)),,drop=FALSE]
  both_models<-grepl("binomial",q,ignore.case=TRUE,perl=TRUE)&&
    grepl("poisson",q,ignore.case=TRUE,perl=TRUE)
  rate_trials<-grepl("out of every|per\\s+[0-9]+",q,ignore.case=TRUE,perl=TRUE)&&
    grepl("randomly selected|selected at random",q,ignore.case=TRUE,perl=TRUE)
  if((both_models||rate_trials)&&all(c("BINOMIAL","POISSON")%in%ranked$id))
    ranked<-ranked[ranked$id%in%c("BINOMIAL","POISSON"),,drop=FALSE]
  normal_model<-grepl("normal|\\bN[[:space:]]*\\(",q,ignore.case=TRUE,perl=TRUE)
  uniform_model<-grepl("uniform|\\bU[[:space:]]*\\(",q,ignore.case=TRUE,perl=TRUE)
  if(!both_models&&normal_model&&uniform_model&&
     all(c("NORMAL","UNIFORM")%in%ranked$id))
    ranked<-ranked[ranked$id%in%c("NORMAL","UNIFORM"),,drop=FALSE]
  ranked<-head(ranked,2L)
  scored<-.exam_topic_bank_order(q)
  ranked$examples<-I(lapply(ranked$id,.exam_topic_examples,scored=scored))
  EXAM_LAST_TOPICS<<-ranked
  groups<-.exam_topic_groups(ranked)
  for(i in seq_along(groups)) {
    g<-groups[[i]];x<-ranked[g$indices[1],]
    clues<-strsplit(x$matched_snippets,"\n",fixed=TRUE)[[1]]
    .exam_print(.exam_topic_fit(paste0("Hint ",i,": ",g$name,". Clue: ",
      .exam_topic_fit(clues[1L],24L))))
    .exam_print(paste0("Formula: ",.exam_topic_fit(x$formula,22L),
      ". R: ",.exam_topic_fit(x$r_commands,12L),
      ". Watch for: ",.exam_topic_fit(x$traps,18L)))
    if(length(g$indices)==2L) {
      y<-ranked[g$indices[2],]
      .exam_print(.exam_topic_fit(paste0("Additional clue (",y$name_en,"): ",y$matched_snippets)))
      .exam_print(paste0("Formula: ",.exam_topic_fit(y$formula,22L),
        ". R: ",.exam_topic_fit(y$r_commands,12L),
        ". Watch for: ",.exam_topic_fit(y$traps,18L)))
    }
    ex<-g$examples
    for(j in seq_len(nrow(ex)))
      for(line in .exam_topic_example_lines(ex[j,],paste0("Example ",j,": ")))
        .exam_print(line)
  }
  if(nrow(ranked)==2L)
    .exam_print(.exam_topic_fit(.exam_topic_distinction(ranked$id,q,ranked$score)))
  invisible(ranked)
}
.exam_go_topics <- function(has_candidates=FALSE) {
  invisible(capture.output(rows<-exam_topic()))
  .exam_print("Topic examples illustrate related methods.")
  if(!nrow(rows)) {
    .exam_print("No topic identified. Choose a model based on the question.")
    return(invisible(rows))
  }
  excluded<-if(has_candidates)EXAM_LAST_RESULTS$id else character()
  groups<-.exam_topic_groups(rows,excluded)
  if(has_candidates) {
    .exam_print(.exam_topic_fit(paste0("Topics: ",paste(vapply(groups,`[[`,"","name"),collapse=" / "))))
    chosen<-list();seen<-character()
    for(g in groups) {
      ex<-g$examples
      if(nrow(ex)){chosen[[length(chosen)+1L]]<-ex[1,,drop=FALSE];seen<-c(seen,ex$id[1])}
    }
    if(length(chosen)<2L)for(g in groups) {
      ex<-g$examples[!g$examples$id%in%seen,,drop=FALSE]
      if(nrow(ex)){chosen[[length(chosen)+1L]]<-ex[1,,drop=FALSE];seen<-c(seen,ex$id[1])}
      if(length(chosen)>=2L)break
    }
    for(ex in head(chosen,2L))for(line in .exam_topic_example_lines(ex)) .exam_print(line)
  } else {
    for(i in seq_along(groups)) {
      .exam_print(.exam_topic_fit(paste0("Hint ",i,": ",groups[[i]]$name)))
      ex<-head(groups[[i]]$examples,if(length(groups)==1L)3L else if(i==1L)2L else 1L)
      for(j in seq_len(nrow(ex)))for(line in .exam_topic_example_lines(ex[j,])) .exam_print(line)
    }
    if(nrow(rows)==2L)
      .exam_print(.exam_topic_fit(.exam_topic_distinction(rows$id,EXAM_LAST_QUERY,rows$score)))
  }
  invisible(rows)
}
