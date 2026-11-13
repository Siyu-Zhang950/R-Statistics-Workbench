# Base R only. Explicit arguments only; never read the query/cache or infer data.
.exam_calc_number <- function(x,name,lower=-Inf,upper=Inf,integer=FALSE,
                              allow_inf=FALSE) {
  valid<-is.numeric(x)&&!is.complex(x)&&length(x)==1L&&!is.na(x)
  if(valid&&!allow_inf)valid<-is.finite(x)
  if(valid)valid<-x>=lower&&x<=upper
  if(valid&&integer)valid<-is.finite(x)&&x==floor(x)
  if(!valid) {
    spec<-if(integer)"an integer"else"a number"
    .exam_error(paste0(name," is missing or invalid. Expected ",spec,
      " in [",lower,", ",upper,"]."))
  }
  as.numeric(x)
}
.exam_calc_digits <- function(digits)
  as.integer(.exam_calc_number(digits,"digits",0,15,integer=TRUE))
.exam_calc_type <- function(type,choices) {
  if(identical(type,choices))return(choices[1])
  if(!is.character(type)||length(type)!=1L||is.na(type)||!type%in%choices)
    .exam_error(paste0("Invalid type. Choose from: ",paste(choices,collapse=", "),"."))
  type
}
.exam_calc_arg <- function(x)paste(format(x,digits=15,trim=TRUE,
                                         scientific=FALSE),collapse=", ")
.exam_calc_result <- function(x,digits) {
  if(any(!is.finite(x))) .exam_error("Result is outside the representable range. Adjust the inputs.")
  formatC(x,format="f",digits=digits)
}
.exam_calc_steps <- function(formula,substitution,value,digits,labels=NULL) {
  value_text<-.exam_calc_result(value,digits)
  if(!is.null(labels))value_text<-paste0(labels,"=",value_text)
  .exam_print("Formula: ",formula)
  .exam_print("Substitution: ",substitution)
  .exam_print("Result: ",paste(value_text,collapse="; "))
  invisible(value)
}
.exam_count_type <- function(type,law) {
  relation<-c(eq="=",le="<=",lt="<",ge=">=",gt=">")[[type]]
  formula<-switch(type,eq=paste0("P(X=k)=",law),
    le=paste0("P(X<=k)=sum(j=0..k, ",law,")"),
    lt="P(X<k)=P(X<=k-1)",ge="P(X>=k)=1-P(X<=k-1)",
    gt="P(X>k)=1-P(X<=k)")
  list(relation=relation,formula=formula)
}
exam_binom <- function(n=NULL,p=NULL,k=NULL,
                       type=c("eq","le","lt","ge","gt"),digits=4L) {
  n<-.exam_calc_number(n,"n",0,integer=TRUE)
  p<-.exam_calc_number(p,"p",0,1)
  k<-.exam_calc_number(k,"k",0,integer=TRUE)
  type<-.exam_calc_type(type,c("eq","le","lt","ge","gt"))
  digits<-.exam_calc_digits(digits)
  value<-switch(type,eq=dbinom(k,n,p),le=pbinom(k,n,p),
    lt=pbinom(k-1,n,p),ge=pbinom(k-1,n,p,lower.tail=FALSE),
    gt=pbinom(k,n,p,lower.tail=FALSE))
  f<-.exam_count_type(type,"choose(n,j)*p^j*(1-p)^(n-j)")
  if(type=="eq")f$formula<-"P(X=k)=choose(n,k)*p^k*(1-p)^(n-k)"
  cutoff<-if(type%in%c("lt","ge"))k-1 else k
  call<-if(type=="eq")"dbinom"else"pbinom"
  substitution<-paste0("P(X",f$relation,.exam_calc_arg(k),")=",call,"(",
    .exam_calc_arg(cutoff),",size=",.exam_calc_arg(n),",prob=",
    .exam_calc_arg(p),if(type%in%c("ge","gt"))",lower.tail=FALSE)"else")")
  .exam_calc_steps(f$formula,substitution,value,digits)
}
exam_pois <- function(lambda=NULL,k=NULL,
                      type=c("eq","le","lt","ge","gt"),digits=4L) {
  lambda<-.exam_calc_number(lambda,"lambda",0)
  k<-.exam_calc_number(k,"k",0,integer=TRUE)
  type<-.exam_calc_type(type,c("eq","le","lt","ge","gt"))
  digits<-.exam_calc_digits(digits)
  value<-switch(type,eq=dpois(k,lambda),le=ppois(k,lambda),
    lt=ppois(k-1,lambda),ge=ppois(k-1,lambda,lower.tail=FALSE),
    gt=ppois(k,lambda,lower.tail=FALSE))
  f<-.exam_count_type(type,"exp(-lambda)*lambda^j/factorial(j)")
  if(type=="eq")f$formula<-"P(X=k)=exp(-lambda)*lambda^k/factorial(k)"
  cutoff<-if(type%in%c("lt","ge"))k-1 else k
  call<-if(type=="eq")"dpois"else"ppois"
  substitution<-paste0("P(X",f$relation,.exam_calc_arg(k),")=",call,"(",
    .exam_calc_arg(cutoff),",lambda=",.exam_calc_arg(lambda),
    if(type%in%c("ge","gt"))",lower.tail=FALSE)"else")")
  .exam_calc_steps(f$formula,substitution,value,digits)
}
exam_norm <- function(mean=NULL,sd=NULL,a=-Inf,b=Inf,digits=4L) {
  mean<-.exam_calc_number(mean,"mean")
  sd<-.exam_calc_number(sd,"sd",0)
  if(sd==0).exam_error("sd must be greater than zero. Use the standard deviation.")
  a<-.exam_calc_number(a,"a",allow_inf=TRUE)
  b<-.exam_calc_number(b,"b",allow_inf=TRUE)
  if(a>b).exam_error("The interval requires a <= b.")
  digits<-.exam_calc_digits(digits)
  za<-(a-mean)/sd;zb<-(b-mean)/sd
  format_z<-function(z)if(is.finite(z)).exam_calc_result(z,digits)else .exam_calc_arg(z)
  # Upper-tail subtraction avoids catastrophic loss for large positive z.
  value<-if(a>=mean)pnorm(a,mean,sd,lower.tail=FALSE)-
    pnorm(b,mean,sd,lower.tail=FALSE) else pnorm(b,mean,sd)-pnorm(a,mean,sd)
  .exam_calc_steps("z=(x-mean)/sd; P(a<X<b)=Phi(z_b)-Phi(z_a)",
    paste0("mean=",.exam_calc_arg(mean),", sd=",.exam_calc_arg(sd),
      "; z_a=(",.exam_calc_arg(a),"-",.exam_calc_arg(mean),")/",
      .exam_calc_arg(sd),"=",format_z(za),
      "; z_b=(",.exam_calc_arg(b),"-",.exam_calc_arg(mean),")/",
      .exam_calc_arg(sd),"=",format_z(zb)),value,digits)
}
exam_exp <- function(rate=NULL,t=NULL,
                     type=c("le","lt","ge","gt","eq"),digits=4L) {
  rate<-.exam_calc_number(rate,"rate",0)
  if(rate==0).exam_error("rate must be greater than zero. Use the reciprocal of the mean wait.")
  t<-.exam_calc_number(t,"t",0)
  type<-.exam_calc_type(type,c("le","lt","ge","gt","eq"))
  digits<-.exam_calc_digits(digits)
  value<-switch(type,le=pexp(t,rate),lt=pexp(t,rate),
    ge=pexp(t,rate,lower.tail=FALSE),gt=pexp(t,rate,lower.tail=FALSE),eq=0)
  formula<-switch(type,le="P(X<=t)=1-exp(-rate*t)",
    lt="P(X<t)=1-exp(-rate*t)",ge="P(X>=t)=exp(-rate*t)",
    gt="P(X>t)=exp(-rate*t)",eq="P(X=t)=0 for a continuous variable")
  substituted<-if(type=="eq")"0 (a single point in a continuous distribution)"else paste0(
    if(type%in%c("le","lt"))"1-"else"", "exp(-",.exam_calc_arg(rate),
    "*",.exam_calc_arg(t),")")
  .exam_calc_steps(formula,paste0("rate=",.exam_calc_arg(rate),
    ", t=",.exam_calc_arg(t),"; ",substituted),value,digits)
}
# s = elapsed waiting time; t = ADDITIONAL waiting time.
exam_exp_cond <- function(rate=NULL,s=NULL,t=NULL,digits=4L) {
  rate<-.exam_calc_number(rate,"rate",0)
  if(rate==0).exam_error("rate must be greater than zero.")
  s<-.exam_calc_number(s,"s (elapsed wait)",0)
  t<-.exam_calc_number(t,"t (additional wait)",0)
  digits<-.exam_calc_digits(digits)
  value<-pexp(t,rate,lower.tail=FALSE)
  .exam_calc_steps("P(X>s+t|X>s)=P(X>t)=exp(-rate*t)",
    paste0("rate=",.exam_calc_arg(rate),", s=",.exam_calc_arg(s),
      ", t=",.exam_calc_arg(t),"; P(X>",.exam_calc_arg(s+t),
      "|X>",.exam_calc_arg(s),")=exp(-",.exam_calc_arg(rate),"*",
      .exam_calc_arg(t),")"),value,digits)
}
exam_bayes <- function(prior=NULL,sens=NULL,fpr=NULL,digits=4L) {
  prior<-.exam_calc_number(prior,"prior",0,1)
  sens<-.exam_calc_number(sens,"sens",0,1)
  fpr<-.exam_calc_number(fpr,"fpr",0,1)
  digits<-.exam_calc_digits(digits)
  positive<-prior*sens+(1-prior)*fpr
  if(positive<=0).exam_error("P(positive)=0, so the conditional probability is undefined.")
  value<-c(disease=prior*sens/positive,
           no_disease=(1-prior)*fpr/positive,positive=positive)
  .exam_calc_steps("P(positive)=prior*sens+(1-prior)*fpr; P(condition|positive)=prior*sens/P(positive)",
    paste0("P(positive)=",.exam_calc_arg(prior),"*",.exam_calc_arg(sens),
      "+(1-",.exam_calc_arg(prior),")*",.exam_calc_arg(fpr),
      "=",.exam_calc_arg(positive),
      "; P(no condition|positive)=(1-prior)*fpr/P(positive)=1-P(condition|positive)"),
    value,digits,c("P(condition|positive)","P(no condition|positive)","P(positive)"))
}
exam_comb <- function(n=NULL,k=NULL,
  kind=c("perm","perm_rep","var","var_rep","comb","comb_rep"),
  groups=NULL,digits=4L) {
  n<-.exam_calc_number(n,"n",0,integer=TRUE)
  kind<-.exam_calc_type(kind,c("perm","perm_rep","var","var_rep","comb","comb_rep"))
  digits<-.exam_calc_digits(digits)
  if(kind%in%c("perm","perm_rep")) {
    if(!is.null(k)&&.exam_calc_number(k,"k",0,integer=TRUE)!=n)
      .exam_error("For full permutations, omit k or set k=n. For partial permutations use kind='var'.")
  } else k<-.exam_calc_number(k,"k",0,integer=TRUE)
  if(kind%in%c("var","comb")&&k>n)
    .exam_error("Permutations and combinations without repetition require k <= n.")
  if(kind!="perm_rep"&&!is.null(groups))
    .exam_error("groups is used with kind='perm_rep'.")
  if(kind=="perm_rep") {
    if(!is.numeric(groups)||is.complex(groups)||!length(groups)||
       any(!is.finite(groups))||any(groups<0)||any(groups!=floor(groups))||
       sum(groups)!=n)
      .exam_error("groups must be a vector of nonnegative integers summing to n.")
  }
  formula<-switch(kind,perm="n!",perm_rep="n!/prod(groups!)",
    var="n!/(n-k)!",var_rep="n^k",comb="choose(n,k)",
    comb_rep="choose(n+k-1,k)")
  value<-switch(kind,perm=factorial(n),
    perm_rep=exp(lfactorial(n)-sum(lfactorial(groups))),
    var=if(k==0)1 else prod(seq.int(n-k+1,n)),var_rep=n^k,
    comb=choose(n,k),comb_rep=choose(n+k-1,k))
  substituted<-switch(kind,perm=paste0("factorial(",n,")"),
    perm_rep=paste0("exp(lfactorial(",n,")-sum(lfactorial(c(",
      .exam_calc_arg(groups),"))))"),
    var=paste0("factorial(",n,")/factorial(",n,"-",k,")"),
    var_rep=paste0(n,"^",k),comb=paste0("choose(",n,",",k,")"),
    comb_rep=paste0("choose(",n,"+",k,"-1,",k,")"))
  .exam_calc_steps(formula,paste0(substituted,"; kind=",kind,", n=",.exam_calc_arg(n),
    if(!is.null(k))paste0(", k=",.exam_calc_arg(k))else"",
    if(!is.null(groups))paste0(", groups=(",.exam_calc_arg(groups),")")else""),
    value,digits)
}
exam_mean_var <- function(x=NULL,p=NULL,digits=4L) {
  if(!is.numeric(x)||is.complex(x)||!length(x)||any(!is.finite(x)))
    .exam_error("x must be a nonempty vector of finite numbers.")
  if(!is.numeric(p)||is.complex(p)||length(p)!=length(x)||
     any(!is.finite(p))||any(p<0|p>1)||abs(sum(p)-1)>1e-10)
    .exam_error("p must match the length of x, contain probabilities from 0 to 1, and sum to 1.")
  digits<-.exam_calc_digits(digits)
  mu<-sum(x*p);variance<-sum(p*(x-mu)^2)
  terms<-paste0("(",vapply(x,.exam_calc_arg,character(1)),")*",
                vapply(p,.exam_calc_arg,character(1)))
  vterms<-paste0("(",vapply(x,.exam_calc_arg,character(1)),"-",
    .exam_calc_arg(mu),")^2*",vapply(p,.exam_calc_arg,character(1)))
  .exam_calc_steps("E(X)=sum(x_i*p_i); Var(X)=sum((x_i-E(X))^2*p_i)",
    paste0("E(X)=",paste(terms,collapse=" + "),"=",.exam_calc_arg(mu),
      "; Var(X)=",paste(vterms,collapse=" + ")),
    c(mean=mu,variance=variance),digits,c("E(X)","Var(X)"))
}
