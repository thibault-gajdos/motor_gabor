## * Prelude
##install.packages('psych') # for describe
##install.packages("tidyverse")
##install.packages('coin') ## for bootsrap
##install.packages('broom') ## for purrr
##install.packages('lme4') ## for lmer
##install.packages('ggrepel') ## for nice labels in graphs
##install.packages('texreg') ## export reg results
##install.packages('stargazer')
##install.packages('latticeExtra')
##install.packages('binom')
##install.packages('sjPlot')
library(ggResidpanel) ## diganosis lmer: resid_panel(model)
## library(rsample)
## library(doParallel)
## library(latticeExtra))
library(pander)
library(stargazer) ## generate nice tables
library(texreg) ## nice tables 
## library(ggrepel)
## library(coin)
## library(broom)
library(afex) ## || for lmer +  graph tools for lmer
##library(lme4)
library(readxl) ## to read xls files
## library(psych)
library(lmerTest) ## to compute p-val for lmer
library(optimx)
## library(car)
library(ggpubr) ## plot customization, correlation ...
library(ggpmisc) ## plot customizaton: table, etc.
## library(lsmeans)
## library(corrr)
library(xtable)
library(brms)
## library(BayesFactor)
## library(BEST)
## library(sjmisc)
library(sjPlot) ## plot interactions 
## library(xtable)
## library(metaSDT)
## library(jtools)
library(MASS)
library(MESS)
## library(beepr)
## library(DEoptim)
## library(simr)
## library(cowplot)
library(tidyverse)
## library(ggthemes)
## library(interplot)
## library(interactions)
## library(latex2exp)
## library(ggpmisc)
## library(effects)
library(rtdists)
library(rstan)
library(Hmisc) ## decribe function
#library(RTconflict)
library(emmeans) ## compute EMM
library(ggeffects) ## to plote EMM
library(jsonlite) ## to read json files
library(ggrepel) ## points labels in ggplot

select <- dplyr::select

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

custom <- theme(plot.title = element_text(size = 16)) +
    theme(axis.title.x = element_text(size = 14)) +
    theme(axis.title.y = element_text(size = 14)) +
    theme(legend.text=element_text(size=14)) +
    theme(axis.text.x=element_text(size=14)) +
    theme(axis.text.y=element_text(size=14)) +
    theme(aspect.ratio=1) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))



options(width=150)
ls.funs <- function(env=sys.frame(-1))unlist(lapply(ls(env=env),function(x)if  (is.function(get(x)))x))
ls.vars <- function(env=sys.frame(-1))unlist(lapply(ls(env=env),function(x)if (!is.function(get(x)))x))

## * paired permutation test using  coin
pptest2 <- function (var1, var2, B=9999){
DV <- c(var1, var2)
IV <- factor(rep(c("var1", "var2"), c(length(var1), length(var2))))
id <- factor(rep(1:length(var1), 2))    # factor for participant
return(oneway_test(DV ~ IV | id, alternative="two.sided", distribution=approximate(B)))
}

## * paired permutation test (pedestrian)
pptest <- function (var1, var2, B=99999){
d <- var1-var2
m0 <- mean(d)
rndmdist <- replicate(B,mean((rbinom(length(d),1,.5)*2-1)*d))
p <-  sum(abs(rndmdist) >= abs(m0))/length(rndmdist)
result <- tibble(x = mean(var1), y = mean(var2), delta = x-y, pval = p)
return(result)
}




## * Vincentization
##  vincentize(dataframe,response time, accuracy, variables, quantiles)
## compute the means for response time and accuracy in dataframe,
## in each modalities of variables, by quantiles
##
## Ex: c <- vincentize(data,data$TR,data$ACC,list(Sujet=data$Sujet, Compatible=data$Compatible),c(0.2,0.4,0.6,0.8))
## compute the means for quantiles (0-0.2,0.2-0.4,0.4-0.6,0.6-0.8,0.8-1)
## for the variables data$TR and data$ACC
## for each Sujet, in each modalities of Compatible
##
## return new variables:
## q = quantile
## nq = proportion of trials within quantile
## rt_m = means of response time by quantile
## accuracy_m = means of accuracy time by quantile



vincentize <- function (dataframe, rt, accuracy, var, quantiles) {
  n <- length(quantiles)
  q <- paste('q', 1:n, sep='')
  b <- paste("b", 1:n,sep='')


  for (i in (1:n)) {
    b <- aggregate(list(q=rt),by=var,FUN = quantile, probs  = quantiles[i])
    names(b)[names(b)=="q"] <- paste('q',i,sep='')
    assign(paste('b',i,sep=''),b)
  }

  data1 <- dataframe
  data1$rt <- rt
  data1$accuracy <- accuracy

  for (i in (1:n)) {
    expr <- substitute(paste('merge(data1,','b',k,',all=TRUE,sort=FALSE)',sep=''),list(k=i))
    chaine <- eval(expr)
    expr_chaine <- parse(text = chaine)
    data1 <- eval(expr_chaine)
  }

  data1$q <- 0
  data1$q0 <-0

  for (i in (0:n-1)) {
    expr <- substitute(
      paste('data1$q[data1$rt>+data1$q',k,'] <-',k+1,sep=''),list(k=i))
    chaine <- eval(expr)
    expr_chaine <- parse(text = chaine)
    eval(expr_chaine)
  }



  data1$nq <- 0
  for (i in (1:n)) {
    data1$nq[data1$q==i] <- length(data1$q[data1$q==i])/length(data1$rt)
  }

  expr <- paste('list(')
  for (i in (1:length(var))){
    expr <- paste(expr,names(var[i]),'=','data1$',names(var[i]),',',sep='')
  }
  expr <- paste(expr,'q=data1$q)',sep='')
  chaine <- eval(expr)
  expr_chaine <- parse(text = chaine)
  var2 <- eval(expr_chaine)
  data_final <- aggregate(list(rt_m=data1$rt, accuracy_m=data1$accuracy,nq=data1$nq), by=var2, FUN = mean)

  return(data_final)
}

## * ELF
## **  Errors Location Function (ELF)
## Compute the distribution of errors RT among all response time
## L(p) = proportion of errors among the pth first quantile of response time (computed on all responses)

elf <- function (rt, accuracy, na.rm = TRUE) {
  p=(1:length(rt))/length(rt)
  errors <- rt[accuracy==0]
  correct <- rt[accuracy==1]
    if (length(correct) == length(rt)){ L <- NA }
  else{
  c_error <- ecdf(errors)
  L <- c_error(quantile(rt,probs = p, na.rm = na.rm))
  return(L)
  }
}


## ** response capture index
eli <- function (L, alpha = 1, na.rm = FALSE){
  n <- length(L)
  a <- round(alpha*n)
  LL <- L[1:a]
  V <- (1/n)*(1/alpha)*sum(LL, na.rm = na.rm)
  return(V)
}


## ** elf group

elf.g <- function(RT,accuracy,sujet, na.rm = FALSE){
        df <- data.frame(RT, accuracy, sujet)
        suj <- unique(sujet)
        ns <- length(suj)
        y <- 0
        for (s in suj) {
                d <- df[df$sujet == s,]
                x  <- elf(d$RT, d$accuracy) %>%  quantile(.,c(1:100)/100,  na.rm = na.rm)
                if (is.na(x[1])  == TRUE){
                    x <- 0
                    ns <- ns -1
                }
                y <- y+x       
                        }
        y <- c(0,y/ns)
        return(y)
}

## * graphs
## ** save graphs pdf
savepdf <- function(file, width=16, height=10)
{
  fname <- paste("~/thib/data/figures/",file,".pdf",sep="") #adapter l'emplacement
  pdf(fname, width=width/2.54, height=height/2.54,
      pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

## ** save graphs eps
saveps <- function(file, width=16, height=10)
{
  fname <- paste("~/thib/data/figures/",file,".eps",sep="") #adapter l'emplacement
  postscript(fname, width=width/2.54, height=height/2.54,
      pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

## ** plot posteriors from stan

## function to plot posteriors
## ex
## sample <- posterior_samples(conf.base.stan) %>%
##     rename('accuracy' = 'b_acc',
##            'force' = 'b_force.scale',
##            'rt' = 'b_rt.scale',
##            'congruent' = 'b_congruent',
##            'incongruent' = 'b_incongruent') %>%
##     select(accuracy, force, rt, congruent, incongruent)
## q.accuracy  <- post_plot(sample$accuracy) + xlab('Accuracy')

post_plot  <-  function (var){
    sample = as.data.frame(x = var)
    cond <- (var < quantile(var, .025) |var > quantile(var, .975))
    mean.var  <- mean(sample$var)
    max.var  <- max(sample$var)

    p <- ggplot(sample, aes(x = var)) +
        geom_histogram(data = subset(sample, cond == TRUE), color = 'black', fill = 'white') +
        geom_histogram(data = subset(sample,  cond == FALSE), color = 'black', fill= 'lightblue') +
        geom_vline(aes(xintercept = mean.var), color="blue", linetype="dashed", size=1) +
        scale_y_continuous(breaks = NULL) +
        labs(y = '') +
        theme(axis.line = element_line(color = 'black')) +
         geom_text(aes(x = mean.var+(max.var-mean.var)*.2, y =.8* max(hist(sample$var, breaks = 30)$count)
),
              label = paste('Mean', formatC(mean.var, format = "fg", digits = 2), sep = '\n'),
              color = 'blue', size = 4)
    return(p)
}

## * data fit
## Part d'un fichier (rt,accuracy,compatible) et génère un  pour les fits dans R:
## (comp,p.correct,p.error) avec:
## p.correct: (.1,.3,.5,.7,.9)
## p.error: NULL, .5, (.3,.5,.7), ou (.1,.3,.5,.7,.9) en fonction de N.error =0, < 5=, <10, >10
## les proportions sont exactes (ajustées pour le taux d'erreur)
datafit<- function(rt,accuracy,compatible)
{
N <- length(rt) # nombre d'essais
N.correct <- length(rt[accuracy == 1]) # nombre essais correct
p.correct <- N.correct/N # taux essais correct
N.error <- length(rt[accuracy == 0]) # nombre erreurs
p.error <- N.error/N # taux erreurs

# quantiles corrects
if (N.correct <=10){p1.correct <- 0}else{
p1.correct <- quantile(rt[accuracy == 1],probs=(0.1))}

if (N.correct <=5){p3.correct <- 0}else{
p3.correct <- quantile(rt[accuracy == 1],probs=(0.3))}

p5.correct <- quantile(rt[accuracy == 1],probs=(0.5))

if (N.correct  <=5){p7.correct <- 0}else{
p7.correct <- quantile(rt[accuracy == 1],probs=(0.7))}

if (N.correct <=10){p9.correct <- 0}else{
p9.correct <- quantile(rt[accuracy == 1],probs=(0.9))}


# quantiles erreurs
if (N.error <=10){p1.error <- 0}else{
p1.error <- quantile(rt[accuracy == 0],probs=(0.1))}

if (N.error <=5){p3.error <- 0}else{
p3.error <- quantile(rt[accuracy == 0],probs=(0.3))}

p5.error <- quantile(rt[accuracy == 0],probs=(0.5))

if (N.error  <=5){p7.error <- 0}else{
p7.error <- quantile(rt[accuracy == 0],probs=(0.7))}

if (N.error <=10){p9.error <- 0}else{
p9.error <- quantile(rt[accuracy == 0],probs=(0.9))}

dsim <- data.frame(N,N.correct, p.correct, p1.correct, p3.correct,p5.correct,p7.correct,p9.correct,N.error,p.error,
                   p1.error,p3.error, p5.error, p7.error, p9.error)
    
return(dsim)
}


## * import accuracy-rt data from csv
## importe les fichiers csv au format (RT_correct, RT_error)
## génère un fichier : (accuracy, RT)

import <- function(correct,errors)
{
errors <- read.csv(file=errors,  sep = ";", header=FALSE)
errors <- errors$V1
correct <- read.csv(file=correct,  sep = ";", header=FALSE)
correct <- correct$V1
N.errors <- rep(0, length(errors))
N.correct <- rep(1, length(correct))
accuracy <- append(N.errors,N.correct)
RT <- append(errors,correct)
data <- data.frame(accuracy,RT)
return(data)
}

## * fits
## ** Miscellaneous functions 

########################################################

## provides quantiles depending on n (to ajust to the number of observations)
quant <- function(rt,n){
    if (n == 0){q <- 0}
    if (n == 1){q <- quantile(rt, probs = .5)}
    if (n == 3){q <- quantile(rt, probs = c(.3,.5,.7))}
    if (n == 5){q <- quantile(rt, probs = c(.1,.3,.5,.7,.9))}
    return(q)
}

## provides bins depending on n (to ajust to the number of observations)
prop <- function(n){
    if (n == 0){q <- 0}
    if (n == 1){q <-  c(.5,.5)}
    if (n == 3){q <-  c(.3,.2,.2,.3)}
    if (n == 5){q <-  c(.1,.2,.2,.2,.2,.1)}
    return(q)
}

## compute bins corresponding to quantiles p
bins <- function(p){
    p <- unlist(p)
    k = length(p)
    if (k == 1){
        bins <- c(pmax(p[1], 0.0001),pmax(1-p[1],0.0001))
    }
    else
    {
    bins <- c(pmax(p[1],0.0001),2:k-1,pmax(1-p[k],0.0001))
    for (i in (2:k)){
        bins[i] <- pmax(p[i]-p[i-1], 0.0001)   
    }
    }    
    return(bins)
}

## ** Miscellaneous functions fit Simon

## prepare data for fit_DMC()
## comp: compatible = c, i
## acc: accuracy = 0, 1
## rt: response time (ms)
prepare_data <- function(comp,acc,rt){
    data <- tibble(comp = comp, acc = acc, rt = rt) # create tibble

    ## compute the rt quantiles for each condition/accuracy
    ## the number of quantiles depends of the number of observation in each cell
    qt <- data %>%
        ## add n = number of each observation class
        ## (eg, n for comp = c and acc = 1 gives the number of correct compatible trials) 
        group_by(comp,acc) %>%
        add_tally() %>%
        ## number of bins for each condition/accuracy: 5 if n>10, 3 if 10>n>5, 1 if n>0, 0 if n=0)
        mutate(r = ifelse(n>10, 5, ifelse(n>5, 3, ifelse(n>0, 1, 0)))) %>%
        group_by(comp,acc) %>%
        nest(rt,r) %>%
        ## compute the quantiles
        ## for each cond/acc: data$rt = response time, data$r = number of bins (constant for each cond/acc)
        mutate(quant.data = map(data, ~ quant(.$rt, .$r[1]))) %>%
        select(-data)

    ## compute the bins for each condition/accuracy
    b <- data %>% 
        select(-rt) %>%
        ## add n = number of each observation class
        ## (eg, n for comp = c and acc = 1 gives the number of correct compatible trials)
        group_by(comp,acc) %>%
        add_tally() %>%
        ## number of bins for each condition/accuracy: 5 if n>10, 3 if 10>n>5, 1 if n>0, 0 if n=0)
        mutate(r = ifelse(n>10, 5, ifelse(n>5, 3, ifelse(n>0, 1, 0)))) %>%
        group_by(comp,acc) %>%
        nest(r) %>%
        ## create bins for each condition/accuracy
        ## if r = 5, bins.data = (.1,.2,.2,.2,.2,.1)
        ## if r = 3, bins.data = (.3,.2,.2,.3)
        ## if r = 1, bins.data = (.5,.5)
        ## if r = 0, bins.data = 0
        mutate(bins.data  = map(data, ~ prop(.$r[1]))) %>%
        select(-data)

    ## freq.data = proportion of correct/incorrect within each condition (sum to 1 for each cond)
    ## n = number of obs in each cell condition/acc
    ## n.comp = number of obs in each condition (compatible/incompatible)
    c <- data %>%
        ## add n.comp = number of each observation class compatible
        group_by(comp) %>%
        add_tally()  %>%
        rename(n.comp = n) %>%
        ## add n = number of each observation class acc/compatible
        group_by(comp, acc) %>%
        add_tally()  %>%
        summarise(n = n(), n.comp = mean(n.comp))  %>%
        ## proportion of correct/incorrect within each compatiblity condition
        ##(sum to 1 for each compatibility cond)
        mutate(freq.data = pmax(n / n.comp, 0.0001))
    return(list(quant.data = qt, bins.data = b, count.data = c))    
}


###################
## create bins sim, merge with bins data and compute G2
## G2 is computed separately for each condition (compatible and incompatible)
## Output = proportion(compatible trial in observed data)*G2(comp)
##        +  proportion(incompatible trial in observed data) G2(incomp)

fun_g2 <- function(data,sim){
    gsquare <- 2 * sum(data*log(data/sim))
    return(gsquare)
}
g2 <- function(sim, d){
    ## avoid division by 0: add fake trial if no error/success
    if (mean(sim[sim$comp == 'c',]$acc) == 1)
    {
        sim  <- add_row(sim, rt = 0, acc = 0, comp = 'c')
    }
    if (mean(sim[sim$comp == 'i',]$acc) == 1)
    {
        sim  <- add_row(sim, rt = 0, acc = 0, comp = 'i')
    }
    if (mean(sim[sim$comp == 'c',]$acc) == 0)
    {
        sim <- add_row(sim, rt = 0, acc = 1, comp = 'c')
    }
    if (mean(sim[sim$comp == 'i',]$acc) == 0)
    {
        sim  <- add_row(sim, rt = 0, acc = 1, comp = 'i')
    }

    ## freq.sim = proportion of correct/incorrect within each condition (sum to 1 for each cond) in sim data
    s.count <- prepare_data(sim$comp,sim$acc,sim$rt)$count.data %>%
                                                   select(acc, comp, freq.data) %>%
                                                   rename(freq.sim = freq.data)
    
    
    s <- sim %>%
        ## compute the rt cdf in sim data corresponding to
        ## the quantiles in observed data
        ## eg: if the 10% quantile in data is x ms, what is the probability that simulated RT < x ?
        group_by(comp,acc) %>%
        inner_join(d$quant.data, by = c('comp','acc')) %>%
        nest() %>%
        mutate(z = map(data, ~ ecdf(.$rt)(unlist(.$quant.data[1])))) %>%
        select(-data)%>%
        nest(z) %>%
        ## compute the appropriate bins for simulated data
        ## in general, 6 bins will be obtained
        mutate(bins.sim = map(data, ~ bins(.$z))) %>%
        select(-data) %>%
        ## include the bins for observed data
        inner_join(d$bins.data,  by = c('comp','acc'))%>%
        ## include number and frequency of simulated data
        inner_join(s.count, by = c('comp', 'acc')) %>%
        ## include number and frequency of observed data
        inner_join(d$count.data, by = c('comp', 'acc')) %>%
        nest(bins.sim, bins.data, freq.sim, freq.data)  %>%
        ## compute proportion of observed and simulated data in each cell
        mutate(bins.data = map(data, ~ unlist(.$bins.data) * .$freq.data))%>%
        mutate(bins.sim = map(data, ~ unlist(.$bins.sim) * .$freq.sim))%>%
        select(-data) %>%
        nest(bins.data, bins.sim) %>%
        ## apply G2 function
        mutate(g2 = map(data, ~ fun_g2(unlist(.$bins.data), unlist(.$bins.sim)))) %>%
        ## weighted  G2 in conditions comp and incomp by proportion of
        ## comp and inncomp in data
        mutate(g2 = unlist(g2) * n.comp)

    ## eventually, sum accross conditions compatible and incompatible
    g2 <- sum(s$g2, na.rm = TRUE)
    return(g2)

}

## ** Miscellaneous functions fit DDM

prepare_data_DDM <- function(acc,rt){
    data <- tibble(acc = acc, rt = rt) # create tibble

    ## compute the rt quantiles for each accuracy
    ## the number of quantiles depends of the number of observation in each cell
    qt <- data %>%
        ## add n = number of each observation class
        ## (eg, n for comp = c and acc = 1 gives the number of correct compatible trials) 
        group_by(acc) %>%
        add_tally() %>%
        ## number of bins for each condition/accuracy: 5 if n>10, 3 if 10>n>5, 1 if n>0, 0 if n=0)
        mutate(r = ifelse(n>10, 5, ifelse(n>5, 3, ifelse(n>0, 1, 0)))) %>%
        group_by(acc) %>%
        nest(rt,r) %>%
        ## compute the quantiles
        ## for each acc: data$rt = response time, data$r = number of bins
        mutate(quant.data = map(data, ~ quant(.$rt, .$r[1]))) %>%
        select(-data)

    ## compute the bins for each accuracy
    b <-  data %>% 
        select(-rt) %>%
        ## add n = number of each observation class
        ## (eg, n  acc = 1 gives the number of correct  trials)
        group_by(acc) %>%
        add_tally() %>%
        ## number of bins for each accuracy: 5 if n>10, 3 if 10>n>5, 1 if n>0, 0 if n=0)
        mutate(r = ifelse(n>10, 5, ifelse(n>5, 3, ifelse(n>0, 1, 0)))) %>%
        group_by(acc) %>%
        nest(r) %>%
        ## create bins for each accuracy
        ## if r = 5, bins.data = (.1,.2,.2,.2,.2,.1)
        ## if r = 3, bins.data = (.3,.2,.2,.3)
        ## if r = 1, bins.data = (.5,.5)
        ## if r = 0, bins.data = 0
        mutate(bins.data  = map(data, ~ prop(.$r[1]))) %>%
        select(-data)

    c <- data %>%
        ## number of total trials
        add_count() %>%
        rename(n.tot = n) %>%
        ## number of correct and incorrect
        group_by(acc) %>%
        add_count(acc) %>%
        ## frequence correct and incorrect
        mutate(freq.data =  pmax(n / n.tot, 0.0001)) %>%
        summarise(freq.data = mean(freq.data))
    
   return(list(quant.data = qt, bins.data = b, count.data = c)) 

}

g2_DDM <- function(params, d){
    s <- d$quant.data %>%
        ## create response "upper" and "lower" for pdiffusion
        mutate(response = ifelse(acc == 1, 'upper', 'lower')) %>%
        ## compute freq.sim =  proportion of correct and incorrect in simulated data
        rowwise() %>%
        mutate(freq.sim = pdiffusion(Inf,
                                   a = params['a'],
                                   v = params['v'],
                                   z = ifelse(is.na(params['z']) == TRUE, .5*params['a'], params['z']*params['a']), 
                                   sz = ifelse(is.na(params['sz']) == TRUE, 0, params['sz']),
                                   sv =ifelse(is.na(params['sz']) == TRUE, 0, params['sv']),
                                   t0 = params['t0'],
                                   st0 = params['st0'],
                                   response = response)) %>%
        ## avoid division par zero
        mutate(freq.sim = ifelse(freq.sim == 0, .0001, freq.sim)) %>%
        mutate(freq.sim = ifelse(freq.sim == 1, .9999, freq.sim)) %>%
        ## compute the rt cdf in sim data corresponding to
        ## the quantiles in observed data
        ## eg: if the 10% quantile in data is x ms, what is the probability that simulated RT < x ?
        mutate(ppmax = freq.sim) %>%
        nest(-c(acc,freq.sim)) %>%
        mutate(q = map(data, ~(1/.$ppmax)*pdiffusion(.$quant.data[[1]],
                                   a = params['a'],
                                   v = params['v'],
                                   z = ifelse(is.na(params['z']) == TRUE, .5*params['a'], params['z']*params['a']), 
                                   t0 = params['t0'],
                                   sz = ifelse(is.na(params['sz']) == TRUE, 0, params['sz']),
                                   sv =ifelse(is.na(params['sz']) == TRUE, 0, params['sv']),
                                   st0 = params['st0'],
                                   response = .$response))) %>%
    select(-data) %>%
    nest(q) %>%
    ## compute the appropriate bins for simulated data
    ## in general, 6 bins will be obtained
    mutate(bins.sim = map(data, ~ bins(.$q))) %>%
    select(-data) %>%
    ## include the bins for observed data
    inner_join(d$bins.data, by = 'acc') %>%
    ## include number and frequency of observed data
    inner_join(d$count.data, by = 'acc') %>%
    nest(bins.sim, bins.data, freq.sim, freq.data) %>%
    ## compute proportion of observed and simulated data in each cell
    mutate(bins.data = map(data, ~ unlist(.$bins.data) * .$freq.data))%>%
    mutate(bins.sim = map(data, ~ unlist(.$bins.sim) * .$freq.sim))%>%
    select(-data) %>%
    nest(bins.data, bins.sim) %>%
    ## apply G2 function
    mutate(g2 = map_dbl(data, ~ fun_g2(unlist(.$bins.data), unlist(.$bins.sim)))) 
    g2 <- sum(s$g2, na.rm = TRUE)
    return(g2)
}


## ** LCA
# This is a wrapper function (and tester) to call the simulateLCA function built in C.

LCA <- function(I, kappa, beta, Z, NDT, nTrials=1000, s=.1, dt=.001, maxT=5, nonLinear=TRUE, x0=NULL) {
  # Function to simulate the Leaky, Competing Accumulator (LCA, Usher & McClelland, 2001) model
  # Parameters #
  # I (double vector): Vector of input for every accumulator. E.g., c(1.2, 1, 1) simulates 3 accumulators, with inputs 1.2, 1, 1, respectively.
  # kappa (double): leakage
  # beta (double): inhibition
  # Z (double): threshold of accumulation
  # nTrials (integer): Number of trials to simulate.
  # s (double): Variance of Gaussian noise. Defaults to 0.1 (as in Miletic et al. 2017).
  # dt (double): temporal resolution of simulation. Defaults to 0.001, corresponding to a milisecond resolution.
  # maxT (double): maximum time to simulate. Defaults to 5, corresponding to 5 seconds of decision time.
  # nonLinear (bool): if TRUE, simulates the LCA with the non-linearity included in the original paper. If FALSE, the non-linearity is ommitted and negative accumulator values are allowed. Defaults to TRUE.
  # x0 (double vector): Vector of start point values for every accumulator. Defaults to NULL, which sets all start points to 0.

  # Check whether the NDT is larger than 1. If so, assume that the NDT is given in miliseconds, and divide by 1000 to convert to s.
  if(NDT > 1) {
  	warning(paste0("The non-decision time provided is larger than 1. I'll assume you meant ", NDT, " miliseconds"))
  	NDT <- NDT/1000
  }

  # Check whether x0 is NULL. If so, sets x0 to 0. If x0 is provided, check whether the length of x0 and I are the same.
  if(is.null(x0)) {
  	x0 <- rep(0, length(I))
  } else if(length(x0) != length(I)) {
  	stop('The number of accumulators in I is not the same as the number of accumulators in x0')
  }

  # Determine maximum number of time steps to simulate
  maxiter <- length(seq(0, maxT, by=dt))

  # Create empty numeric vectors for the responses and rts
  rts <- resps <- numeric(nTrials)

    ## Call C-code
  dyn.load('~/thib/projects/lcaC.so') ## librairie C
    
  out = .C('simulateLCA',
           nAcc = as.integer(length(I)), # Pass the number of accumulators to C. All R objects are passed as *pointers*, which makes it surprisingly difficult to obtain the length of the array in C itself.
           I = as.double(I),
           kappa = as.double(kappa),
           beta = as.double(beta),
           Z = as.double(Z),
           s = as.double(s),
           dt = as.double(dt),
           maxiter = as.integer(maxiter),
           resp = as.integer(resps),
           rt = as.double(rts),
           nTrials = as.integer(nTrials),
           nonLinear = as.integer(as.logical(nonLinear)),
           x0 = as.double(x0))  
    
  # Structure output dataframe
  dat <- list(out$rt, out$resp)
  dat <- as.data.frame(dat)
  colnames(dat) <- c('rt','response')

  # Check which answers are correct
  dat$corr <- dat$response == 1

  # Add non-decision time to rt
  dat$rt = dat$rt+NDT

  # Round reaction times to 3 decimals
  dat$rt <- round(dat$rt, 3)
  return(dat)
}

testLCAsim <- function() {
	# Small function to test whether the LCA simulation works properly. Simulates two datasets and plots the data.

	# Simulate some data
	dat1 <- LCA(nTrials=1000, I=c(1.2, 1, 1), kappa=3, beta=3, Z=.2, NDT=.450, s=.1, dt=.001, maxT=5, nonLinear=TRUE, x0=c(.01, .02, .03))
	dat2 <- LCA(nTrials=1000, I=c(1.2, 1, 1), kappa=3, beta=3, Z=.2, NDT=.450, s=.1, dt=.001, maxT=5, nonLinear=FALSE, x0=c(0.01, .02, .03))

	print('Succesfully simulated two sets of data. Example data:')
	print(head(dat1))

	# Plot densities
	par(mfrow=c(1,2))
	plot(density(dat1$rt[dat1$corr == TRUE]), main='', xlab='nTrials = 1000')
	title('Simulated 3 accumulators with non-linearity', sub='I=c(1.2, 1, 1), k=3, b=3, Z=.2, NDT=.450')
	lines(density(dat1$rt[dat1$corr == FALSE]), col='red')

	plot(density(dat2$rt[dat2$corr == TRUE]), main='', xlab='nTrials = 1000')
	title('Simulated 3 accumulators without non-linearity', sub='I=c(1.2, 1, 1), k=3, b=3, Z=.2, NDT=.450')
	lines(density(dat2$rt[dat2$corr == FALSE]), col='red')
	print('Everything seems OK!')
}


## ** DMC (seconds)
## *** simulate DMC (seconds)
## Simulation DMC (Nathan)
sim.DMCs <- function(params, N = 8e5){
    params.co <- params
    params.in <- params
    params.in['gamma'] <-  - params.co['gamma']
    sim.co <- simulate.DMCs(params=params.co, N) %>%
        as.tibble() %>%
        mutate(acc = 2 - resp) %>%
        mutate(comp = 'c') 
    sim.in <- simulate.DMCs(params=params.in, N) %>%
        as.tibble() %>%
        mutate(acc = 2-resp) %>%
        mutate(comp = 'i' )
    sim <- bind_rows(sim.co,sim.in) %>%
        select(-resp) %>%
        mutate(comp = as.factor(comp))
    return(sim)
}

simulate.DMCs=function(params, N = 8e5) {
    z = ifelse(is.na(params['z']), 0, params['z'])
    v = params['v']
    sv = ifelse(is.na(params['sv']), 0, params['sv'])
    a = params['a']
    ter = params['ter']
    ster =  ifelse(is.na(params['ster']), 0, params['ster'])
    sz =  ifelse(is.na(params['sz']), 0, params['sz'])
    gamma = params['gamma']
    alpha = ifelse(is.na(params['alpha']), 2, params['alpha'])
    tau = params['tau']
    stoch.s =  ifelse(is.na(params['stoch.s']), 4/sqrt(1000), params['stoch.s'])
    
    dyn.load('~/thib/projects/tools/DMC.so') ## librairie C
    use.interval= 0.0001
    stepSize =  1/1000
    maxCounter = 20000
    use.table=qnorm(seq(use.interval,1-use.interval,use.interval))
    n.table.options=length(use.table)
    rts=rep(0,N)
    resps=rep(0,N)
  
tmp=.C("DMC",z=z,v=v,
         aU=a,aL=-a,ter=ter,
         sv=sv,sz=sz,ster=ster,
         gamma=gamma,alpha=alpha,tau=tau,
         s=stoch.s,h=stepSize,resp=resps,rt=rts,
         n=N,maxiter=as.double(round(maxCounter)),
         rangeLow=as.integer(0),rangeHigh=as.integer(n.table.options-1),
         randomTable=as.double(use.table)) 
  
out=list(rt=tmp$rt,resp=tmp$resp)
}

## *** fit DMC  (seconds)

fit.de.DMCs <- function(d, N = 8e5, nc = 3){
    cl <- makeCluster(nc, type="FORK")
    registerDoParallel(cl)

   ##fct <- c('sim.DMCs', 'simulate.DMCs', 'g2', 'fun_g2', 'prepare_data', 'quant', 'prop', 'bins')
   ##  parVar = fct,
   ## package = c('tidyverse'),
   lower <- c(v=.05, a=15/1000, ter=20/1000, ster=0, sz = 0, gamma=.001/1000, alpha = 1.5, tau =5/1000)
   upper <- c(v= 2, a=150/1000, ter=600/1000, ster=300/1000, sz=70/1000, gamma=100/1000, alpha = 4.5, tau=500/1000)
   ##lower <- c(v=.05, a=15/1000, ter=20/1000, gamma=.001/1000, alpha = 1.5, tau =5/1000)
   ##upper <- c(v= 2, a=150/1000, ter=600/1000, gamma=100/1000, alpha = 4.5, tau=500/1000)
   out <- DEoptim(fn = fitstat.DMCs, lower, upper,
                  DEoptim.control(NP = 250, parallelType = 2,  cluster = cl, itermax = 800),
                  d = d, N = N)  ## NP = 10(#param)
   stopCluster(cl)
   
   return(out)
}


fitstat.DMCs <- function(params, d, N){
    p <- c('v' = params[1], 'a' = params[2], 'ter' = params[3], 'ster' = params[4],
           'sz' = params[5], 'gamma' = params[6], 'alpha' = params[7], 'tau' = params[8])
    ##     p <- c('v' = params[1], 'a' = params[2], 'ter' = params[3],  'gamma' = params[4], 'alpha' = params[5], 'tau' = params[6])
    sim <-sim.DMCs(p, N)
    g2 <- g2(sim, d)
    return(g2)
}


## ** KH (seconds)
## *** Simulate KH


simulate.KH.comp <- function(params, N = 4e4){
    a.couleur <- params['a.couleur']
    v.couleur  <- params['v.couleur']
    sz.couleur  <-ifelse(is.na(params['sz.couleur']) == TRUE, 0, params['sz.couleur'])
    a.inhib  <-params['a.inhib']
    v.inhib  <- params['v.inhib']
    sz.inhib  <- ifelse(is.na(params['sz.inhib']) == TRUE, 0, params['sz.inhib'])
    mt  <- params['mt']
    st.mt <- ifelse(is.na(params['st.mt']) == TRUE, 0, params['st.mt'])
    mu  <-  ifelse(is.na(params['mu']) == TRUE, 0, params['mu'])
    lambda  <- ifelse(is.na(params['lambda']) == TRUE, 20, params['lambda'])
    theta  <- ifelse(is.na(params['theta']) == TRUE, 0, params['theta'])
    f  <- ifelse(is.na(params['f']) == TRUE, 0, params['f'])

    x.couleur  <- rdiffusion(n = N, a = a.couleur, v = v.couleur, t0=0, z = .5*a.couleur, sz = sz.couleur*a.couleur, s = 1, stop_on_error = TRUE) %>%
        mutate(resp.couleur = as.character(response)) %>%
        mutate(rt.couleur = rt) %>%
        select(-c(response, rt))

    x.inhib  <-  rdiffusion(n = N, a = a.inhib, v = v.inhib, t0=0, z = .5 *a.inhib, sz = sz.inhib*a.inhib, s = 1, stop_on_error = TRUE) %>%
        mutate(resp.inhib = ifelse(as.character(response) == 'upper', 'stop', 'go')) %>%
        mutate(rt.inhib = rt) %>%
        select(-c(response, rt))

    d  <- bind_cols(x.couleur, x.inhib) %>%
        mutate(mt = runif(n(), mt-st.mt*mt/2, mt+st.mt*mt/2)) %>%
        mutate(t.inhib = case_when(
                   rt.inhib < rt.couleur ~ pmax(rt.inhib + mu - rt.couleur,0),
                   rt.inhib > rt.couleur & rt.inhib <rt.couleur + mt  ~ mu,
                   rt.inhib > rt.couleur + mt ~ 0))  %>%
        mutate(resp = case_when(
                   resp.inhib == 'go' ~ 'upper',
                   resp.inhib == 'stop' & rt.inhib < rt.couleur + mt ~ resp.couleur,
                   resp.inhib == 'stop' & rt.inhib > rt.couleur + mt ~ 'upper')) %>%
        mutate(rt = case_when(
                   resp.inhib == 'go' & rt.inhib < rt.couleur ~ rt.inhib + pmax(mt - f, 0),
                   resp.inhib == 'go' & rt.inhib > rt.couleur ~ rt.couleur + pmax(mt - f, 0),
                   resp.inhib == 'stop' & rt.inhib < rt.couleur & resp.couleur == 'upper' ~ rt.couleur +  pmax(mt - f, 0),
                   resp.inhib == 'stop' & rt.inhib < rt.couleur & resp.couleur == 'lower' ~ rt.couleur + t.inhib + mt,
                   resp.inhib == 'stop' & rt.inhib > rt.couleur & rt.inhib < rt.couleur + mt & resp.couleur == 'upper' ~ rt.couleur +  pmax(mt - f, 0),
                   resp.inhib == 'stop' & rt.inhib > rt.couleur & rt.inhib < rt.couleur + mt & resp.couleur == 'lower' ~ rt.inhib + t.inhib + mt,
                   resp.inhib == 'stop' & rt.inhib > rt.couleur + mt ~ rt.couleur +  pmax(mt - f, 0))) %>%
        mutate(comp = 'c') %>%
        mutate(acc = ifelse(resp == 'upper', 1, 0))   
    return(d)
}

simulate.KH.incomp <- function(params, N = 4e4){
    a.couleur <- params['a.couleur']
    v.couleur  <- params['v.couleur']
    sz.couleur  <-ifelse(is.na(params['sz.couleur']) == TRUE, 0, params['sz.couleur'])
    a.inhib  <-params['a.inhib']
    v.inhib  <- params['v.inhib']
    sz.inhib  <- ifelse(is.na(params['sz.inhib']) == TRUE, 0, params['sz.inhib'])
    mt  <- params['mt']
    st.mt <- ifelse(is.na(params['st.mt']) == TRUE, 0, params['st.mt'])
    mu  <-  ifelse(is.na(params['mu']) == TRUE, 0, params['mu'])
    lambda  <- ifelse(is.na(params['lambda']) == TRUE, 20, params['lambda'])
    theta  <- ifelse(is.na(params['theta']) == TRUE, 0, params['theta'])
    f  <- ifelse(is.na(params['f']) == TRUE, 0, params['f'])


    x.couleur  <- rdiffusion(n = N, a = a.couleur, v = v.couleur,  t0=0, z = .5*a.couleur, sz = sz.couleur*a.couleur, s = 1, stop_on_error = TRUE) %>%
        mutate(resp.couleur = as.character(response)) %>%
        mutate(rt.couleur = rt) %>%
        select(-c(response, rt))

    x.inhib  <-  rdiffusion(n = N, a = a.inhib, v = v.inhib, t0=0, z = .5*a.inhib,  sz = sz.inhib*a.inhib, s = 1, stop_on_error = TRUE) %>%
        mutate(resp.inhib = ifelse(as.character(response) == 'upper', 'stop', 'go')) %>%
        mutate(rt.inhib = rt) %>%
        select(-c(response, rt))

    d  <- bind_cols(x.couleur, x.inhib) %>%
        mutate(mt = runif(n(), mt-st.mt*mt/2, mt+st.mt*mt/2)) %>%
       mutate(t.inhib = case_when(
                   rt.inhib < rt.couleur ~ pmax(rt.inhib + mu - rt.couleur,0),
                   rt.inhib > rt.couleur & rt.inhib <rt.couleur + mt  ~ mu,
                   rt.inhib > rt.couleur + mt ~ 0))  %>%
        mutate(resp = case_when(
                   resp.inhib == 'go' ~ 'lower',
                   resp.inhib == 'stop' & rt.inhib < rt.couleur + mt ~ resp.couleur,
                   resp.inhib == 'stop' & rt.inhib > rt.couleur + mt ~ 'lower')) %>%
        mutate(rt = case_when(
                   resp.inhib == 'go' & rt.inhib < rt.couleur ~ rt.inhib +  pmax(mt - f, 0),
                   resp.inhib == 'go' & rt.inhib > rt.couleur ~ rt.couleur +   pmax(mt - f, 0),
                   resp.inhib == 'stop' & rt.inhib < rt.couleur & resp.couleur == 'upper' ~ rt.couleur + t.inhib + mt,
                   resp.inhib == 'stop' & rt.inhib < rt.couleur & resp.couleur == 'lower' ~ rt.couleur + pmax(mt-f,0),
                   resp.inhib == 'stop' & rt.inhib > rt.couleur & rt.inhib < rt.couleur + mt & resp.couleur == 'lower' ~ rt.couleur + pmax(mt-f,0),
                   resp.inhib == 'stop' & rt.inhib > rt.couleur & rt.inhib < rt.couleur + mt & resp.couleur == 'upper' ~ rt.inhib + t.inhib + mt,
                   resp.inhib == 'stop' & rt.inhib > rt.couleur + mt ~ rt.couleur +  pmax(mt - f, 0))) %>%
        mutate(comp = 'i') %>%
        mutate(acc = ifelse(resp == 'upper', 1, 0))
          
    return(d)
}


### Simulate data KH
sim.KH <- function(params, N = 4e4){
    sim.co <- simulate.KH.comp(params = params, N = N)
    sim.in <- simulate.KH.incomp(params = params, N = N)
    sim <- bind_rows(sim.co,sim.in)
    return(sim)
}

 
## *** fit KH
fit.de.KH <- function(d, N = 4e4, nc = 12){
    cl <- makeCluster(nc, type="FORK")
    registerDoParallel(cl)
   lower <- c(a.couleur = .1,
              v.couleur = .1,
              sz.couleur = 0,
              a.inhib = .1,
              v.inhib = .1,
              mt = .1,
              st.mt = 0,
              mu = 0
              )
    
   upper <- c(a.couleur = 8,
              v.couleur = 8,
              sz.couleur = .5,
              a.inhib = 8,
              v.inhib = 8,
              mt = .4,
              st.mt = .5,
              mu = .5
              )
   
   out <- DEoptim(fn = fitstat.KH, lower, upper,
                  DEoptim.control(NP = 150, parallelType = 2, itermax = 800, cluster = cl),
                  d = d, N = N)  ## NP = 10(#param)
   stopCluster(cl)
   return(out)
}


## fit.simplex.KH <- function(d, N = 8e4){
##     a  <- runif(1, .1, 5)
##     v.couleur  <- runif(1, .1, 5)
##     v.localisation  <- runif(1, .1, 5)
##     sz  <- a/2
##     t  <- runif(1, 0, 1)
##     st  <- t/2
##     lambda  <- runif(1, 0, 1)
##     theta  <- runif(1, 0, 1)
##    params <- c(a=a, v.couleur=v.couleur, v.localisation=v.localisation, sz=sz, t=t, st = .1, lambda = lambda, theta = theta, mu = mu)
##     out <- optim(par=params, fn=fitstat.KH, N=N, d=d, method = 'Nelder-Mead',  control = list( trace = 1))
##    return(out)
## }

fitstat.KH <- function(params, N, d){
    p  <- c(a.couleur = params[1],
            v.couleur = params[2],
            sz.couleur = params[3],
            a.inhib = params[4],
            v.inhib = params[5],
            mt = params[6],
            st.mt = params[7],
            mu = params[8]
            )
    
    ##sim <- sim.KH(p, N)
    g2.val  <- tryCatch({g2(sim.KH(p, N),d)}, error=function(cond) {return(.Machine$double.xmax)})
    g2.val  <- ifelse(is.nan(g2.val) == TRUE | is.na(g2.val) == TRUE, .Machine$double.xmax, g2.val)
    return(g2.val)
}

## ** fit DDM
fit.de.DDM <- function(d, N = 4e4, nc = 12){
    cl <- makeCluster(nc, type="FORK")
    registerDoParallel(cl)

    lower <- c(a = .1,
              v = .1,
              z = .01,
              sz = 0,
              sv = 0,
              t0 = .1,
              st0 = 0
              )
    
   upper <- c(a = 8,
              v = 8,
              z = .8,
              sz = .5,
              sv = 1,
              t0 = 2,
              st0 = .5
              )
   
   out <- DEoptim(fn = fitstat.DDM, lower, upper,
                  DEoptim.control(NP = 150, parallelType = 2, itermax = 10, cluster = cl),
                  d = d, N = N)  ## NP = 10(#param)
    
   stopCluster(cl)
   return(out)
}

fitstat.DDM <- function(params, N, d){
    p  <- c(a = params[1],
            v = params[2],
            z = params[3],
            sz = params[4],
            sv = params[5],
            t0 = params[6],
            st0 = params[7]
            )
    g2.val  <-  tryCatch({g2_DDM(p,d)}, error=function(cond) {return(.Machine$double.xmax)})
    g2.val  <- ifelse(is.nan(g2.val) == TRUE | is.na(g2.val) == TRUE, .Machine$double.xmax, g2.val)
    return(g2.val)
}

## ** KH prob
## *** Simulate KH prob

simulate.KHprob.comp <- function(params, N = 4e4){
    a.couleur <- params[1]
    v.couleur <- params[2]
    sz.couleur <- params[3]
    tm <- params[4]
    st.tm  <- params[5]
    t.localisation  <- params[6]
    st.localisation  <- params[7]
    theta <- params[8]
    mu  <- params[9]

    ## Accumulateur couleur + localisation
    x.couleur <- rdiffusion(n = N, a = a.couleur, v = v.couleur, t0=0, z = .5*a.couleur, sz = sz.couleur, s = 1, stop_on_error = FALSE)  %>%
        mutate(rt.couleur = rt,
               mt.couleur = runif(n(), tm-st.tm/2, tm+st.tm/2),
               resp.couleur = as.character(response)) %>%
        select(-c(rt, response)) %>%
        mutate(rt.localisation = runif(n(),t.localisation-st.localisation/2,t.localisation+st.localisation/2)) %>%
        mutate(mt.localisation = runif(n(),tm-st.tm/2,tm+st.tm/2)) %>%
        mutate(rt.diff = rt.couleur - rt.localisation) %>%
        mutate(inhib = ifelse(rt.diff<=0, 0, rbinom(1, 1, (rt.diff^mu)/(rt.diff^mu + theta^mu))))
    
     
    d  <- x.couleur %>%
        ## Response
        mutate(resp = case_when(
                   ## case 1
                   rt.couleur <= rt.localisation ~ resp.couleur,
                   ## case 2
                   rt.localisation + mt.localisation < rt.couleur ~ 'upper',
                   ## case 3
                   rt.couleur > rt.localisation &  rt.couleur <= rt.localisation+ mt.localisation ~ resp.couleur,
                  )) %>%
        ## RT
        mutate(rt = case_when(
                   ## case 1
                   rt.couleur <= rt.localisation ~ rt.couleur + mt.couleur,
                   ## case 2
                   rt.localisation + mt.localisation < rt.couleur ~ rt.localisation + mt.localisation,
                   ## case 3
                   rt.couleur > rt.localisation &  rt.couleur <= rt.localisation+ mt.localisation &  resp.couleur == 'lower' ~  rt.couleur + mt.couleur, 
                   ## case 4
                   rt.couleur > rt.localisation &  rt.couleur <= rt.localisation+ mt.localisation & resp.couleur == 'upper' ~ rt.localisation + mt.localisation)) %>%
        mutate(acc = ifelse(resp == 'upper', 1, 0)) %>%
        mutate(comp = 'c') 
    return(d)
}

simulate.KHprob.incomp <- function(params, N = 4e4){
    a.couleur <- params[1]
    v.couleur <- params[2]
    sz.couleur <- params[3]
    tm <- params[4]
    st.tm  <- params[5]
    t.localisation  <- params[6]
    st.localisation  <- params[7]
    theta <- params[8]
    mu  <- params[9]

    ## Accumulateur couleur + localisation
    x.couleur <- rdiffusion(n = N, a = a.couleur, v = v.couleur, t0=0, z = .5*a.couleur, sz = sz.couleur, s = 1, stop_on_error = FALSE)  %>%
        mutate(rt.couleur = rt,
               mt.couleur = runif(n(), tm-st.tm/2, tm+st.tm/2),
               resp.couleur = as.character(response)) %>%
        select(-c(rt, response)) %>%
        mutate(rt.localisation = runif(n(),t.localisation-st.localisation/2,t.localisation+st.localisation/2)) %>%
        mutate(mt.localisation = runif(n(),tm-st.tm/2,tm+st.tm/2)) %>%
        mutate(rt.diff = rt.couleur - rt.localisation) %>%
        mutate(inhib = ifelse(rt.diff<=0, 0, rbinom(1, 1, (rt.diff^mu)/(rt.diff^mu + theta^mu))))
    
    d  <- x.couleur %>%
        ## Response
        mutate(resp = case_when(
                   ## case 1
                   rt.couleur <= rt.localisation ~ resp.couleur,
                   ## case 2
                   rt.localisation + mt.localisation < rt.couleur ~ 'lower',
                   ## case 3
                   rt.couleur > rt.localisation &  rt.couleur <= rt.localisation+ mt.localisation & inhib == 1  ~  resp.couleur,
                   ## case 4
                   rt.couleur > rt.localisation &  rt.couleur <= rt.localisation+ mt.localisation & inhib == 0  ~  'lower'
               )) %>%
        ## RT
        mutate(rt = case_when(
                   ## case 1
                   rt.couleur <= rt.localisation ~ rt.couleur + mt.couleur,
                   ## case 2
                   rt.localisation + mt.localisation < rt.couleur ~ rt.localisation + mt.localisation,
                   ## case 3
                   rt.couleur > rt.localisation &  rt.couleur <= rt.localisation+ mt.localisation & inhib == 1 ~ rt.couleur + mt.couleur, 
                   ## case 4
                   rt.couleur > rt.localisation &  rt.couleur <= rt.localisation+ mt.localisation & inhib == 0 ~ rt.localisation + mt.localisation)) %>%
        mutate(acc = ifelse(resp == 'upper', 1, 0)) %>%
        mutate(comp = 'i')    
    return(d)
}


### Simulate data KH
sim.KHprob <- function(params, N = 4e4){
    sim.co <- simulate.KHprob.comp(params = params, N = N)
    sim.in <- simulate.KHprob.incomp(params = params, N = N)
    sim <- bind_rows(sim.co,sim.in)
    return(sim)
}

## *** fit KHprob
fit.de.KHprob <- function(d, N = 4e4, nc = 12){
   cl <- makeCluster(nc, type="FORK")
   lower <- c(a.couleur=.1,
              v.couleur=2,
              sz.couleur = 0,
              tm = 0,
              st.tm = 0,
              t.localisation=0,
              st.localisation = 0,
              theta = 0.01,
              mu = .01
              )
   upper <- c(a.couleur = 2,
              v.couleur=5,
              sz.couleur = 2,
              tm = .5,
              st.tm = .2,
              t.localisation=.5,
              st.localisation = .2,
              theta = 10,
              mu = 10)
   out <- DEoptim(fn = fitstat.KHprob, lower, upper,
                  DEoptim.control(NP = 130, parallelType = 2, itermax = 200, cluster = cl),
                  d = d, N = N)  ## NP = 10(#param)
   stopCluster(cl)
   return(out)
}


## fit.simplex.KH <- function(d, N = 8e4){
##     a  <- runif(1, .1, 5)
##     v.couleur  <- runif(1, .1, 5)
##     v.localisation  <- runif(1, .1, 5)
##     sz  <- a/2
##     t  <- runif(1, 0, 1)
##     st  <- t/2
##     lambda  <- runif(1, 0, 1)
##     theta  <- runif(1, 0, 1)
##    params <- c(a=a, v.couleur=v.couleur, v.localisation=v.localisation, sz=sz, t=t, st = .1, lambda = lambda, theta = theta, mu = mu)
##     out <- optim(par=params, fn=fitstat.KH, N=N, d=d, method = 'Nelder-Mead',  control = list( trace = 1))
##    return(out)
## }


fitstat.KHprob <- function(params, N, d){
    p  <- c('a.couleur'=params[1], 'v.couleur'=params[2], 'sz.couleur' = params[3], 'tm' = params[4],
            'st.tm' = params[5], 't.localisation'=params[6], 'st.localisation' = params[7],
            'theta'=params[8], 'mu'=params[9])
    sim <-sim.KH(p, N)
    params.ok <- ifelse(mean(sim[sim$comp == 'c',]$acc) == 1 | mean(sim[sim$comp == 'i',]$acc) == 1 |
                   mean(sim[sim$comp == 'c',]$acc) == 0 | mean(sim[sim$comp == 'i',]$acc) == 0 , 0, 1)
    g2 <- ifelse(params.ok == 1, g2(sim,d), .Machine$double.xmax)
    return(g2)
}

