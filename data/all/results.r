rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/motor_gabor/data/all/')
dir = '~/thib/projects/motor_gabor/data/'

## * accuracy
load(paste(dir,'MG1/fit_acc_MG1.rdata',sep=''))
acc1 <- l.acc
load(paste(dir,'MG2/fit_acc_MG2.rdata',sep=''))
acc2 <- l.acc
load(paste(dir,'MG3/fit_acc_MG3.rdata',sep=''))
acc3 <- l.acc
tab_model(acc1,acc2,acc3, file = "accuracy.html")

load(paste(dir,'MG1/fit_acc_bayes_MG1.rdata',sep=''))
acc_bayes1 <- fit_acc
load(paste(dir,'MG2/fit_acc_bayes_MG2.rdata',sep=''))
acc_bayes2 <- fit_acc
load(paste(dir,'MG3/fit_acc_bayes_MG3.rdata',sep=''))
acc_bayes3 <- fit_acc
tab_model(acc_bayes1,acc_bayes2,acc_bayes3, file = "accuracy_bayes.html")

## * rt
load(paste(dir,'MG1/fit_rt_MG1.rdata',sep=''))
rt1 <- l.rt
load(paste(dir,'MG2/fit_rt_MG2.rdata',sep=''))
rt2 <- l.rt
load(paste(dir,'MG3/fit_rt_MG3.rdata',sep=''))
rt3 <- l.rt
tab_model(rt1,rt2,rt3, file = "rt.html")

load(paste(dir,'MG1/fit_rt_bayes_MG1.rdata',sep=''))
rt_bayes1 <- fit.rt
load(paste(dir,'MG2/rt_bayes_MG2.rdata',sep=''))
rt_bayes2 <- fit.rt
load(paste(dir,'MG3/rt_bayes_MG3.rdata',sep=''))
rt_bayes3 <- fit.rt
tab_model(rt_bayes1,rt_bayes2,rt_bayes3, file = "rt_bayes.html")

## * confidence
load(paste(dir,'MG1/fit_conf_MG1.rdata',sep=''))
conf1 <- l.conf
load(paste(dir,'MG2/fit_conf_MG2.rdata',sep=''))
conf2 <- l.conf
load(paste(dir,'MG3/fit_conf_MG3.rdata',sep=''))
conf3 <- l.conf
tab_model(conf1,conf2,conf3, file = "confidence.html")

load(paste(dir,'MG1/fit_conf_bayes_MG1.rdata',sep=''))
conf_bayes1 <- fit_conf
load(paste(dir,'MG2/conf_MG2.rdata',sep=''))
conf_bayes2 <- fit_conf
load(paste(dir,'MG3/conf.rdata',sep=''))
conf_bayes3 <- fit_conf
tab_model(conf_bayes1,conf_bayes2,conf_bayes3, file = "conf_bayes.html")
