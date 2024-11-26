rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/motor_gabor/data/all/')
dir = '~/thib/projects/motor_gabor/data/'


#  ---------------------------------------------------
## LOAD DATA
# ---------------------------------------------------

load('../MG1/big_data_final.rda')
data_12 <- big_data %>%
    select(subject_id, rt_gabor, accuracy_gabor, conf, effector, participants, congruency) %>%
    rename(RT = rt_gabor, accuracy = accuracy_gabor, confidence = conf, experiment = participants)  %>%
    filter(experiment == 'mg1' | experiment == 'mg2')
load('../MG3/data_final_MG3.rda')
data_3 <- data %>% 
    filter(conf>0) %>%
    mutate(congruency = ifelse(laterality == "ipsilateral","congruent","incongruent")) %>%
    mutate(subject_id = subject_id + 60) %>%
    select(subject_id, rt_gabor, accuracy_gabor, conf, congruency) %>%
    rename(RT = rt_gabor, accuracy = accuracy_gabor, confidence = conf) %>%
    mutate(effector = 'NA', experiment = 'mg3')
data <- rbind(data_12, data_3) %>%
    mutate(experiment = case_when(experiment == 'mg1' ~ 1, experiment == 'mg2' ~ 2, experiment == 'mg3' ~ 3)) %>%
    mutate(experiment = as.factor(experiment))

# ---------------------------------------------------
#  BASIC STATS
# ---------------------------------------------------
d <- data %>%
    mutate(accuracy = as.numeric(accuracy) - 1) %>%    
    group_by(subject_id, experiment) %>%
    summarise(rt = mean(RT),
           accuracy = mean(accuracy),
           confidence = mean(confidence)) %>%
    ungroup() %>%
    group_by(experiment) %>%
    summarise(mean_rt = mean(rt), se_rt = sd(rt)/sqrt(n()),
           mean_accuracy = mean(accuracy), se_accuracy = sd(accuracy)/ sqrt(n()),
           mean_confidence = mean(confidence), se_confidence = sd(confidence)/ sqrt(n()))

kable(d)

## |experiment |   mean_rt|     se_rt| mean_accuracy| se_accuracy| mean_confidence| se_confidence|
## |:----------|---------:|---------:|-------------:|-----------:|---------------:|-------------:|
## |mg1        | 0.4604600| 0.0154526|     0.6758996|   0.0255973|        2.343040|     0.1196989|
## |mg2        | 0.7035694| 0.0182283|     0.7344898|   0.0143528|        2.507367|     0.1256065|
## |mg3        | 0.6824053| 0.0224481|     0.7320154|   0.0160592|        2.050573|     0.0738339|

# ---------------------------------------------------
## RT vs CONGRUENCY
# ---------------------------------------------------
## ## NB: CI représentent la dispersion des moyennes individuelles corrigés par la méthode de Cousnieau & Morey 
## (Morey, R. D. (2008). Confidence intervals from normalized data: A correction to Cousineau (2005). 
## Tutorials in quantitative methods for psychology, 4(2), 61-64.)

## STATS
d <- data %>%
    group_by(subject_id, experiment, congruency) %>%
    summarise(rt = mean(RT)) %>%
    ungroup() %>%
    group_by(experiment, congruency) %>%
    summarise(mean_rt = mean(rt), se_rt = sd(rt)/sqrt(n()))

kable(d)
## |experiment |congruency  |   mean_rt|     se_rt|
## |:----------|:-----------|---------:|---------:|
## |mg1        |congruent   | 0.4572602| 0.0158877|
## |mg1        |incongruent | 0.4642652| 0.0153168|
## |mg2        |congruent   | 0.6953642| 0.0198557|
## |mg2        |incongruent | 0.7129334| 0.0166909|
## |mg3        |congruent   | 0.6732324| 0.0226957|
## |mg3        |incongruent | 0.6935079| 0.0227064|

## PLOT

d <- data %>% 
    ## individual means
    group_by(experiment, subject_id, congruency) %>%
    summarise(rt = mean(RT)) %>%
    ungroup() %>%
    ## number of subjects
    group_by(experiment) %>%
    mutate(n_subj = n_distinct(subject_id)) %>% 
    ungroup() %>%
    ## grand means by experiment
    group_by(experiment) %>%
    mutate(grand_mean = mean(rt)) %>%
    ungroup() %>%
    ## individual grand means
    group_by(subject_id) %>%
    mutate(indiv_grand_mean = mean(rt)) %>%
    ungroup() %>%
    ## Cousineau-Morey CI
    group_by(experiment, congruency) %>%
    mutate(norm_rt = rt - indiv_grand_mean + grand_mean) %>%
    mutate(CI = sd(norm_rt)/sqrt(n_subj)*sqrt(2)*qt(.975,n_subj-1))  %>%
    summarise(rt = mean(rt), CI = mean(CI)) 

plot <- ggplot(data = d,  aes(x = experiment, y = rt, color = congruency)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) + 
    geom_pointrange(aes(ymin = rt-CI, ymax = rt+CI),position = position_dodge(width = 0.5))
ggsave('rt_congruency.svg', plot)

# ---------------------------------------------------
## CONFIDENCE vs ACCURACY
# ---------------------------------------------------
## ## NB: CI représentent la dispersion des moyennes individuelles corrigés par la méthode de Cousnieau & Morey 
## (Morey, R. D. (2008). Confidence intervals from normalized data: A correction to Cousineau (2005). 
## Tutorials in quantitative methods for psychology, 4(2), 61-64.)

## STATS
d <- data %>%
    group_by(subject_id, experiment, accuracy) %>%
    summarise(confidence = mean(confidence)) %>%
    ungroup() %>%
    group_by(experiment, accuracy) %>%
    summarise(mean_confidence = mean(confidence), se_confidence = sd(confidence)/sqrt(n()))

kable(d)
## | experiment|accuracy | mean_confidence| se_confidence|
## |----------:|:--------|---------------:|-------------:|
## |          1|0        |        1.675179|     0.1221330|
## |          1|1        |        2.627373|     0.1296369|
## |          2|0        |        2.059675|     0.1256574|
## |          2|1        |        2.654241|     0.1271250|
## |          3|0        |        1.512907|     0.0587721|
## |          3|1        |        2.226811|     0.0784138|

## PLOT

d <- data %>% 
    ## individual means
    group_by(experiment, subject_id, accuracy) %>%
    summarise(confidence = mean(confidence)) %>%
    ungroup() %>%
    ## number of subjects
    group_by(experiment) %>%
    mutate(n_subj = n_distinct(subject_id)) %>% 
    ungroup() %>%
    ## grand means by experiment
    group_by(experiment) %>%
    mutate(grand_mean = mean(confidence)) %>%
    ungroup() %>%
    ## individual grand means
    group_by(subject_id) %>%
    mutate(indiv_grand_mean = mean(confidence)) %>%
    ungroup() %>%
    ## Cousineau-Morey CI
    group_by(experiment, accuracy) %>%
    mutate(norm_confidence = confidence - indiv_grand_mean + grand_mean) %>%
    mutate(CI = sd(norm_confidence)/sqrt(n_subj)*sqrt(2)*qt(.975,n_subj-1))  %>%
    summarise(confidence = mean(confidence), CI = mean(CI)) 

plot <- ggplot(data = d,  aes(x = experiment, y = confidence, color = accuracy)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) + 
    geom_pointrange(aes(ymin = confidence-CI, ymax = confidence+CI),position = position_dodge(width = 0.5))
ggsave('confidence_accuracy.svg', plot)


# ---------------------------------------------------
## CONFIDENCE vs EFFECTOR
# ---------------------------------------------------
## ## NB: CI représentent la dispersion des moyennes individuelles corrigés par la méthode de Cousnieau & Morey 
## (Morey, R. D. (2008). Confidence intervals from normalized data: A correction to Cousineau (2005). 
## Tutorials in quantitative methods for psychology, 4(2), 61-64.)

## STATS
d <- data %>%
    filter(experiment != 3) %>%
    group_by(subject_id, experiment, effector) %>%
    summarise(confidence = mean(confidence)) %>%
    ungroup() %>%
    group_by(experiment, effector) %>%
    summarise(mean_confidence = mean(confidence), se_confidence = sd(confidence)/sqrt(n()))

kable(d)
01.10.2024|experiment |effector | mean_confidence| se_confidence|
|:----------|:--------|---------------:|-------------:|
|1          |other    |        2.265151|     0.1297185|
|1          |same     |        2.420928|     0.1190523|
|2          |other    |        2.484519|     0.1320152|
|2          |same     |        2.527034|     0.1381596|

## PLOT

d <- data %>% 
    filter(experiment !=3) %>%
    ## individual means
    group_by(experiment, subject_id, effector) %>%
    summarise(confidence = mean(confidence)) %>%
    ungroup() %>%
    ## number of subjects
    group_by(experiment) %>%
    mutate(n_subj = n_distinct(subject_id)) %>% 
    ungroup() %>%
    ## grand means by experiment
    group_by(experiment) %>%
    mutate(grand_mean = mean(confidence)) %>%
    ungroup() %>%
    ## individual grand means
    group_by(subject_id) %>%
    mutate(indiv_grand_mean = mean(confidence)) %>%
    ungroup() %>%
    ## Cousineau-Morey CI
    group_by(experiment, effector) %>%
    mutate(norm_confidence = confidence - indiv_grand_mean + grand_mean) %>%
    mutate(CI = sd(norm_confidence)/sqrt(n_subj)*sqrt(2)*qt(.975,n_subj-1))  %>%
    summarise(confidence = mean(confidence), CI = mean(CI)) 

plot <- ggplot(data = d,  aes(x = experiment, y = confidence, color = effector)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) + 
    geom_pointrange(aes(ymin = confidence-CI, ymax = confidence+CI),position = position_dodge(width = 0.5))
plot
ggsave('confidence_effector.svg', plot)

# ---------------------------------------------------
## ACCURACY vs CONGRUENCY
# ---------------------------------------------------
## ## NB: CI représentent la dispersion des moyennes individuelles corrigés par la méthode de Cousnieau & Morey 
## (Morey, R. D. (2008). Confidence intervals from normalized data: A correction to Cousineau (2005). 
## Tutorials in quantitative methods for psychology, 4(2), 61-64.)

## STATS
d <- data %>%
    group_by(subject_id, experiment, congruency) %>%
    summarise(accuracy = mean(as.numeric(accuracy)-1)) %>%
    ungroup() %>%
    group_by(experiment, congruency) %>%
    summarise(mean_accuracy = mean(accuracy), se_accuracy = sd(accuracy)/sqrt(n()))

kable(d)
## |experiment |congruency  | mean_accuracy| se_accuracy|
## |:----------|:-----------|-------------:|-----------:|
## |1          |congruent   |     0.6747436|   0.0263160|
## |1          |incongruent |     0.6781461|   0.0251465|
## |2          |congruent   |     0.7276262|   0.0140596|
## |2          |incongruent |     0.7445716|   0.0152577|
## |3          |congruent   |     0.7237167|   0.0170180|
## |3          |incongruent |     0.7439239|   0.0157445|

## PLOT

d <- data %>% 
    ## individual means
    group_by(experiment, subject_id, congruency) %>%
    summarise(accuracy = mean(as.numeric(accuracy)-1)) %>%
    ungroup() %>%
    ## number of subjects
    group_by(experiment) %>%
    mutate(n_subj = n_distinct(subject_id)) %>% 
    ungroup() %>%
    ## grand means by experiment
    group_by(experiment) %>%
    mutate(grand_mean = mean(accuracy)) %>%
    ungroup() %>%
    ## individual grand means
    group_by(subject_id) %>%
    mutate(indiv_grand_mean = mean(accuracy)) %>%
    ungroup() %>%
    ## Cousineau-Morey CI
    group_by(experiment, congruency) %>%
    mutate(norm_accuracy = accuracy - indiv_grand_mean + grand_mean) %>%
    mutate(CI = sd(norm_accuracy)/sqrt(n_subj)*sqrt(2)*qt(.975,n_subj-1))  %>%
    summarise(accuracy = mean(accuracy), CI = mean(CI)) 

plot <- ggplot(data = d,  aes(x = experiment, y = accuracy, color = congruency)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) + 
    geom_pointrange(aes(ymin = accuracy-CI, ymax = accuracy+CI),position = position_dodge(width = 0.5))

    
ggsave('accuracy_congruency.svg', plot)

# ---------------------------------------------------
## *             FIT SUMMARIES
# ---------------------------------------------------


## ** accuracy
load(paste(dir,'MG1/fit_acc_MG1.rdata',sep=''))
acc1 <- l.acc
load(paste(dir,'MG2/fit_acc_MG2.rdata',sep=''))
acc2 <- l.acc
load(paste(dir,'MG3/fit_acc_MG3.rdata',sep=''))
acc3 <- l.acc
tab_model(acc1,acc2,acc3, file = "accuracy.html")
stargazer(acc1, acc2, acc3, type = "latex",  title = "Accuracy",  out = "accuracys.tex")

load(paste(dir,'MG1/fit_acc_bayes_MG1.rdata',sep=''))
acc_bayes1 <- fit_acc
load(paste(dir,'MG2/fit_acc_bayes_MG2.rdata',sep=''))
acc_bayes2 <- fit_acc
load(paste(dir,'MG3/fit_acc_bayes_MG3.rdata',sep=''))
acc_bayes3 <- fit_acc
tab_model(acc_bayes1,acc_bayes2,acc_bayes3, file = "accuracy_bayes.html")


#** rt
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

## ** confidence
load(paste(dir,'MG1/fit_conf_MG1.rdata',sep=''))
conf1 <- l.conf
load(paste(dir,'MG2/fit_conf_MG2.rdata',sep=''))
conf2 <- l.conf
load(paste(dir,'MG3/fit_conf_MG3.rdata',sep=''))
conf3 <- l.conf
tab_model(conf1,conf2,conf3, file = "confidence.html")
stargazer(conf1, conf2, conf3, type = "latex",  title = "Confidence",  out = "confidence.tex")
texreg(list(conf1, conf2, conf3),   
       file = "confidence.tex", 
       booktabs = TRUE,
       caption = "Confidence ",
       label = "tab:regression_results")

load(paste(dir,'MG1/fit_conf_bayes_MG1.rdata',sep=''))
conf_bayes1 <- fit_conf
load(paste(dir,'MG2/conf_MG2.rdata',sep=''))
conf_bayes2 <- fit_conf
load(paste(dir,'MG3/conf.rdata',sep=''))
conf_bayes3 <- fit_conf
tab_model(conf_bayes1,conf_bayes2,conf_bayes3, file = "conf_bayes.html")

library("httpgd")
