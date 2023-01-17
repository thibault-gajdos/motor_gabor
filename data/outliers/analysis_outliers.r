rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/motor_gabor/data/outliers')

## * preparation

load('big_data_final.rda')
data <- big_data

data$accuracy_gabor <- as.factor(data$accuracy_gabor)
contrasts(data$accuracy_gabor) <- - contr.sum(2) ## erreur: -1; correct: 1

data$effector <- as.factor(data$effector)
contrasts(data$effector) <-  -contr.sum(2) ## other: -1; same: 1

data$effector_order <- as.factor(data$effector_order)
contrasts(data$effector_order) <-  contr.sum(2) ## same1: -1; other1: 1

data$congruency <- as.factor(data$congruency)
contrasts(data$congruency) <-  contr.sum(2) ## incongruent: -1; congruent: 1

data$conf_ord = as.factor(data$conf)

data1 <- data %>%
    filter(participants == 'mg1'|participants == 'mg1_excluded')   ## experience 1
data2 <- data %>%
    filter(participants == 'mg1'|participants == 'mg1_excluded')   ## experience 2
   
data1$rt_gabor_centered <- data1$rt_gabor - mean(data1$rt_gabor, na.rm = TRUE)
data2$rt_gabor_centered <- data2$rt_gabor - mean(data2$rt_gabor, na.rm = TRUE)


## * RT

fit1.rt <- brm(rt_gabor_centered*1000 ~  accuracy_gabor * congruency * effector * effector_order +
                     (1 + accuracy_gabor + congruency + effector + effector_order  ||subject_id) ,
           data = data1,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 2000, seed = 123,
           save_model = 'rt.stan',
           save_pars = save_pars(all = TRUE)
           )
save(fit1.rt, file = 'rt_outliers_MG1.rdata')

fit2.rt <- brm(rt_gabor_centered*1000 ~  accuracy_gabor * congruency * effector * effector_order +
                     (1 + accuracy_gabor + congruency + effector + effector_order  ||subject_id) ,
           data = data2,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 2000, seed = 123,
           save_model = 'rt.stan',
           save_pars = save_pars(all = TRUE)
           )
save(fit2.rt, file = 'rt_outliers_MG2.rdata')

tab_model(fit1.rt, fit2.rt, file = "rt_outliers.html")


## * Accuracy

fit1.acc <- brm(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
               (1 + congruency * effector + effector_order  +  rt_gabor_centered ||subject_id),
               family = bernoulli(link = "logit"),
           data = data1,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .98,  max_treedepth = 12),
           iter = 6000,  warmup = 4000, seed = 123,
           save_model = 'acc.stan',
           save_pars = save_pars(all = TRUE)
           )
save(fit1.acc, file ='acc_outliers_MG1.rdata')

fit2.acc <- brm(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
               (1 + congruency * effector + effector_order  +  rt_gabor_centered ||subject_id),
               family = bernoulli(link = "logit"),
           data = data2,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .98,  max_treedepth = 12),
           iter = 6000,  warmup = 4000, seed = 123,
           save_model = 'acc.stan',
           save_pars = save_pars(all = TRUE)
           )
save(fit2.acc, file ='acc_outliers_MG2.rdata')
tab_model(fit1.acc, fit2.acc, file = "acc_outliers.html")


## * Confidence

fit1.conf <- brm(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
                    (1  +  accuracy_gabor  + congruency + effector * effector_order + rt_gabor_centered ||subject_id),
                init_r = 0.05,
        data = data1,
    family=cumulative("probit"),
    prior = c(set_prior("normal(0,1)", class = "b")),
    cores = 4, chains = 4,
    control = list(adapt_delta = .95,  max_treedepth = 12),
    iter = 6000,  warmup = 4000, seed = 123,
    save_model = 'conf.stan',
    save_pars = save_pars(all = TRUE)
    )
save(fit1.conf, file = 'conf_outliers_MG1.rdata')

fit2.conf <- brm(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
                    (1  +  accuracy_gabor  + congruency + effector * effector_order + rt_gabor_centered ||subject_id),
                init_r = 0.05,
        data = data2,
    family=cumulative("probit"),
    prior = c(set_prior("normal(0,1)", class = "b")),
    cores = 4, chains = 4,
    control = list(adapt_delta = .95,  max_treedepth = 12),
    iter = 6000,  warmup = 4000, seed = 123,
    save_model = 'conf.stan',
    save_pars = save_pars(all = TRUE)
    )
save(fit2.conf, file = 'conf_outliers_MG2.rdata')

tab_model(fit1.conf, fit2.conf, file = "conf_outliers.html")

