rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/motor_gabor/data/MG2_data')
#setwd('C:/Users/remil/OneDrive/Documents/GitHub/motor_gabor/data/MG2_data')
#source('C:/Users/remil/OneDrive/Documents/GitHub/motor_gabor/data/MG2_data/R_lib.r')
#library('ggeffects')

## * preparation

load('data_final_MG2.rda')
data_complete <- data %>%
    mutate(block_10 = rep(sort(rep(1:10,40) , decreasing = FALSE),24)) %>%
    mutate(rt_gabor = ifelse(rt_gabor <0, NA, rt_gabor)) %>%
    mutate(rt_cue = ifelse(rt_cue <0, NA, rt_cue)) %>%
    mutate(accuracy_cue = ifelse(accuracy_cue <0, NA, accuracy_cue)) %>%
    mutate(accuracy_gabor = ifelse(accuracy_gabor <0, NA, accuracy_gabor)) %>%
    mutate(expected_gabor = ifelse(expected_gabor <0, NA, expected_gabor)) %>%
    mutate(pressed_gabor = ifelse(pressed_gabor <0, NA, pressed_gabor)) %>%
    mutate(gabor.contrast = ifelse(gabor.contrast <0, NA, gabor.contrast)) %>%
    mutate(pressed_cue = ifelse(pressed_cue =="-1", NA, pressed_cue)) %>%
    mutate(expected_cue = ifelse(expected_cue =="-1", NA, expected_cue)) %>%
    mutate(congruency = ifelse(congruency =="none", NA, congruency)) %>%
    mutate(effector_order = case_when(effector_order == 'other2' | effector_order == 'same1'  ~ 'same1',
                                    effector_order == 'same2' | effector_order == 'other1' ~ 'other1')) %>%
    mutate(congruency2 = case_when(
    (expected_cue == 'n' | expected_cue == 'i') & expected_gabor == 'n' ~ 1,
    (expected_cue == 'x' | expected_cue == 'z') & expected_gabor == 'x' ~ 1,
    (expected_cue == 'n' | expected_cue == 'i') & expected_gabor == 'x' ~ -1,
    (expected_cue == 'x' | expected_cue == 'z') & expected_gabor == 'n' ~ -1)) %>%
    select(-c(trial_start_time_rel2bloc, bloc_start_time_rel2exp, exp_start_date))

data_g <- data_complete %>%
    filter(!is.na(rt_gabor))

data <- data_g %>%
    filter(!grepl('WE', congruency)) %>% ## remove trials with wrong effector
    filter(conf > 0) ## remove detected errors

## define contrasts
data$acc_gabor_num <- data$accuracy_gabor ## on garde la variable 0/1 pour l'analyse de accuracy
data$accuracy_gabor <- as.factor(data$accuracy_gabor)
data$congruency2 <- as.factor(data$congruency2)
contrasts(data$accuracy_gabor) <- - contr.sum(2) ## erreur: -1; correct: 1
data$acc_cue_num <- data$accuracy_cue
data$effector <- as.factor(data$effector)
contrasts(data$effector) <-  -contr.sum(2) ## feet: -1; hand: 1
data$condition <- as.factor(data$condition)
data$effector_order <- as.factor(data$effector_order)
contrasts(data$effector_order) <-  contr.sum(2) ## hand1: -1; feet: 1
data$congruency <- as.factor(data$congruency)
contrasts(data$congruency) <-  contr.sum(2) ## incongruent: -1; congruent: 1
contrasts(data$congruency2) <-  contr.sum(2) ## incongruent: -1; congruent: 1
data$rt_gabor_centered <- data$rt_gabor - mean(data$rt_gabor, na.rm = TRUE)


## * Accuracy

data$rt_gabor_centered <- data$rt_gabor - mean(data$rt_gabor, na.rm = TRUE) # center rt

## ** Frequentist

## Planned model
l.acc <- lmer_alt(acc_gabor_num ~  congruency2 * effector * effector_order  *  rt_gabor_centered +
		    (1 + congruency * effector + effector_order  +  rt_gabor_centered ||subject_id),
		  family = binomial(link = "logit"),
		  data = data)
summary(l.acc)
summary(rePCA(l.acc))


## remove: congruency

l.acc2 <- lmer_alt(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
		    (1 + effector + effector_order  +  rt_gabor_centered ||subject_id),
		  family = binomial(link = "logit"),
		  data = data)
summary(l.acc2)

## ** Bayesian lmer
fit_acc <- brm(acc_gabor_num | trials(1) ~  congruency * effector * effector_order  *  rt_gabor_centered +
               (1 + congruency * effector + effector_order  +  rt_gabor_centered ||subject_id),
               family = binomial(link = "logit"),
           data = data,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 2000, seed = 123,
           save_model = 'acc.stan',
           save_pars = save_pars(all = TRUE)
           )

save(fit_acc, file ='fit_acc.rdata')

## * Confidence
## * frequentist
## planned model

l.conf <- lmer_alt(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
		     (1 +  accuracy_gabor  + congruency * effector + effector_order + rt_gabor_centered||subject_id),
		   REML = TRUE,
		   data = data)

summary(l.conf)
summary(rePCA(l.conf))

e <- emmeans(l.conf2, pairwise ~ congruency * effector)
e

plot <-  ggemmeans(l.conf3, c('accuracy_gabor','laterality','effectors')) %>% plot()
print(plot)

## remove congruency
l.conf2 <- lmer_alt(conf ~ accuracy_gabor  * congruency2 * effector * effector_order +  rt_gabor_centered +
		     plot <-  ggemmeans(l.conf3, c('accuracy_gabor','laterality','effectors')) %>% plot()
print(plot)

## ** Bayesian

fit_conf <- brm(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
		     (1 +  accuracy_gabor  + congruency * effector + effector_order + rt_gabor_centered||subject_id),
           data = data,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 2000, seed = 123,
           save_model = 'conf.stan',
           save_pars = save_pars(all = TRUE)
           )
save(fit, file = 'conf.rdata')
summary(fit_conf)
plot(fit_conf)

save(fit_conf, file = 'fit_conf.rdata')

plot(fit_conf)
plot_model(fit_conf)
plot_model(fit_conf, type = "pred", terms = c('effector', "congruency",  "accuracy_gabor"))
conditions <- data.frame(accuracy_gabor = c(0, 1))
plot(conditional_effects(fit_conf, effects = 'effector:congruency',
                         conditions = conditions))

## * RT

## ** frequentist

l.rt <- lmer_alt(rt_gabor_centered ~  accuracy_gabor * congruency * effector * effector_order +
                     (1 + accuracy_gabor + congruency + effector + effector_order  ||subject_id),
                   REML = TRUE,
                   data = data)
summary(l.rt)
summary(rePCA(l.rt))

## remove congruency
l.rt2 <- lmer_alt(rt_gabor_centered ~  accuracy_gabor * congruency * effector * effector_order +
                   (1  + accuracy_gabor + effector + effector_order  ||subject_id),
                 REML = TRUE,
                 data = data)
summary(l.rt2)

## ** bayesian
fit_rt <- brm(rt_gabor_centered ~  accuracy_gabor * congruency * effector * effector_order +
                     (1 + accuracy_gabor + congruency + effector + effector_order  ||subject_id) ,
           data = data,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 2000, seed = 123,
           save_model = 'rt.stan',
           save_pars = save_pars(all = TRUE)
           )

save(fit_rt, file = 'fit_rt.rdata')


l.conf <- lmer_alt(conf ~ accuracy_gabor  * congruency2 * effector * effector_order +  rt_gabor_centered +
		     (1  + effector + effector_order + rt_gabor_centered||subject_id),
		   REML = TRUE,
		   data = data)
summary(l.conf)
l.conf <- lmer_alt(conf ~ congruency * congruency2  *  rt_gabor_centered +
		     (1 +   rt_gabor_centered||subject_id),
		   REML = TRUE,
		   data = data)

summary(l.conf)
summary(rePCA(l.conf))

e <- emmeans(l.conf2, pairwise ~ congruency * effector)
e

plot <-  ggemmeans(l.conf, c('accuracy_gabor','congruency2')) %>% plot()
print(plot)

l.acc2 <- lmer_alt(acc_gabor_num ~ congruency2 * effector * effector_order  *  rt_gabor_centered +
		    (1 + effector + effector_order  +  rt_gabor_centered ||subject_id),
		  family = binomial(link = "logit"),
		  data = data)
summary(l.acc2)
plot <-  ggemmeans(l.acc2, terms = c('congruency2')) %>% plot()
print(plot)
