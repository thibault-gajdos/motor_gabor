rm(list=ls(all=TRUE))  ## efface les données
source('C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/MG2_data/R_lib.r')
setwd('C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/AG_data')
#setwd('C:/Users/remil/OneDrive/Documents/GitHub/motor_gabor/data/MG2_data')
#source('C:/Users/remil/OneDrive/Documents/GitHub/motor_gabor/data/MG2_data/R_lib.r')
#library('ggeffects')

## * preparation

load('data_final_MG1.rda')
data_complete <- data %>%
  mutate(block_22 = rep(sort(rep(1:22,36) , decreasing = FALSE),16)) %>%
  mutate(rt_gabor = ifelse(rt_gabor <0, NA, rt_gabor)) %>%
  mutate(rt_cue = ifelse(rt_cue <0, NA, rt_cue)) %>%
  mutate(rt_conf = ifelse(rt_conf <0, NA, rt_conf)) %>%
  mutate(conf = ifelse(conf <0, NA, conf)) %>%
  mutate(accuracy_cue = ifelse(accuracy_cue <0, NA, accuracy_cue)) %>%
  mutate(accuracy_gabor = ifelse(accuracy_gabor <0, NA, accuracy_gabor)) %>%
  mutate(expected_gabor = ifelse(expected_gabor <0, NA, expected_gabor)) %>%
  mutate(pressed_gabor = ifelse(pressed_gabor <0, NA, pressed_gabor)) %>%
  mutate(gabor.contrast = ifelse(gabor.contrast <0, NA, gabor.contrast)) %>%
  mutate(pressed_cue = ifelse(pressed_cue =="-1", NA, pressed_cue)) %>%
  mutate(expected_cue = ifelse(expected_cue =="-1", NA, expected_cue)) %>%
  mutate(congruency = ifelse(congruency =="none", NA, congruency)) %>%
  mutate(effector_order = case_when(effector_order == 'feet2' | effector_order == 'hand1'  ~ 'hand1',
                                    effector_order == 'hand2' | effector_order == 'feet1' ~ 'feet1')) %>%
  select(-c(trial_start_time_rel2bloc, bloc_start_time_rel2exp, exp_start_date,  pressed_conf))

data <- data_complete %>%
  filter(!is.na(rt_gabor))

## define contrasts
data$acc_gabor_num <- data$accuracy_gabor ## on garde la variable 0/1 pour l'analyse de accuracy
data$accuracy_gabor <- as.factor(data$accuracy_gabor)
contrasts(data$accuracy_gabor) <- - contr.sum(2) ## erreur: -1; correct: 1
data$acc_cue_num <- data$accuracy_cue
data$effector <- as.factor(data$effector)
contrasts(data$effector) <-  -contr.sum(2) ## feet: -1; hand: 1
data$condition <- as.factor(data$condition)
data$effector_order <- as.factor(data$effector_order)
contrasts(data$effector_order) <-  contr.sum(2) ## hand1: -1; feet: 1
data$congruency <- as.factor(data$congruency)
contrasts(data$congruency) <-  contr.sum(2) ## incongruent: -1; congruent: 1
data$rt_gabor_centered <- data$rt_gabor - mean(data$rt_gabor, na.rm = TRUE)
#data <- data %>%
#  mutate(bloc4 = case_when(trial <= 198 ~ 1,
#                           trial>198 & trial <= 396 ~ 2,
#                           trial >396 & trial <= 594 ~ 3,
#                           trial >594 ~ 4))

data2 =  data %>% filter(is.na(accuracy_gabor) == FALSE)

data2$effectors[data2$effector == 'hand'] = 'same (index fingers)'
data2$effectors[data2$effector == 'feet'] = 'other (feet)'
data2$laterality[data2$congruency == 'congruent'] = 'same side'
data2$laterality[data2$congruency == 'incongruent'] = 'other side'


d1 <- data_complete %>%
  group_by(subject_id) %>%
  filter(effector_order == 'hand1') %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
            accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
            conf = mean(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE),
            rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d1), type = "html", caption = "Same effector first", caption.placement = 'top')

d2 <- data_complete %>%
  group_by(subject_id) %>%
  filter(effector_order == 'feet1') %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
            accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
            conf = mean(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE),
            rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d2), type = "html", caption = "Feet first", caption.placement = 'top')

d2full <- data_complete %>%
  group_by(subject_id) %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
            accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
            confidence = mean(conf, na.rm=TRUE),
            'rt_gabor (s)' = mean(rt_gabor, na.rm=TRUE),
            'rt_cue (s)' = mean(rt_cue, na.rm=TRUE))
print(xtable(d2full), type = "html", caption = "Summary", file = 'MG1summary.html', caption.placement = 'top')

dall <- data_complete %>%
  group_by(subject_id) %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
            accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
            conf = mean(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE),
            rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d2), type = "html", caption = "Feet first", caption.placement = 'top')


d3 <- data_complete %>%
  group_by(subject_id, effector) %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
            accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
            sd_cue = sd(accuracy_cue, na.rm = TRUE),
            conf = mean(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE),
            rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d3), type = "html", caption = "Feet first", caption.placement = 'top')

d4 <- data %>%
  group_by(subject_id) %>%
  summarise(accuracy_gabor = mean(acc_gabor_num, na.rm = TRUE),
            conf = mean(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE),
            rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d4), type = "html", caption = "Feet first", caption.placement = 'top')

d5 <- data %>%
  summarise(mean = mean(acc_gabor_num, na.rm = TRUE),
            sd_acc = sd(acc_gabor_num, na.rm=TRUE),
            conf = mean(conf, na.rm=TRUE),
            sd_conf = sd(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE),
            sd_rt = sd(rt_gabor),
            rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d4), type = "html", caption = "Feet first", caption.placement = 'top')


dcontrast <- data %>%
  group_by(subject_id) %>%
  summarise(contrast = mean(gabor.contrast))

library('stargazer')
stargazer(dcontrast, type = "text", title="Gabor contrast values", digits=1, out="MG1_gabor_contrast.txt")

## ** Frequentist

## Planned model
l.acc <- lmer_alt(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
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

plot_model(l.acc2, type = "pred", terms = c('rt_gabor_centered',"effector_order"), dot.size = 2, line.size = 1)

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

save(fit_acc, file ='fit_acc_MG1.rdata')

load('fit_acc_MG1.rdata')
summary(fit_acc)
plot(fit_acc)
plot_acc <- plot_model(fit_acc,ci.lvl = .95, size.inner = 0, line.size = 1.4) 
print(plot_acc)


#+BEGIN_SRC R  :results output graphics :file acc_bayes2.png :exports results 
sample <- posterior_samples(fit_acc) %>%
  rename('congruency' = 'b_congruency1') %>%
  select(congruency)
q.accuracy  <- post_plot(sample$congruency) + xlab('Congruency')
plot(q.accuracy)
unique(data$congruency)
#+END_SRC


## * Confidence
## * frequentist
## planned model

l.conf <- lmer_alt(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
		     (1 +  accuracy_gabor  + congruency * effector + effector_order + rt_gabor_centered||subject_id),
		   REML = TRUE,
		   data = data)

summary(l.conf)
summary(rePCA(l.conf))

## remove congruency, effector and order
l.conf2 <- lmer_alt(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
		     (1 +  accuracy_gabor   + rt_gabor_centered||subject_id),
		   REML = TRUE,
		   data = data)
summary(l.conf2)

## ** Bayesian

fit_conf <- brm(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
		     (1 +  accuracy_gabor  + congruency * effector + effector_order + rt_gabor_centered||subject_id),
           data = data,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .98,  max_treedepth = 12),
           iter = 4000,  warmup = 2000, seed = 123,  
           save_model = 'conf.stan', 
           save_pars = save_pars(all = TRUE)
           )
save(fit_conf, file = 'fit_conf_MG1.rdata')

load('fit_conf_MG1.rdata')
summary(fit_conf)
plot(fit_conf,dot.size = 2, line.size = 1)

plot_model(fit_conf,  ci.lvl = .95, size.inner = 0, line.size = 1.4)
plot_model(fit_conf, type = "pred", terms = c('effector', "congruency",  "accuracy_gabor"), dot.size = 2, line.size = 1)

plot_model(l.conf2, type = "pred", terms = c("effector_order",'congruency'), dot.size = 2, line.size = 1)
plot_model(l.conf2, type = "pred", terms = c("effector_order",'effector'), dot.size = 2, line.size = 1)

plot(l.conf2, terms = c('effector',  "effector_order"))
# Zoom on congruency
#+BEGIN_SRC R  :results output graphics :file conf_bayes2.png :exports results 
sample <- posterior_samples(fit_conf) %>%
  rename('congruency' = 'b_congruency1') %>%
  select(congruency)
q.accuracy  <- post_plot(sample$congruency) + xlab('Congruency')
plot(q.accuracy)
unique(data$congruency)
#+END_SRC


#Zoom on congruency:effector:accuracy interaction.
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

l.rt2 <- lmer_alt(rt_gabor ~  accuracy_gabor * congruency * effector * effector_order +
                    (1  + effector_order  ||subject_id),
                  REML = TRUE,
                  data = data)

## remove congruency and accuracy and effector
l.rt2cent <- lmer_alt(rt_gabor_centered ~  accuracy_gabor * congruency * effector * effector_order +
                   (1  + effector_order  ||subject_id),
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

save(fit_rt, file = 'fit_rt_MG1.rdata')

load('fit_rt_MG1.rdata')
summary(fit_rt)
plot(fit_rt)
plot_rt <- plot_model(fit_rt,ci.lvl = .95,  size.inner = 0, line.size = 1.4 ) 
print(plot_rt)
plot_model(fit_rt, type = "pred", terms = c('effector', "congruency"), dot.size = 2, line.size = 1)


e <- emmeans(l.conf2, pairwise ~ effector * accuracy_gabor)
e
e <- emmeans(l.conf2, pairwise ~ effector * effector_order)


data2 =  data %>% filter(is.na(accuracy_gabor) == FALSE)

data2$effectors[data2$effector == 'hand'] = 'same (index fingers)'
data2$effectors[data2$effector == 'feet'] = 'other (feet)'
data2$laterality[data2$congruency == 'congruent'] = 'same side'
data2$laterality[data2$congruency == 'incongruent'] = 'other side'

l.conf3 <- lmer_alt(conf ~ accuracy_gabor  * laterality * effectors * effector_order +  rt_gabor_centered +
                      (1  +  accuracy_gabor  + rt_gabor_centered||subject_id),
                    REML = TRUE,
                    data = data2 %>% filter(is.na(accuracy_gabor) == FALSE))




plot <-  ggemmeans(l.conf3, c('laterality')) %>% plot()
print(plot)


e
plot <-  ggemmeans(l.conf2, c('effector','congruency')) %>% plot()
print(plot)

plot <-  ggemmeans(l.rt2, c('congruency'), show.title = FALSE) %>% plot()
print(plot)

e <- emmeans(l.conf2, pairwise ~ congruency * effector)
e
plot <-  ggpredict(l.conf2, c('effector_order',"congruency")) %>% plot()
plot
plot_model(l.conf2, type = "pred", terms = c("accuracy_gabor",'effector'), dot.size = 2, line.size = 1)









###### try a summary
name.label <- c('acc_gabor_num_mean' = "Perceptual Accuracy", 'rt_gabor_mean' = "Perceptual Response Times (s)", "rt_conf_mean" = "Confidence Response Times (s)","conf_mean" = "Perceptual Confidence (1-4)")
data2 %>% group_by(laterality,subject_id) %>% summarise_at(vars(acc_gabor_num,rt_gabor,rt_conf,conf),lst(mean)) %>%
  pivot_longer(col=c(3,4,5,6)) %>%
  ggplot(aes(laterality,value, color=laterality)) + geom_violin(trim=FALSE) +  geom_dotplot(binaxis='y', stackdir='center',dotsize= 3)+
  stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="orange")  + facet_wrap(~name,scales='free',labeller = labeller(name=name.label))+
  labs(y="mean values")+
  labs(x="Laterality of the motor cue")

