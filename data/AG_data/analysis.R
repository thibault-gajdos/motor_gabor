rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/motor_gabor/data/AG_data/')
load('data_final.rda')

## * cleaning & contrasts

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
data <- data %>%
    mutate(bloc4 = case_when(trial <= 198 ~ 1,
                             trial>198 & trial <= 396 ~ 2,
                             trial >396 & trial <= 594 ~ 3,
                             trial >594 ~ 4))

## * Accuracy
l.acc <- lmer_alt(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
		    (1 + congruency * effector + effector_order  +  rt_gabor_centered ||subject_id),
		  family = binomial(link = "logit"),
		  data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.acc)

summary(rePCA(l.acc))

l.acc2 <- lmer_alt(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
		   (1 +  effector + effector_order  +  rt_gabor_centered ||subject_id),
		   family = binomial(link = "logit"),
		   data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.acc2)

## * Confidence
l.conf <- lmer_alt(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
		     (1 +  accuracy_gabor  + congruency * effector + effector_order +
		      rt_gabor_centered||subject_id),
		   REML = TRUE,
		   data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.conf)

summary(rePCA(l.conf))
data$trialnorm <- (data$trial - mean(data$trial))/100
l.conf2 <- lmer_alt(conf ~ accuracy_gabor  * congruency * effector_order * effector * trialnorm +
		      rt_gabor_centered +
		      (1 +  accuracy_gabor + rt_gabor_centered + trialnorm||subject_id),
		    REML = TRUE,
		    data = data %>% filter(rt_gabor>.2))
summary(l.conf2)
l.conf3 <- lmer_alt(conf ~ accuracy_gabor  * congruency * (effector + effector_order) *
		      rt_gabor_centered +  trialnorm  +
		      (1 +  accuracy_gabor + rt_gabor_centered + trialnorm||subject_id),
		    REML = TRUE,
		    data = data %>% filter(rt_gabor>.1))
summary(l.conf3)
anova(l.conf2, l.conf3)




e <- emmeans(l.conf2, pairwise ~ effector * accuracy_gabor)
e
plot <-  ggemmeans(l.conf2, c('effector','accuracy_gabor')) %>% plot()
print(plot)

plot <-  ggemmeans(l.conf2, c('effector','effector_order')) %>% plot()
print(plot)
e <- emmeans(l.conf2, pairwise ~ effector * effector_order)
e

plot <-  ggemmeans(l.conf2, c('congruency','effector_order')) %>% plot()
print(plot)
e <- emmeans(l.conf2, pairwise ~ congruency * effector_order)
e



l.rt.gabor <- lmer_alt(log(rt_gabor) ~ accuracy_gabor  * congruency * effector * effector_order  +
		      (1 +  accuracy_gabor  + congruency + effector + effector_order||subject_id),
                 REML = TRUE, data = data %>% filter(bloc4 %in% c(2,3)))
summary(l.rt.gabor)

l.rt.cue <- lmer_alt(log(rt_cue) ~ accuracy_cue   * effector * effector_order +
		      (1 +  accuracy_cue + effector + effector_order||subject_id),
                 REML = TRUE, data = data)
summary(l.rt.cue)

l.acc <- lmer_alt(acc_gabor_num ~  congruency * (effector + effector_order)  *  rt_gabor_centered * trialnorm +
                      (1 + effector_order + effector +
                       rt_gabor_centered ||subject_id),
		  family = binomial(link = "logit"),
		  data = data %>% filter(rt_gabor >.1))
summary(l.acc)

l.acc2 <- lmer_alt(acc_gabor_num ~  congruency * (effector + effector_order)  *  rt_gabor_centered +
                       trialnorm  + 
                       (1 + rt_gabor_centered ||subject_id),
		   family = binomial(link = "logit"),
		   data = data)

summary(l.acc2)
l.acc3 <- lmer_alt(acc_gabor_num ~  rt_gabor_centered +
                       (1 + rt_gabor_centered ||subject_id),
		   family = binomial(link = "logit"),
		   data = data)


anova(l.acc3, l.acc4)      






