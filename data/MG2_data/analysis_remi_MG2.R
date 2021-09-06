rm(list=ls(all=TRUE))  ## efface les données
setwd('C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/MG2_data')
source('C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/MG2_data/R_lib.r')
library('ggeffects')
load('data_final_MG2.rda')

data_complete <- data %>%
  mutate(block_10 = rep(sort(rep(1:10,40) , decreasing = FALSE),24)) %>%
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
  mutate(effector_order = case_when(effector_order == 'other2' | effector_order == 'same1'  ~ 'same1',
                                    effector_order == 'same2' | effector_order == 'other1' ~ 'other1')) %>%
  select(-c(trial_start_time_rel2bloc, bloc_start_time_rel2exp, exp_start_date))

data_g <- data_complete %>%
    filter(!is.na(rt_gabor))

fulldata_noerror <- data_complete %>%
  filter(!grepl('WE', congruency)) %>%
  filter(!grepl(0, conf))


# REMOVE REPORTED ERRORS 
data <- data_g %>%
  filter(!grepl('WE', congruency)) %>% ## remove trials with wrong effector
  filter(conf > 0) ## remove detected errors

# wrong effector used to answer + errors reported:
errors = data_g[data_g$conf == '0'| data_g$congruency == 'sameWEcongruent'| data_g$congruency == 'sameWEincongruent'| data_g$congruency == 'otherWEcongruent'| data_g$congruency == 'otherWEincongruent',]


## define contrasts
data$acc_gabor_num <- data$accuracy_gabor ## on garde la variable 0/1 pour l'analyse de accuracy
data$accuracy_gabor <- as.factor(data$accuracy_gabor)
contrasts(data$accuracy_gabor) <- - contr.sum(2) ## erreur: -1; correct: 1
data$acc_cue_num <- data$accuracy_cue
data$effector <- as.factor(data$effector)
contrasts(data$effector) <-  -contr.sum(2) ## other: -1; same: 1
data$condition <- as.factor(data$condition)
data$effector_order <- as.factor(data$effector_order)
contrasts(data$effector_order) <-  contr.sum(2) ## hand1: -1; feet: 1
data$congruency <- as.factor(data$congruency)
contrasts(data$congruency) <-  contr.sum(2) ## incongruent: -1; congruent: 1
data$rt_gabor_centered <- data$rt_gabor - mean(data$rt_gabor, na.rm = TRUE)



################## change variable names for display
data2 =  data %>% filter(is.na(accuracy_gabor) == FALSE)
data2$effectors[data2$effector == 'same'] = 'same (index fingers)'
data2$effectors[data2$effector == 'other'] = 'other (middle fingers)'
data2$laterality[data2$congruency == 'congruent'] = 'same side'
data2$laterality[data2$congruency == 'incongruent'] = 'other side'
data2$effectors = as.factor(data2$effectors)
contrasts(data2$effectors) <-  -contr.sum(2) ## other: -1; same: 1
data2$laterality = as.factor(data2$laterality)
contrasts(data2$laterality) <-  -contr.sum(2) ## other: -1; same: 1

dall <- fulldata_noerror %>%
  group_by(subject_id) %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
            accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
            conf = mean(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE),
            rt_cue = mean(rt_cue, na.rm=TRUE))



d1 <- data_noerror %>%
  group_by(subject_id) %>%
  filter(effector_order == 'same1') %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
	    accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
	    conf = mean(conf, na.rm=TRUE),
	    rt_gabor = mean(rt_gabor, na.rm=TRUE),
	    rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d1), type = "html", caption = "Same effector first", caption.placement = 'top')

d2 <- data_noerror %>%
  group_by(subject_id) %>%
  filter(effector_order == 'other1') %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
	    accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
	    conf = mean(conf, na.rm=TRUE),
	    rt_gabor = mean(rt_gabor, na.rm=TRUE),
	    rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d2), type = "html", caption = "Feet first", caption.placement = 'top')

d2full <- fulldata_noerror %>%
  group_by(subject_id) %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
            accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
            confidence = mean(conf, na.rm=TRUE),
            'rt_gabor (s)' = mean(rt_gabor, na.rm=TRUE),
            'rt_cue (s)' = mean(rt_cue, na.rm=TRUE))
print(xtable(d2full), type = "html", caption = "Summary", file = 'MG2summary.html', caption.placement = 'top')



d3 <- data %>%
  #group_by(subject_id, congruency, effector) %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
            accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
            conf = mean(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE),
            rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d3), type = "html", caption = "Feet first", caption.placement = 'top')

d4 <- data %>%
  group_by(congruency) %>%
  summarise(conf = mean(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE),
            sd_rt = sd(rt_gabor))


d5 <- fulldata_noerror  %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
            sd_acc = sd(accuracy_gabor, na.rm=TRUE),
            accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
            conf = mean(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE),
            rt_cue = mean(rt_cue, na.rm=TRUE))


dcontrast <- data %>%
  group_by(subject_id) %>%
  summarise(contrast = mean(gabor.contrast))
write.table(dcontrast, sep = ",")

p  <- ggplot(data = data, aes(rt_gabor)) +
  geom_histogram()  
  facet_wrap( ~ congruency) +
  ggtitle('rt gabor')
print(p)

d <- data %>%
  group_by(subject_id, effector) %>%
  summarise(acc_gabor = mean(acc_gabor_num))
p <- ggplot(data = d, aes(x = block_10 , y = acc_gabor_num)) +
  geom_line() +
  facet_wrap( ~ subject_id)
print(p)

p  <- ggplot(data = data, aes(conf)) +
  geom_histogram()  +
  facet_wrap( ~ subject_id) +
  ggtitle('confidence')
print(p)

p  <- ggplot(data = data, aes(rt_gabor_centered)) +
  geom_histogram()  +
  facet_wrap( ~ subject_id) +
  ggtitle('rt gabor')
print(p)

p  <- ggplot(data = data, aes(rt_gabor_centered)) +
  geom_histogram()  +
  facet_wrap( ~ congruency) +
  ggtitle('rt confidence')
print(p)

d <- data %>%
  group_by(block_10, subject_id) %>%
  summarise(accuracy_gabor = mean(acc_gabor_num))
p <- ggplot(data = d, aes(x = block_10 , y = accuracy_gabor)) +
  geom_line() +
  facet_wrap( ~ subject_id)
print(p)

d <- data %>%
  group_by(rt_gabor_centered, congruency) %>%
  summarise(rt_gabor_centered = mean(rt_gabor_centered))
p <- ggplot(data = d, aes(x = congruency , y = rt_gabor_centered)) +
  geom_line() 
print(p)

d <- data %>%
  group_by(effector, subject_id) %>%
  summarise(accuracy_gabor = mean(acc_gabor_num), conf = mean(conf))
p <- ggplot(data = d, aes(x = block_10 , y = conf)) +
  geom_line() +
  facet_wrap( ~ subject_id)
print(p)

data$rt_gabor_centered <- data$rt_gabor - mean(data$rt_gabor, na.rm = TRUE)
l.acc <- lmer_alt(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
		    (1 + congruency * effector + effector_order  +  rt_gabor_centered ||subject_id),
		  family = binomial(link = "logit"),
		  data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.acc)

summary(rePCA(l.acc))

l.acc2 <- lmer_alt(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
		   (1 + effector + effector_order  +  rt_gabor_centered ||subject_id),
		   family = binomial(link = "logit"),
		   data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.acc2)

l.acc3 <- lmer_alt(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
                     (1 + effector +  rt_gabor_centered ||subject_id),
                   family = binomial(link = "logit"),
                   data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.acc3)

l.conf <- lmer_alt(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
		     (1 +  accuracy_gabor  + congruency * effector + effector_order + rt_gabor_centered||subject_id),
		   REML = TRUE,
		   data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.conf)

summary(rePCA(l.conf))

l.conf2 <- lmer_alt(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
                     (1  +  effector + effector_order + rt_gabor_centered||subject_id),
                   REML = TRUE,
                   data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.conf2)


e <-  emmeans(l.conf2, pairwise ~congruency) %>% plot()
e

e <- emmeans(l.conf2, pairwise ~ congruency * effector)
e


plot <-  ggemmeans(l.conf2, c('effector','congruency')) %>% plot()
print(plot)
e <- emmeans(l.conf2, pairwise ~ accuracy_gabor * congruency * effector)
e

plot <-  ggemmeans(l.conf2, c('rt_gabor_centered','accuracy_gabor')) %>% plot()
print(plot)
e <- emmeans(l.conf2, pairwise ~ effector * effector_order)
e

plot <-  ggpredict(l.conf3, c('rt_gabor_centered','congruency')) %>% plot()
print(plot)
e <- emmeans(l.conf2, pairwise ~ congruency * effector_order)
e


plot <-  ggemmeans(l.conf2, c('congruency','effector')) %>% plot(show.title = FALSE) 
print(plot)
e <- emmeans(l.conf2, pairwise ~ congruency * effector)
e





l.conf3 <- lmer_alt(conf ~ accuracy_gabor  * laterality * effectors * effector_order +  rt_gabor_centered +
                      (1  +  effectors + effector_order + rt_gabor_centered||subject_id),
                    REML = TRUE,
                    data = data2 %>% filter(is.na(accuracy_gabor) == FALSE))


l.conf2 <- lmer_alt(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
                      (1  +  effector + effector_order + rt_gabor_centered||subject_id),
                    REML = TRUE,
                    data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.conf2)
summary(l.conf3)
e <- emmeans(l.conf2, pairwise ~ congruency * effector)
e

plot <-  ggemmeans(l.conf3, c('accuracy_gabor','laterality','effectors')) %>% plot()
print(plot)


plot <-  ggemmeans(l.conf3, c('laterality')) %>% plot()
print(plot)




l.rt <- lmer_alt(rt_gabor_centered ~  accuracy_gabor * congruency * effector * effector_order +
                     (1 + accuracy_gabor + congruency + effector + effector_order  ||subject_id),
                   REML = TRUE,
                   data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.rt)

summary(rePCA(l.rt))

l.rt2 <- lmer_alt(rt_gabor ~  accuracy_gabor * congruency * effector * effector_order +
                        (1  + accuracy_gabor + effector + effector_order  ||subject_id),
                      REML = TRUE,
                      data = data %>% filter(is.na(accuracy_gabor) == FALSE))

l.rt2 <- lmer_alt(rt_gabor ~  accuracy_gabor * laterality * effectors * effector_order +
                    (1  + accuracy_gabor + effectors + effector_order  ||subject_id),
                  REML = TRUE,
                  data = data2 %>% filter(is.na(accuracy_gabor) == FALSE))

l.rt2cent <- lmer_alt(rt_gabor_centered ~  accuracy_gabor * congruency * effector * effector_order +
                   (1  + accuracy_gabor + effector + effector_order  ||subject_id),
                 REML = TRUE,
                 data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.rt2)
Anova(l.rt2)

plotrt <-  ggemmeans(l.rt2, c('laterality')) %>% plot()
print(plotrt)
e <- emmeans(l.rt2, pairwise ~ effector * congruency)
e

plot <-  ggemmeans(l.rt2, c('effector','congruency')) %>% plot()
print(plot)
######################### RTs

hand_data = data[data$effector=='hand',]
feet_data = data[data$effector=='feet',]

hand_data_correct = hand_data[hand_data$accuracy_gabor=='1',]
feet_data_correct = feet_data[feet_data$accuracy_gabor=='1',]

hand_data_incorrect = hand_data[hand_data$accuracy_gabor=='0',]
feet_data_incorrect = feet_data[feet_data$accuracy_gabor=='0',]

d_rt_hand <- hand_data  %>%
  group_by(subject_id, congruency) %>%
  summarise(rt_gabor_centered = mean(rt_gabor_centered, na.rm = TRUE))

d_rt_hand_data_correct <- hand_data_correct  %>%
  group_by(subject_id, congruency) %>%
  summarise(rt_gabor_centered = mean(rt_gabor_centered, na.rm = TRUE))

d_rt_hand_data_incorrect <- hand_data_incorrect  %>%
  group_by(subject_id, congruency) %>%
  summarise(rt_gabor_centered = mean(rt_gabor_centered, na.rm = TRUE))

d_rt_feet <- feet_data  %>%
  group_by(subject_id, congruency) %>%
  summarise(rt_gabor_centered = mean(rt_gabor_centered, na.rm = TRUE))

d_rt_feet_data_correct <- feet_data_correct  %>%
  group_by(subject_id, congruency) %>%
  summarise(rt_gabor_centered = mean(rt_gabor_centered, na.rm = TRUE))

d_conf <- hand_data %>%
  group_by(subject_id, congruency) %>%
  summarise(conf = mean(conf, na.rm = TRUE))

d_conf_correct <- hand_data_correct %>%
  group_by(subject_id, congruency) %>%
  summarise(conf = mean(conf, na.rm = TRUE))

d_conf_incorrect <- hand_data_incorrect %>%
  group_by(subject_id, congruency) %>%
  summarise(conf = mean(conf, na.rm = TRUE))

d_acc <- hand_data %>%
  group_by(subject_id) %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE))

k = 1
diff_rt_hand = data.frame()
for (f in 1:16){
  g = (d_rt_hand[k,3] - d_rt_hand[k+1,3])
  diff_rt_hand[f,1] = c(g)
  k = k + 2
}

k = 1
diff_rt_hand_data_correct = data.frame()
for (f in 1:16){
  g = (d_rt_hand_data_correct[k,3] - d_rt_hand_data_correct[k+1,3])
  diff_rt_hand_data_correct[f,1] = c(g)
  k = k + 2
}

k = 1
diff_rt_hand_data_incorrect = data.frame()
for (f in 1:16){
  g = (d_rt_hand_data_incorrect[k,3] - d_rt_hand_data_incorrect[k+1,3])
  diff_rt_hand_data_incorrect[f,1] = c(g)
  k = k + 2
}

k = 1
diff_rt_feet = data.frame()
for (f in 1:16){
  g = (d_rt_feet[k,3] - d_rt_feet[k+1,3])
  diff_rt_feet[f,1] = c(g)
  k = k + 2
}

k = 1
diff_rt_feet_data_correct = data.frame()
for (f in 1:16){
  g = (d_rt_feet_data_correct[k,3] - d_rt_feet_data_correct[k+1,3])
  diff_rt_feet_data_correct[f,1] = c(g)
  k = k + 2
}

k = 1
diff_conf = data.frame()
for (f in 1:16){
  g = (d_conf[k,3] - d_conf[k+1,3])
  diff_conf[f,1] = c(g)
  k = k + 2
}

k = 1
diff_conf_correct = data.frame()
for (f in 1:16){
  g = (d_conf_correct[k,3] - d_conf_correct[k+1,3])
  diff_conf_correct[f,1] = c(g)
  k = k + 2
}

k = 1
diff_conf_incorrect = data.frame()
for (f in 1:16){
  g = (d_conf_incorrect[k,3] - d_conf_incorrect[k+1,3])
  diff_conf_incorrect[f,1] = c(g)
  k = k + 2
}

plot(diff_rt_hand)
plot(diff_rt_hand_data_correct)
plot(diff_rt_feet_data_correct)
plot(diff_rt_feet)
plot(diff_rt_hand_data_incorrect)

congruent_correct_hand = d_rt_hand_data_correct[d_rt_hand_data_correct$congruency=='congruent',]
congruent_correct_hand_rt = congruent_correct_hand$rt_gabor_centered

incongruent_correct_hand = d_rt_hand_data_correct[d_rt_hand_data_correct$congruency=='incongruent',]
incongruent_correct_hand_rt = incongruent_correct_hand$rt_gabor_centered
t.test(congruent_incorrect_hand_rt,incongruent_incorrect_hand_rt,paired=TRUE)

scatterplot(diff_conf_incorrect[,1]~diff_rt_hand_data_incorrect[,1])

prest.lm1 <- lm(diff_conf_correct[,1]~diff_rt_hand_data_correct[,1])
summary(prest.lm1)

prest.lm2 <- lm(diff_conf[,1]~diff_rt_hand_data_incorrect[,1])
summary(prest.lm2)

plot(prest.lm2,1)
acf(residuals(prest.lm1), main="prest.lm1")
durbinWatsonTest (prest.lm1)
shapiro.test(residuals(prest.lm1))
plot(prest.lm1,3)
ncvTest(prest.lm1)

################# cue acc
cue_first = data_complete[data_complete$effector_order == 'hand1' & data_complete$effector == 'hand' | data_complete$effector_order == 'feet1' & data_complete$effector == 'feet', ]
cue_first_correct = cue_first[cue_first$accuracy_cue=="1",]
cue_first_accuracy = nrow(cue_first_correct) / nrow(cue_first)

cue_second = cue_first = data_complete[data_complete$effector_order == 'hand1' & data_complete$effector == 'feet' | data_complete$effector_order == 'feet1' & data_complete$effector == 'hand', ]
cue_second_correct = cue_second[cue_second$accuracy_cue=="1",]
cue_second_accuracy = nrow(cue_second_correct) / nrow(cue_second)

plot(data$effector_order,data$effector)

########### tests
d2 <- data_long %>%
  group_by(subject_id, effector, congruency) %>%
  summarise(rt_gabor = mean(rt_gabor))



###### try a summary
name.label <- c('acc_gabor_num_mean' = "Perceptual Accuracy", 'rt_gabor_mean' = "Perceptual Response Times (s)", "rt_conf_mean" = "Confidence Response Times (s)","conf_mean" = "Perceptual Confidence (1-4)")
data2 %>% group_by(laterality,subject_id) %>% summarise_at(vars(acc_gabor_num,rt_gabor,rt_conf,conf),lst(mean)) %>%
  pivot_longer(col=c(3,4,5,6)) %>%
  ggplot(aes(laterality,value, color=laterality)) + geom_violin(trim=FALSE) +  geom_dotplot(binaxis='y', stackdir='center',dotsize= 3)+
  stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="orange")  + facet_wrap(~name,scales='free',labeller = labeller(name=name.label))+
  labs(y="mean values")+
  labs(x="Laterality of the motor cue")
