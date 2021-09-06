rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/motor_gabor/data/AG2_data')


#rm(list=ls(all=TRUE))  ## efface les données
#source('~/thib/projects/tools/R_lib.r')
#setwd('C:/Users/remil/PhD/R/GitHub/data/AG2_data')
#source('C:/Users/remil/PhD/R/GitHub/data/AG2_data/R_lib.r')
#library('ggeffects')
load('data_final.rda')

data_complete <- data %>%
    mutate(block_10 = rep(sort(rep(1:10,40) , decreasing = FALSE),8))%>% ## rep:6->8?
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
  select(-c(trial_start_time_rel2bloc, bloc_start_time_rel2exp, exp_start_date))

data_g <- data_complete %>%
    filter(!is.na(rt_gabor))

# REMOVE REPORTED ERRORS 
data = data_g
data = data[data$conf != '0' & data$congruency == 'congruent' | data$conf != '0' & data$congruency == 'incongruent' ,]
# wrong effector used to answer + errors reported:
errors = data_g[data_g$conf == '0'| data_g$congruency == 'sameWEcongruent'| data_g$congruency == 'sameWEincongruent'| data_g$congruency == 'otherWEcongruent'| data_g$congruency == 'otherWEincongruent',]


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


d1 <- data_complete %>%
  group_by(subject_id) %>%
  filter(effector_order == 'hand1') %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
	    accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
	    conf = mean(conf, na.rm=TRUE),
	    rt_gabor = mean(rt_gabor, na.rm=TRUE),
	    rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d1), type = "html", caption = "Hands first", caption.placement = 'top')

d2 <- data_complete %>%
  group_by(subject_id) %>%
  filter(effector_order == 'feet1') %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
	    accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
	    conf = mean(conf, na.rm=TRUE),
	    rt_gabor = mean(rt_gabor, na.rm=TRUE),
	    rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d2), type = "html", caption = "Feet first", caption.placement = 'top')

d3 <- data_complete %>%
  group_by(subject_id, congruency, effector) %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
            accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
            conf = mean(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE),
            rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d3), type = "html", caption = "Feet first", caption.placement = 'top')

d4 <- data_complete %>%
  group_by(subject_id, effector) %>%
  summarise(accuracy_gabor = mean(accuracy_gabor, na.rm = TRUE),
            accuracy_cue = mean(accuracy_cue, na.rm=TRUE),
            conf = mean(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE),
            rt_cue = mean(rt_cue, na.rm=TRUE))
print(xtable(d4), type = "html", caption = "Feet first", caption.placement = 'top')

data_long = data[data$rt_gabor>=0.250,]
p  <- ggplot(data = data_long, aes(rt_gabor)) +
  geom_histogram()  
  facet_wrap( ~ congruency) +
  ggtitle('rt gabor > 400 ms')
print(p)

d1 <- data_long %>%
  group_by(subject_id, effector) %>%
  summarise(acc_gabor = mean(acc_gabor_num))
p <- ggplot(data = d, aes(x = block_10 , y = rt_gabor)) +
  geom_line() +
  facet_wrap( ~ subject_id, scales = 'free')
print(p)

p  <- ggplot(data = data_complete, aes(rt_cue)) +
  geom_histogram()  +
  facet_wrap( ~ subject_id) +
  ggtitle('rt cue')
print(p)

p  <- ggplot(data = data, aes(conf)) +
  geom_histogram()  +
  facet_wrap( ~ subject_id) +
  ggtitle('confidence')
print(p)

p  <- ggplot(data = data, aes(rt_conf)) +
  geom_histogram()  +
  facet_wrap( ~ subject_id) +
  ggtitle('rt confidence')
print(p)

d <- data %>%
  group_by(block_10, subject_id) %>%
  summarise(accuracy_gabor = mean(acc_gabor_num))
p <- ggplot(data = d, aes(x = block_10 , y = accuracy_gabor)) +
  geom_line() +
  facet_wrap( ~ subject_id, scales = 'free')
print(p)

d <- data %>%
  group_by(block_22, subject_id) %>%
  summarise(accuracy_gabor = mean(acc_gabor_num), conf = mean(conf))
p <- ggplot(data = d, aes(x = block_22 , y = conf)) +
  geom_line() +
  facet_wrap( ~ subject_id, scales = 'free')
print(p)

data$rt_gabor_centered <- data$rt_gabor - mean(data$rt_gabor, na.rm = TRUE)
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

l.conf <- lmer_alt(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
		     (1 +  accuracy_gabor  + congruency * effector + effector_order +
		      rt_gabor_centered||subject_id),
		   REML = TRUE,
		   data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.conf)

summary(rePCA(l.conf))

l.conf2 <- lmer_alt(conf ~  accuracy_gabor * congruency * effector * effector_order  +
		      rt_gabor_centered +
		      (1  + accuracy_gabor +  congruency + effector + effector_order + rt_gabor_centered ||subject_id),
		    REML = TRUE,
		    data = data)
summary(l.conf2)

l.conf3 <- lmer_alt(conf ~  congruency * effector +
                      rt_gabor_centered +
                      (1  + congruency *  effector +
                         rt_gabor_centered ||subject_id),
                    REML = TRUE,
                    data = data)
summary(l.conf3)
plot <-  ggemmeans(l.conf3, c('effector','congruency')) %>% plot()
print(plot)

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

plot <-  ggemmeans(l.conf2, c('congruency','effector')) %>% plot()
print(plot)
e <- emmeans(l.conf2, pairwise ~ congruency * effector)
e

plot <-  ggemmeans(l.conf2, c('accuracy_gabor','congruency')) %>% plot()
print(plot)
plot <-  ggemmeans(l.conf2, c('accuracy_gabor','congruency','effector')) %>% plot()
print(plot)


l.rt <- lmer_alt(rt_gabor_centered ~ accuracy_gabor * congruency * effector * effector_order +
                     (1 + accuracy_gabor + congruency * effector + effector_order  ||subject_id),
                   REML = TRUE,
                   data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.rt)

summary(rePCA(l.rt))

l.rt2 <- lmer_alt(rt_gabor_centered ~ accuracy_gabor  * congruency * effector * effector_order + 
                      (1 + accuracy_gabor + effector_order  + effector ||subject_id),
                    REML = TRUE,
                    data = data)
summary(l.rt2)
summary(rePCA(l.rt2))
Anova(l.rt2)

plot <-  ggemmeans(l.rt2, c('effector_order','effector')) %>% plot()
print(plot)
e <- emmeans(l.rt2, pairwise ~ effector_order * effector)
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
