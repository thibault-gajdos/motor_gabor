rm(list=ls(all=TRUE))  ## efface les donnÃ©es
#source('~/thib/projects/tools/R_lib.r')
setwd('C:/Users/remil/OneDrive/Documents/GitHub/motor_gabor/data/MG2_data')
source('C:/Users/remil/OneDrive/Documents/GitHub/motor_gabor/data/MG2_data/R_lib.r')
library('ggeffects')
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
  select(-c(trial_start_time_rel2bloc, bloc_start_time_rel2exp, exp_start_date))

data_g <- data_complete %>%
  filter(!is.na(rt_gabor))

data_noerror <- data_g %>%
  filter(!grepl('WE', congruency)) %>% ## remove trials with wrong effector
  filter(conf > 0) ## remove detected errors

data_noerror <- data_noerror %>%
  mutate(conf = ifelse(conf < 0, NA, conf))

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
contrasts(drata$effector_order) <-  contr.sum(2) ## hand1: -1; feet: 1
data$congruency <- as.factor(data$congruency)
contrasts(data$congruency) <-  contr.sum(2) ## incongruent: -1; congruent: 1
data$rt_gabor_centered <- data$rt_gabor - mean(data$rt_gabor, na.rm = TRUE)

d1 <- data %>%
  group_by(subject_id, congruency) %>%
  summarise(accuracy_gabor = mean(acc_gabor_num, na.rm = TRUE),
            conf = mean(conf, na.rm=TRUE),
            rt_gabor = mean(rt_gabor, na.rm=TRUE))
print(xtable(d1), type = "html", caption = "Same effector first", caption.placement = 'top')

########################### diff_rt  ~ diff_conf
k=1
diff_rt <- data.frame(matrix(ncol = 1, nrow = 24))
diff_conf <- data.frame(matrix(ncol = 1, nrow = 24))
colnames(diff_conf) = 'diff_conf'
colnames(diff_rt) = 'diff_rt'
for (f in seq(from=1, to=47, by=2)) {
  diff_rt[k,1] = d1[f,5] - d1[f+1,5]
  diff_conf[k,1] = d1[f,4] - d1[f+1,4]
  k = k + 1
}
diff_rt = as.data.frame(diff_rt)
diff_conf = as.data.frame(diff_conf)
model <- lm(as.matrix(diff_conf)~as.matrix(diff_rt))
plot(as.matrix(diff_conf)~as.matrix(diff_rt))
###########################