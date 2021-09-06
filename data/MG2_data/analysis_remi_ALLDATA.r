rm(list=ls(all=TRUE))  ## efface les données

################################################################################################# MG2
source('C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/MG2_data/R_lib.r')
setwd('C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/MG2_data/final_csv')

## * Prepare
# loop over files and append
files = list.files(path='C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/MG2_data/final_csv', pattern = 'csv', recursive = F)
a = list()
for (f in 1:length(files)) {
    df2 <- read.csv(file = files[f],header = FALSE,stringsAsFactors = F)
    df2[,2] = c(1:400)
    a = bind_rows(a, df2)
}
names(a)[1] <- "subject_id"
names(a)[2] <- "trial"
names(a)[3] <- "effector_order"
names(a)[4] <- "block"
names(a)[5] <- "condition"
names(a)[6] <- "stim1"
names(a)[7] <- "gabor.contrast"
names(a)[8] <- "pressed_gabor"
names(a)[9] <- "expected_gabor"
names(a)[10] <- "pressed_cue"
names(a)[11] <- "expected_cue"
names(a)[12] <- "rt_gabor"
names(a)[13] <- "rt_cue"
names(a)[14] <- "accuracy_gabor"
names(a)[15] <- "accuracy_cue"
names(a)[16] <- "congruency"
names(a)[17] <- "SDT"
names(a)[18] <- "pressed_conf"
names(a)[19] <- "conf"
names(a)[20] <- "rt_conf"
names(a)[21] <- "trial_start_time_rel2bloc"
names(a)[22] <- "bloc_start_time_rel2exp"
names(a)[23] <- "exp_start_date"
names(a)[24] <- "effector"

a$effector[a$subject_id=='1' & a$effector_order=='same1'] = 'same' # add effector for subject 1 missing block2 effector
a$effector[a$subject_id=='1' & a$effector_order=='other2'] = 'other'

a[4001:4400,1] = '19' # change subject_id
a[9201:9600,1] = '17' # change subject_id

mg2 <- a %>%
  filter(condition=='gabor')

mg2$participants = 'mg2'

########## add excluded MG2 subjects ######################

setwd('C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/MG2_data/excluded/final_csv_exc')
files = list.files(path='C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/MG2_data/excluded/final_csv_exc', pattern = 'csv', recursive = F)
c = list()
for (f in 1:length(files)) {
  df2ex <- read.csv(file = files[f],header = FALSE,stringsAsFactors = F)
  df2ex[,2] = c(1:400)
  c = bind_rows(c, df2ex)
}

names(c)[1] <- "subject_id"
names(c)[2] <- "trial"
names(c)[3] <- "effector_order"
names(c)[4] <- "block"
names(c)[5] <- "condition"
names(c)[6] <- "stim1"
names(c)[7] <- "gabor.contrast"
names(c)[8] <- "pressed_gabor"
names(c)[9] <- "expected_gabor"
names(c)[10] <- "pressed_cue"
names(c)[11] <- "expected_cue"
names(c)[12] <- "rt_gabor"
names(c)[13] <- "rt_cue"
names(c)[14] <- "accuracy_gabor"
names(c)[15] <- "accuracy_cue"
names(c)[16] <- "congruency"
names(c)[17] <- "SDT"
names(c)[18] <- "pressed_conf"
names(c)[19] <- "conf"
names(c)[20] <- "rt_conf"
names(c)[21] <- "trial_start_time_rel2bloc"
names(c)[22] <- "bloc_start_time_rel2exp"
names(c)[23] <- "exp_start_date"
names(c)[24] <- "effector"

c[1:400,1] = '25' # change subject_id
c[401:800,1] = '26' # change subject_id
c[801:1200,1] = '27' # change subject_id
c[1201:1600,1] = '28' # change subject_id

c[1601:2000,1] = '29' # change subject_id
c[2001:2400,1] = '30' # change subject_id
c[2401:2800,1] = '31' # change subject_id
c[2801:3200,1] = '32' # change subject_id

mg2_excluded <- c %>%
  filter(condition=='gabor')

mg2_excluded$participants = 'mg2_excluded'


################################################################################################# MG1

setwd('C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/AG_data/final_csv')

## * Prepare
# loop over files and append
files = list.files(path="C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/AG_data/final_csv", pattern = 'csv', recursive = F)
b = list()
for (f in 1:length(files)) {
  df1 <- read.csv(file = files[f],header = FALSE,stringsAsFactors = F)
  df1[,2] = c(1:792)
  b = bind_rows(b, df1)
}
names(b)[1] <- "subject_id"
names(b)[2] <- "trial"
names(b)[3] <- "effector_order"
names(b)[4] <- "block"
names(b)[5] <- "condition"
names(b)[6] <- "stim1"
names(b)[7] <- "gabor.contrast"
names(b)[8] <- "pressed_gabor"
names(b)[9] <- "expected_gabor"
names(b)[10] <- "pressed_cue"
names(b)[11] <- "expected_cue"
names(b)[12] <- "rt_gabor"
names(b)[13] <- "rt_cue"
names(b)[14] <- "accuracy_gabor"
names(b)[15] <- "accuracy_cue"
names(b)[16] <- "congruency"
names(b)[17] <- "SDT"
names(b)[18] <- "pressed_conf"
names(b)[19] <- "conf"
names(b)[20] <- "rt_conf"
names(b)[21] <- "trial_start_time_rel2bloc"
names(b)[22] <- "bloc_start_time_rel2exp"
names(b)[23] <- "exp_start_date"
names(b)[24] <- "effector"


for (i in 1:16){
  b[1+(792*(i-1)):((792*(i-1)) + 792),1] <- i + 32 # change subject_id
}
b <- b[-nrow(b),]

mg1 <- b %>%
  filter(condition=='gabor')

mg1$participants = 'mg1'

mg1$subject_id <- as.character(mg1$subject_id)
mg1[,1] <- as.character(mg1[,1])
mg1$pressed_gabor <- as.character(mg1$pressed_gabor)
mg1$expected_gabor <- as.character(mg1$expected_gabor)

########## add excluded MG1 subjects ######################

setwd('C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/AG_data/excluded_data/final csv exc')

files = list.files(path='C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/AG_data/excluded_data/final csv exc', pattern = 'csv', recursive = F)
d = list()
for (f in 1:length(files)) {
  df2ex <- read.csv(file = files[f],header = FALSE,stringsAsFactors = F)
  df2ex[,2] = c(1:792)
  d = bind_rows(d, df2ex)
}

names(d)[1] <- "subject_id"
names(d)[2] <- "trial"
names(d)[3] <- "effector_order"
names(d)[4] <- "block"
names(d)[5] <- "condition"
names(d)[6] <- "stim1"
names(d)[7] <- "gabor.contrast"
names(d)[8] <- "pressed_gabor"
names(d)[9] <- "expected_gabor"
names(d)[10] <- "pressed_cue"
names(d)[11] <- "expected_cue"
names(d)[12] <- "rt_gabor"
names(d)[13] <- "rt_cue"
names(d)[14] <- "accuracy_gabor"
names(d)[15] <- "accuracy_cue"
names(d)[16] <- "congruency"
names(d)[17] <- "SDT"
names(d)[18] <- "pressed_conf"
names(d)[19] <- "conf"
names(d)[20] <- "rt_conf"
names(d)[21] <- "trial_start_time_rel2bloc"
names(d)[22] <- "bloc_start_time_rel2exp"
names(d)[23] <- "exp_start_date"
names(d)[24] <- "effector"


for (i in 1:10){
  d[1+(792*(i-1)):((792*(i-1)) + 792),1] <- i + 48 # change subject_id
}
d <- d[-nrow(d),]

mg1_excluded <- d %>%
  filter(condition=='gabor')

mg1_excluded$participants = 'mg1_excluded'

mg1_excluded$subject_id <- as.character(mg1_excluded$subject_id)
mg1_excluded[,1] <- as.character(mg1_excluded[,1])
mg1_excluded$pressed_gabor <- as.character(mg1_excluded$pressed_gabor)
mg1_excluded$expected_gabor <- as.character(mg1_excluded$expected_gabor)

############################################################################## FINAL PREPROCESS AND SAVE
mg1_final = list()
mg1_final = bind_rows(mg1_final,mg1)
mg1_final = bind_rows(mg1_final,mg1_excluded)

mg2_final = list()
mg2_final = bind_rows(mg2_final,mg2)
mg2_final = bind_rows(mg2_final,mg2_excluded)


############### PREPROCESS MG1

# change feet and hands to other and same
mg1_final$effector[mg1_final$effector=='hand'] = 'same'
mg1_final$effector[mg1_final$effector=='feet'] = 'other'
mg1_final$effector_order[mg1_final$effector_order=='hand1'] = 'same1'
mg1_final$effector_order[mg1_final$effector_order=='hand2'] = 'same2'
mg1_final$effector_order[mg1_final$effector_order=='feet1'] = 'other1'
mg1_final$effector_order[mg1_final$effector_order=='feet2'] = 'other2'



mg1_final <- mg1_final  %>%
  mutate(block_22 = rep(sort(rep(1:22,12) , decreasing = FALSE),26)) %>%
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
  select(-c(trial_start_time_rel2bloc, bloc_start_time_rel2exp, exp_start_date,  pressed_conf))


############### PREPROCESS MG2

mg2_final <- mg2_final  %>%
  mutate(block_10 = rep(sort(rep(1:10,20) , decreasing = FALSE),32)) %>%
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


mg2_final <- mg2_final %>%
  filter(!grepl('WE', congruency)) %>%
  filter(!grepl(0, conf))






final_df = list()
final_df = bind_rows(final_df,mg2_final)
final_df = bind_rows(final_df,mg1_final)

big_data <- final_df

## define contrasts
big_data$acc_gabor_num <- big_data$accuracy_gabor ## on garde la variable 0/1 pour l'analyse de accuracy
big_data$accuracy_gabor <- as.factor(big_data$accuracy_gabor)
contrasts(big_data$accuracy_gabor) <- - contr.sum(2) ## erreur: -1; correct: 1
big_data$acc_cue_num <- big_data$accuracy_cue
big_data$effector <- as.factor(big_data$effector)
contrasts(big_data$effector) <-  -contr.sum(2) ## same: -1; other: 1
big_data$condition <- as.factor(big_data$condition)
big_data$effector_order <- as.factor(big_data$effector_order)
contrasts(big_data$effector_order) <-  contr.sum(2) ## same1: -1; other1: 1
big_data$congruency <- as.factor(big_data$congruency)
contrasts(big_data$congruency) <-  contr.sum(2) ## incongruent: -1; congruent: 1
big_data$rt_gabor_centered <- big_data$rt_gabor - mean(big_data$rt_gabor, na.rm = TRUE)

big_data$participants <- as.factor(big_data$participants)
contrasts(big_data$participants) <- contr.sum(4)

#save(big_data, file = 'C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/MG2_data/big_data_final.rda')





########################## ANALYSES ##########################


l.conf <- lmer_alt(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
                     (1 +  accuracy_gabor  + congruency * effector + effector_order + rt_gabor_centered||subject_id),
                   REML = TRUE,
                    data = big_data)
summary(l.conf)
summary(rePCA(l.conf))


l.conf2 <- lmer_alt(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
                     (1 +  accuracy_gabor  + effector + effector_order + rt_gabor_centered||subject_id),
                   REML = TRUE,
                   data = big_data)

summary(l.conf2)

plot <-  ggemmeans(l.conf2, c('accuracy_gabor','congruency','effector')) %>% plot()
print(plot)


l.acc2 <- lmer_alt(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
                    (1  |subject_id),
                  family = binomial(link = "logit"),
                  data = big_data)
summary(l.acc2)

summary(rePCA(l.acc2))

l.rt <- lmer_alt(rt_gabor_centered ~  accuracy_gabor * congruency * effector * effector_order +
                   (1 + accuracy_gabor + congruency + effector + effector_order  ||subject_id),
                 REML = TRUE,
                 data = data %>% filter(is.na(accuracy_gabor) == FALSE))
summary(l.rt)

summary(rePCA(l.rt))
