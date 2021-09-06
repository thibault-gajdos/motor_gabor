rm(list=ls(all=TRUE))  ## efface les données
library(purrr)
library(ggplot2)
setwd("C:/Users/remil/OneDrive/Documents/GitHub/Anticipatory_gabor/AG2_data/")
a <- read.csv(file = 'final_csv/3_final.csv',header = FALSE,stringsAsFactors = F)
a[,2] = c(1:400)
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
names(a)[18] <- "trial_start_time_rel2bloc"
names(a)[19] <- "bloc_start_time_rel2exp"
names(a)[20] <- "exp_start_date"
names(a)[21] <- "effector"

####### separate conditions 
gabor = a[a$condition=="gabor",]

congruent = a[a$congruency=="congruent",]
incongruent = a[a$congruency=="incongruent",]

cue_correct = a[a$accuracy_cue=="1",]
cue_incorrect = a[a$accuracy_cue=="0",]

gabor_correct = a[a$accuracy_gabor=="1",]
gabor_incorrect = a[a$accuracy_gabor=="0",]

same = gabor[gabor$effector == 'same', ]
other = gabor[gabor$effector =='other', ]

same_correct = same[same$accuracy_gabor=="1",]
other_correct = other[other$accuracy_gabor=="1",]

same_acc = nrow(same_correct)/nrow(same)
other_acc = nrow(other_correct)/nrow(other)

hand = gabor[gabor$effector == 'same', ]
feet = gabor[gabor$effector =='other', ]


###################################################### HANDS #######################################

hand_diff_acc = hand$accuracy_gabor
hand_diff_acc = as.numeric(unlist(hand_diff_acc))
hand_diff_acc = mean(hand_diff_acc)


################ CORRECT
hand_correct = hand[hand$accuracy_gabor=="1",]
hand_correct_congr = hand_correct[hand_correct$congruency=="congruent",]
hand_correct_incongr = hand_correct[hand_correct$congruency=="incongruent",]

################ INCORRECT
hand_incorrect = hand[hand$accuracy_gabor=="0",]
hand_incorrect_congr= hand_incorrect[hand_incorrect$congruency=="congruent",]
hand_incorrect_incongr= hand_incorrect[hand_incorrect$congruency=="incongruent",]

# RT CONGRUENT
hand_congruent_rt = congruent[congruent$effector == 'hand',]
hand_congruent_rt = hand_congruent_rt$rt_gabor
hand_congruent_rt = as.numeric(unlist(hand_congruent_rt))
hand_mean_congruent_rt = mean(hand_congruent_rt)

# RT INCONGRUENT
hand_incongruent_rt = incongruent[incongruent$effector == 'hand',]
hand_incongruent_rt = hand_incongruent_rt$rt_gabor
hand_incongruent_rt = as.numeric(unlist(hand_incongruent_rt))
hand_mean_incongruent_rt = mean(hand_incongruent_rt)

# RT CONGRUENT FEET
feet_congruent_rt = congruent[congruent$effector == 'feet',]
feet_congruent_rt = feet_congruent_rt$rt_gabor
feet_congruent_rt = as.numeric(unlist(feet_congruent_rt))
feet_mean_congruent_rt = mean(feet_congruent_rt)

# RT INCONGRUENT FEET
feet_incongruent_rt = incongruent[incongruent$effector == 'feet',]
feet_incongruent_rt = feet_incongruent_rt$rt_gabor
feet_incongruent_rt = as.numeric(unlist(feet_incongruent_rt))
feet_mean_incongruent_rt = mean(feet_incongruent_rt)

################ CONGRUENCY correct RT mean
hand_congruent_correct_rt = hand_correct_congr$rt_gabor
hand_congruent_correct_rt = as.numeric(unlist(hand_congruent_correct_rt))
hand_mean_congruent_correct_rt = mean(hand_congruent_correct_rt)

hand_incongruent_correct_rt = hand_correct_incongr$rt_gabor
hand_incongruent_correct_rt = as.numeric(unlist(hand_incongruent_correct_rt))
hand_mean_incongruent_correct_rt = mean(hand_incongruent_correct_rt)

################ CONGRUENCY incorrect RT mean
hand_congruent_incorrect_rt = hand_incorrect_congr$rt_gabor
hand_congruent_incorrect_rt = as.numeric(unlist(hand_congruent_incorrect_rt))
hand_mean_congruent_incorrect_rt = mean(hand_congruent_incorrect_rt)

hand_incongruent_incorrect_rt = hand_incorrect_incongr$rt_gabor
hand_incongruent_incorrect_rt = as.numeric(unlist(hand_incongruent_incorrect_rt))
hand_mean_incongruent_incorrect_rt = mean(hand_incongruent_incorrect_rt)

################ CONGRUENCY accuracy
hand_congruent = hand[hand$congruency == 'congruent',]
hand_congruent_acc = hand_congruent$accuracy_gabor
hand_congruent_acc = as.numeric(unlist(hand_congruent_acc))
hand_mean_congruent_acc = mean(hand_congruent_acc)

hand_incongruent = hand[hand$congruency == 'incongruent',]
hand_incongruent_acc = hand_incongruent$accuracy_gabor
hand_incongruent_acc = as.numeric(unlist(hand_incongruent_acc))
hand_mean_incongruent_acc = mean(hand_incongruent_acc)

################ CORRECT
feet_correct = feet[feet$accuracy_gabor=="1",]
feet_correct_congr = feet_correct[feet_correct$congruency=="congruent",]
feet_correct_incongr = feet_correct[feet_correct$congruency=="incongruent",]

################ INCORRECT
feet_incorrect = feet[feet$accuracy_gabor=="0",]
feet_incorrect_congr= feet_incorrect[feet_incorrect$congruency=="congruent",]
feet_incorrect_incongr= feet_incorrect[feet_incorrect$congruency=="incongruent",]

################ CONGRUENCY correct RT mean
feet_congruent_correct_rt = feet_correct_congr$rt_gabor
feet_congruent_correct_rt = as.numeric(unlist(feet_congruent_correct_rt))
feet_mean_congruent_correct_rt = mean(feet_congruent_correct_rt)

feet_incongruent_correct_rt = feet_correct_incongr$rt_gabor
feet_incongruent_correct_rt = as.numeric(unlist(feet_incongruent_correct_rt))
feet_mean_incongruent_correct_rt = mean(feet_incongruent_correct_rt)

################ CONGRUENCY incorrect RT mean
feet_congruent_incorrect_rt = feet_incorrect_congr$rt_gabor
feet_congruent_incorrect_rt = as.numeric(unlist(feet_congruent_incorrect_rt))
feet_mean_congruent_incorrect_rt = mean(feet_congruent_incorrect_rt)

feet_incongruent_incorrect_rt = feet_incorrect_incongr$rt_gabor
feet_incongruent_incorrect_rt = as.numeric(unlist(feet_incongruent_incorrect_rt))
feet_mean_incongruent_incorrect_rt = mean(feet_incongruent_incorrect_rt)

################ CONGRUENCY accuracy
feet_congruent = feet[feet$congruency == 'congruent',]
feet_congruent_acc = feet_congruent$accuracy_gabor
feet_congruent_acc = as.numeric(unlist(feet_congruent_acc))
feet_mean_congruent_acc = mean(feet_congruent_acc)

feet_incongruent = feet[feet$congruency == 'incongruent',]
feet_incongruent_acc = feet_incongruent$accuracy_gabor
feet_incongruent_acc = as.numeric(unlist(feet_incongruent_acc))
feet_mean_incongruent_acc = mean(feet_incongruent_acc)


######################################## DISPLAY  ############################################ 

cat("acc = ",hand_diff_acc,"\n")

#hand
cat("same_acc = ", same_acc,"\n")
cat("other_acc = ", other_acc,"\n")

#### TABLES
table_sum <- matrix(c(hand_mean_congruent_acc,feet_mean_congruent_acc,hand_mean_incongruent_acc,feet_mean_incongruent_acc,hand_mean_congruent_correct_rt,feet_mean_congruent_correct_rt,hand_mean_incongruent_correct_rt,feet_mean_incongruent_correct_rt,hand_mean_congruent_incorrect_rt,feet_mean_congruent_incorrect_rt,hand_mean_incongruent_incorrect_rt,feet_mean_incongruent_incorrect_rt),ncol=2,byrow=TRUE)
colnames(table_sum) <- c("same","other")
rownames(table_sum) <- c("congruent_acc","incongruent_acc","congruent_correct_rt","incongruent_correct_rt","congruent_incorrect_rt","incongruent_incorrect_rt")
table_sum <- as.table(table_sum)

table_sum

