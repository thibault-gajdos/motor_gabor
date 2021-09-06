rm(list=ls(all=TRUE))  ## efface les données
library(purrr)
library(ggplot2)
setwd("C:/Users/remil/OneDrive/Documents/GitHub/Anticipatory_gabor/AG2_data/")
a <- read.csv(file = 'final_csv/2_final.csv',header = FALSE,stringsAsFactors = F)
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
names(a)[18] <- "pressed_conf"
names(a)[19] <- "conf"
names(a)[20] <- "rt_conf"
names(a)[21] <- "trial_start_time_rel2bloc"
names(a)[22] <- "bloc_start_time_rel2exp"
names(a)[23] <- "exp_start_date"
names(a)[24] <- "effector"

####### separate conditions 
data = a[a$condition=="gabor",]
gabor = data[data$conf != '0',]
errors = data[data$conf == '0',]


gabor$accuracy_gabor[gabor$accuracy_gabor == '0' & gabor$effector_order == 'same2'] = '3'
gabor$accuracy_gabor[gabor$accuracy_gabor == '1'& gabor$effector_order == 'same2'] = '4'
gabor$accuracy_gabor[gabor$accuracy_gabor == '3'& gabor$effector_order == 'same2'] = '1'
gabor$accuracy_gabor[gabor$accuracy_gabor == '4'& gabor$effector_order == 'same2'] = '0'

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
acc = nrow(gabor_correct)/nrow(gabor)

same = gabor[gabor$effector == 'same', ]
other = gabor[gabor$effector =='other', ]


###################################################### sameS #######################################

same_diff_acc = same$accuracy_gabor
same_diff_acc = as.numeric(unlist(same_diff_acc))
same_diff_acc = mean(same_diff_acc)


################ CORRECT
same_correct = same[same$accuracy_gabor=="1",]
same_correct_congr = same_correct[same_correct$congruency=="congruent",]
same_correct_incongr = same_correct[same_correct$congruency=="incongruent",]

################ INCORRECT
same_incorrect = same[same$accuracy_gabor=="0",]
same_incorrect_congr= same_incorrect[same_incorrect$congruency=="congruent",]
same_incorrect_incongr= same_incorrect[same_incorrect$congruency=="incongruent",]

################ CONGRUENCY correct RT mean
same_congruent_correct_rt = same_correct_congr$rt_gabor
same_congruent_correct_rt = as.numeric(unlist(same_congruent_correct_rt))
same_mean_congruent_correct_rt = mean(same_congruent_correct_rt)

same_incongruent_correct_rt = same_correct_incongr$rt_gabor
same_incongruent_correct_rt = as.numeric(unlist(same_incongruent_correct_rt))
same_mean_incongruent_correct_rt = mean(same_incongruent_correct_rt)

################ CONGRUENCY incorrect RT mean
same_congruent_incorrect_rt = same_incorrect_congr$rt_gabor
same_congruent_incorrect_rt = as.numeric(unlist(same_congruent_incorrect_rt))
same_mean_congruent_incorrect_rt = mean(same_congruent_incorrect_rt)

same_incongruent_incorrect_rt = same_incorrect_incongr$rt_gabor
same_incongruent_incorrect_rt = as.numeric(unlist(same_incongruent_incorrect_rt))
same_mean_incongruent_incorrect_rt = mean(same_incongruent_incorrect_rt)

################ CONGRUENCY accuracy
same_congruent = same[same$congruency == 'congruent',]
same_congruent_acc = same_congruent$accuracy_gabor
same_congruent_acc = as.numeric(unlist(same_congruent_acc))
same_mean_congruent_acc = mean(same_congruent_acc)

same_incongruent = same[same$congruency == 'incongruent',]
same_incongruent_acc = same_incongruent$accuracy_gabor
same_incongruent_acc = as.numeric(unlist(same_incongruent_acc))
same_mean_incongruent_acc = mean(same_incongruent_acc)

################ CORRECT
other_correct = other[other$accuracy_gabor=="1",]
other_correct_congr = other_correct[other_correct$congruency=="congruent",]
other_correct_incongr = other_correct[other_correct$congruency=="incongruent",]

################ INCORRECT
other_incorrect = other[other$accuracy_gabor=="0",]
other_incorrect_congr= other_incorrect[other_incorrect$congruency=="congruent",]
other_incorrect_incongr= other_incorrect[other_incorrect$congruency=="incongruent",]

################ CONGRUENCY correct RT mean
other_congruent_correct_rt = other_correct_congr$rt_gabor
other_congruent_correct_rt = as.numeric(unlist(other_congruent_correct_rt))
other_mean_congruent_correct_rt = mean(other_congruent_correct_rt)

other_incongruent_correct_rt = other_correct_incongr$rt_gabor
other_incongruent_correct_rt = as.numeric(unlist(other_incongruent_correct_rt))
other_mean_incongruent_correct_rt = mean(other_incongruent_correct_rt)

################ CONGRUENCY incorrect RT mean
other_congruent_incorrect_rt = other_incorrect_congr$rt_gabor
other_congruent_incorrect_rt = as.numeric(unlist(other_congruent_incorrect_rt))
other_mean_congruent_incorrect_rt = mean(other_congruent_incorrect_rt)

other_incongruent_incorrect_rt = other_incorrect_incongr$rt_gabor
other_incongruent_incorrect_rt = as.numeric(unlist(other_incongruent_incorrect_rt))
other_mean_incongruent_incorrect_rt = mean(other_incongruent_incorrect_rt)

################ CONGRUENCY accuracy
other_congruent = other[other$congruency == 'congruent',]
other_congruent_acc = other_congruent$accuracy_gabor
other_congruent_acc = as.numeric(unlist(other_congruent_acc))
other_mean_congruent_acc = mean(other_congruent_acc)

other_incongruent = other[other$congruency == 'incongruent',]
other_incongruent_acc = other_incongruent$accuracy_gabor
other_incongruent_acc = as.numeric(unlist(other_incongruent_acc))
other_mean_incongruent_acc = mean(other_incongruent_acc)


################ CONFIDENCE same
same_conf = same
same_conf_mean = same_conf$conf
same_conf_mean = as.numeric(unlist(same_conf_mean))
same_conf_mean = mean(same_conf_mean)

same_conf_congruent = same_conf[same_conf$congruency == 'congruent',]
same_conf_congruent_mean = same_conf_congruent$conf
same_conf_congruent_mean = as.numeric(unlist(same_conf_congruent_mean))
same_conf_congruent_mean = mean(same_conf_congruent_mean)

same_conf_incongruent = same_conf[same_conf$congruency == 'incongruent',]
same_conf_incongruent_mean = same_conf_incongruent$conf
same_conf_incongruent_mean = as.numeric(unlist(same_conf_incongruent_mean))
same_conf_incongruent_mean = mean(same_conf_incongruent_mean)

################ CONFIDENCE other
other_conf = other
other_conf_mean = other_conf$conf
other_conf_mean = as.numeric(unlist(other_conf_mean))
other_conf_mean = mean(other_conf_mean)

other_conf_congruent = other_conf[other_conf$congruency == 'congruent',]
other_conf_congruent_mean = other_conf_congruent$conf
other_conf_congruent_mean = as.numeric(unlist(other_conf_congruent_mean))
other_conf_congruent_mean = mean(other_conf_congruent_mean)

other_conf_incongruent = other_conf[other_conf$congruency == 'incongruent',]
other_conf_incongruent_mean = other_conf_incongruent$conf
other_conf_incongruent_mean = as.numeric(unlist(other_conf_incongruent_mean))
other_conf_incongruent_mean = mean(other_conf_incongruent_mean)

######################################## DISPLAY  ############################################ 

cat("acc = ",acc,"\n")

#same
cat("same_acc = ", same_acc,"\n")
cat("other_acc = ", other_acc,"\n")

#### TABLES
table_sum <- matrix(c(same_mean_congruent_acc,other_mean_congruent_acc,same_mean_incongruent_acc,other_mean_incongruent_acc,same_mean_congruent_correct_rt,other_mean_congruent_correct_rt,same_mean_incongruent_correct_rt,other_mean_incongruent_correct_rt,same_mean_congruent_incorrect_rt,other_mean_congruent_incorrect_rt,same_mean_incongruent_incorrect_rt,other_mean_incongruent_incorrect_rt),ncol=2,byrow=TRUE)
colnames(table_sum) <- c("same","other")
rownames(table_sum) <- c("congruent_acc","incongruent_acc","congruent_correct_rt","incongruent_correct_rt","congruent_incorrect_rt","incongruent_incorrect_rt")
table_sum <- as.table(table_sum)

table_conf <- matrix(c(same_conf_mean,other_conf_mean,same_conf_congruent_mean,other_conf_congruent_mean,same_conf_incongruent_mean,other_conf_incongruent_mean ),ncol=2,byrow=TRUE)
colnames(table_conf) <- c("same","other")
rownames(table_conf) <- c("confidence","confidence_congruent","confidence_incongruent")
table_conf <- as.table(table_conf)


gabor %>% # 
  ggplot(aes(conf))+
  geom_histogram()+
  labs(x="Confidence (1-4)")

gabor_block = gabor
gabor_block$block[gabor_block$effector_order == 'other2' & gabor_block$block == '1'] = '6'
gabor_block$block[gabor_block$effector_order == 'other2' & gabor_block$block == '2'] = '7'
gabor_block$block[gabor_block$effector_order == 'other2' & gabor_block$block == '3'] = '8'
gabor_block$block[gabor_block$effector_order == 'other2' & gabor_block$block == '4'] = '9'
gabor_block$block[gabor_block$effector_order == 'other2' & gabor_block$block == '5'] = '10'

gabor_block$block[gabor_block$effector_order == 'same2' & gabor_block$block == '1'] = '6'
gabor_block$block[gabor_block$effector_order == 'same2' & gabor_block$block == '2'] = '7'
gabor_block$block[gabor_block$effector_order == 'same2' & gabor_block$block == '3'] = '8'
gabor_block$block[gabor_block$effector_order == 'same2' & gabor_block$block == '4'] = '9'
gabor_block$block[gabor_block$effector_order == 'same2' & gabor_block$block == '5'] = '10'


#d1 <- gabor_block %>%
#  group_by(block) %>%
#  summarise(acc_gabor = mean(accuracy_gabor))
#plot(x=d1$block, y=d1$acc_gabor) 


table_sum
table_conf