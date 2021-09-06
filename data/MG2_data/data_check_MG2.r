setwd("C:/Users/remil/OneDrive/Documents/GitHub/Anticipatory_gabor/AG_data/final_csv/") # repertory with final participants' dataframes
library(dplyr)
library(ggplot2)
a <- read.csv(file = '16_final.csv',header = FALSE,stringsAsFactors = F) # replace with subject's number
for (i in 1:396) # need to re-index trials from second block from 396 to 792
{
  a[396 + i,2] <- i + 396
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

####### separate conditions 
gabor = a[a$condition=="gabor",]

congruent = a[a$congruency=="congruent",]
incongruent = a[a$congruency=="incongruent",]

cue_correct = a[a$accuracy_cue=="1",]
cue_incorrect = a[a$accuracy_cue=="0",]

gabor_correct = a[a$accuracy_gabor=="1",]
gabor_incorrect = a[a$accuracy_gabor=="0",]

hand = gabor[gabor$effector == 'hand', ]
feet = gabor[gabor$effector =='feet', ]

hand_correct = hand[hand$accuracy_gabor=="1",]
feet_correct = feet[feet$accuracy_gabor=="1",]

hand_acc = nrow(hand_correct)/nrow(hand)
feet_acc = nrow(feet_correct)/nrow(feet)

acc = nrow(gabor_correct)/nrow(gabor)

gabor %>% # 
  ggplot(aes(conf))+
  geom_histogram()+
  labs(x="Confidence (1-4)")

conf_table <- table(gabor$conf)

acc
hand_acc
feet_acc
conf_table