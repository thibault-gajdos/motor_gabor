rm(list=ls(all=TRUE))  ## efface les données
library(purrr)
library(ggplot2)
setwd("C:/Users/remil/OneDrive/Documents/GitHub/Anticipatory_gabor/AG2_data/")
a <- read.csv(file = 'final_csv/6_final.csv',header = FALSE,stringsAsFactors = F)
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

a$effector[201:400] = 'same'
####### separate conditions 
data = a[a$condition=="gabor",]
gabor = data[data$conf != '0' & data$congruency == 'congruent' | data$conf != '0' & data$congruency == 'incongruent' ,]
errors = data[data$conf == '0'| data$congruency == 'sameWEcongruent'| data$congruency == 'sameWEincongruent'| data$congruency == 'otherWEcongruent'| data$congruency == 'otherWEincongruent',]

congruent = gabor[gabor$congruency=="congruent",]
incongruent = gabor[gabor$congruency=="incongruent",]

cue_correct = a[a$accuracy_cue=="1",]
cue_incorrect = a[a$accuracy_cue=="0",]

gabor_correct = gabor[gabor$accuracy_gabor=="1",]
gabor_incorrect = gabor[gabor$accuracy_gabor=="0",]

############################################################################### SDT
################# same 

same_gabor = gabor[gabor$effector  == 'same',]
same_gabor_congruent = same_gabor[same_gabor$congruency == 'congruent',]
same_gabor_incongruent = same_gabor[same_gabor$congruency == 'incongruent',]

### congruent
  
same_CONG_hit <- same_gabor_congruent[same_gabor_congruent$SDT=='hit',] 
same_CONG_hit <- as.data.frame(table(same_CONG_hit$accuracy_gabor))
if (is_empty(same_CONG_hit[1,2])){
  same_CONG_hit[1,2] = 0
}

same_CONG_FA <- same_gabor_congruent[same_gabor_congruent$SDT=='FA',]
same_CONG_FA <- as.data.frame(table(same_CONG_FA$accuracy_gabor))
if (is_empty(same_CONG_FA[1,2])){
  same_CONG_FA[1,2] = 0
}

same_CONG_miss <- same_gabor_congruent[same_gabor_congruent$SDT=='miss',]
same_CONG_miss <- as.data.frame(table(same_CONG_miss$accuracy_gabor))
if (is_empty(same_CONG_miss[1,2])){
  same_CONG_miss[1,2] = 0
}

same_CONG_CR <- same_gabor_congruent[same_gabor_congruent$SDT=='CR',]
same_CONG_CR <- as.data.frame(table(same_CONG_CR$accuracy_gabor))
if (is_empty(same_CONG_CR[1,2])){
  same_CONG_CR[1,2] = 0
}

#### incongruent 
same_INC_hit <- same_gabor_incongruent[same_gabor_incongruent$SDT=='hit',]
same_INC_hit <- as.data.frame(table(same_INC_hit$accuracy_gabor))
if (is_empty(same_INC_hit[1,2])){
  same_INC_hit[1,2] = 0
}

same_INC_FA <- same_gabor_incongruent[same_gabor_incongruent$SDT=='FA',]
same_INC_FA <- as.data.frame(table(same_INC_FA$accuracy_gabor))
if (is_empty(same_INC_FA[1,2])){
  same_INC_FA[1,2] = 0
}

same_INC_miss <- same_gabor_incongruent[same_gabor_incongruent$SDT=='miss',]
same_INC_miss <- as.data.frame(table(same_INC_miss$accuracy_gabor))
if (is_empty(same_INC_miss[1,2])){
  same_INC_miss[1,2] = 0
}

same_INC_CR <- same_gabor_incongruent[same_gabor_incongruent$SDT=='CR',]
same_INC_CR <- as.data.frame(table(same_INC_CR$accuracy_gabor))
if (is_empty(same_INC_CR[1,2])){
  same_INC_CR[1,2] = 0
}

same_SDTdf <- data.frame(Tache = c("congruent","incongruent"),
                    n_hit = c(same_CONG_hit[1,2],same_INC_hit[1,2]),
                    n_fa = c(same_CONG_FA[1,2],same_INC_FA[1,2]),
                    n_miss = c(same_CONG_miss[1,2],same_INC_miss[1,2]),
                    n_cr = c(same_CONG_CR[1,2],same_INC_CR[1,2]))
same_indices <- psycho::dprime(same_SDTdf$n_hit, same_SDTdf$n_fa, same_SDTdf$n_miss, same_SDTdf$n_cr)
same_SDTdf <- cbind(same_SDTdf, same_indices)

same_congruent_dprime = same_SDTdf[1,6]
same_incongruent_dprime = same_SDTdf[2,6]
same_congruent_bias = same_SDTdf[1,10]
same_incongruent_bias = same_SDTdf[2,10]

################# other

other_gabor = gabor[gabor$effector  == 'other',]
other_gabor_congruent = other_gabor[other_gabor$congruency == 'congruent',]
other_gabor_incongruent = other_gabor[other_gabor$congruency == 'incongruent',]

### congruent
other_CONG_hit <- other_gabor_congruent[other_gabor_congruent$SDT=='hit',]
other_CONG_hit <- as.data.frame(table(other_CONG_hit$accuracy_gabor))
if (is_empty(other_CONG_hit[1,2])){
  other_CONG_hit[1,2] = 0
}

other_CONG_FA <- other_gabor_congruent[other_gabor_congruent$SDT=='FA',]
other_CONG_FA <- as.data.frame(table(other_CONG_FA$accuracy_gabor))
if (is_empty(other_CONG_FA[1,2])){
  other_CONG_FA[1,2] = 0
}

other_CONG_miss <- other_gabor_congruent[other_gabor_congruent$SDT=='miss',]
other_CONG_miss <- as.data.frame(table(other_CONG_miss$accuracy_gabor))
if (is_empty(other_CONG_miss[1,2])){
  other_CONG_miss[1,2] = 0
}

other_CONG_CR <- other_gabor_congruent[other_gabor_congruent$SDT=='CR',]
other_CONG_CR <- as.data.frame(table(other_CONG_CR$accuracy_gabor))
if (is_empty(other_CONG_CR[1,2])){
  other_CONG_CR[1,2] = 0
}

#### incongruent 
other_INC_hit <- other_gabor_incongruent[other_gabor_incongruent$SDT=='hit',]
other_INC_hit <- as.data.frame(table(other_INC_hit$accuracy_gabor))
if (is_empty(other_INC_hit[1,2])){
  other_INC_hit[1,2] = 0
}

other_INC_FA <- other_gabor_incongruent[other_gabor_incongruent$SDT=='FA',]
other_INC_FA <- as.data.frame(table(other_INC_FA$accuracy_gabor))
if (is_empty(other_INC_FA[1,2])){
  other_INC_FA[1,2] = 0
}

other_INC_miss <- other_gabor_incongruent[other_gabor_incongruent$SDT=='miss',]
other_INC_miss <- as.data.frame(table(other_INC_miss$accuracy_gabor))
if (is_empty(other_INC_miss[1,2])){
  other_INC_miss[1,2] = 0
}

other_INC_CR <- other_gabor_incongruent[other_gabor_incongruent$SDT=='CR',]
other_INC_CR <- as.data.frame(table(other_INC_CR$accuracy_gabor))
if (is_empty(other_INC_CR[1,2])){
  other_INC_CR[1,2] = 0
}


other_SDTdf <- data.frame(Tache = c("congruent","incongruent"),
                         n_hit = c(other_CONG_hit[1,2],other_INC_hit[1,2]),
                         n_fa = c(other_CONG_FA[1,2],other_INC_FA[1,2]),
                         n_miss = c(other_CONG_miss[1,2],other_INC_miss[1,2]),
                         n_cr = c(other_CONG_CR[1,2],other_INC_CR[1,2]))
other_indices <- psycho::dprime(other_SDTdf$n_hit, other_SDTdf$n_fa, other_SDTdf$n_miss, other_SDTdf$n_cr)
other_SDTdf <- cbind(other_SDTdf, other_indices)

other_congruent_dprime = other_SDTdf[1,6]
other_incongruent_dprime = other_SDTdf[2,6]
other_congruent_bias = other_SDTdf[1,10]
other_incongruent_bias = other_SDTdf[2,10]

######################################## DISPLAY  ############################################ 


table_SDT <- matrix(c(same_congruent_dprime,other_congruent_dprime,same_incongruent_dprime,other_incongruent_dprime,same_congruent_bias,other_congruent_bias,same_incongruent_bias,other_incongruent_bias),ncol=2,byrow=TRUE)
colnames(table_SDT) <- c("same","other")
rownames(table_SDT) <- c("dprime_congruent","dprime_incongruent","bias_congruent","bias_incongruent")
table_SDT <- as.table(table_SDT)

table_SDT