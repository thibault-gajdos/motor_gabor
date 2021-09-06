rm(list=ls(all=TRUE))  ## efface les données
source('C:/Users/remil/PhD/R/GitHub/data/AG2_data/R_lib.r')
setwd('C:/Users/remil/PhD/R/GitHub/data/AG2_data/final_csv')

## * Prepare
# loop over files and append
files = list.files(path='C:/Users/remil/PhD/R/GitHub/data/AG2_data/final_csv', pattern = 'csv', recursive = F)
a = list()
for (f in 1:length(files)) {
    df <- read.csv(file = files[f],header = FALSE,stringsAsFactors = F)
    df[,2] = c(1:400)
    a = bind_rows(a, df)
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

a$effector[a$subject_id=='6' & a$effector_order=='same2'] = 'same' # add effector for subject 6 missing block2 effector

# inverse accuracy and buttosn because wrong condition 
a$accuracy_gabor[a$accuracy_gabor == '0' & a$effector_order == 'same2' & a$subject_id=='2'] = '3'
a$accuracy_gabor[a$accuracy_gabor == '1'& a$effector_order == 'same2'& a$subject_id=='2'] = '4'
a$accuracy_gabor[a$accuracy_gabor == '3'& a$effector_order == 'same2'& a$subject_id=='2'] = '1'
a$accuracy_gabor[a$accuracy_gabor == '4'& a$effector_order == 'same2'& a$subject_id=='2'] = '0'

a$SDT[a$SDT == 'hit' & a$effector_order == 'same2'& a$subject_id=='2'] = '6'
a$SDT[a$SDT == 'miss' & a$effector_order == 'same2'& a$subject_id=='2'] = '7'
a$SDT[a$SDT == 'FA' & a$effector_order == 'same2'& a$subject_id=='2'] = '8'
a$SDT[a$SDT == 'CR' & a$effector_order == 'same2'& a$subject_id=='2'] = '9'

a$SDT[a$SDT == '6' & a$effector_order == 'same2'& a$subject_id=='2'] = 'FA'
a$SDT[a$SDT == '7' & a$effector_order == 'same2'& a$subject_id=='2'] = 'CR'
a$SDT[a$SDT == '8' & a$effector_order == 'same2'& a$subject_id=='2'] = 'hit'
a$SDT[a$SDT == '9' & a$effector_order == 'same2'& a$subject_id=='2'] = 'miss'

data <- a

save(data, file = 'C:/Users/remil/PhD/R/GitHub/data/AG2_data/data_final.rda')

