rm(list=ls(all=TRUE))  ## efface les données
source('C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/MG2_data/R_lib.r')
setwd('C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/AG_data/final_csv')

## * Prepare
# loop over files and append
files = list.files(path="C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/AG_data/final_csv", pattern = 'csv', recursive = F)
a = list()
for (f in 1:length(files)) {
  df <- read.csv(file = files[f],header = FALSE,stringsAsFactors = F)
  df[,2] = c(1:792)
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

data <- a

save(data, file = 'C:/Users/remil/OneDrive/Documents/GitHub/repo_motor_gabor/motor_gabor/data/AG_data/data_final_MG1.rda')

