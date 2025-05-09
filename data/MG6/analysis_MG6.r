rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/motor_gabor/data/MG6')
library('svglite')        

## * preparation

data <- read.csv('mg6_all.csv', sep = ';')

## identify outliers (accuracy < .55)
d <- data %>% 
    group_by(subject_id) %>%
    summarise(acc = mean(accuracy_gabor)) %>%
    filter(acc <.55)
outlier <- unique(d$subject_id)

## remove outliers and E trials
## accuracy2: whereas 2nd accuracy judg. is correct
## meta_accuracy: whereas it was correct to ask a 2nd stim
data <- data %>% 
    filter(!(subject_id %in% outlier), conf %in% c('O','N')) %>%
    mutate(accuracy2 = ifelse((accuracy_gabor == 1 &  meta_evaluation == "C") | (accuracy_gabor == 0 & meta_evaluation == "I"), 1, 0)) %>%
    mutate(accuracy2 = ifelse(view_again == 1, accuracy2, NA)) %>%
    mutate(meta_accuracy = ifelse((view_again == 0 & accuracy_gabor == 1)|(view_again == 1 & accuracy_gabor == 0), 1, 0))

    
 
## define variables and  contrasts
data$cond_order = as.factor(data$cond_order)

data$rt_gabor_centered <- data$rt_gabor - mean(data$rt_gabor, na.rm = TRUE)
data$dt <-  data$OOZ_time
data$dt_centered <- data$OOZ_time - mean(data$OOZ_time, na.rm = TRUE)

data$acc_num <- data$accuracy_gabor
data$accuracy_gabor <- as.factor(data$accuracy_gabor)
contrasts(data$accuracy_gabor) <- - contr.sum(2) ## erreur: -1; correct: 1

data$size <- as.factor(data$size)
contrasts(data$size) <-  -contr.sum(2) ## big: -1; small: 1

data$position <- as.factor(data$circle_pos)
contrasts(data$position) <-  contr.sum(3)

data <- data %>%  mutate(view_again_num  =  ifelse(conf == 'O', 1, 0))
data$view_again <- as.factor(data$view_again)
contrasts(data$view_again) <-  -contr.sum(2) ## O=1, N=-1

data$acc2_num <- data$accuracy2
data$accuracy2 <- as.factor(data$accuracy2)
contrasts(data$accuracy2) <- -contr.sum(2) ## C=1, I=-1


data$meta_acc_num <- data$meta_accuracy
data$meta_accuracy <- as.factor(data$meta_accuracy)
contrasts(data$meta_accuracy) <- -contr.sum(2) ## C=1, I=-1


## * DESCRIPTIVE DATA
## rt, accuracy, conf mean by subject

d <- data  %>%
    group_by(size) %>%
    summarise(
        mean_rt = mean(rt_gabor),
        sd_rt = sd(rt_gabor),
        mean_dt = mean(OOZ_time),
        sd_dt = sd(OOZ_time),
        mean_accuracy = mean(acc_num), 
        sd_accuracy = sd(acc_num), 
        mean_view_again = mean(view_again_num),
        sd_view_again = sd(view_again_num),
        mean_meta_accuracy = mean(meta_acc_num),
        sd_meta_accuracy = sd(meta_acc_num))    
tab_df(d, file = 'descriptive.html')


## plot.rt  <- ggplot(data = data, aes(rt_gabor)) +
##   geom_histogram()  +
##   facet_wrap( ~ subject_id) +
##   ggtitle('rt gabor')
## ggsave(plot.rt, file  = "rt_des.jpeg")

## plot.conf  <- ggplot(data = data, aes(conf)) +
##   geom_histogram()  +
##   facet_wrap( ~ subject_id) +
##   ggtitle('confidence')
## ggsave(plot.conf, file = "conf_des.jpeg")


## plot <- ggplot(d_indiv, aes(x = subject_id, y = accuracy, fill = size)) +
##      geom_bar(position=position_dodge(width = 0.8), stat="identity", width = 0.8) +
##     coord_cartesian(ylim=c(.5,1)) +
##     labs(title = "Accuracy by subject and condition", x = "Subject", y = "Accuracy")+
##     theme_minimal() +
##     scale_fill_brewer(palette = "Set1")
## plot


## * RESPONSE TIME AND DECISION TIME
## response time: time from onset to decision (rt_gabor)
## decision time: time from onset to time out of decision zone (OOZ_time)

## ** Descriptive
plot.rt <- ggplot(data, aes(x=size, y=rt_gabor)) + 
    geom_violin() +
    stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1) , geom="pointrange", color="blue")
ggsave(plot.rt, file = "rt_violin.jpeg")

plot.dt <- ggplot(data, aes(x=size, y=OOZ_time)) + 
    geom_violin() +
    stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1) , geom="pointrange", color="blue")
ggsave(plot.dt, file = "dt_violin.jpeg")


## ** fit RT
fit.rt <- brm(rt_gabor*1000 ~  accuracy_gabor * size * position  +
                     (1 + accuracy_gabor * size * position |subject_id) ,
           family=exgaussian(link="identity"),
           data = data,
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 1000, seed = 123,
           save_pars = save_pars(all = TRUE)
           )
saveRDS(fit.rt, "fit_rt.rds")
fit.rt <- readRDS("fit_rt.rds")
fixef(fit.rt)


## ** fit DT
fit.dt <- brm(OOZ_time*1000 ~  accuracy_gabor * size * position  +
                     (1 + accuracy_gabor * size * position |subject_id) ,
           family=exgaussian(link="identity"),
           data = data,
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 1000, seed = 123,
           save_model = 'dt.stan',
           save_pars = save_pars(all = TRUE)
           )
saveRDS(fit.dt, "fit_dt.rds")
fit.dt <- readRDS("fit_dt.rds")
fixef(fit.dt)

tab_model(fit.dt, fit.rt, file = "bayes_dtrt.html")


## * ACCURACY
## ** descriptive

## Descriptive plot
d <- data %>%
    group_by(subject_id, size) %>%
    summarise(accuracy = mean(acc_num)) %>%
    ungroup()
plot <- ggplot(data = d, aes(x = size, y = accuracy)) +
    geom_bar(aes(size, accuracy, fill = size), position='dodge', stat='summary', fun='mean')+
    geom_line(aes(group=subject_id)) +
    geom_point() +
    labs(fill = "Size") +
    xlab('Size')
plot
ggsave('acc_size.svg', plot)

## ** Bayesian fit

fit_acc <- brm(accuracy_gabor  ~ size * position * dt_centered + (1 + size * position * dt_centered | subject_id),
               family = bernoulli(link = "logit"),
           data = data,
           cores = 4, chains = 4,
           control = list(adapt_delta = .9,  max_treedepth = 12),
           iter = 3000,  warmup = 1000, seed = 123,
           )
saveRDS(fit_acc, "fit_acc.rds")
tab_model(fit_acc, file = "acc_bayes.html")
fit.acc <- readRDS("fit_acc.rds")
fixef(fit.acc)
## Signif effects
##                               Estimate  Est.Error        Q2.5        Q97.5
## dt_centered                 -1.26140008 0.17910723 -1.62207505 -0.922090278
## size1:position2             -0.11699656 0.05544853 -0.22540972 -0.008194695


dt <-  conditional_effects(fit.acc, "dt_centered")
plot <- plot(dt, plot = FALSE)[[1]]
ggsave('pred_acc_dt.svg', plot)

size_position <-  conditional_effects(fit.acc, "size:position")
plot <- plot(size_position, plot = FALSE)[[1]]
ggsave('pred_acc_size_position.svg', plot)

## * METACOG
## ** Desscriptive

## Descriptive Plot 

d <- data %>%
    group_by(subject_id, size) %>%
    summarise(conf = mean(view_again_num)) %>%
    ungroup()
plot <- ggplot(data = d, aes(x = size, y = conf)) +
    geom_bar(aes(size, conf, fill = size), position='dodge', stat='summary', fun='mean')+
    geom_line(aes(group=subject_id)) +
    geom_point() +
    labs(fill = "Size") +
    xlab('Size') + 
    ylab('View again')
plot
ggsave('acc_size.svg', plot)

## ** fit

fit.conf <- brm(view_again ~ accuracy_gabor * size * position * rt_gabor_centered + (1 + accuracy_gabor * size * position * rt_gabor_centered | subject_id),
                family = bernoulli(link = "logit"),
           data = data,
           init = 0,
           cores = 4, chains = 4,
           control = list(adapt_delta = .9,  max_treedepth = 12),
           iter = 3000,  warmup = 1000, seed = 123,
           )
saveRDS(fit.conf, "fit_conf.rds")
tab_model(fit.conf, file = "conf_bayes.html")

fit.conf <- readRDS("fit_conf.rds")
fixef(fit.conf)
## Signif effects
##                                                     Estimate  Est.Error        Q2.5       Q97.5
## accuracy_gabor1                                   -0.433879171 0.05838317 -0.54822002 -0.31932967
## size1                                             -0.363117918 0.06533389 -0.49810714 -0.23932145
## rt_gabor_centered                                  2.501698520 0.30877300  1.90177929  3.12919922
## accuracy_gabor1:size1                             -0.181411911 0.05020978 -0.28085189 -0.08398869
## accuracy_gabor1:rt_gabor_centered                  0.570973108 0.13733714  0.29861875  0.83295953
## size1:rt_gabor_centered                           -0.494418410 0.18169480 -0.86900164 -0.15539198

size_rt <-  conditional_effects(fit.conf, "rt_gabor_centered:size")
plot <- plot(size_rt, plot = FALSE)[[1]]
ggsave('pred_conf_size_rt.svg', plot)

acc_rt <-  conditional_effects(fit.conf, "rt_gabor_centered:accuracy_gabor")
plot <- plot(acc_rt, plot = FALSE)[[1]]
ggsave('pred_conf_acc_rt.svg', plot)

acc_size <-  conditional_effects(fit.conf, "accuracy_gabor:size")
plot <- plot(acc_size, plot = FALSE)[[1]]
ggsave('pred_cond_acc_size.svg', plot)

