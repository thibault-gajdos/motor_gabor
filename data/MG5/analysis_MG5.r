rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/motor_gabor/data/MG5')
library(gridExtra)
library(grid)
## * preparation
## load data
data <- read_csv('data_final_MG5.csv') %>%
    filter(conf > 0) %>%
    mutate(position = ifelse(position == "mid", "middle", position))

## define variables and  contrasts
data$conf_ord = as.factor(data$conf)
data$rt_gabor_centered <- data$rt_gabor - mean(data$rt_gabor, na.rm = TRUE)
data$acc_num <- data$accuracy_gabor
data$dt_centered <- data$OOZ_time - mean(data$OOZ_time, na.rm = TRUE)
data$accuracy_gabor <- as.factor(data$accuracy_gabor)
contrasts(data$accuracy_gabor) <- - contr.sum(2) ## erreur: -1; correct: 1
data$size <- as.factor(data$size)
contrasts(data$size) <-  -contr.sum(2) ## big: -1; small: 1
data$position <- as.factor(data$position)
contrasts(data$position) <-  contr.sum(3)
data$dt <-  data$OOZ_time

## * preliminary analysis
## rt, accuracy, conf mean by subject

nrow(data)
d <- data  %>%
  group_by(subject_id) %>%
  summarise(rt = mean(rt_gabor), accuracy = mean(acc_num), conf = mean(conf))
kable(d)

mean(d$accuracy)
sd(d$accuracy)

mean(d$conf)
sd(d$conf)

## rt, accuracy, conf mean by condition

d <- data  %>%
    group_by(size, subject_id) %>%
    summarise(rt = mean(rt_gabor), dt = mean(dt), accuracy = mean(acc_num), conf = mean(conf)) %>%
    ungroup() %>%
    group_by(size) %>%
    summarise(mean_rt = mean(rt), mean_dt = mean(dt), mean_accuracy = mean(accuracy), mean_conf = mean(conf),
              sd_rt = sd(rt),  sd_dt = sd(dt), sd_accuracy = sd(accuracy), sd_conf = sd(conf))

d_indiv <- data  %>%
    group_by(size, subject_id) %>%
    summarise(rt = mean(rt_gabor), dt = mean(dt), accuracy = mean(acc_num), conf = mean(conf)) %>%
    ungroup()
    

plot.rt  <- ggplot(data = data, aes(rt_gabor)) +
  geom_histogram()  +
  facet_wrap( ~ subject_id) +
  ggtitle('rt gabor')
ggsave(plot.rt, file  = "rt_des.jpeg")

plot.conf  <- ggplot(data = data, aes(conf)) +
  geom_histogram()  +
  facet_wrap( ~ subject_id) +
  ggtitle('confidence')
ggsave(plot.conf, file = "conf_des.jpeg")


plot <- ggplot(d_indiv, aes(x = subject_id, y = accuracy, fill = size)) +
     geom_bar(position=position_dodge(width = 0.8), stat="identity", width = 0.8) +
    coord_cartesian(ylim=c(.5,1)) +
    labs(title = "Accuracy by subject and condition", x = "Subject", y = "Accuracy")+
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
plot

## * Response Time and decision time
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
ggsave(plot.rt, file = "dt_violin.jpeg")

## ** frequentist
l.rt <- lmer_alt(rt_gabor ~ accuracy_gabor + size +  position+ (1+accuracy_gabor+size+position| subject_id), data = data)
summary(l.rt)


l.dt <- lmer_alt(OOZ_time ~ accuracy_gabor + size + position + (1+accuracy_gabor*size+position | subject_id), data = data)
## singular

l.dt <- lmer_alt(OOZ_time ~ accuracy_gabor +size  + position + (1+accuracy_gabor+size+position || subject_id), data = data)
summary(l.dt)
## singumar

l.dt <- lmer_alt(OOZ_time ~ accuracy_gabor +size  + position + (1+accuracy_gabor+position | subject_id), data = data)
summary(l.dt)

tab_model(l.rt, l.dt, show.ci = FALSE, file = "rtdt.html", digits = 3)

## ** Bayesian
fit.rt <- brm(rt_gabor ~ accuracy_gabor +size + position + (1+accuracy_gabor+size + position| subject_id),
           data = data,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 2000, seed = 123)
save(fit.rt, file = "bayes_rt.rdata")
tab_model(fit.rt, file = "bayes_rt.html")

fit.dt <- brm(OOZ_time ~ accuracy_gabor +size + position  + (1+accuracy_gabor+size+ position| subject_id),
           data = data,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 2000, seed = 123)
save(fit.dt, file = "bayes_dt.rdata")
tab_model(fit.dt, file = "bayes_dt.html")

tab_model(fit.dt, fit.rt, file = "bayes_dtrt.html")


## * Accuracy
## ** descriptive

## ** frequentist
## Planned model
l.acc <- lmer_alt(accuracy_gabor  ~ size * position * dt_centered + (1 + size * position * dt_centered | subject_id), family = binomial(link = "logit"), data = data)

## doesn't converge: simplify
l.acc <- lmer_alt(accuracy_gabor  ~ size * position * dt_centered + (1 + size * position * dt_centered || subject_id), family = binomial(link = "logit"), data = data)

## singular
summary(l.acc)
summary(rePCA(l.acc))

## remove interactions
l.acc <- lmer_alt(accuracy_gabor  ~ size * position * dt_centered + (1 + size + position + dt_centered || subject_id), family = binomial(link = "logit"), data = data)
summary(l.acc)

## remove: position
l.acc <- lmer_alt(accuracy_gabor  ~ size * position * dt_centered  + (1 + size  + dt_centered || subject_id),
                 family = binomial(link = "logit"),
                 data = data)
summary(l.acc)

## remove: size
l.acc <- lmer_alt(accuracy_gabor  ~ size * position * dt_centered  + (1 + dt_centered || subject_id),
                 family = binomial(link = "logit"),
                 data = data)

summary(l.acc)
tab_model(l.acc, file = "fit_acc.html")

## interaction: size*position*dt
predict <- ggemmeans(l.acc, c('dt_centered','size','position'))
plot <- plot(predict) + 
  labs(x = "Centered Decision Time (ms)", 
       y = "Accuracy", 
       title = "Accuracy as a function of \n Decision Time,  Size and  Position") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave('acc_pred.jpeg', plot)
ggsave('acc_pred.svg', plot)

## ** Bayesian 

fit_acc <- brm(accuracy_gabor  ~ size * position * dt_centered + (1 + size * position * dt_centered | subject_id),
               family = bernoulli(link = "logit"),
           data = data,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .98,  max_treedepth = 12),
           iter = 6000,  warmup = 4000, seed = 123,
           )
summary(fit_acc)
save(fit_acc, file ='bayes_acc.rdata')
tab_model(fit_acc, file = "acc_bayes.html")

## * Confidence

## ** frequentist

## planned model

l.conf <- clmm(conf_ord ~ accuracy_gabor * size * position * rt_gabor_centered + (1 * accuracy_gabor * size * position * rt_gabor_centered | subject_id),
                  data = data,
                  link = c("probit"))

tab_model(l.conf, file = "conf.html")
tab_model(l.acc, l.conf, file = "acc_conf.html",  show.ci = FALSE, digits.p = 3)
summary(l.conf)

################################################
## extra
data$mt <- data$rt_gabor - data$OOZ_time
data$mt_centered <- data$mt - mean(data$mt)
l.conf2 <- clmm(conf_ord ~ accuracy_gabor * size * position * (dt_centered + mt_centered) + (1 * accuracy_gabor * size * position * (dt_centered + mt_centered) | subject_id),
                  data = data,
                  link = c("probit"))
summary(l.conf2)

predict <- ggemmeans(l.conf2, c('mt_centered','size'))
plot2 <- plot(predict) + 
  labs(x = "Centered response Time (ms)", 
       y = "probability", 
       title = "Confidence as a function of \n Response Time and Size") +
    theme(plot.title = element_text(hjust = 0.5))
plot2

predict <- ggemmeans(l.conf2, c('dt_centered','size'))
plot3 <- plot(predict) + 
  labs(x = "Centered decision Time (ms)", 
       y = "probability", 
       title = "Confidence as a function of \n Decision Time and Size") +
    theme(plot.title = element_text(hjust = 0.5))
plot3

#################################################

## plot
## size:accuracy interaction
predict <- ggemmeans(l.conf, c('size','accuracy_gabor'))
predict <- as.data.frame(predict) %>%
    rename(confidence = response.level, size = x, accuracy = group)

plot1 <- ggplot(data = predict, aes(x = accuracy, y = predicted, colour = size)) +
    geom_point(position = position_dodge(width = .5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .5, position = "dodge") +
    facet_wrap(~ confidence) +
    labs(x = "Accuracy", y = "Probability", title = "Confidence as a function of \n Size and Accuracy") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave('conf_pred_1.jpeg', plot1)
ggsave('conf_pred_1.svg', plot1)

## size:rt interaction
predict <- ggemmeans(l.conf, c('rt_gabor_centered','size'))
plot2 <- plot(predict) + 
  labs(x = "Centered response Time (ms)", 
       y = "probability", 
       title = "Confidence as a function of \n Response Time and Size") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave('conf_pred_2.jpeg', plot2)
ggsave('conf_pred_2.svg', plot2)

## size main effect
predict <- ggemmeans(l.conf, c('size'))
predict <- as.data.frame(predict) %>%
    rename(confidence = response.level, size = x)

plot3 <- ggplot(data = predict, aes(x = confidence, y = predicted, colour = size)) +
    geom_point(position = position_dodge(width = .5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .5, position = "dodge") +
    labs(x = "Confidence", y = "Probability", title = "Confidence as a function \n of size") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) ## +
    ##scale_colour_discrete(name="Circle size")
ggsave('conf_pred_3.jpeg', plot3)
ggsave('conf_pred_3.svg', plot3)

## acc:rt interaction
predict <- ggemmeans(l.conf, c('rt_gabor_centered','accuracy_gabor'))
plot4 <- plot(predict) + 
  labs(x = "Centered Response Time (ms)", 
       y = "probability", 
       title = "Confidence as a function of \n Response Time and Accuracy interaction") +
    theme(plot.title = element_text(hjust = 0.5))     
ggsave('conf_pred_4.jpeg', plot4)
ggsave('conf_pred_4.svg', plot4)



plot_conf <- ggarrange(ggarrange(plot3, plot1+theme(legend.position = "none"), ncol = 2,  common.legend = FALSE, labels = "AUTO"),
                       plot2,
                       nrow = 2,
                       common.legend = FALSE,
                       labels = c("", "C"))
                       
plot_conf
ggsave(plot_conf, file = "plot_conf.svg")

## ** bayesian
fit_conf <- brm(conf ~ accuracy_gabor * size * position * rt_gabor_centered + (1 * accuracy_gabor * size * position * rt_gabor_centered | subject_id),
                init_r = 0.05,
        data = data,
    family=cumulative("probit"),
    prior = c(set_prior("normal(0,1)", class = "b")),
    cores = 4, chains = 4,
    control = list(adapt_delta = .95,  max_treedepth = 12),
    iter = 4000,  warmup = 2000, seed = 123,
    save_model = 'conf.stan',
    save_pars = save_pars(all = TRUE)
)
save(fit_conf, file = 'conf.rdata')
tab_model(fit_conf, file = "conf_bayes_MG5.html")


