rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/motor_gabor/data/MG5')

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


## * preliminary analysis
## rt, accuracy, conf mean by subject
d <- data  %>%
  group_by(subject_id) %>%
  summarise(rt = mean(rt_gabor), accuracy = mean(acc_num), conf = mean(conf))
kable(d)

## rt, accuracy, conf mean by condition
d <- data  %>%
  group_by(size) %>%
  summarise(rt = mean(rt_gabor), accuracy = mean(acc_num), conf = mean(conf))

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
l.rt <- lmer_alt(rt_gabor ~ accuracy_gabor *size + (1+accuracy_gabor*size| subject_id), data = data)
summary(l.rt)
tab_model(l.rt, file = "rt.html")

l.dt <- lmer_alt(OOZ_time ~ accuracy_gabor *size + (1+accuracy_gabor*size| subject_id), data = data)
## singular
l.dt <- lmer_alt(OOZ_time ~ accuracy_gabor *size + (1+accuracy_gabor+size| subject_id), data = data)
## singular
l.dt <- lmer_alt(OOZ_time ~ accuracy_gabor *size + (1+accuracy_gabor| subject_id), data = data)
tab_model(l.dt, file = "dt.html")

## ** Bayesian
fit.rt <- brm(rt_gabor ~ accuracy_gabor *size + (1+accuracy_gabor*size| subject_id),
           data = data,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 2000, seed = 123)
save(fit.rt, file = "bayes_rt.rdata")
tab_model(fit.rt, file = "bayes_rt.html")

fit.dt <- brm(OOZ_time ~ accuracy_gabor *size + (1+accuracy_gabor*size|| subject_id),
           data = data,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 2000, seed = 123)
save(fit.dt, file = "bayes_dt.rdata")
tab_model(fit.dt, file = "bayes_dt.html")


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
       title = "Decision Time x Size x Position interaction on Accuracy") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave('acc_pred.jpeg', plot)

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

l.conf <- clmm(conf_ord ~ accuracy_gabor * size * position * rt_gabor_centered + (1 * size * position * rt_gabor_centered | subject_id),
                  data = data,
                  link = c("probit"))
tab_model(l.conf, file = "conf.html")

## plot
## size:accuracy interaction
predict <- ggemmeans(l.conf, c('size','accuracy_gabor'))
predict <- as.data.frame(predict) %>%
    rename(confidence = response.level, size = x, accuracy = group)

plot <- ggplot(data = predict, aes(x = confidence, y = predicted, colour = size)) +
    geom_point(position = position_dodge(width = .5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .5, position = "dodge") +
    facet_wrap(~ accuracy) +
    labs(x = "Confidence", y = "Probability", title = "Size x Accuracy interaction on Confidence") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave('conf_pred_1.jpeg', plot)

## size:rt interaction
predict <- ggemmeans(l.conf, c('rt_gabor_centered','size'))
plot <- plot(predict) + 
  labs(x = "Centered response Time (ms)", 
       y = "probability", 
       title = "Response Time x Size interaction on Confidence") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave('conf_pred_2.jpeg', plot)

## size main effect
predict <- ggemmeans(l.conf, c('size'))
predict <- as.data.frame(predict) %>%
    rename(confidence = response.level, size = x)

plot <- ggplot(data = predict, aes(x = confidence, y = predicted, colour = size)) +
    geom_point(position = position_dodge(width = .5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .5, position = "dodge") +
    labs(x = "Confidence", y = "Probability", title = "Size main effect on Confidence") +
    theme(plot.title = element_text(hjust = 0.5))

ggsave('conf_pred_3.jpeg', plot)

## ** bayesian
fit_conf <- brm(conf ~ accuracy_gabor * size * position * rt_gabor_centered + (1 * size * position * rt_gabor_centered | subject_id),
                init_r = 0.05,
        data = data,
    family=cumulative("probit"),
    prior = c(set_prior("normal(0,1)", class = "b")),
    cores = 4, chains = 4,
    control = list(adapt_delta = .95,  max_treedepth = 12),
    iter = 6000,  warmup = 4000, seed = 123,
    save_model = 'conf.stan',
    save_pars = save_pars(all = TRUE)
)
save(fit_conf, file = 'conf.rdata')
tab_model(fit_conf, file = "conf_bayes_MG5.html")

