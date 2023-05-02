rm(list=ls(all=TRUE))  ## efface les données

source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/motor_gabor/data/MG5')

## * preparation
## load data
data <- read_csv('data_final_MG5.csv') %>%
    filter(conf > 0)

## define variables and  contrasts
data$conf_ord = as.factor(data$conf)
data$rt_gabor_centered <- data$rt_gabor - mean(data$rt_gabor, na.rm = TRUE)
data$acc_num <- data$accuracy_gabor
data$accuracy_gabor <- as.factor(data$accuracy_gabor)
contrasts(data$accuracy_gabor) <- - contr.sum(2) ## erreur: -1; correct: 1
data$size <- as.factor(data$size)
contrasts(data$size) <-  -contr.sum(2) ## big: -1; small: 1
data$block <- as.factor(data$block)
contrasts(data$block) <-  contr.sum(8)
data$condition <- as.factor(data$condition)
contrasts(data$condition) <-  contr.sum(2)
data$position <- as.factor(data$position)
contrasts(data$position) <-  contr.sum(3)

## * preliminary analysis
d <- data  %>%
  group_by(subject_id) %>%
  summarise(rt = mean(rt_gabor), accuracy = mean(acc_num), conf = mean(conf))
kable(d)

p  <- ggplot(data = data, aes(rt_gabor)) +
  geom_histogram()  +
  facet_wrap( ~ subject_id) +
  ggtitle('rt gabor')
print(p)

p  <- ggplot(data = data, aes(conf)) +
  geom_histogram()  +
  facet_wrap( ~ subject_id) +
  ggtitle('conf')
print(p)

d <- data  %>%
  group_by(size) %>%
  summarise(rt = mean(rt_gabor), accuracy = mean(acc_num), conf = mean(conf))
kable(d)

d <- data  %>%
  group_by(size, accuracy_gabor) %>%
  summarise(rt = mean(rt_gabor), conf = mean(conf))
kable(d)


## * Accuracy

## ** frequentist
## Planned model
l.acc <- lmer_alt(accuracy_gabor  ~ size * position * rt_gabor_centered + (1 + size * position * rt_gabor_centered || subject_id),
                 family = binomial(link = "logit"),
                 data = data)
summary(l.acc)
summary(rePCA(l.acc))

## remove interactions
l.acc <- lmer_alt(accuracy_gabor  ~ size * position * rt_gabor_centered + (1 + size + position + rt_gabor_centered || subject_id),
                 family = binomial(link = "logit"),
                 data = data)
summary(l.acc)

## remove: position
l.acc <- lmer_alt(accuracy_gabor  ~ size * position * rt_gabor_centered  + (1 + size  + rt_gabor_centered || subject_id),
                 family = binomial(link = "logit"),
                 data = data)
summary(l.acc)

save(l.acc, file = 'fit_acc_MG5.rdata')
tab_model(l.acc, file = "acc_MG5.html")

## interaction: size*position*rt
predict <- ggemmeans(l.acc, c('rt_gabor_centered','size','position'))
plot(predict)
plot <- plot(predict) + 
  labs(x = "Centered Response Time (ms)", 
       y = "Accuracy", 
       title = "Response Time x Size x Position interaction on Accuracy") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave('acc_MG5.jpeg', plot)

## ** Bayesian lmer

fit_acc <- brm(accuracy_gabor  ~ size * position * rt_gabor_centered + (1 + size * position * rt_gabor_centered || subject_id),
               family = bernoulli(link = "logit"),
           data = data,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .98,  max_treedepth = 12),
           iter = 6000,  warmup = 4000, seed = 123,
           save_model = 'acc.stan',
           save_pars = save_pars(all = TRUE)
           )
summary(fit_acc)
save(fit_acc, file ='fit_acc_bayes_MG5.rdata')
tab_model(fit_acc, file = "acc_bayes_MG5.html")

## * Confidence

## ** frequentist

## planned model

l.conf <- clmm(conf_ord ~ accuracy_gabor * size * position * rt_gabor_centered + (1 * size * position * rt_gabor_centered | subject_id),
                  data = data,
                  link = c("probit"))
summary(l.conf)

save(l.conf, file = "fit_conf_MG5.rdata")
tab_model(l.conf, file = "conf_MG5.html")

## plot
predict <- ggemmeans(l.conf, c('size','accuracy_gabor'))
plot(predict)
ggsave('conf_MG5_1.jpeg', plot)

predict <- ggemmeans(l.conf, c('size'))
predict <- as.data.frame(predict) %>%
    rename( confidence = response.level, size = x)

plot <- ggplot(data = predict, aes(x = confidence, y = predicted, colour = size)) +
    geom_point(position = position_dodge(width = .5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = .5, position = "dodge") + 
    labs(y = "Probabilty", 
        title = "Effect of Size on Confidence") +
    theme(plot.title = element_text(hjust = 0.5))
print(plot)
ggsave('conf_MG5_2.jpeg', plot)


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

