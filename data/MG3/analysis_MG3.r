rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/motor_gabor/data/MG3')

## * preparation

load('data_final_MG3.rda')
data <- data %>%
    filter(conf>0) %>%
    mutate(congruency = ifelse(laterality == "ipsilateral","congruent","incongruent")) %>%
    select(subject_id, block, accuracy_gabor, rt_gabor, conf, laterality, congruency)
   
## define variables and  contrasts
data$conf_ord = as.factor(data$conf)
data$rt_gabor_centered <- data$rt_gabor - mean(data$rt_gabor, na.rm = TRUE)

data$acc_num <- data$accuracy_gabor
data$accuracy_gabor <- as.factor(data$accuracy_gabor)
contrasts(data$accuracy_gabor) <- - contr.sum(2) ## erreur: -1; correct: 1

data$congruency <- as.factor(data$congruency)
contrasts(data$congruency) <-  contr.sum(2) ## incongruent: -1; congruent: 1

## * RT

## ** frequentist

l.rt <- lmer_alt(rt_gabor*1000 ~  accuracy_gabor * congruency +
                     (1 + accuracy_gabor + congruency ||subject_id),
                   family=inverse.gaussian(link="identity"),
                   data = data)
summary(l.rt)

save(l.rt, file = 'fit_rt_MG3.rdata')
tab_model(l.rt, file = "rt_MG3.html")

## plot interactions
## effects are averaged over the levels of factors
load('fit_rt_MG3.rdata')
summary(l.rt)
## accuracy:congruency
predict <-  ggemmeans(l.rt, c('accuracy_gabor','congruency'))
plot <- plot(predict) + 
  labs(x = "Accuracy", 
       y = "Response Time (ms)", 
       title = "Accuracy x Congruency interaction on RT") +
    scale_x_continuous(breaks = c(0, 1), labels=c("error", "correct")) +
    #scale_colour_discrete(labels=c('feet first', 'hand first'), name = "Block order") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave('rt_acc_x_congruency_MG3.jpeg', plot)

## ** bayesian
fit.rt <- brm(rt_gabor*1000 ~  accuracy_gabor * congruency  +
                     (1 + accuracy_gabor + congruency ||subject_id) ,
           data = data,
           family=exgaussian(link="identity"),
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 2000, seed = 123,
           save_model = 'rt.stan',
           save_pars = save_pars(all = TRUE)
           )

save(fit.rt, file = 'rt_bayes_MG3.rdata')
tab_model(fit.rt, file = "rt_bayes_MG3.html")


## RT evolution accross blocks
d <- data %>%
    group_by(subject_id, block) %>%
    summarise(acc = mean(acc_gabor_num), rt = mean(rt_gabor))

l <- lmer_alt(rt ~ block + acc + (1 + block + acc | subject_id) , data = d)
summary(l)


## * Accuracy

## Planned model
l.acc <- lmer_alt(acc_num ~  congruency  * rt_gabor_centered +
                      (1 + congruency +  rt_gabor_centered ||subject_id),
		  family = binomial(link = "logit"),
		  data = data)
summary(l.acc)
summary(rePCA(l.acc))


## remove: congruency

l.acc <- lmer_alt(acc_num ~  congruency  *  rt_gabor_centered +
		    (1 +  rt_gabor_centered ||subject_id),
		  family = binomial(link = "logit"),
		  data = data)

summary(l.acc)
save(l.acc, file = 'fit_acc_MG3.rdata')
tab_model(l.acc, file = "acc_MG3.html")

## plot interactions
## effects are averaged over the levels of factors

## rt:congruency interaction 
predict <- ggemmeans(l.acc, c('rt_gabor_centered','congruency'))
plot <- plot(predict) + 
  labs(x = "Centered Response Time (ms)", 
       y = "Accuracy", 
       title = "Response Time x Congruency interaction on Accuracy") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave('congruency_rt_x_order_MG3.jpeg', plot)

## ** Bayesian lmer

fit_acc <- brm(acc_num ~  congruency * rt_gabor_centered +
               (1 + congruency   +  rt_gabor_centered ||subject_id),
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
save(fit_acc, file ='fit_acc_bayes_MG3.rdata')
tab_model(fit_acc, file = "acc_bayes_MG3.html")

## * Confidence

## ** frequentist

## planned model

l.conf <- clmm(conf_ord ~ accuracy_gabor  * congruency  +  rt_gabor_centered +
		     (1 +  accuracy_gabor  + congruency  +
		      rt_gabor_centered|subject_id),
                  data = data %>% filter(is.na(accuracy_gabor) == FALSE),
                  link = c("probit"))
summary(l.conf)

save(l.conf, file = "fit_conf_MG3.rdata")
tab_model(l.conf, file = "conf_MG3.html")

## plot
predict <- ggemmeans(l.conf, c('congruency'))
predict <- as.data.frame(predict) %>%
    rename( confidence = response.level, congruency = x)


plot <- ggplot(data = predict, aes(x = confidence, y = predicted, colour = congruency)) +
    geom_point(position = position_dodge(width = .5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = .5, position = "dodge") + 
    labs(y = "Probabilty", 
        title = "Effect of Congruency on Confidence") +
    theme(plot.title = element_text(hjust = 0.5))
print(plot)
ggsave('conf_MG3.jpeg', plot)


## ** bayesian
fit_conf <- brm(conf ~ accuracy_gabor  * congruency +  rt_gabor_centered +
                    (1  +  accuracy_gabor  + congruency  + rt_gabor_centered ||subject_id),
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
tab_model(fit_conf, file = "conf_bayes_MG3.html")



## * Plot revision
## CONFIDENCE
d <- data %>%
    group_by(subject_id, congruency) %>%
    summarise(confidence = mean(conf)) %>%
    ungroup()

plot_1 <- ggplot(data = d, aes(x = congruency, y = confidence)) +
    coord_cartesian(ylim = c(1, 4)) +
    geom_bar(aes(congruency, confidence, fill = congruency), position='dodge', stat='summary', fun='mean')+
    scale_y_continuous(expand = c(0, 0))  +
    geom_line(aes(group=subject_id)) +
    geom_point() +
    labs(fill = "Congruency") +
    xlab('Congruency')
plot_1
ggsave('conf_congruency_exp3.svg', plot_1)

## CONFIDENCE VS ACCURACY
d <- data %>%
    group_by(subject_id, accuracy_gabor) %>%
    summarise(confidence = mean(conf)) %>%
    ungroup()
plot_2 <- ggplot(data = d, aes(x = accuracy_gabor, y = confidence)) +
    coord_cartesian(ylim = c(1, 4)) +
    geom_bar(aes(accuracy_gabor, confidence, fill = accuracy_gabor), position='dodge', stat='summary', fun='mean')+
    scale_y_continuous(expand = c(0, 0))  +
    geom_line(aes(group=subject_id)) +
    geom_point() +
    scale_x_discrete(labels = c("0" = "Error", "1" = "Correct")) +
    labs(fill = "Accuracy") +
    scale_fill_discrete(name = "Accuracy", labels = c("Error", "Correct"))
plot_2
ggsave('conf_accuracy_exp3.svg', plot_2)

## EFFECTOR 
d <- data %>%
    group_by(subject_id, effector) %>%
    mutate(effector = fct_rev(effector)) %>%
    summarise(confidence = mean(conf)) %>%
    ungroup()
plot_3 <- ggplot(data = d, aes(x = effector, y = confidence)) +
    coord_cartesian(ylim = c(1, 4)) +
    geom_bar(aes(effector, confidence, fill = effector), position='dodge', stat='summary', fun='mean')+
    scale_y_continuous(expand = c(0, 0))  +
    geom_line(aes(group=subject_id)) +
    geom_point() +
    labs(fill = "Effector") +
    xlab('Effector')
plot_3
ggsave('conf_effector_exp1.svg', plot_3)

