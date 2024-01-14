rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/motor_gabor/data/MG2')
library(svglite)

## * preparation

load('big_data_final.rda')
data <- big_data %>%
    filter(participants == 'mg2')   ## experience 2


## define variables and  contrasts
data$conf_ord = as.factor(data$conf)
data$rt_gabor_centered <- data$rt_gabor - mean(data$rt_gabor, na.rm = TRUE)

data$accuracy_gabor <- as.factor(data$accuracy_gabor)
contrasts(data$accuracy_gabor) <- - contr.sum(2) ## erreur: -1; correct: 1

data$effector <- as.factor(data$effector)
contrasts(data$effector) <-  -contr.sum(2) ## other: -1; same: 1

data$effector_order <- as.factor(data$effector_order)
contrasts(data$effector_order) <-  contr.sum(2) ## same1: -1; other1: 1

data$congruency <- as.factor(data$congruency)
contrasts(data$congruency) <-  contr.sum(2) ## incongruent: -1; congruent: 1

## * RT

## ** frequentist

l.rt <- lmer_alt(rt_gabor_centered*1000 ~  accuracy_gabor * congruency * effector * effector_order +
                     (1 + accuracy_gabor + congruency + effector + effector_order  ||subject_id),
                   REML = TRUE,
                   data = data)
summary(l.rt)
summary(rePCA(l.rt))

## remove congruency
l.rt <- lmer_alt(rt_gabor_centered*1000 ~  accuracy_gabor * congruency * effector * effector_order +
                   (1  + accuracy_gabor + effector + effector_order  ||subject_id),
                 REML = TRUE,
                 data = data)
save(l.rt, file = 'fit_rt_MG2.rdata')
tab_model(l.rt, file = "rt_MG2.html")

## ** bayesian
fit.rt <- brm(rt_gabor_centered*1000 ~  accuracy_gabor * congruency * effector * effector_order +
                     (1 + accuracy_gabor + congruency + effector + effector_order  ||subject_id) ,
           data = data,
           prior = c(set_prior("normal(0,1)", class = "b")),
           cores = 4, chains = 4,
           control = list(adapt_delta = .95,  max_treedepth = 12),
           iter = 4000,  warmup = 2000, seed = 123,
           save_model = 'rt.stan',
           save_pars = save_pars(all = TRUE)
           )

save(fit.rt, file = 'rt_bayes_MG2.rdata')
tab_model(fit.rt, file = "rt_bayes_MG2.html")


## RT evolution accross blocks
d <- data %>%
    group_by(subject_id, block_10) %>%
    summarise(acc = mean(acc_gabor_num), rt = mean(rt_gabor))

l <- lmer_alt(rt ~ block_10 + acc + (1 + block_10 + acc | subject_id) , data = d)
summary(l)


## * Accuracy

## Planned model
l.acc <- lmer_alt(acc_gabor_num ~  congruency * effector * effector_order  * rt_gabor_centered +
                      (1 + congruency * effector + effector_order  +  rt_gabor_centered ||subject_id),
		  family = binomial(link = "logit"),
		  data = data)
summary(l.acc)
summary(rePCA(l.acc))
 

## remove: congruency

l.acc <- lmer_alt(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
		    (1 + effector + effector_order  +  rt_gabor_centered ||subject_id),
		  family = binomial(link = "logit"),
		  data = data)
summary(l.acc)
summary(rePCA(l.acc))

## remove: effector order

l.acc <- lmer_alt(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
		    (1 +  effector  +  rt_gabor_centered ||subject_id),
		  family = binomial(link = "logit"),
		  data = data)
save(l.acc, file = 'fit_acc_MG2.rdata')
tab_model(l.acc, file = "acc_MG2.html")


## ** Bayesian lmer

fit_acc <- brm(acc_gabor_num ~  congruency * effector * effector_order  *  rt_gabor_centered +
               (1 + congruency * effector + effector_order  +  rt_gabor_centered ||subject_id),
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
save(fit_acc, file ='fit_acc_bayes_MG2.rdata')
tab_model(fit_acc, file = "acc_bayes_MG2.html")

## * Confidence

## ** frequentist

## planned model

l.conf <- clmm(conf_ord ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
		     (1 +  accuracy_gabor  + congruency * effector + effector_order +
		      rt_gabor_centered|subject_id),
                  data = data %>% filter(is.na(accuracy_gabor) == FALSE),
                  link = c("probit"))
summary(l.conf)

save(l.conf, file = "fit_conf_MG2.rdata")
tab_model(l.conf, file = "conf_MG2.html")
##load( file = "fit_conf_MG2.rdata")
## plot interactions
## effects are averaged over the levels of factors
## congruency
predict <- ggemmeans(l.conf, c('congruency'))
predict <- as.data.frame(predict) %>%
    rename(confidence = response.level, congruency = x)

plot <- ggplot(data = predict, aes(x = confidence, y = predicted, colour = congruency)) +
    geom_point(position = position_dodge(width = .5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = .5, position = "dodge") +
    labs(y = "Probabilty", 
        title = "Effect of Congruency on Confidence") +
    theme(plot.title = element_text(hjust = 0.5))
print(plot)
ggsave('conf_MG2.svg', plot)


## acc:congruency:order interaction 
predict <- ggemmeans(l.conf, c('accuracy_gabor','congruency','effector_order'))
predict <- as.data.frame(predict) %>%
    rename(accuracy = x, confidence = response.level, congruency = group, order = facet)
labels.accuracy = c("0" = "error", "1" = "correct")
labels.order = c(other1 = "different first", same1= "same first")


plot <- ggplot(data = predict, aes(x = confidence, y = predicted, colour = congruency)) +
    geom_point(position = position_dodge(width = .5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = .5, position = "dodge") + 
    facet_grid(order ~ accuracy, switch = "both",
               labeller=labeller(accuracy = labels.accuracy, order = labels.order)) +
    labs(y = "Probabilty", 
         title = "Accuracy x Congruency x Order interaction on Confidence") +
    theme(plot.title = element_text(hjust = 0.5))

ggsave('conf_acc_x_order_MG2.jpeg', plot)

## ** bayesian
fit_conf <- brm(conf ~ accuracy_gabor  * congruency * effector * effector_order +  rt_gabor_centered +
                    (1  +  accuracy_gabor  + congruency + effector * effector_order + rt_gabor_centered ||subject_id),
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
save(fit_conf, file = 'conf_MG2.rdata')
tab_model(fit_conf, file = "conf_bayes_MG2.html")


## additional



l <- lmer_alt(rt_conf ~    accuracy_gabor  * congruency +
                  (1 +  accuracy_gabor  + congruency |subject_id), data =  data)
summary(l)
data %>% group_by( accuracy_gabor, congruency) %>% summarise(rt_conf = mean(rt_conf))
