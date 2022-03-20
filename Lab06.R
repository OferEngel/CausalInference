# Lab 06
library(tidyverse)
library(modelsummary)


# Generate the data
n        <-  1000
beta_SOD2SBP  <-  1.05 
alpha_SOD2PRO  <-  0.5
alpha_SBP2PRO  <-  0.5

set.seed(777)
age_years   <- rnorm(n, 65, 5)
sodium_gr   <- age_years / 18 + rnorm(n)
sodium_cutoff <- mean(sodium_gr)
sodium.bin <- ifelse(sodium_gr>sodium_cutoff,1,0)


sbp_in_mmHg <- rnorm(n, beta_SOD2SBP * sodium.bin + 
                       2.00 * age_years, 1)

proteinuria_in_mg <- rnorm(n, alpha_SOD2PRO * sodium.bin + 
                             alpha_SBP2PRO * sbp_in_mmHg, 1)

df <- data.frame(sbp_in_mmHg, sodium.bin, age_years, proteinuria_in_mg)

# 
# Estimating the ATE 
ATE.naive <- lm(sbp_in_mmHg ~ sodium.bin)
ATE.ctrl.all <- lm(sbp_in_mmHg ~ age_years + proteinuria_in_mg + sodium.bin)
ATE.ctrl.age <- lm(sbp_in_mmHg ~ age_years + sodium.bin)

# Question 3, predicting the potential outcomes
# 
# start with the naive model
Y0.naive <-  predict(ATE.naive, data.frame(age_years, proteinuria_in_mg, sodium.bin=0))
Y1.naive <-  predict(ATE.naive, data.frame(age_years, proteinuria_in_mg, sodium.bin=1))
mean(Y1.naive - Y0.naive)  # estimated ATE for the naive model


Y0.ctrl.age <-  predict(ATE.ctrl.age, data.frame(age_years, proteinuria_in_mg, sodium.bin=0))
Y1.ctrl.age <-  predict(ATE.ctrl.age, data.frame(age_years, proteinuria_in_mg, sodium.bin=1))
mean(Y1.ctrl.age - Y0.ctrl.age)

Y0.ctrl.all <-  predict(ATE.ctrl.all, data.frame(age_years, proteinuria_in_mg, sodium.bin=0))
Y1.ctrl.all <-  predict(ATE.ctrl.all, data.frame(age_years, proteinuria_in_mg, sodium.bin=1))
mean(Y1.ctrl.all - Y0.ctrl.all)




modelsummary(list(naive=ATE.naive, all=ATE.ctrl.all, age=ATE.ctrl.age), stars=TRUE, statistic=NA)
