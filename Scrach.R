rVariable <- rnorm(100, 3, 5)
mean(rVariable)
sd(rVariable)
sample <- data.frame(rVariable=rVariable)
write.csv(sample, "data/sample.csv")

sample <- read.csv("https://bit.ly/3uZsuQv")
str(sample)



b <- c(seq(-317,-307, by=2), 
       round(seq(-308, -306.3258, length.out=4),1))

b <- c(seq(-317,-307, by=2), 
       round(log(seq(exp(-308), exp(-306.3258), length.out=4)),2))

b <- c(round(log(seq(exp(-317), exp(-306.3258), length.out=8)),2))


df %>%  
  ggplot(aes(x=mu, y=sigma, 
             z=loglik)) +
  geom_contour(breaks=b) + 
  geom_text_contour(breaks=b) 

max(df$loglik)


seq(-308, -306.3258, length.out=4)
log(seq(exp(-308), exp(-306.3258), length.out=4))

df[which.max(df$loglik),]
df %>%  
  ggplot(aes(x=mu, y=sigma, 
             z=loglik)) +
  geom_contour(breaks=b) + 
  geom_text_contour(breaks=b) + 
  geom_vline(xintercept = 2.72, color="grey", alpha=.8) + 
  geom_hline(yintercept = 5.18, color="grey", alpha=.8) + 
  ylim(c(4.5,6)) + 
  scale_x_continuous(breaks=2.72 +c(-1,1)*seq(0,2,by=.2))+ 
  scale_y_continuous(breaks=5.18 +c(-1,1)*seq(0,2,by=.2))

b <- c(round(log(seq(exp(-315), exp(-306.3258), length.out=9)),2))

df %>%  
  ggplot(aes(x=mu, y=sigma, 
             z=loglik)) +
  geom_contour(breaks=b) + 
  geom_text_contour(breaks=b) + 
  geom_vline(xintercept = 2.72, color="grey", alpha=.8) + 
  geom_hline(yintercept = 5.18, color="grey", alpha=.8) + 
  scale_x_continuous(breaks=2.72 +c(-1,1)*seq(0,2,by=.2))+ 
  scale_y_continuous(breaks=5.18 +c(-1,1)*seq(0,2,by=.2), 
                     limits=c(4.5,6))




df %>%  
  ggplot(aes(x=mu, y=sigma, 
             z=loglik)) +
  geom_contour(breaks=b) + 
  geom_text_contour(breaks=b) + 
  geom_vline(xintercept = 2.72, color="grey", alpha=.8) + 
  geom_hline(yintercept = 5.18, color="grey", alpha=.8) + 
  scale_x_continuous(breaks = bx, limits=c(1.9,3.8))+ 
  scale_y_continuous(breaks = by, limits=c(4,6))


library(tidyverse)
library(metR) # <-- this library for the contour plot...


X <- read.csv("https://bit.ly/3uZsuQv")
# likelihood: given the parameters of a normal distribution, 
# sigma and mu, what is the probability of observing 
# a certain outcome, x
# p(x|mu,sigma) = dnorm(x, mean=mu, sd=sigma )
lik <- function(x, mu, sigma){
  return(sum(log(dnorm(x, mean=mu, sd=sigma))))  
}

# Now we want to create a data frame with combinations of 
# potential values for mu and sigma. You may want to 
# decrease the range of parameters or the number of 
# combinations to improve your plot.
df <- expand.grid(mu = seq(2,4, b=.02), 
                  sigma = seq(4,6, b=.02))


# Here we calculate the log-likelihood for each combination
df$loglik <- rep(NA, nrow(df))
for(i in 1:nrow(df)){
  mu    <- df$mu[i]
  sigma <- df$sigma[i]
  df$loglik[i] <- lik(X$rVariable,mu,sigma)
}

# Finally, we plot a heat map and a contour map, 
# a representation of a three dimensional space where the x-axis
# represents the mu, the y-axis represents the standard deviation, 
# and the colour or contour represents the log likelihood.
df %>%
  ggplot(aes(x=mu, y=sigma,
             fill=exp(loglik))) +
  geom_tile() +  scale_fill_binned(type = "viridis")  


df %>%  
  ggplot(aes(x=mu, y=sigma, 
             z=loglik)) +
  geom_contour() + 
  geom_text_contour() 


var(c(0,1,-1))*2/3
sqrt(2/3)



# Here we calculate the log-likelihood for each combination
df$loglik <- rep(NA, nrow(df))
for(i in 1:nrow(df)){
  mu    <- df$mu[i]
  sigma <- df$sigma[i]
  df$loglik[i] <- lik(X$rVariable,mu,sigma)
}

df$col <- rank(loglik)/100







# Finally, we plot a heat map and a contour map, 
# a representation of a three dimensional space where the x-axis
# represents the mu, the y-axis represents the standard deviation, 
# and the colour or contour represents the log likelihood.
df %>%
  ggplot(aes(x=mu, y=sigma,
             fill=col)) +
  geom_tile() + scale_fill_binned(type = "viridis")  




# see https://www.theguardian.com/world/2006/jul/20/secondworldwar.tvandradio
# 
library(tidyverse)
max.n <- 10000
n.german.tanks <- 50
x.tanks <- 1:max.n
# p(obs|hypothesis)
pr.obs.given.hyp <- function(obs, hyp) 
  ifelse(hyp<obs, 0, 1/hyp)

# Calculate the posterior
pr.hyp.given.obs <- function(prior, obs) {
  lik <- pr.obs.given.hyp(obs, x.tanks)
  lik*prior/sum(lik*prior)
}


prior <- rep(1, max.n)

obs <- sample(1:n.german.tanks, 1)
post <- pr.hyp.given.obs(prior, obs)
ggplot() + geom_line(aes(x=x.tanks, y=post)) + scale_x_log10()
x.tanks[sum(cumsum(post)==0)]
x.tanks[sum(cumsum(post)<0.95)]
prior <- post


n.observations <- 50
percentile.95 <- data.frame(min=rep(NA,n.observations), 
                            max=rep(NA,n.observations))
prior <- rep(1, max.n)
for(i in 1:n.observations){
  obs <- sample(1:n.german.tanks, 1)
  post <- pr.hyp.given.obs(prior, obs)
  percentile.95[i,] <- c(x.tanks[sum(cumsum(post)==0)]+1, 
                         x.tanks[sum(cumsum(post)<0.95)])
  prior <- post
}
percentile.95
ggplot() + geom_ribbon(aes(x=1:n.observations, 
                           ymin=percentile.95$min, 
                           ymax=percentile.95$max), 
                       alpha=.4) + 
  geom_hline(yintercept = n.german.tanks, color="red") + 
  scale_y_log10()







roll2dice <- function(){
  d1 <- sample(1:6,1)
  d2 <- sample(1:6,1)
  d3 <- sample(1:6,1)
  d4 <- sample(1:6,1)
  d5 <- sample(1:6,1)
  sum(d1,d2,d3,d4,d5)
}


rolldice <- function(){
  sum(sample(1:6, 1000, replace=TRUE))
}

outcome <- replicate(100000, rolldice())
mu <- mean(outcome)
sd <- sd(outcome)
mean(outcome<=3550)



ggplot() + 
  geom_histogram(aes(x=outcome, y=..density..), binwidth=1) + 
  scale_x_continuous(breaks=seq(310,500,by=20)) + 
  geom_function(fun=dnorm, args=list(mean=mu, sd=sd))





var(1:6)
var(outcome)*9999/10000
mean(outcome^2)-(mean(outcome)^2)



mean(outcome>375)


summary(outcome)

read.csv("https://raw.githubusercontent.com/rstudio-education/datascience-box/main/course-materials/slides/u4-d01-language-of-models/data/paris-paintings.csv")




library(tidyverse)

ggplot(paintings) + 
  geom_histogram(aes(x=logprice))


paintings <- read.csv("https://bit.ly/36IfLaO")

ggplot(paintings) + 
  geom_point(aes(x=Width_in, y=Height_in)) 



  scale_color_continuous(type = "viridis")  + 
  scale_x_log10() + scale_y_log10()

library(mosaicData)
data(Whickham)

levels(Whickham$outcome)
levels(Whickham$smoker)
Whickham %>% mutate(ismoke=as.numeric(smoker)-1) -> d




glm( outcome ~ age + I(age^2) + ismoke + I(age*ismoke), 
    data=d, 
    family=binomial) %>% summary()

glm( outcome ~ smoker, 
     data=d, 
     family=binomial) %>% summary()

glm( outcome ~ age, 
     data=d, 
     family=binomial) %>% summary()


Whickham %>%
  count(smoker, outcome)


library(janitor)
Whickham %>% mutate(fct_age=case_when(
  age <= 44 ~ "young", 
  age < 65  ~ "midage",
  TRUE ~ "old"
  )) %>% 
  mutate(fct_age = 
                  factor(fct_age,levels=c("young", "midage", "old"))) ->
  d
  


d %>% 
  tabyl(outcome, smoker)  %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% adorn_title("top")

d %>% 
  tabyl(fct_age, smoker)  %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% adorn_title("top")


d %>% 
  tabyl(fct_age, outcome) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% adorn_title("top")

d %>% 
  tabyl(outcome, smoker, fct_age) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% adorn_title("top")

d %>% 
  tabyl(fct_age) %>% 
  adorn_pct_formatting()

