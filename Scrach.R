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
