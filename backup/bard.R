# https://cran.r-project.org/web/packages/deconvolveR/vignettes/deconvolution.html
## Source this file in R.
cat("\n")
cat("********************************************\n")
cat("This script reproduces all figures, tables\n")
cat("  and results in the paper. For a more detailed\n")
cat("  description, refer to the package vignette.\n")
cat("\n")
cat("********************************************\n")

library("deconvolveR")
library("ggplot2")
library("cowplot")
library("knitr")

theme_set(theme_get() +
            theme(
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(colour = "gray90",
                                              size = 0.2),
              panel.grid.minor = element_line(colour = "gray98",
                                              size = 0.5),
              panel.border = element_rect(colour = "black", fill=NA)
            ))

## Section 2: Empirical Bayes estimation theory
## Table 1
cat("********************************************\n")
cat("Table 1\n")
cat("********************************************\n")

data("bardWordCount", package = "deconvolveR")
str(bardWordCount)
print(knitr::kable({
  d <- data.frame(matrix(bardWordCount, ncol = 10, byrow = TRUE))
  rownames(d) <- paste0(seq(0, 90, by = 10), "+")
  names(d) <- as.character(1:10)
  d
}))


## Section 3 The Shakespeare data
cat("********************************************\n")
cat("Section 3 The Shakespeare data\n")
cat("********************************************\n")

lambda <- seq(-4, 4.5, .025)
tau <- exp(lambda)
result <- deconv(tau = tau, y = bardWordCount, n = 100, c0=2)
stats <- result$stats

## Figure 1 plot
cat("********************************************\n")
cat("Figure 1 Plot (expect some harmless warnings)\n")
cat("********************************************\n")
d <- data.frame(lambda = lambda, g = stats[, "g"], tg = stats[, "tg"],
                SE.g = stats[, "SE.g"])
indices <- seq(1, length(lambda), 5)

print(
  ggplot(data = d) +
    geom_line(mapping = aes(x = lambda, y = g)) +
    geom_errorbar(data = d[indices, ],
                  mapping = aes(x = lambda, ymin = g - SE.g, ymax = g + SE.g),
                  width = .01, color = "green") +
    labs(x = expression(log(theta)), y = expression(g(theta))) +
    ##ylim(-0.001, 0.006) +
    xlim(-4, 4) +
    geom_vline(xintercept = 0.0, linetype = "dotted", color = "blue") +
    geom_hline(yintercept = 0.0, linetype = "dotted", color = "blue") +
    geom_line(mapping = aes(x = lambda, y = tg),
              linetype = "dashed", color = "red") +
    annotate("text", x = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
             y = rep(-0.0005, 9),
             label = c("0.02", "0.05", "0.14", "0.37", "1.00", "2.72", "7.39", "20.09", "90.02"), size = 2) +
    scale_y_continuous(breaks = c(-0.0005, 0.0, 0.002, 0.004, 0.006),
                       labels = c(expression(theta), "0.000", "0.002", "0.004", "0.006"),
                       limits = c(-0.0005, 0.006)) +
    labs(caption="Figure 1")
)

## ---- fig.keep='all', fig.width=7.5, fig.height=10-----------------------
cat("********************************************\n")
cat("Figure 2 Plot (expect some harmless warnings)\n")
cat("********************************************\n")
gPost <- sapply(seq_len(100), function(i) local({tg <- d$tg * result$P[i, ]; tg / sum(tg)}))
plots <- lapply(c(1, 2, 4, 8), function(i) {
  ggplot() +
    geom_line(mapping = aes(x = tau, y = gPost[, i])) +
    geom_vline(xintercept = i, linetype = "dotted", color = "blue") +
    geom_hline(yintercept = 0.0, linetype = "dotted", color = "blue") +
    labs(x = expression(theta), y = expression(g(theta)),
         title = sprintf("x = %d", i))
})
plots <- Map(f = function(p, xlim) p + xlim(0, xlim) + theme(plot.title=element_text(hjust=0.5)),
             plots, list(6, 8, 14, 20))

print(plot_grid(plotlist = plots, ncol = 2))

## Figure 3
cat("********************************************\n")
cat("Figure 3 Plot\n")
cat("********************************************\n")

set.seed(1783)
B <- 200
fHat <- as.numeric(result$P %*% d$g)
fHat <- fHat / sum(fHat)
yStar <- rmultinom(n = B, size = sum(bardWordCount), prob = fHat)
gBoot <- apply(yStar, 2,
               function(y) deconv(tau = tau, y = y, n = 100, c0 = 2)$stats[, "g"])
seG <- apply(gBoot, 1, sd)

print(
  ggplot(data = d) +
    geom_line(mapping = aes(x = lambda, y = SE.g,
                            color = "Theoretical", linetype = "Theoretical")) +
    geom_line(mapping = aes(x = lambda, y = seG,
                            color = "Bootstrap", linetype = "Bootstrap")) +
    scale_color_manual(name = "Legend",
                       values = c("Bootstrap" = "black", "Theoretical" = "red")) +
    scale_linetype_manual(name = "Legend",
                          values = c("Bootstrap" = "solid", "Theoretical" = "dashed")) +
    theme(legend.position = "none:") +
    scale_x_continuous(breaks=c(-4, -2, 0, 2, 4)) +
    labs(x = expression(log(theta)), y = expression(sigma(hat(g))), caption = "Figure 3")
)

## Figure 4
cat("********************************************\n")
cat("Figure 4 plot\n")
cat("********************************************\n")
gHat <- stats[, "g"]
Rfn <- function(t) {
  sum( gHat * (1 - exp(-tau * t)) / (exp(tau) - 1) )
}
time_mult <- seq(from = 0, to = 10, by = 0.1)
time_sub  <- seq.int(from = 0, to = 100, by = 5) + 1L
r <- sapply(time_mult, Rfn)
cov_Rfn <- function(t) {
  rj_t <- matrix((1 - exp(-tau * t)) / (exp(tau) - 1), ncol = 1)
  t(rj_t) %*% result$cov.g %*% rj_t
}

Rfn_se <- sqrt(sapply(time_mult, cov_Rfn))

## Fisher model, Efron & Thisted (1976) Biometrika 63, 3.
alphaHat  <- -0.3954
betaHat  <- 104.263
cHat  <- betaHat / (1 + betaHat)
N1  <- bardWordCount[1]
fisherFn <- function(t) {
  N1 * (1 - 1 / ((1 + cHat * t)^alphaHat)) / (cHat * alphaHat)
}

rf  <- sapply(time_mult, fisherFn) / sum(bardWordCount)

print(
  suppressWarnings({
    ggplot() +
      geom_line(mapping = aes(x = time_mult, y = r), size = 0.25) +
      geom_line(mapping = aes(x = time_mult, y = rf), color = "blue", size = 0.1, linetype = "dashed") +
      ## geom_hline(yintercept = 0.0, color = "blue", linetype="dotted") +
      ## geom_vline(xintercept = 0.0, color = "blue", linetype="dotted") +
      geom_errorbar(mapping = aes(x = time_mult[time_sub], ymin = (r - Rfn_se)[time_sub],
                                  ymax = (r + Rfn_se)[time_sub]),
                    width=0.1, color="red", size = 0.2) +
      labs(x = "time multiple t", y = expression(R(t)), caption = "Figure 4")
  })
)


## Section 4 A guide to a new package deconvolveR
cat("**********************************************\n")
cat("Section 4 A guide to a new package deconvolveR\n")
cat("**********************************************\n")

## Table 2
cat("********************************************\n")
cat("Table 2\n")
cat("********************************************\n")
set.seed(238923) ## for reproducibility
N <- 1000
Theta <- rchisq(N,  df = 10)
nSIM <- 1000
data <- sapply(seq_len(nSIM), function(x) rpois(n = N, lambda = Theta))
tau <- seq(1, 32)
results <- apply(data, 2,
                 function(x) deconv(tau = tau, X = x, ignoreZero = FALSE,
                                    c0 = 1))
g <- sapply(results, function(x) x$stats[, "g"])
mean <- apply(g, 1, mean)
SE.g <- sapply(results, function(x) x$stats[, "SE.g"])
sd <- apply(SE.g, 1, mean)
Bias.g <- sapply(results, function(x) x$stats[, "Bias.g"])
bias <- apply(Bias.g, 1, mean)
gTheta <- pchisq(tau, df = 10) - pchisq(c(0, tau[-length(tau)]), df = 10)
gTheta <- gTheta / sum(gTheta)
simData <- data.frame(theta = tau, gTheta = gTheta,
                      Mean = mean, StdDev = sd, Bias = bias,
                      CoefVar = sd / mean)
table2 <- transform(simData,
                    gTheta = 100 * gTheta,
                    Mean = 100 * Mean,
                    StdDev = 100 * StdDev,
                    Bias = 100 * Bias)

print(knitr::kable(table2[c(5, 10, 15, 20, 25), ],
                   row.names=FALSE, digits = 2))

## Figure 5
cat("********************************************\n")
cat("Figure 5 Plot\n")
cat("********************************************\n")

p1 <- ggplot(data = as.data.frame(results[[1]]$stats)) +
  geom_line(mapping = aes(x = theta, y = SE.g),
            color = "black", linetype = "solid") +
  geom_line(mapping = aes(x = simData$theta, y = simData$StdDev),
            color = "red", linetype = "dashed") +
  labs(x = expression(theta), y = "Std. Dev")

p2 <- ggplot(data = as.data.frame(results[[1]]$stats)) +
  geom_line(mapping = aes(x = theta, y = Bias.g),
            color = "black", linetype = "solid") +
  geom_line(mapping = aes(x = simData$theta, y = simData$Bias),
            color = "red", linetype = "dashed") +
  labs(x = expression(theta), y = "Bias")
print(plot_grid(plotlist = list(p1, p2), ncol = 2))

## Figure 6
cat("********************************************\n")
cat("Figure 6 plot\n")
cat("********************************************\n")
set.seed(129023)
N <- 10000
pi0 <- .90
data <- local({
  nullCase <- (runif(N) <= pi0)
  muAndZ <- t(sapply(nullCase, function(isNull) {
    if (isNull) {
      mu <- 0
      c(mu, rnorm(1))
    } else {
      mu <- rnorm(1, mean = -3)
      c(mu, rnorm(1, mean = mu))
    }
  }))
  data.frame(nullCase = nullCase, mu = muAndZ[, 1], z = muAndZ[, 2])
})

tau <- seq(from = -6, to = 3, by = 0.25)
atomIndex <- which(tau == 0)
result <- deconv(tau = tau, X = data$z, deltaAt = 0, family = "Normal", pDegree = 5)

print(knitr::kable(result$stats))

p <- pnorm(data$z)
orderP <- order(p)
p <- p[orderP]

## FCR
q <- 0.05
R <- max(which(p <= seq_len(N) * q / N))
discIdx <- orderP[1:R]
disc <- data[discIdx, ]
cat("BY_q procedure discoveries", R, "cases,", sum(disc$nullCase),
    "actual nulls among them.\n")

alphaR <- 1 - R * q / N
zAlpha <- qnorm(alphaR, lower.tail = FALSE)
zMarker <- max(disc$z)
xlim <- c(-7.6, 0.0)
ylim <- c(-10, 0.0)
BY.lo <- c(xlim[1] - zAlpha, xlim[2] - zAlpha)
BY.up <- c(xlim[1] + zAlpha, xlim[2] + zAlpha)
Bayes.lo <- c(0.5 * (xlim[1] - 3) - 1.96 / sqrt(2), 0.5 * (xlim[2] - 3) - 1.96 / sqrt(2))
Bayes.up <- c(0.5 * (xlim[1] - 3) + 1.96 / sqrt(2), 0.5 * (xlim[2] - 3) + 1.96 / sqrt(2))

d <- data[order(data$mu), ]
muVals <- unique(d$mu)
s <- as.data.frame(result$stats)
indices <- findInterval(muVals, s$theta) + 1
gMu <- s$g[indices]
st <- seq(min(data$z), -2.0, length.out = 40)
gMuPhi <- sapply(st, function(z) gMu * dnorm(z - muVals))
g2 <- apply(gMuPhi, 2, function(x) cumsum(x)/sum(x))
pct <- apply(g2, 2, function(dist) approx(y = muVals, x = dist, xout = c(0.025, 0.975)))
qVals <- sapply(pct, function(item) item$y)

print(
  ggplot() +
    geom_line(mapping = aes(x = xlim, y = Bayes.lo), color = "magenta",
              linetype = "dashed") +
    geom_line(mapping = aes(x = xlim, y = Bayes.up), color = "magenta",
              linetype = "dashed") +
    geom_point(mapping = aes(x = disc$z, y = disc$mu), color = "red") +
    geom_point(mapping = aes(x = disc$z[disc$nullCase], y = disc$mu[disc$nullCase]),
               color = "orange") +
    geom_line(mapping = aes(x = rep(zMarker, 2), y = c(-10, 1))) +
    geom_line(mapping = aes(x = st, y = qVals[1, ]), color = "brown") +
    geom_line(mapping = aes(x = st, y = qVals[2, ]), color = "brown") +
    labs(x = "Observed z", y = expression(mu), caption = "Figure 6") +
    annotate("text", x = -7.5, y = -6.1, label = "Bayes.lo") +
    annotate("text", x = -7.5, y = -3.4, label = "Bayes.up") +
    annotate("text", x = -2.0, y = -1.75, label = "EB.lo") +
    annotate("text", x = -2.0, y = -3.9, label = "EB.up") +
    annotate("text", x = zMarker, y = 1.25, label = as.character(round(zMarker, 2)))
)

## Figure 7
cat("********************************************\n")
cat("Figure 7 plot\n")
cat("********************************************\n")
p1 <- ggplot(mapping = aes(x = disjointTheta)) +
  geom_histogram(mapping = aes(y  = ..count.. / sum(..count..) ),
                 color = "brown", bins = 60, alpha = 0.5) +
  labs(x = expression(theta), y = "Density")
set.seed (2332)
z <- rnorm(n = length(disjointTheta), mean = disjointTheta)
p2 <- ggplot(mapping = aes(x = z)) +
  geom_histogram(mapping = aes(y  = ..count.. / sum(..count..) ),
                 color = "brown", bins = 60, alpha = 0.5) +
  labs(x = "z", y = "Density")
print(plot_grid(plotlist = list(p1, p2), ncol = 2))

## Figure 8
cat("********************************************\n")
cat("Figure 8 Plot \n")
cat("********************************************\n")
tau <- seq(from = -4, to = 6, by = 0.2)
plots1 <- lapply(2:8,
                 function(p) {
                   result <- deconv(tau = tau, X = z, family = "Normal", pDegree = p)
                   g <- result$stats[, "g"]
                   ggplot(mapping = aes(x = disjointTheta)) +
                     geom_histogram(mapping = aes(y = ..count.. / sum(..count..)),
                                    color = "brown", bins = 60, alpha = 0.2) +
                     geom_line(mapping = aes(x = tau, y = g), color = "blue") +
                     labs(x = expression(theta), y = "Density",
                          title = sprintf("DF = %d", p))
                 })

plots2 <- lapply(c(0.5, 1, 2, 4, 8, 16, 32),
                 function(c0) {
                   result <- deconv(tau = tau, X = z, family = "Normal", pDegree = 6,
                                    c0 = c0)
                   g <- result$stats[, "g"]
                   ggplot(mapping = aes(x = disjointTheta)) +
                     geom_histogram(mapping = aes(y = ..count.. / sum(..count..)),
                                    color = "brown", bins = 60, alpha = 0.2) +
                     geom_line(mapping = aes(x = tau, y = g), color = "blue") +
                     labs(x = expression(theta), y = "Density",
                          title = sprintf("C0 = %.1f", c0))
                 })
plots <- mapply(function(x, y) list(x, y), plots1, plots2)
print(plot_grid(plotlist = plots, ncol = 2))


## Table 3
cat("********************************************\n")
cat("Table 3\n")
cat("********************************************\n")

c0_values <- c(.5, 1, 2, 4, 8, 16, 32)
stable <- rbind(
  `DF 5` = sapply(c0_values, function(c0) deconv(tau = tau, X = z, family = "Normal", pDegree = 5, c0 = c0)$S),
  `DF 6` = sapply(c0_values, function(c0) deconv(tau = tau, X = z, family = "Normal", pDegree = 6, c0 = c0)$S),
  `DF 7` = sapply(c0_values, function(c0) deconv(tau = tau, X = z, family = "Normal", pDegree = 7, c0 = c0)$S)
)
colnames(stable) <- paste("c0 =", c0_values)
print(knitr::kable(as.data.frame(stable), digits = 2, caption = "Table 3"))


## Shakespeare example
cat("********************************************\n")
cat("Shakespeare Example Session, Section 4.4\n")
cat("********************************************\n")

lambda <- seq(-4, 4.5, .025)
tau <- exp(lambda)
result <- deconv(tau = tau, y = bardWordCount, n = 100, c0 = 2)
stats <- result$stats
print(head(stats))
print(tail(stats))
print(result$S)

## ------------------------------------------------------------------------
## Intestinal Surgery Example
cat("********************************************\n")
cat("Intestinal Surgery Example, Section 4.5\n")
cat("********************************************\n")

## ------------------------------------------------------------------------
tau <- seq(from = 0.01, to = 0.99, by = 0.01)
result <- deconv(tau = tau, X = surg, family = "Binomial", c0 = 1)
d <- data.frame(result$stats)
indices <- seq(5, 99, 5)
errorX <- tau[indices]
print(
  ggplot() +
    geom_line(data = d, mapping = aes(x = tau, y = g)) +
    geom_errorbar(data = d[indices, ],
                  mapping = aes(x = theta, ymin = g - SE.g, ymax = g + SE.g),
                  width = .01, color = "blue") +
    labs(x = expression(theta), y = expression(paste(g(theta), " +/- SE")), caption = "Figure 9")
)

## ------------------------------------------------------------------------
print(knitr::kable(d[indices, ], row.names = FALSE))

## Table 4
cat("********************************************\n")
cat("Table 4, computationally intensive...\n")
cat("********************************************\n")
set.seed(32776)
B <- 1000
gHat <- d$g
N <- nrow(surg)

genBootSample <- function() {
  thetaStar <- sample(tau, size = N, replace = TRUE, prob = gHat)
  sStar <- sapply(seq_len(N),
                  function(i)
                    rbinom(n = 1 , size = surg$n[i], prob = thetaStar[i]))
  data.frame(n = surg$n, s = sStar)
}

bootResults <- lapply(seq_len(B),
                      function(k) {
                        surgBoot <- genBootSample()
                        mat <- deconv(tau = tau, X = surgBoot, family = "Binomial",
                                      c0 = 1)$stats
                        mat[, c("g", "Bias.g")]
                      })

gBoot <- sapply(bootResults, function(x) x[, 1])
BiasBoot <- sapply(bootResults, function(x) x[, 2])

indices <- c(seq(1, 99, 11), 99)

table4 <- data.frame(theta = tau,
                     gTheta = round(gHat * 100, 3),
                     sdFormula = round(d$SE.g * 100, 3),
                     sdSimul = round(apply(gBoot, 1, sd) * 100, 3),
                     BiasFormula = round(d$Bias.g * 100, 3),
                     BiasSimul = round(apply(BiasBoot, 1, mean) * 100, 3))[ indices, ]



## ------------------------------------------------------------------------
print(knitr::kable(table4, row.names = FALSE))

