# sign-testlibrary(ggpubr)
library(ggplot2)

mu0 <- 2
sigma <- 1

mua <- c(1.5,1,0)
Ns <- seq(10,510,50)
for_plot <- dplyr::bind_rows(lapply(mua, function(mua) {
  lapply(Ns, function(N) {
    pvals <- replicate(1000, {
      dat <- rcauchy(N, mua, 1)
      wilcox.test(dat,alternative="less", mu = mu0)$p.value
    })
    data.frame(
      mua = mua,
      N = N,
      prob_reject = mean(pvals < .05)
    )
  })
}))
ggplot(for_plot, aes(x = N, y = prob_reject, color = factor(mua))) +
  geom_line() +
  labs(
    x = "number of samples",
    y = "Probability of rejecting the null hypothesis when it is false",
    color = "Actual population median",
    title = "Consistency of left tailed Sign test"
  )


mua <- c(2.5,3,4)
for_plot <- dplyr::bind_rows(lapply(mua, function(mua) {
  lapply(Ns, function(N) {
    pvals <- replicate(1000, {
      dat <- rcauchy(N, mua, 1)
      wilcox.test(dat,alternative="greater", mu = mu0)$p.value
    })
    data.frame(
      mua = mua,
      N = N,
      prob_reject = mean(pvals < .05)
    )
  })
}))
ggplot(for_plot, aes(x = N, y = prob_reject, color = factor(mua))) +
  geom_line() +
  labs(
    x = "number of samples",
    y = "Probability of rejecting the null hypothesis when it is false",
    color = "Actual population median",
    title = "Consistency of right tailed Sign test"
  )


mua <- c(1.5,3,4,0.5)
for_plot <- dplyr::bind_rows(lapply(mua, function(mua) {
  lapply(Ns, function(N) {
    pvals <- replicate(1000, {
      dat <- rcauchy(N, mua, 1)
      wilcox.test(dat, mu = mu0)$p.value
    })
    data.frame(
      mua = mua,
      N = N,
      prob_reject = mean(pvals < .05)
    )
  })
}))
ggplot(for_plot, aes(x = N, y = prob_reject, color = factor(mua))) +
  geom_line() +
  labs(
    x = "number of samples",
    y = "Probability of rejecting the null hypothesis when it is false",
    color = "Actual population median",
    title = "Consistency of both tailed Sign test"
  )
