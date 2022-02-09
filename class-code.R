if (!require("pacman")) install.packages("packman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, MatchIt, cobalt)

#install.packages('Matching')

##Simulate data
set.seed(12345678)
n <- 10000
#tibble is easier to read than data frame in R
select.dat <- tibble(
  x = rnorm(n, .2, 1),
  z = rnorm(n, -.1, 1),
  d = (x + rnorm(n, 0, 1)>0),
  y0 = -2.5 + 1.5*x + rnorm(n, 0, 1),
  y1 = y0 + 4,
  y = y1*d + y0*(1-d),
  d_alt = (x+z+rnorm(n, 0, 1)>0),
  y0_alt = -2.5 +1.5*x +2.25*z + rnorm(n, 0, 1),
  y1_alt = y0_alt + 4,
  y_alt = y1_alt*d_alt + y0_alt*(1-d_alt)
)
select.dat

#average treatment effect = E[y1] - E[y0] = 4+(alpha_beta*x+gamma) - (alpha+beta*x+gamma) = 4
# y = -2.5 + 4*d + 1.5x + rnorm(n,0,1)

## nearest neighbor matching with euclidean distance weights
nn.est1 <- Matching::Match(Y=select.dat$y,
                           Tr=select.dat$d,
                           X = select.dat$x,
                           M=1, #number of people mathced to each person
                           Weight=1, #euclidean
                           estimand = "ATE")
# default and best practice is to match with replacement

summary(nn.est1)

## nearest neighbor matching with mahalanobis weights - accounts for covariance

nn.est2 <- Matching::Match(Y=select.dat$y,
                           Tr=select.dat$d,
                           X = select.dat$x,
                           M=1, #number of people mathced to each person
                           Weight=2, #mahalanobis
                           estimand = "ATE")
summary(nn.est2)

## regression
reg1.dat <- select.dat %>% filter(d==1)
reg1 <- lm(y ~ x, data=reg1.dat)

reg0.dat <- select.dat %>% filter(d==0)
reg0 <- lm(y ~ x, data=reg0.dat)

pred1 <- predict(reg1, new=select.dat)
pred0 <- predict(reg0, new=select.dat)

mean(pred1-pred0)

bind_cols(select.dat,pred1,pred0)
