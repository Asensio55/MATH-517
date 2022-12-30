set.seed(1315)
### oackages and libraries ####
install.packages("locpol")
install.packages("fANCOVA")
install.packages("KernSmooth")
install.packages("wavethresh")
library(wavethresh)
library(locpol)
library(fANCOVA)
library(KernSmooth)
library(ggplot2)
library(patchwork)
#body(locpol)
#body(loess) # + lowess
# https://subscription.packtpub.com/book/data/9781788627306/1/ch01lvl1sec18/r-packages-for-regression

#caret 

#knnreg ?
#( package lpridge  -> lpepa, lpridge)
# package locfit -> locfit
# package locpol -> locpol
### gam package, stl2, operator

# loess.as{fANCOVA}
### Plan: find datasets of different sizes, noise them or generate them (powers of 10)
### apply loess, locpol, locfit,locpoly (kernsmooth) time it. Check performance.
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/loess.html  https://search.r-project.org/CRAN/refmans/locfit/html/locfit.raw.html
# https://www.rdocumentation.org/packages/KernSmooth/versions/2.23-20/topics/locpoly


### I'd say we use loess.as (fANCOVA), locpol (locpol), loess(stats, base), locpoly(KernSmooth) only, as the other ones kinda have mean parameter selection. Maybe drop Loess as well ?

### Choice of data of different difficulties, size 10^3,4,5,6,(7) 

# simple : evenly spaced gaussian data ####

# points for different sizes:
X3 <- seq(from = -5, to = 5, by= 0.1)
X4 <- seq(from = -5, to = 5, by= 0.01)
X5 <- seq(from = -5, to = 5, by= 0.001)
X6 <- seq(from = -5, to = 5, by= 0.0001)
X7 <- seq(from = -5, to = 5, by= 0.00001)

# generation :
d3 <- dnorm(X3)
d4 <- dnorm(X4)
d5 <- dnorm(X5)
d6 <- dnorm(X6) 
d7 <- dnorm(X7) # around 20sec for plotting.

#dataframes

df3 <-data.frame(X3,d3)
df4 <-data.frame(X4,d4)
df5 <-data.frame(X5,d5)
df6 <-data.frame(X6,d6)
df7 <-data.frame(X7,d7)

rm(d3,d4,d5,d6,d7)
# applying regressions. which bandwith/span. should reduce span and bw by factor 10 each time for consistency ?  ?

Lo3 <- loess(d3 ~ X3, data = df3, family = "gaussian", method = "loess", span = 0.75)
Lp3 <- locpol(d3 ~ X3, data = df3, bw = 0.25, kernel = gaussK, deg = 2)
Loas3 <- loess.as(df3$X3, df3$d3, degree = 2, criterion = "aicc", family = "gaussian")
LoLy3 <- locpoly(df3$X3, df3$d3, degree = 2, kernel = "normal", bandwidth = 0.25)

Lo4 <- loess(d4 ~ X4, data = df4, family = "gaussian", method = "loess", span = 0.075)
Lp4 <- locpol(d4 ~ X4, data = df4, bw = 0.025, kernel = gaussK, deg = 2)
Loas4 <- loess.as(df4$X4, df4$d4, degree = 2, criterion = "aicc", family = "gaussian")
LoLy4 <- locpoly(df4$X4, df4$d4, degree = 2, kernel = "normal", bandwidth = 0.025, gridsize = 1001)

Lo5 <- loess(d5 ~ X5, data = df5, family = "gaussian", method = "loess", span = 0.0075)
#Lp5 <- locpol(d5 ~ X5, data = df5, bw = 0.0025, kernel = gaussK, deg = 2) # maxevalpts is 5000 -> does not work for more ?
Loas5 <- loess.as(df5$X5, df5$d5, degree = 2, criterion = "aicc", family = "gaussian")
LoLy5 <- locpoly(df5$X5, df5$d5, degree = 2, kernel = "normal", bandwidth = 0.025, gridsize = 10001)

Lo6 <- loess(d6 ~ X6, data = df6, family = "gaussian", method = "loess", span = 0.00075)
#Lp6 <- locpol(d6 ~ X6, data = df6, bw = 0.00025, kernel = gaussK, deg = 2)
Loas6 <- loess.as(df6$X6, df6$d6, degree = 2, criterion = "aicc", family = "gaussian") # takes long, fix bw maybe ?
Loas6_fixed <- loess.as(df6$X6, df6$d6, degree = 2, criterion = "aicc", family = "gaussian", user.span = 0.00075) # way faster.
LoLy6 <- locpoly(df6$X6, df6$d6, degree = 2, kernel = "normal", bandwidth = 0.025, gridsize = 100001)# min bw

Lo7 <- loess(d7 ~ X7, data = df7, family = "gaussian", method = "loess", span = 0.000075) # works
#Lp7 <- locpol(d7 ~ X7, data = df7, bw = 0.000025, kernel = gaussK, deg = 2)
Loas7_fixed <- loess.as(df7$X7, df7$d7, degree = 2, criterion = "aicc", family = "gaussian", user.span = 0.000075) # works
LoLy7 <- locpoly(df7$X7, df7$d7, degree = 2, kernel = "normal", bandwidth = 0.025, gridsize = 1000001) # works

# now, time it and report results.
# hard ? mixture of gaussian/laplace distributions. not uniformly spaced; different lateral densities ####
# Claw distribution with uniform gaussian noise 
#Example with 101:
X3 <- seq(from = -5, to = 5, by= 0.1)
X4 <- seq(from = -5, to = 5, by= 0.01)
X5 <- seq(from = -5, to = 5, by= 0.001)
X6 <- seq(from = -5, to = 5, by= 0.0001)
X7 <- seq(from = -5, to = 5, by= 0.00001) # X-axis evaluation points

Y3 <- dclaw(X3) # claw distribution
n3 <- rnorm(X3, sd = 0.1) # gaussian noise
Y3 <- Y3 + n3

# applying the functions:
daf3 <- data.frame(X3,Y3)
Lo3c <- loess(Y3 ~ X3, data = daf3, family = "gaussian", method = "loess", span = 0.587) # Lo3c$residuals
Lpc3 <- locpol(Y3 ~ X3, data = daf3, bw = 0.25, kernel = gaussK, deg = 2) # Lpc3$residuals
Loasc3 <- loess.as(daf3$X3, daf3$Y3, degree = 2, criterion = "aicc", family = "gaussian") # Loasc3$residuals
LoLyc3 <- locpoly(daf3$X3, daf3$Y3, degree = 2, kernel = "normal", bandwidth = 0.25, gridsize = 101) # only gives results


# R^2 coefficients
#Loess
SSres <- sum((Y3 - Lo3c$fitted)^2)
SStot <- sum((Y3 - mean(Y3))^2)
R2Lo3 <- 1 - SSres/SStot
#Locpol
SSres <- sum((Y3 - Lpc3$mf[1])^2)
SStot <- sum((Y3 - mean(Y3))^2)
R2Lp3 <- 1 - SSres/SStot
#Loess.as
SSres <- sum((Y3 - Loasc3$fitted)^2)
SStot <- sum((Y3 - mean(Y3))^2)
R2Loas3 <- 1 - SSres/SStot
#locpoly
SSres <- sum((Y3 - LoLyc3$y)^2)
SStot <- sum((Y3 - mean(Y3))^2)
R2Loly3 <- 1 - SSres/SStot

#plot
fitted_loc <- Lo3c$fitted
fitted_lp <- Lpc3$mf[1]
fitted_loas <- Loasc3$fitted
fitted_Loly <- LoLyc3$y


pc2 <- ggplot() + geom_point(mapping = aes(x = X3, y = fitted_loc, color = "loess")) + 
  geom_point(mapping = aes(x = X3, y = unlist(fitted_lp), color = "locpol")) + 
  geom_point(mapping = aes(x = X3, y = fitted_loas, color = "loess.as")) + 
  geom_point(mapping = aes(x = X3, y = fitted_Loly, color = "locpoly")) + 
  scale_color_manual(name = "Regression Function",
                     values = c( "loess" = "blue", "locpol" = "red", "loess.as" = "green", "locpoly" = "purple"),
                     labels = c("loess", "locpol", "loess.as","locpoly")) + 
  labs(y = "fit") +
  ggtitle("Claw distribution, 10^2 data points")



## 1001
#Distribution
Y4 <- dclaw(X4)
n4 <- rnorm(X4, sd = 0.1)
Y4 <- Y4 + n4

# Application
daf4 <- data.frame(X4,Y4)
Lo4c <- loess(Y4 ~ X4, data = daf4, family = "gaussian", method = "loess", span = 0.211) # Lo3c$residuals
Lpc4 <- locpol(Y4 ~ X4, data = daf4, bw = 0.25, kernel = gaussK, deg = 2) # Lpc3$residuals
Loasc4 <- loess.as(daf4$X4, daf4$Y4, degree = 2, criterion = "aicc", family = "gaussian") # Loasc3$residuals
LoLyc4 <- locpoly(daf4$X4, daf4$Y4, degree = 2, kernel = "normal", bandwidth = 0.0025, gridsize = 1001) # only gives results


# R^2 coefficients
#Loess
SSres <- sum((Y4 - Lo4c$fitted)^2)
SStot <- sum((Y4 - mean(Y4))^2)
R2Lo4 <- 1 - SSres/SStot
#Locpol
SSres <- sum((Y4 - Lpc4$mf[1])^2)
SStot <- sum((Y4 - mean(Y4))^2)
R2Lp4 <- 1 - SSres/SStot
#Loess.as
SSres <- sum((Y4 - Loasc4$fitted)^2)
SStot <- sum((Y4 - mean(Y4))^2)
R2Loas4 <- 1 - SSres/SStot
#locpoly
SSres <- sum((Y4 - LoLyc4$y)^2)
SStot <- sum((Y4 - mean(Y4))^2)
R2Loly4 <- 1 - SSres/SStot

#plot
fitted_loc <- Lo4c$fitted
fitted_lp <- Lpc4$mf[1]
fitted_loas <- Loasc4$fitted
fitted_Loly <- LoLyc4$y


pc3 <- ggplot() + geom_point(mapping = aes(x = X4, y = fitted_loc, color = "loess")) + 
  geom_point(mapping = aes(x = X4, y = unlist(fitted_lp), color = "locpol")) + 
  geom_point(mapping = aes(x = X4, y = fitted_loas, color = "loess.as")) + 
  geom_point(mapping = aes(x = X4, y = fitted_Loly, color = "locpoly")) + 
  scale_color_manual(name = "Regression Function",
                     values = c( "loess" = "blue", "locpol" = "red", "loess.as" = "green", "locpoly" = "purple"),
                     labels = c("loess", "locpol", "loess.as","locpoly")) + 
  labs(y = "fit") +
  ggtitle("Claw distribution, 10^3 data points") # loess is under loess.as

# 10001 

#distribution
Y5 <- dclaw(X5)
n5 <- rnorm(X5, sd = 0.1)
Y5 <- Y5 + n5

# Application
daf5 <- data.frame(X5,Y5)
Lo5c <- loess(Y5 ~ X5, data = daf5, family = "gaussian", method = "loess", span = 0.05) # Lo3c$residuals
#Lpc5 <- locpol(Y5 ~ X5, data = daf3, bw = 0.25, kernel = gaussK, deg = 2) # Lpc3$residuals
Loasc5 <- loess.as(daf5$X5, daf5$Y5, degree = 2, criterion = "aicc", family = "gaussian") # Loasc3$residuals
LoLyc5 <- locpoly(daf5$X5, daf5$Y5, degree = 2, kernel = "normal", bandwidth = 0.025, gridsize = 10001) # only gives results

# R^2 Coefficients
#Loess
SSres <- sum((Y5 - Lo5c$fitted)^2)
SStot <- sum((Y5 - mean(Y5))^2)
R2Lo5 <- 1 - SSres/SStot
#Loess.as
SSres <- sum((Y5 - Loasc5$fitted)^2)
SStot <- sum((Y5 - mean(Y5))^2)
R2Loas5 <- 1 - SSres/SStot
#locpoly
SSres <- sum((Y5 - LoLyc5$y)^2)
SStot <- sum((Y5 - mean(Y5))^2)
R2Loly5 <- 1 - SSres/SStot

#plot
fitted_loc <- Lo5c$fitted
fitted_loas <- Loasc5$fitted
fitted_Loly <- LoLyc5$y


pc4 <- ggplot() + geom_point(mapping = aes(x = X5, y = fitted_loc, color = "loess")) + 
  geom_point(mapping = aes(x = X5, y = fitted_loas, color = "loess.as")) + 
  geom_point(mapping = aes(x = X5, y = fitted_Loly, color = "locpoly")) + 
  scale_color_manual(name = "Regression Function",
                     values = c( "loess" = "blue",  "loess.as" = "green", "locpoly" = "purple"),
                     labels = c("loess", "loess.as","locpoly")) + 
  labs(y = "fit") +
  ggtitle("Claw distribution, 10^4 data points") # loess is under loess.as

# X6
# Distribution
Y6 <- dclaw(X6)
n6 <- rnorm(X6, sd = 0.1)
Y6 <- Y6 + n6

# Application
daf6 <- data.frame(X6,Y6)
Lo6c <- loess(Y6 ~ X6, data = daf6, family = "gaussian", method = "loess", span = 0.05) # Lo3c$residuals with span obtained from loess.as

Loasc6 <- loess.as(daf6$X6, daf6$Y6, degree = 2, criterion = "aicc", family = "gaussian") # Loasc3$residuals
LoLyc6 <- locpoly(daf6$X6, daf6$Y6, degree = 2, kernel = "normal", bandwidth = 0.25, gridsize = 100001) # only gives results

# R^2 Coefficients
# Loess
SSres <- sum((Y6 - Lo6c$fitted)^2)
SStot <- sum((Y6 - mean(Y5))^2)
R2Lo6 <- 1 - SSres/SStot
#loess.as
SSres <- sum((Y6 - Loasc6$fitted)^2)
SStot <- sum((Y6 - mean(Y6))^2)
R2Loas6 <- 1 - SSres/SStot
#locpoly
SSres <- sum((Y6 - LoLyc6$y)^2)
SStot <- sum((Y6 - mean(Y6))^2)
R2Loly6 <- 1 - SSres/SStot

#plot
fitted_loc <- Lo6c$fitted
#fitted_lp <- Lpc5$mf[1]
fitted_loas <- Loasc6$fitted
fitted_Loly <- LoLyc6$y


pc5 <- ggplot() + geom_point(mapping = aes(x = X6, y = fitted_loc, color = "loess")) + 
  geom_point(mapping = aes(x = X6, y = fitted_loas, color = "loess.as")) + 
  geom_point(mapping = aes(x = X6, y = fitted_Loly, color = "locpoly")) + 
  scale_color_manual(name = "Regression Function",
                     values = c( "loess" = "blue",  "loess.as" = "green", "locpoly" = "purple"),
                     labels = c("loess", "loess.as","locpoly")) + 
  labs(y = "fit") +
  ggtitle("Claw distribution, 10^5 data points") # loess is under loess.as
# at this point basically, loess does the same as loess.as, loess.as is great, locpoly underfits a lot (too big bw most likely)

# fitting into a table 

tab <- matrix(c(R2Lo3, R2Lp3, R2Loas3, R2Loly3, R2Lo4, R2Lp4, R2Loas4, R2Loly4,R2Lo5, "-", R2Loas5, R2Loly5, R2Lo6, "-", R2Loas6, R2Loly6), ncol=4, byrow=TRUE)
colnames(tab) <- c('Loess','locpol','loess.as',"locpoly")
rownames(tab) <- c('10^2','10^3','10^4',"10^5")
tab <- as.table(tab)

# plotting r^2 coefficients
locr2 <- c(R2Lo3,R2Lo4,R2Lo5,R2Lo6)
lpr2 <- c(R2Lp3,R2Lp4,0,0)
loasr2 <- c(R2Loas3,R2Loas4,R2Loas5,R2Loas6)
locyr2 <- c(R2Loly3,R2Loly4,R2Loly5,R2Loly6)
X <- c(10^2,10^3,10^4,10^5)

pr2c <- ggplot() + geom_point(mapping = aes(x = log10(X), y = locr2, color = "loess")) + 
  geom_point(mapping = aes(x = log10(X), y = lpr2, color = "locpol")) + 
  geom_point(mapping = aes(x = log10(X), y = loasr2, color = "loess.as")) + 
  geom_point(mapping = aes(x = log10(X), y = locyr2, color = "locpoly")) + 
  scale_color_manual(name = "Regression Function",
                     values = c( "loess" = "blue", "locpol" = "red", "loess.as" = "green", "locpoly" = "purple"),
                     labels = c("loess", "locpol", "loess.as","locpoly")) + 
  labs(y = "R2") +
  ggtitle("R2 coefficients applied to claw distribution, for different functions, and data size")

# medium : gaussian with sinusoidal factor or maybe generation of two different uniform and random permutation. ####
# Gaussian with sinusoidal addition and  gaussian noise 

#101:

Y3 <- dnorm(X3) # normal distribution
s3 <- 1/10*sin(4*X3) # sinusoidal noise
n3 <- rnorm(X3, sd = 0.05) # gaussian noise
Y3 <- Y3 + s3 + n3

# applying the functions:
daf3 <- data.frame(X3,Y3)
Lo3c <- loess(Y3 ~ X3, data = daf3, family = "gaussian", method = "loess", span = 0.587) # Lo3c$residuals
Lpc3 <- locpol(Y3 ~ X3, data = daf3, bw = 0.25, kernel = gaussK, deg = 2) # Lpc3$residuals
Loasc3 <- loess.as(daf3$X3, daf3$Y3, degree = 2, criterion = "aicc", family = "gaussian") # Loasc3$residuals
LoLyc3 <- locpoly(daf3$X3, daf3$Y3, degree = 2, kernel = "normal", bandwidth = 0.25, gridsize = 101) # only gives results


# R^2 coefficients
#Loess
SSres <- sum((Y3 - Lo3c$fitted)^2)
SStot <- sum((Y3 - mean(Y3))^2)
R2Lo3 <- 1 - SSres/SStot
#Locpol
SSres <- sum((Y3 - Lpc3$mf[1])^2)
SStot <- sum((Y3 - mean(Y3))^2)
R2Lp3 <- 1 - SSres/SStot
#Loess.as
SSres <- sum((Y3 - Loasc3$fitted)^2)
SStot <- sum((Y3 - mean(Y3))^2)
R2Loas3 <- 1 - SSres/SStot
#locpoly
SSres <- sum((Y3 - LoLyc3$y)^2)
SStot <- sum((Y3 - mean(Y3))^2)
R2Loly3 <- 1 - SSres/SStot

#plot
fitted_loc <- Lo3c$fitted
fitted_lp <- Lpc3$mf[1]
fitted_loas <- Loasc3$fitted
fitted_Loly <- LoLyc3$y


pc2 <- ggplot() + geom_point(mapping = aes(x = X3, y = fitted_loc, color = "loess")) + 
  geom_point(mapping = aes(x = X3, y = unlist(fitted_lp), color = "locpol")) + 
  geom_point(mapping = aes(x = X3, y = fitted_loas, color = "loess.as")) + 
  geom_point(mapping = aes(x = X3, y = fitted_Loly, color = "locpoly")) + 
  scale_color_manual(name = "Regression Function",
                     values = c( "loess" = "blue", "locpol" = "red", "loess.as" = "green", "locpoly" = "purple"),
                     labels = c("loess", "locpol", "loess.as","locpoly")) + 
  labs(y = "fit") +
  ggtitle("Claw distribution, 10^2 data points")



## 1001
#Distribution
Y4 <- dnorm(X4) # normal distribution
s4 <- 1/10*sin(4*X4) # sinusoidal noise
n4 <- rnorm(X4, sd = 0.05) # gaussian noise
Y4 <- Y4 + s4 + n4

# Application
daf4 <- data.frame(X4,Y4)
Lo4c <- loess(Y4 ~ X4, data = daf4, family = "gaussian", method = "loess", span = 0.211) # Lo3c$residuals
Lpc4 <- locpol(Y4 ~ X4, data = daf4, bw = 0.25, kernel = gaussK, deg = 2) # Lpc3$residuals
Loasc4 <- loess.as(daf4$X4, daf4$Y4, degree = 2, criterion = "aicc", family = "gaussian") # Loasc3$residuals
LoLyc4 <- locpoly(daf4$X4, daf4$Y4, degree = 2, kernel = "normal", bandwidth = 0.25, gridsize = 1001) # only gives results


# R^2 coefficients
#Loess
SSres <- sum((Y4 - Lo4c$fitted)^2)
SStot <- sum((Y4 - mean(Y4))^2)
R2Lo4 <- 1 - SSres/SStot
#Locpol
SSres <- sum((Y4 - Lpc4$mf[1])^2)
SStot <- sum((Y4 - mean(Y4))^2)
R2Lp4 <- 1 - SSres/SStot
#Loess.as
SSres <- sum((Y4 - Loasc4$fitted)^2)
SStot <- sum((Y4 - mean(Y4))^2)
R2Loas4 <- 1 - SSres/SStot
#locpoly
SSres <- sum((Y4 - LoLyc4$y)^2)
SStot <- sum((Y4 - mean(Y4))^2)
R2Loly4 <- 1 - SSres/SStot

#plot
fitted_loc <- Lo4c$fitted
fitted_lp <- Lpc4$mf[1]
fitted_loas <- Loasc4$fitted
fitted_Loly <- LoLyc4$y


pc3 <- ggplot() + geom_point(mapping = aes(x = X4, y = fitted_loc, color = "loess")) + 
  geom_point(mapping = aes(x = X4, y = unlist(fitted_lp), color = "locpol")) + 
  geom_point(mapping = aes(x = X4, y = fitted_loas, color = "loess.as")) + 
  geom_point(mapping = aes(x = X4, y = fitted_Loly, color = "locpoly")) + 
  scale_color_manual(name = "Regression Function",
                     values = c( "loess" = "blue", "locpol" = "red", "loess.as" = "green", "locpoly" = "purple"),
                     labels = c("loess", "locpol", "loess.as","locpoly")) + 
  labs(y = "fit") +
  ggtitle("Claw distribution, 10^3 data points") # loess is under loess.as

# 10001 

#distribution
Y5 <- dnorm(X5) # normal distribution
s5 <- 1/10*sin(4*X5) # sinusoidal noise
n5 <- rnorm(X5, sd = 0.05) # gaussian noise
Y5 <- Y5 + s5 + n5

# Application
daf5 <- data.frame(X5,Y5)
Lo5c <- loess(Y5 ~ X5, data = daf5, family = "gaussian", method = "loess", span = 0.05) # Lo3c$residuals
#Lpc5 <- locpol(Y5 ~ X5, data = daf3, bw = 0.25, kernel = gaussK, deg = 2) # Lpc3$residuals
Loasc5 <- loess.as(daf5$X5, daf5$Y5, degree = 2, criterion = "aicc", family = "gaussian") # Loasc3$residuals
LoLyc5 <- locpoly(daf5$X5, daf5$Y5, degree = 2, kernel = "normal", bandwidth = 0.025, gridsize = 10001) # only gives results

# R^2 Coefficients
#Loess
SSres <- sum((Y5 - Lo5c$fitted)^2)
SStot <- sum((Y5 - mean(Y5))^2)
R2Lo5 <- 1 - SSres/SStot
#Loess.as
SSres <- sum((Y5 - Loasc5$fitted)^2)
SStot <- sum((Y5 - mean(Y5))^2)
R2Loas5 <- 1 - SSres/SStot
#locpoly
SSres <- sum((Y5 - LoLyc5$y)^2)
SStot <- sum((Y5 - mean(Y5))^2)
R2Loly5 <- 1 - SSres/SStot

#plot
fitted_loc <- Lo5c$fitted
fitted_loas <- Loasc5$fitted
fitted_Loly <- LoLyc5$y


pc4 <- ggplot() + geom_point(mapping = aes(x = X5, y = fitted_loc, color = "loess")) + 
  geom_point(mapping = aes(x = X5, y = fitted_loas, color = "loess.as")) + 
  geom_point(mapping = aes(x = X5, y = fitted_Loly, color = "locpoly")) + 
  scale_color_manual(name = "Regression Function",
                     values = c( "loess" = "blue",  "loess.as" = "green", "locpoly" = "purple"),
                     labels = c("loess", "loess.as","locpoly")) + 
  labs(y = "fit") +
  ggtitle("Claw distribution, 10^4 data points") # loess is under loess.as

# X6
# Distribution
Y6 <- dnorm(X6) # normal distribution
s6 <- 1/10*sin(4*X6) # sinusoidal noise
n6 <- rnorm(X6, sd = 0.05) # gaussian noise
Y6 <- Y6 + s6 + n6

# Application
daf6 <- data.frame(X6,Y6)
Lo6c <- loess(Y6 ~ X6, data = daf6, family = "gaussian", method = "loess", span = 0.05) # Lo3c$residuals with span obtained from loess.as

Loasc6 <- loess.as(daf6$X6, daf6$Y6, degree = 2, criterion = "aicc", family = "gaussian") # Loasc3$residuals
LoLyc6 <- locpoly(daf6$X6, daf6$Y6, degree = 2, kernel = "normal", bandwidth = 0.025, gridsize = 100001) # only gives results

# R^2 Coefficients
# Loess
SSres <- sum((Y6 - Lo6c$fitted)^2)
SStot <- sum((Y6 - mean(Y5))^2)
R2Lo6 <- 1 - SSres/SStot
#loess.as
SSres <- sum((Y6 - Loasc6$fitted)^2)
SStot <- sum((Y6 - mean(Y6))^2)
R2Loas6 <- 1 - SSres/SStot
#locpoly
SSres <- sum((Y6 - LoLyc6$y)^2)
SStot <- sum((Y6 - mean(Y6))^2)
R2Loly6 <- 1 - SSres/SStot

#plot
fitted_loc <- Lo6c$fitted
#fitted_lp <- Lpc5$mf[1]
fitted_loas <- Loasc6$fitted
fitted_Loly <- LoLyc6$y


pc5 <- ggplot() + geom_point(mapping = aes(x = X6, y = fitted_loc, color = "loess")) + 
  geom_point(mapping = aes(x = X6, y = fitted_loas, color = "loess.as")) + 
  geom_point(mapping = aes(x = X6, y = fitted_Loly, color = "locpoly")) + 
  scale_color_manual(name = "Regression Function",
                     values = c( "loess" = "blue",  "loess.as" = "green", "locpoly" = "purple"),
                     labels = c("loess", "loess.as","locpoly")) + 
  labs(y = "fit") +
  ggtitle("Claw distribution, 10^5 data points") # loess is under loess.as
# at this point basically, loess does the same as loess.as, loess.as is great, locpoly underfits a lot (too big bw most likely)

# fitting into a table 

tab <- matrix(c(R2Lo3, R2Lp3, R2Loas3, R2Loly3, R2Lo4, R2Lp4, R2Loas4, R2Loly4,R2Lo5, "-", R2Loas5, R2Loly5, R2Lo6, "-", R2Loas6, R2Loly6), ncol=4, byrow=TRUE)
colnames(tab) <- c('Loess','locpol','loess.as',"locpoly")
rownames(tab) <- c('10^2','10^3','10^4',"10^5")
tab <- as.table(tab)

# plotting r^2 coefficients
locr2 <- c(R2Lo3,R2Lo4,R2Lo5,R2Lo6)
lpr2 <- c(R2Lp3,R2Lp4,0,0)
loasr2 <- c(R2Loas3,R2Loas4,R2Loas5,R2Loas6)
locyr2 <- c(R2Loly3,R2Loly4,R2Loly5,R2Loly6)
X <- c(10^2,10^3,10^4,10^5)

pr2c <- ggplot() + geom_point(mapping = aes(x = log10(X), y = locr2, color = "loess")) + 
  geom_point(mapping = aes(x = log10(X), y = lpr2, color = "locpol")) + 
  geom_point(mapping = aes(x = log10(X), y = loasr2, color = "loess.as")) + 
  geom_point(mapping = aes(x = log10(X), y = locyr2, color = "locpoly")) + 
  scale_color_manual(name = "Regression Function",
                     values = c( "loess" = "blue", "locpol" = "red", "loess.as" = "green", "locpoly" = "purple"),
                     labels = c("loess", "locpol", "loess.as","locpoly")) + 
  labs(y = "R2") +
  ggtitle("R2 coefficients applied to claw distribution, for different functions, and data size")




