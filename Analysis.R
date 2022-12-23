set.seed(1315)
### oackages and libraries ####
install.packages("locpol")
install.packages("fANCOVA")
install.packages("KernSmooth")
library(locpol)
library(fANCOVA)
library(KernSmooth)
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

rm(X3,X4,X5,X6,X7,d3,d4,d5,d6,d7)
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
# medium : evenly spaced uniform ? or maybe generation of two different uniform and random permutation. ####

# hard ? mixture of gaussian/laplace distributions. not uniformly spaced; different lateral densities ####
