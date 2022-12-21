### to examine the code of a function :
install.packages("locpol")
body(locpol)
body(loess) # + lowess
# https://subscription.packtpub.com/book/data/9781788627306/1/ch01lvl1sec18/r-packages-for-regression

#caret 
install.packages("caret")
body(caret::train)
body(caret::knnreg)
#knnreg ?
#( package lpridge  -> lpepa, lpridge)
# package locfit -> locfit
# package locpol -> locpol
### gam package, stl2, operator

# loess.as{fANCOVA}
### Plan: find datasets of different sizes, noise them or generate them (powers of 10)
### apply loess, locpol, locfit, time it. Check performance. maybe knn ?
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/loess.html  https://search.r-project.org/CRAN/refmans/locfit/html/locfit.raw.html

