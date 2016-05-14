# the data come from the bayesm package
library(bayesm)
data(Scotch)

library(NMF)
fit<-nmf(Scotch, 20, "lee", nrun=20)
coefmap(fit)
basismap(fit)

fit<-nmf(Scotch, 5, "lee", nrun=20)
basismap(fit)
coefmap(fit)

# code for sorting and printing
# the two factor matrices
h<-coef(fit)
library(psych)
fa.sort(t(round(h,3)))
w<-basis(fit)
wp<-w/apply(w,1,sum)
fa.sort(round(wp,3))

# hard clustering
type<-max.col(w)
table(type)
t(aggregate(Scotch, by=list(type), FUN=mean))