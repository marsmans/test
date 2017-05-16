

i.seed<-23846
set.seed(i.seed)

N.samp<-100; X.lhs.samp<-randomLHS(N.samp, 4)

plot(X.lhs.samp)

X.rnd.samp<-cbind(runif(N.samp),runif(N.samp),runif(N.samp),runif(N.samp))
plot(X.rnd.samp)

