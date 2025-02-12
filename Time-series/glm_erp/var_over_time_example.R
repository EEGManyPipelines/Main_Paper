# This simulates data sampled from two independent sources of random variation (i.e,
# subject variation and group variation), then do a linear mixed-effect model to 
# test if the random variance can be adequately estimated. First on single value 
# data, then on a "time series ERP" consisting of a half-sine wave that is scaled
# with the random effect.

library(RobPer)
library(lme4)

# Sim parameters
n.sim      <- 1
n.subj     <-33
n.grp      <- 100
n.t        <- 200

# Variation parameters
sigma.subj <- 5
sigma.grp  <- 3
sigma.err  <- 1

dat.0 <- rep(0, n.t)
erp.0   <- sin(seq(0, pi, length.out=100))

x.subj <- rnorm(n.subj, 10, sigma.subj)
x.grp  <- rnorm(n.grp, 0, sigma.grp)

# scalar data
sub.dat <- data.frame(1:n.subj, x.subj)
names(sub.dat) <- c("subj", "x")
sub.dat$subj <- as.factor(sub.dat$subj)

all.dat <- data.frame()
for (ii in 1:n.grp){
  tmp.dat <- sub.dat
  tmp.dat$x <- tmp.dat$x + x.grp[ii]
  tmp.dat$grp <- as.factor(ii)
  all.dat <- rbind(all.dat,tmp.dat)
}

all.dat$x <- all.dat$x + rnorm(length(all.dat$x), 0, sigma.err)  # Model need noise to converge or residual is too small

mod.rx <- lmer(x ~ 1 + (1|subj) + (1|grp), data=all.dat, REML=FALSE)
summary(mod.rx)


################################################################################
# Time series data

all.t.dat <- matrix(nrow=0, ncol=n.t)
idx.tmp <- data.frame(1:n.subj)
names(idx.tmp) <- c("subj")
idx.tmp$subj <- as.factor(idx.tmp$subj)
idx.dat <- data.frame()
for (gg in 1:n.grp){
  sub.dat <- t(replicate(n.subj, dat.0))
  for (ss in 1:n.subj){
    sub.dat[ss, 50:149] <- (x.grp[gg] + x.subj[ss]) * erp.0
    noise <- TK95(n.t, 1)
    noise <- noise/max(noise) * sigma.err
    sub.dat[ss,] <- sub.dat[ss,] + noise
  }
  all.t.dat <- rbind(all.t.dat, sub.dat)
  idx.tmp$grp <- as.factor(gg)
  idx.dat <- rbind(idx.dat, idx.tmp)
}

t.thet.grp <- rep(0, n.t)
t.thet.subj <- rep(0, n.t)
t.thet.res  <- rep(0, n.t)

for (tt in 1:n.t){
  y <- all.t.dat[,tt]
  idx.dat$y <- y
  
  tmp.mod <- lmer(y ~ 1 + (1|subj) + (1|grp), data=idx.dat, REML=FALSE)
  
  smry <- as.data.frame(VarCorr(tmp.mod, which="theta_"))
  
  t.thet.grp[tt] <- smry$sdcor[1]
  t.thet.subj[tt] <- smry$sdcor[2]
  t.thet.res[tt] <- smry$sdcor[3]
}

plot(t.thet.subj, type="l", col="blue", ylab = "Theta", ylim = c(-1, max(sigma.subj, sigma.err, sigma.grp)+1))
lines(t.thet.grp, col="red")
lines(t.thet.res,)
points(100, sigma.subj, col="blue")
points(100, sigma.grp, col="red")
legend(1, 6, legend=c("Subj", "Grp", "Resid"),
       col=c("red", "blue", "black"), lty=1, cex=0.8)

# Add some "outlier" teams
x.grp.new <- x.grp
all.t.dat <- matrix(nrow=0, ncol=n.t)
idx.tmp <- data.frame(1:n.subj)
names(idx.tmp) <- c("subj")
idx.tmp$subj <- as.factor(idx.tmp$subj)
idx.dat <- data.frame()
for (gg in 1:n.grp){
  sub.dat <- t(replicate(n.subj, dat.0))
  if (gg%%10==0){
    x.grp.new[ss] = x.grp.new[ss] + 10
  }
  for (ss in 1:n.subj){
    sub.dat[ss, 50:149] <- (x.grp[gg] + x.subj[ss]) * erp.0
    noise <- TK95(n.t, 1)
    noise <- noise/max(noise) * sigma.err
    sub.dat[ss,] <- sub.dat[ss,] + noise
  }
  all.t.dat <- rbind(all.t.dat, sub.dat)
  idx.tmp$grp <- as.factor(gg)
  idx.dat <- rbind(idx.dat, idx.tmp)
}

for (tt in 1:n.t){
  y <- all.t.dat[,tt]
  idx.dat$y <- y
  
  tmp.mod <- lmer(y ~ 1 + (1|subj) + (1|grp), data=idx.dat, REML=FALSE)
  
  as.data.frame(VarCorr(tmp.mod))
  smry <- as.data.frame(VarCorr(tmp.mod, which="theta_"))
  
  t.thet.grp[tt] <- smry$sdcor[1]
  t.thet.subj[tt] <- smry$sdcor[2]
  t.thet.res[tt] <- smry$sdcor[3]
}

plot(t.thet.subj, type="l", col="blue", ylab = "Theta")
lines(t.thet.grp, col="red")
lines(t.thet.res,)
ylim = c(-1, max(sigma.subj, sigma.err, sigma.grp)+1)

