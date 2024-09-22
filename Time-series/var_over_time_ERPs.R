# Test regression approach on subset of real ERP data.

library(R.matlab)
library(lme4)

dat <- readMat('C:\\Users\\ncb623\\EMP\\data\\Standardized\\alldatmat.mat')
dat <- dat$alldatmat

n.grp <- dim(dat)[1]
n.subj <-  dim(dat)[2]
n.elec <- dim(dat)[3]
n.t <- dim(dat)[4]

# Init.
t.thet.grp <- rep(0, n.t)
t.thet.subj <- rep(0, n.t)
t.thet.res  <- rep(0, n.t)


dat.thet.grp <- matrix(0, n.elec, n.t)
dat.thet.subj <- matrix(0, n.elec, n.t)
dat.thet.res  <- matrix(0, n.elec, n.t)

idx.dat <- data.frame(rep(1:n.grp, n.subj), rep(1:n.subj, each=n.grp))
names(idx.dat) <- c("grp", "subj")
idx.dat$grp <- as.factor(idx.dat$grp)
idx.dat$subj <- as.factor(idx.dat$subj)

for (tt in 1:n.t){
  cat("t =", tt, "\n")
  for (jj in 1:n.elec){
    y <- c(dat[,,jj,tt])
    idx.dat$y <- y
    
    tmp.mod <- lmer(y ~ 1 + (1|subj) + (1|grp), data=idx.dat, REML=FALSE)
    smry <- as.data.frame(VarCorr(tmp.mod, which="theta_"))
    
    dat.thet.subj[jj,tt] <- smry$sdcor[1]
    dat.thet.grp[jj,tt] <- smry$sdcor[2]
    dat.thet.res[jj,tt] <- smry$sdcor[3]
  }
}

writeMat('C:\\Users\\ncb623\\EMP\\data\\Standardized\\thetadat.mat', 
         subj=dat.thet.subj,  
         grp=dat.thet.grp, 
         resid=dat.thet.res)
