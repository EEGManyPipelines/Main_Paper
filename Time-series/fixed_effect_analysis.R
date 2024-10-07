## The fixed effect analysis
# Effect to include:
# * Re-reference (as co-variate - most likely a large effect)  [reref]
# * ICA (either binary yes/no or grouped by algorithm incl. none) [ans_ica_algo]
# * HP filter, continuous variable or dummy coded 0 [hf_cutoff]

library(R.matlab)
library(lme4)

## Get ERP data
tmp <- readMat('C:\\Users\\ncb623\\EMP\\data\\alldatmat.mat')
alldat <- tmp$alldatmat
  
n.grp  <- dim(alldat)[1]
n.subj <- dim(alldat)[2]
n.elec <- dim(alldat)[3]
n.t    <- dim(alldat)[4]

## Get questionnaire data and arrange
qdat  <- read.csv('C:\\Users\\ncb623\\EMP\\data\\all_var_AQ_h1.csv')
iddat <- read.csv('C:\\Users\\ncb623\\EMP\\data\\IDs_linear_models.csv')
gdat  <- read.csv('C:\\Users\\ncb623\\EMP\\data\\grptab.csv')
gdat$Var1 <- as.factor(gdat$Var1)

qdat$Team_name <- as.factor(iddat$teamID)
qdat <- qdat[qdat$Team_name %in% gdat$Var1, ]
qdat$order <- match(qdat$Team_name, gdat$Var1)
qdat <- qdat[order(qdat$order),]

qdat$software[qdat$software=="eeglab_erplab" | qdat$software=="eeglab_limo"] <- "eeglab"
qdat$ica <- as.factor(ifelse(qdat$ans_ica_algo=="", "no", "yes"))

qdat$reref <- as.factor(qdat$reref)
qdat$reref[qdat$reref=="unknown" | qdat$reref=="other"] <- NaN

qdat$ans_ica_algo <- as.factor(qdat$ans_ica_algo)
qdat$result_h1 <- as.factor(qdat$result_h1)
qdat$software <- as.factor(qdat$software)


# Regression
dat.beta.ica  <- array(dim=c(n.elec, n.t, 2))    # Effect of ICA
dat.beta.ref  <- array(dim=c(n.elec, n.t, 3))    # Effect of re-reference
dat.beta.hp   <- array(dim=c(n.elec, n.t))       # Effect of HP sauce
dat.thet.grp  <- array(dim=c(n.elec, n.t))
dat.thet.subj <- array(dim=c(n.elec, n.t))
dat.thet.res  <- array(dim=c(n.elec, n.t))


###
idx.dat <- data.frame(rep(qdat$Team_name, n.subj), rep(1:n.subj, each=n.grp), rep(qdat$ica, n.subj), rep(qdat$reref, n.subj))

names(idx.dat) <- c("grp", "subj", "ica", "reref")
idx.dat$grp <- as.factor(idx.dat$grp)
idx.dat$subj <- as.factor(idx.dat$subj)

###

for (tt in 1:n.t){
  cat("t =", tt, "\n")
  for (jj in 1:n.elec){
    y <- c(alldat[,,jj,tt])
    idx.dat$y <- y
    
    tmp.mod <- lmer(y ~ 1 + ica + reref + (1|subj) + (1|grp), 
                    data=idx.dat, REML=FALSE)
    
    rfx <- as.data.frame(VarCorr(tmp.mod, which="theta_"))
    ffx <- fixef(tmp.mod)
    
    dat.beta.ica[jj,tt,1]  <- ffx[1]          # No ICA
    dat.beta.ica[jj,tt,2]  <- ffx[1]+ffx[2]   # yes ICA
    dat.beta.ref[jj,tt,1]  <- ffx[1]          # Ref = AVG
    dat.beta.ref[jj,tt,2]  <- ffx[1]+ffx[3]   # Ref = mastoid
    dat.beta.ref[jj,tt,3]  <- ffx[1]+ffx[4]   # Ref = original
    
    dat.thet.subj[jj,tt] <- rfx[rfx$grp=="grp",]$sdcor
    dat.thet.grp[jj,tt]  <- rfx[rfx$grp=="subj",]$sdcor
    dat.thet.res[jj,tt]  <- rfx[rfx$grp=="Residual",]$sdcor
  }
}

save(dat.beta.ica, dat.beta.ref, dat.thet.grp, dat.thet.res, dat.thet.subj,
     file='C:\\Users\\ncb623\\EMP\\data\\fxmod.Rdata')
writeMat('C:\\Users\\ncb623\\EMP\\data\\fxmod.mat', 
         beta_ica=dat.beta.ica,
         beta_ref=dat.beta.ref,
         thet_sub=dat.thet.subj,  
         thet_grp=dat.thet.grp, 
         thet_resid=dat.thet.res)

#END