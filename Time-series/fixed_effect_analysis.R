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
qdat$software[qdat$software=="custom"] <- "other"

qdat$ica <- as.factor(ifelse(qdat$ans_ica_algo=="", "no", "yes"))

qdat$reref <- as.factor(qdat$reref)
qdat$reref[qdat$reref=="unknown" | qdat$reref=="other"] <- NaN

qdat$ans_ica_algo <- as.factor(qdat$ans_ica_algo)
qdat$result_h1 <- as.factor(qdat$result_h1)
qdat$software <- as.factor(qdat$software)

# Prepare data
idx.dat <- data.frame(rep(qdat$Team_name, n.subj), rep(1:n.subj, each=n.grp), rep(qdat$ica, n.subj), rep(qdat$reref, n.subj),
                      rep(qdat$software, n.subj), rep(qdat$result_h1))

names(idx.dat) <- c("grp", "subj", "ica", "reref", "software", "result")
idx.dat$grp <- as.factor(idx.dat$grp)
idx.dat$subj <- as.factor(idx.dat$subj)

# Regression output
dat.beta.ica  <- array(dim=c(n.elec, n.t, 2))    # Effect of ICA
dat.beta.ref  <- array(dim=c(n.elec, n.t, 3))    # Effect of re-reference
dat.beta.hp   <- array(dim=c(n.elec, n.t))       # Effect of HP sauce
dat.thet.grp  <- array(dim=c(n.elec, n.t))
dat.thet.subj <- array(dim=c(n.elec, n.t))
dat.thet.res  <- array(dim=c(n.elec, n.t))
dat.t.ica     <- array(dim=c(n.elec, n.t))

# Run
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
    dat.beta.ica[jj,tt,2]  <- ffx[2]          # yes ICA
    dat.beta.ref[jj,tt,1]  <- ffx[1]          # Ref = AVG
    dat.beta.ref[jj,tt,2]  <- ffx[3]          # Ref = mastoid
    dat.beta.ref[jj,tt,3]  <- ffx[4]          # Ref = original
    
    dat.thet.subj[jj,tt] <- rfx[rfx$grp=="grp",]$sdcor
    dat.thet.grp[jj,tt]  <- rfx[rfx$grp=="subj",]$sdcor
    dat.thet.res[jj,tt]  <- rfx[rfx$grp=="Residual",]$sdcor
    
    dat.t.ica[jj,tt] <- coef(summary(tmp.mod))[2,3]
  }
}

save(dat.beta.ica, dat.beta.ref, 
     dat.thet.grp, dat.thet.res, dat.thet.subj,
     dat.t.ica, 
     file='C:\\Users\\ncb623\\EMP\\data\\fxmod.Rdata')
writeMat('C:\\Users\\ncb623\\EMP\\data\\fxmod.mat', 
         beta_ica=dat.beta.ica,
         beta_ref=dat.beta.ref,
         thet_sub=dat.thet.subj,  
         thet_grp=dat.thet.grp, 
         thet_resid=dat.thet.res,
         t_ica=dat.t.ica)

################################################################################
## MODEL WITH SOFTWARE
dat.beta.tool  <- array(dim=c(n.elec, n.t, 5))    # Effect of software
dat.beta.ref  <- array(dim=c(n.elec, n.t, 3))    # Effect of re-reference
dat.thet.grp  <- array(dim=c(n.elec, n.t))
dat.thet.subj <- array(dim=c(n.elec, n.t))
dat.thet.res  <- array(dim=c(n.elec, n.t))

# Run
for (tt in 1:n.t){
  cat("t =", tt, "\n")
  for (jj in 1:n.elec){
    y <- c(alldat[,,jj,tt])
    idx.dat$y <- y
    
    tmp.mod <- lmer(y ~ 0 + software + reref + (1|subj) + (1|grp), 
                    data=idx.dat, REML=FALSE)
    
    rfx <- as.data.frame(VarCorr(tmp.mod, which="theta_"))
    ffx <- fixef(tmp.mod)
    
    dat.beta.tool[jj,tt,1]  <- ffx[1]          # BrainVision
    dat.beta.tool[jj,tt,2]  <- ffx[2]          # EEGlab
    dat.beta.tool[jj,tt,3]  <- ffx[3]          # FieldTrip
    dat.beta.tool[jj,tt,4]  <- ffx[4]          # Other
    dat.beta.tool[jj,tt,5]  <- ffx[5]          # SPM

    
    dat.beta.ref[jj,tt,1]  <- ffx[1]          # Ref = AVG
    dat.beta.ref[jj,tt,2]  <- ffx[6]          # Ref = mastoid
    dat.beta.ref[jj,tt,3]  <- ffx[7]          # Ref = original
    
    dat.thet.subj[jj,tt] <- rfx[rfx$grp=="grp",]$sdcor
    dat.thet.grp[jj,tt]  <- rfx[rfx$grp=="subj",]$sdcor
    dat.thet.res[jj,tt]  <- rfx[rfx$grp=="Residual",]$sdcor
  }
}

save(dat.beta.tool, dat.beta.ref,
     dat.thet.grp, dat.thet.res, dat.thet.subj,
     file='C:\\Users\\ncb623\\EMP\\data\\fxmod_software.Rdata')
writeMat('C:\\Users\\ncb623\\EMP\\data\\fxmod_software.mat',
         beta_tool=dat.beta.tool,
         beta_ref=dat.beta.ref,
         thet_sub=dat.thet.subj,
         thet_grp=dat.thet.grp,
         thet_resid=dat.thet.res)

################################################################################
## MODEL WITH CONFIRM/REHECT H1
dat.beta.H1  <- array(dim=c(n.elec, n.t, 2))     # Effect of hypothesis testing
dat.beta.ref  <- array(dim=c(n.elec, n.t, 3))    # Effect of re-reference
dat.thet.grp  <- array(dim=c(n.elec, n.t))
dat.thet.subj <- array(dim=c(n.elec, n.t))
dat.thet.res  <- array(dim=c(n.elec, n.t))
dat.t.H1     <- array(dim=c(n.elec, n.t))

# Run
for (tt in 1:n.t){
  cat("t =", tt, "\n")
  for (jj in 1:n.elec){
    y <- c(alldat[,,jj,tt])
    idx.dat$y <- y
    
    tmp.mod <- lmer(y ~ 1 + result + reref + (1|subj) + (1|grp), 
                    data=idx.dat, REML=FALSE)
    
    rfx <- as.data.frame(VarCorr(tmp.mod, which="theta_"))
    ffx <- fixef(tmp.mod)
    
    dat.beta.H1[jj,tt,1]  <- ffx[1]          # FALSE
    dat.beta.H1[jj,tt,2]  <- ffx[2]          # TRUE
    dat.beta.ref[jj,tt,1]  <- ffx[1]          # Ref = AVG
    dat.beta.ref[jj,tt,2]  <- ffx[3]          # Ref = mastoid
    dat.beta.ref[jj,tt,3]  <- ffx[4]          # Ref = original
    
    dat.thet.subj[jj,tt] <- rfx[rfx$grp=="grp",]$sdcor
    dat.thet.grp[jj,tt]  <- rfx[rfx$grp=="subj",]$sdcor
    dat.thet.res[jj,tt]  <- rfx[rfx$grp=="Residual",]$sdcor
    
    dat.t.H1[jj,tt] <- coef(summary(tmp.mod))[2,3]
    
  }
}

save(dat.beta.tool, dat.beta.ref,
     dat.thet.grp, dat.thet.res, dat.thet.subj,
     dat.t.H1,
     file='C:\\Users\\ncb623\\EMP\\data\\fxmod_H1.Rdata')
writeMat('C:\\Users\\ncb623\\EMP\\data\\fxmod_H1.mat',
         beta_H1=dat.beta.H1,
         beta_ref=dat.beta.ref,
         thet_sub=dat.thet.subj,
         thet_grp=dat.thet.grp,
         thet_resid=dat.thet.res,
         t_H1=dat.t.H1)

#END