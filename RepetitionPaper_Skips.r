rm(list=ls())

library(lme4)

load("data_with_skips.Rda")

# New variables
data$rpt2 = data$rpt^2
data$rpt3 = data$rpt^3

data$sentence_border[data$sentence_border=="2"] = 0

data$rpt_noncentered = data$rpt
data$freq_noncentered = data$freq
data$pfreq_noncentered = data$pfreq

# Centered variables
data$Nlaunchsite = scale(data$launchsite,scale=TRUE,center=TRUE)
data$invlength   = scale(data$invlength,center=TRUE,scale=FALSE)
data$pinvlength  = scale(data$pinvlength,center=TRUE,scale=FALSE)
data$freq = scale(data$freq,center=TRUE,scale=FALSE)
data$pfreq = scale(data$pfreq,center=TRUE,scale=FALSE)
data$rpl = scale(data$rpl,center=TRUE,scale=FALSE)
data$rpt = scale(data$rpt,center=TRUE,scale=FALSE)
data$rpt2 = scale(data$rpt2,center=TRUE,scale=FALSE)
data$rpt3 = scale(data$rpt3,center=TRUE,scale=FALSE)
data$rps = scale(data$rps,center=TRUE,scale=FALSE)
data$nREP_dist2 = scale(data$nREP_dist^2,center=TRUE,scale=FALSE)
data$nREP_dist = scale(data$nREP_dist,center=TRUE,scale=FALSE)

data$invnREP = scale(data$invnREP,center=TRUE,scale=FALSE)

##########################################################################################################
# Supplementary Table 4: Repetition models for other measures
# First Fixation Durations (FFD), Number of Fixations in First Pass (NFFP), Regression Path Duration (RPD) and Re-Reading Time (RRT).
##########################################################################################################
system.time(SKIPS<-glmer(skip ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                              rpl + rpt + rps + sentence_border +
                              invnREP + invnREP:freq +
                              (1|sujid) + (1|textid) + (1|wordid), data = data, family=binomial(link="logit")))
print(summary(SKIPS),cor=F)

ss <- getME(SKIPS,c("theta","fixef"))
system.time(SKIPSit <- update(SKIPS,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4))))
print(summary(SKIPSit),cor=F)
# cSKIPSit<-confint(SKIPSit)