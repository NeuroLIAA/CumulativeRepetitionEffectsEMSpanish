rm(list=ls())

library(lme4)

load("data_without_skips.Rda")
summary(data)

# New variables
data$rpt2 = data$rpt^2
data$rpt3 = data$rpt^3

data$sentence_border[data$sentence_border=="2"] = 0

data$rpt_noncentered = data$rpt
data$freq_noncentered = data$freq
data$pfreq_noncentered = data$pfreq

# Center variables
data$Nlaunchsite<-scale(data$launchsite,scale=TRUE,center=TRUE)

data$invlength = scale(data$invlength,center=TRUE,scale=FALSE)
data$pinvlength = scale(data$pinvlength,center=TRUE,scale=FALSE)
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
# Table 1: Repetition effect
##########################################################################################################
BaselineModel<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                      rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                      (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(BaselineModel),cor=F)
# cBaselineModel<-confint(BaselineModel)

SimpleRepetitionModel<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                      rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                      invnREP +
                      (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(SimpleRepetitionModel),cor=F)
# cSimpleRepetitionModel<-confint(SimpleRepetitionModel)

FullRepetitionModel<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                              rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                              invnREP + invnREP:freq +
                              (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(FullRepetitionModel),cor=F)
# cFullRepetitionModel<-confint(FullRepetitionModel)

anova(BaselineModel,SimpleRepetitionModel,FullRepetitionModel)

##########################################################################################################
# Linear versus Inverse Repetition
##########################################################################################################
SimpleLinearRepetitionModel<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                           rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                           nREP + 
                           (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(SimpleLinearRepetitionModel),cor=F)
# cSimpleLinearRepetitionModel<-confint(SimpleLinearRepetitionModel)

anova(BaselineModel,SimpleLinearRepetitionModel,SimpleRepetitionModel)

##########################################################################################################
# Local versus Global Repetition
##########################################################################################################
filtro = data$ordenlectura>1 & data$textid!=5
GlobalRepetitionModel<-lmer(logFPRT ~Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                             rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                             invnREP_global + 
                             (1|sujid) + (1|textid) + (1|wordid), data = data[filtro,])
# cGlobalRepetitionModel<-confint(GlobalRepetitionModel)

SimpleRepetitionSmallModel <-lmer(logFPRT ~Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                       rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                       invnREP + 
                       (1|sujid) + (1|textid) + (1|wordid), data = data[filtro,])
# cSimpleRepetitionSmallModel<-confint(SimpleRepetitionSmallModel)

print(summary(GlobalRepetitionModel),cor=F)
print(summary(SimpleRepetitionSmallModel),cor=F)
anova(GlobalRepetitionModel,SimpleRepetitionSmallModel)

##########################################################################################################
# Repetition of Lemma (full data)
##########################################################################################################
LemmaRepetitionModel<-lmer(logFPRT ~  Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                      rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                      invlemanREP + 
                      (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(LemmaRepetitionModel),cor=F)
# cLemmaRepetitionModel<-confint(LemmaRepetitionModel,"invlemanREP")

##########################################################################################################
# Repetition of Lemma (reduced dataset)
##########################################################################################################
## Case 1: NREP = 1 & NREP LEMMA = 1
## Case 2: NREP = 1 & NREP LEMMA = 2
## Case 3: NREP = 2 & NREP LEMMA = 2
## Case 4: NREP = 2 & NREP LEMMA = 1
t<-matrix(0,nrow(data),1)
t[data$nREP_notcentered==1 & data$lemanREP==1,]<-1 # LW1: First appearence of a given word or lemma
t[data$nREP_notcentered==1 & data$lemanREP==2,]<-2 # L2: Repetition of lemma (but not word form)
t[data$nREP_notcentered==2 & data$lemanREP==2,]<-3 # W2: Repetition of word

dlemma <- data
dlemma$lemmacat <- t
dlemma <- dlemma[dlemma$lemmacat>0,]

dlemma$lemmacat <- as.factor(dlemma$lemmacat)
(contrasts(dlemma$lemmacat) <- contr.treatment(n=3,base=1))

## lemmacat: L2 - LW1
## lemmacat: W2 - LW1
LemmaCatRepetitionModel<-lmer(logFPRT ~  Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                               rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                               lemmacat + 
                               (1|sujid) + (1|textid) + (1|wordid), data = dlemma)
print(summary(LemmaCatRepetitionModel),cor=F)
# cLemmaCatRepetitionModel<-confint(LemmaCatRepetitionModel,"lemmacat")
rm("dlemma")

##########################################################################################################
# Distance effect
##########################################################################################################
filtro = data$nREP_notcentered>1
ddist <- data[filtro,]
ddist$NnREP_dist  = scale(ddist$nREP_dist,scale=TRUE,center=TRUE)
DistanceRepetitionLargeModel<-lmer(logFPRT ~  Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                          rpl +rpt + rpt2 + rpt3 + rps + sentence_border +
                          invnREP + NnREP_dist + invnREP:NnREP_dist +
                          (1|sujid) + (1|textid) + (1|wordid), data = ddist)
print(summary(DistanceRepetitionLargeModel),cor=F)
# cDistanceRepetitionLargeModel<-confint(DistanceRepetitionLargeModel,"NnREP_dist","invnREP:NnREP_dist")

ddist$NnREP_dist2  = scale(ddist$nREP_dist2,scale=TRUE,center=TRUE)
QuadraticDistanceRepetitionLargeModel<-lmer(logFPRT ~  Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                                     rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                                     invnREP + NnREP_dist + NnREP_dist2 + 
                                     (1|sujid) + (1|textid) + (1|wordid), data = ddist)
print(summary(QuadraticDistanceRepetitionLargeModel),cor=F)

filtro = data$nREP_notcentered>1 & data$nREP_notcentered<3
ddist <- data[filtro,]
ddist$NnREP_dist  = scale(ddist$nREP_dist,scale=TRUE,center=TRUE)
DistanceRepetitionSmallModel<-lmer(logFPRT ~  Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                                     rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                                     NnREP_dist + 
                                     (1|sujid) + (1|textid) + (1|wordid), data = ddist)
print(summary(DistanceRepetitionSmallModel),cor=F)
# cDistanceRepetitionSmallModel<-confint(DistanceRepetitionSmallModel,"NnREP_dist")
rm("ddist")

##########################################################################################################
# Supplementary Table 3: Discrete Frequency Repetition Model
##########################################################################################################
data$freqcat<-matrix(0,nrow(data$freq),ncol(data$freq))
q=quantile(data$freq,probs=seq(0,1,1/10))
xfreq = rep(0,1,length(q)-1)
for (i in 2:length(q)) {
  data$freqcat[data$freq>=q[i-1] & data$freq<=q[i],]=i-1
  xfreq[i-1] = median(data$freq_noncentered[data$freqcat==i-1])
}
data$freqcat = as.factor(data$freqcat)

DiscreteFrequencyRepetitionModel<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                               rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                               freqcat:invnREP +
                               (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(DiscreteFrequencyRepetitionModel),cor=F)
# cDiscreteFrequencyRepetitionModel<-confint(DiscreteFrequencyRepetitionModel)

##########################################################################################################
# Figure 1: Interaction of repetition number with frequence
##########################################################################################################
library(ggplot2)
library(Rmisc)
library(Hmisc)
axfsiz = 12
tifsiz = 16
lefsiz = 16
pafsiz = 8

# Panel A)
data$Y = data$FPRT
nrepmax = 5
temp = data$nREP_notcentered
temp[temp>nrepmax] = nrepmax
data$X = temp
data$X[data$X==nrepmax] = nrepmax+1

data$freqcat_binary = array(NA,dim=length(data$freqcat))
data$freqcat_binary[is.element(data$freqcat,c(8,9,10))] = 1
data$freqcat_binary[is.element(data$freqcat,c(1,2,3))] = 2

df <- data[!is.na(data$freqcat_binary),]
df$Frequency <- df$freqcat_binary
df$Frequency[df$Frequency==1] <- "Low Frequency"
df$Frequency[df$Frequency==2] <- "High Frequency"

ajuste <- ddply(df, .(Frequency), function(x) {coef(lm(Y ~ 1 + X, data=x))})

p0 <- ggplot(df, aes(x=X, y=Y)) + 
  stat_summary(fun.data="mean_cl_normal", geom="pointrange", position=position_dodge(width=0.025), na.rm=TRUE, aes(colour = Frequency) ) +
  geom_abline(data=ajuste, aes(slope=X, intercept=`(Intercept)`, color=factor(Frequency))) + 
  theme_bw() +
  xlab("NREP") +
  ylab("Gaze Duration (ms)") +  
  scale_colour_grey(start = 0, end = 0.5, na.value = "red") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=axfsiz,color="black")) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size=axfsiz,color="black")) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90, vjust=1)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00, vjust=-0.5)) +
  scale_x_continuous(breaks=c(1,2,3,4,nrepmax+1),labels=c("1", "2", "3", "4", ">5"),limits=c(0.5,6.5)) +
  scale_y_continuous(breaks=seq(225,355,25)) +
  theme(legend.position=c(.75, .85)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=lefsiz,color="black"))+
  annotate("text", x = 0.5, y = 335, label = "a)", size=pafsiz)

data$X = 1/data$nREP_notcentered
ajuste1 <- ddply(data, .(freqcat), function(x) {coef(lm(Y ~ 1 + X, data=x))})
ajuste1_se <- ddply(data, .(freqcat), function(x) {coef(summary(lm(Y ~ 1 + X, data=x)))[,2]})

p1 <- ggplot(data, aes(x=X, y=Y)) + 
  stat_summary(fun.data="mean_cl_normal", geom="pointrange", position=position_dodge(width=0.025), aes(colour = factor(freqcat)) ) +
  geom_abline(data=ajuste1, aes(slope=X, intercept=`(Intercept)`, color=factor(freqcat))) + 
  theme_bw() +
  xlab("1/NREP") +
  ylab("Gaze Duration [ms]") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=axfsiz,color="black")) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size=axfsiz,color="black")) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90, vjust=1)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00, vjust=-0.5)) +
  theme(legend.position="none") +
  scale_colour_grey(start = 0, end = 0.8, na.value = "red") +
  scale_x_continuous(breaks=c(0.2,0.25,0.33,0.5,1),labels=c("1/5", "1/4", "1/3", "1/2", "1"), limits=c(0.19,1.05) ) +
  annotate("text", x = 0.2, y = 370, label = "b)", size=pafsiz)

fe <- ajuste1[,3]
se <- ajuste1_se[,3]
df <- data.frame(fe,se)
p2 <- ggplot(df, aes(x=1:10, y=fe)) + 
  geom_errorbar(aes(ymin=fe-se, ymax=fe+se), width=.25, size=1) +
  geom_line(size=1) +
  geom_point(shape=21, fill="white", size=1) +
  xlab("Frequency Decile") +
  ylab("Slope [ms]") + 
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=axfsiz,color="black")) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size=axfsiz,color="black")) +
  theme(axis.title.y = element_text(size = rel(1), angle = 90, vjust=1)) +
  theme(axis.title.x = element_text(size = rel(1), angle = 00, vjust=-0.5)) +
  scale_x_continuous( breaks=seq(1,10,1), limits=c(0,10)) +
  scale_y_continuous( breaks=seq(0,100,50)) +
  geom_hline(yintercept=0) +
  annotate("text", x = 0.2, y = 130, label = "c)", size=pafsiz*0.8)

fe <- ajuste1[,2]
se <- ajuste1_se[,2]
df <- data.frame(fe,se)
p3 <- ggplot(df, aes(x=1:10, y=fe)) + 
  geom_errorbar(aes(ymin=fe-se, ymax=fe+se), width=.25, size=1) +
  geom_line(size=1) +
  geom_point(shape=21, fill="white", size=1) +
  xlab("Frequency Decile") +
  ylab("Intercept [ms]") +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=axfsiz,color="black")) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size=axfsiz,color="black")) +
  theme(axis.title.y = element_text(size = rel(1), angle = 90, vjust=1)) +
  theme(axis.title.x = element_text(size = rel(1), angle = 00, vjust=-0.5)) +
  scale_x_continuous( breaks=seq(1,10,1), limits=c(0,10)) +
  scale_y_continuous( breaks=seq(150,350,100), limits=c(150,350)) +
  annotate("text", x = 0.2, y = 325, label = "d)", size=pafsiz*0.8)


layout <- matrix(c(1, 1, 1, 1, 1, 1, 
                   2, 2, 3, 2, 2, 4), nrow = 4, byrow = TRUE)

pdf("Figure1_NREP.pdf")
multiplot(p0, p1, p2, p3, layout = layout)
dev.off()

##########################################################################################################
# Reduced datasets to constrain the interaction between frequency and repetition
##########################################################################################################
data2 = data[data$nREP_tot>=2 & data$nREP_notcentered<=2 & data$freq<quantile(data$freq,probs=0.5),]
IntFrequencyRepetition_ReducedModel0<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                      rpl +rpt + rpt2 + rpt3 + rps + sentence_border +
                      invnREP*freq + 
                      (1|sujid) + (1|textid) + (1|wordid), data = data2)
print(summary(IntFrequencyRepetition_ReducedModel0),cor=F)
# cIntFrequencyRepetition_ReducedModel0<-confint(IntFrequencyRepetition_ReducedModel0,"invnREP:freq")

##########################################################################################################
# Interaction between frequency and repetition of the previous word
##########################################################################################################
PreviousFrequencyRepetition_AllModel<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                        rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                        invnREP_Prev*pfreq +
                        (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(PreviousFrequencyRepetition_AllModel),cor=F)
# cPreviousFrequencyRepetition_AllModel<-confint(PreviousFrequencyRepetition_AllModel,"pfreq:invnREP_Prev")

data2 = data[data$lexical_status_Prev==TRUE,]
PreviousFrequencyRepetition_ContentModel<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                            rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                            invnREP_Prev*pfreq +
                            (1|sujid) + (1|textid) + (1|wordid), data = data2)
print(summary(PreviousFrequencyRepetition_ContentModel),cor=F)
# cPreviousFrequencyRepetition_ContentModel<-confint(PreviousFrequencyRepetition_ContentModel,"pfreq:invnREP_Prev")

data2 = data[data$lexical_status_Prev==FALSE,]
PreviousFrequencyRepetition_FunctionModel<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                             rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                             invnREP_Prev*pfreq +
                             (1|sujid) + (1|textid) + (1|wordid), data = data2)
print(summary(PreviousFrequencyRepetition_FunctionModel),cor=F)
# cPreviousFrequencyRepetition_FunctionModel<-confint(PreviousFrequencyRepetition_FunctionModel,"pfreq:invnREP_Prev")

data2 = data[data$freq<quantile(data$freq,0.5),]
PreviousFrequencyRepetition_ModelReduced<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                                                  rpl + rpt + rpt2 + rpt3 + rps + sentence_border +
                                                  invnREP_Prev*pfreq +
                                                  (1|sujid) + (1|textid) + (1|wordid), data = data2)
print(summary(PreviousFrequencyRepetition_ModelReduced),cor=F)
# cPreviousFrequencyRepetition_ModelReduced<-confint(PreviousFrequencyRepetition_ModelReduced,     "pfreq:invnREP_Prev")

##########################################################################################################
# Reduced datasets to constrain the interaction between frequency and repetition
##########################################################################################################
data2 = data[data$lexical_status_Prev==TRUE,]
data2$pfreq<-scale(data2$pfreq_noncentered,center=TRUE,scale=FALSE)
data2$pfreqcat<-matrix(0,nrow(data2$pfreq),ncol(data2$pfreq))
q=quantile(data2$pfreq,probs=seq(0,1,1/5))
xpfreq = rep(0,1,length(q)-1)
for (i in 2:length(q)) {
  data2$pfreqcat[data2$pfreq>=q[i-1] & data2$pfreq<=q[i],]=i-1
  xpfreq[i-1] = median(data2$pfreq_noncentered[data2$pfreqcat==i-1])
}
data2$pfreqcat = as.factor(data2$pfreqcat)

data2 = data2[data2$pfreqcat==5,]
PreviousRepetition_ModelReduced<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                                  rpl + rpt + rpt2 + rpt3 + rps + sentence_border + 
                                  invnREP_Prev +
                                  (1|sujid) + (1|textid) + (1|wordid), data = data2)
print(summary(PreviousRepetition_ModelReduced),cor=F)
# cPreviousRepetition_ModelReduced<-confint(PreviousRepetition_ModelReduced,     "invnREP_Prev")

##########################################################################################################
# Supplementary Table 4: Repetition models for other measures
# First Fixation Durations (FFD), Number of Fixations in First Pass (NFFP), Regression Path Duration (RPD) and Re-Reading Time (RRT).
##########################################################################################################
# First Fixation Durations (FFD)
FFD <- lmer(logFFD ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                                 rpl + rpt + rps + sentence_border + 
                                 invnREP + invnREP:freq +
                                 (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(FFD),cor=F)
# cFFD<-confint(FFD)

# Gaze Duration (GD)
GD <- lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                                 rpl + rpt + rps + sentence_border + 
                                 invnREP + invnREP:freq +
                                 (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(GD),cor=F)
# cGD<-confint(GD)

# Regression Path Duration (RPD)
RPD<-lmer(logRPD ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                                 rpl + rpt + rps + sentence_border +
                                 invnREP + invnREP:freq +
                                 (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(RPD),cor=F)
# cRPD<-confint(RPD)

# Re-Reading Time (RRT).
data2 = data[data$logRRT>0,]
RRT<-lmer(logRRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
            rpl +rpt + rps + sentence_border + 
            invnREP + invnREP:freq +
            (1|sujid) + (1|textid) + (1|wordid), data = data2)
print(summary(RRT),cor=F)
# cRRT<-confint(RRT)

# Number of Fixations in First Pass (NFFP)
NFFP<-glmer(NFFP ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                              rpl + rpt + rps + sentence_border +
                              invnREP + invnREP:freq +
                              (1|sujid) + (1|textid) + (1|wordid), data = data, family=poisson)
print(summary(NFFP),cor=F)
# cNFFP<-confint(NFFP)
