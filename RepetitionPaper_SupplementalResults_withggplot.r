rm(list=ls()) #borro todas las variables del workspace (rm)

library(lme4)

load("data_without_skips.Rda")

# Higher orders
data$rpt2 = data$rpt^2
data$rpt3 = data$rpt^3

data$sentence_border[data$sentence_border=="2"] = 0

# Center variables
data$rpt_noncentered = data$rpt
data$freq_noncentered = data$freq
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


##########################################################################################################
# Supplementary Table 2: Baseline model
##########################################################################################################
B0<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                        (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(B0),cor=F)
# cB0<-confint(B0)

B1<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
           rpl +
           (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(B1),cor=F)
# cB1<-confint(B1)

B2<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                                rpl + rpt + rpt2 + rpt3 +
                                (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(B2),cor=F)
# cB2<-confint(B2)

BaselineModel<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                                     rpl +
                                     rpt + rpt2 + rpt3 +
                                     rps + sentence_border +
                                     (1|sujid) + (1|textid) + (1|wordid), data = data)
print(summary(BaselineModel),cor=F)
# cBaselineModel<-confint(BaselineModel)

anova(B0,B1,B2,BaselineModel)

##########################################################################################################
# Supplementary Figure 1: Word position in text
##########################################################################################################

source("./remef.v0.6.10.R")
library(ggplot2)

# A) Absolute (aligned to BGN, first 2K words)
  data2 = data[data$rpt_abs<1975 & data$textid!=5,]
    data2$rpt_abs1 = data2$rpt_abs
    data2$rpt_abs2 = data2$rpt_abs1^2
    data2$rpt_abs3 = data2$rpt_abs1^3
    AbsoluteRPT_begin<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                              rpl + rpt_abs1 + rpt_abs2 + rpt_abs3 + 
                              (1|sujid) + (1|textid) + (1|wordid), data = data2)
    
    nombres<-names(fixef(AbsoluteRPT_begin))
    indices=which(is.element(nombres,c("(Intercept)","rpt_abs1","rpt_abs2","rpt_abs3")))
    nombres[indices]
    data2$remef1=remef(AbsoluteRPT_begin,ran=NULL,fix=indices,keep=TRUE)
    data2$gd_remef=10^data2$remef1
    
    # data2$gd_remef=10^remef(AbsoluteRPT_begin,ran="all")

  ybreaks = seq(255, 285, 10)
  xbreaks = seq(0, 2000, 500)
  p <- ggplot(data2, aes(y=gd_remef, x = rpt_abs1)   )
    p <- p + scale_y_continuous(name="Gaze duration [ms]", breaks = (ybreaks), labels = as.character(ybreaks))
    p <- p + stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1.2, colour = "black")
    p <- p + scale_x_continuous(name = "Absolute Position-in-Text", breaks =(xbreaks), labels = as.character(xbreaks))
    p <- p + coord_cartesian(xlim =(c(min(xbreaks), max(xbreaks))), ylim =(c(min(ybreaks), max(ybreaks))))
    p <- p + theme_bw()
    p <- p + theme(legend.position = "top")
    p <- p + theme(strip.background = element_rect(colour = "darkgrey", fill = "lightgrey", size = .2))
    p <- p + theme(text = element_text(size=20))
    p

# B) Relative
  data2 = data[data$textid!=5,]
    RelativeRPT_begin<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                                    rpl + rpt + rpt2 + rpt3 +
                                    (1|sujid) + (1|textid) + (1|wordid), data = data2)

    nombres<-names(fixef(RelativeRPT_begin))
    indices=which(is.element(nombres,c("(Intercept)","rpt","rpt2","rpt3")))
    nombres[indices]
    data2$remef1=remef(RelativeRPT_begin,ran=NULL,fix=indices,keep=TRUE)
    data2$gd_remef=10^data2$remef1
    
      
    xbreaks = seq(0, 1, 0.250)
  p <- ggplot(data2, aes(y=gd_remef, x = rpt_noncentered)   )
    p <- p + scale_y_continuous(name="Gaze duration [ms]", breaks = (ybreaks), labels = as.character(ybreaks))
    p <- p + stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1.2, colour = "black")
    p <- p + scale_x_continuous(name = "Relative Position-in-Text", breaks =(xbreaks), labels = as.character(xbreaks))
    p <- p + coord_cartesian(xlim =(c(min(xbreaks), max(xbreaks))), ylim =(c(min(ybreaks), max(ybreaks))))
    p <- p + theme_bw()
    p <- p + theme(legend.position = "top")
    p <- p + theme(strip.background = element_rect(colour = "darkgrey", fill = "lightgrey", size = .2))
    p <- p + theme(text = element_text(size=20))
    p

# C) Absolute (aligned to END, last 2K words)
  data2 = data[data$rpt_abs_end<1975 & data$textid!=5,]
    data2$rpt_abs_end1 = data2$rpt_abs_end
    data2$rpt_abs_end2 = data2$rpt_abs_end1^2
    data2$rpt_abs_end3 = data2$rpt_abs_end1^3
    AbsoluteRPT_end<-lmer(logFPRT ~ Nlaunchsite + invlength + pinvlength + freq + pfreq + invlength:freq + 
                                            rpl + rpt_abs_end1 + rpt_abs_end2 + rpt_abs_end3 + 
                                            (1|sujid) + (1|textid) + (1|wordid), data = data2)
    
    nombres<-names(fixef(AbsoluteRPT_end))
    indices=which(is.element(nombres,c("(Intercept)","rpt_abs_end1","rpt_abs_end2","rpt_abs_end3")))
    nombres[indices]
    data2$remef1=remef(AbsoluteRPT_end,ran=NULL,fix=indices,keep=TRUE)
    data2$gd_remef=10^data2$remef1
    
    data2$rpt_abs_end_remef=-(1975 - data2$rpt_abs_end1)

    xbreaks = seq(-2000, 0, 250)
    p <- ggplot(data2, aes(y=gd_remef, x = rpt_abs_end_remef)   )
      p <- p + scale_y_continuous(name="Gaze duration [ms]", breaks = (ybreaks), labels = as.character(ybreaks))
      p <- p + stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1.2, colour = "black")
      p <- p + scale_x_continuous(name = "Absolute Position-in-Text", breaks =(xbreaks), labels = as.character(xbreaks))
      p <- p + coord_cartesian(xlim =(c(min(xbreaks), max(xbreaks))), ylim =(c(min(ybreaks), max(ybreaks))))
      p <- p + theme_bw()
      p <- p + theme(legend.position = "top")
      p <- p + theme(strip.background = element_rect(colour = "darkgrey", fill = "lightgrey", size = .2))
      p <- p + theme(text = element_text(size=20))
      p
    