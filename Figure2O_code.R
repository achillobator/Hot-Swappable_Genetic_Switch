library(ggthemes)
library(ggplot2)
library("viridis") 

dat <- read.csv("LRDI_quantification.csv")
dat <- LRDI_quantification

#This piece takes a data file and orders it based on the category after the money sign
#dat$genotype <- factor(dat$genotype, levels=c("Col-0","5057","299","409","406","414","411","408","407","slr"),ordered=TRUE)
A_colors <- c("#424242","#FF5733", "#FF5733","#FF9900", "#FF9900", "#fa9fb5", "#c51b8a","#0099FF","#0033CC","#a1d99b","#31a354","#bdbdbd","#636363")
##########
#This plots all the data initially to ensure the data has been loaded in
plot1<- ggplot(dat, x=Parent) + 
  geom_boxplot(aes(x=Parent,y=as.numeric(LRILRDens)), outlier.size = 0, alpha = 0.5) 
  
plot1

is.factor(dat$LRILRDens)
is.factor(dat$Parent)


plot1<- ggplot(dat_B, aes(x=Parent, y = as.numeric(LRILRDens), color=Parent)) + 
  geom_boxplot(lwd=1, outlier.size = 0, alpha = 0.5) +
  geom_jitter(size =1, width = 0.1, alpha = 0.5) +
  #coord_cartesian(ylim = c(0.4, 1.25)) +
  ylab('LRID') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 12)+
  scale_colour_manual(values = A_colors)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="none")
  #facet_wrap(~Day, nrow=1) 
plot1


dat_B <- subset(dat, switch !=  "not-specific")
dat_B <- subset(dat, PRL > 30)
dat_B <- na.omit(dat_B)


plot2<- ggplot(dat_B, aes(as.factor(Parent), y = PRL, color=Parent)) +
  geom_boxplot(outlier.size = 0.1) + 
  geom_jitter(size =1, width = 0.1) +
  coord_cartesian(ylim = c(0, 125)) +
  ylab('Root Length') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 10)+
  scale_colour_manual(values = A_colors)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="none")

plot2

A_colors <- c("#bdbdbd","#636363","#000000","#F71735","#FFCC00", "#FF9900", "#fa9fb5", "#c51b8a","#0099FF","#0033CC","#a1d99b","#31a354","#bdbdbd","#636363")

dat_pheno <- read.csv("SPT6_R3_LRDdata_pooled_pheno.csv")
plot1_pheno<- ggplot(dat_pheno, aes(x= genotype, y = LRD)) + 
  geom_boxplot(lwd=1, outlier.size = 0, alpha = 0.7, aes(color=genotype)) +
  geom_jitter(size =2.5, width = 0.12, alpha = 0.5,aes(color=Plant_num)) +
  coord_cartesian(ylim = c(0.2, .8)) +
  ylab('LRD') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 10)+
  scale_colour_manual(values = A_colors)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="none")+
  facet_wrap(~Day, nrow=1) 
plot1_pheno



dat_B <- subset(dat, Day ==  10)
dat_C <- subset(dat, days ==  11)
dat_D <- subset(dat, days ==  12)
dat_13 <- subset(dat, days ==  13)
dat_14 <- subset(dat, days ==  14)


plot3 <- ggplot(dat_D, aes(x = as.factor(construct), y = LRD, color=as.factor(construct), show.legend = FALSE)) + 
  geom_boxplot(lwd=0.6, outlier.size = 0) +
  geom_jitter(size =.7, width = 0.2, height = 0.001, stroke=.4, colour = "black", alpha=0.5) +
  coord_cartesian(ylim = c(0, 0.5)) +
  ylab('LRD') +
  xlab('Genotype') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_family = 'Arial Bold', base_size = 10)+
  #scale_color_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #+ theme(legend.position="none")
plot3
plot3 + facet_wrap(~days)


dat5 <- subset(dat, days == 14)
dat7 <- subset(dat5, length_mm > 5)
dat8 <- subset(dat5, length_mm > 25)
dat8 <- subset(dat8, comment !=  "fungus")
dat9 <- subset(dat, length_mm > 30)
dat_x <- subset(dat9, comment !=  "fungus")

plot4<- ggplot(dat_x, aes(x= construct, y = LRD, color=construct)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size=.5, width=.15, color="black", alpha=0.5) +
  #ylim(c(0,0.7)) +
  ylab('LRD') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_family = 'Arial Bold', base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(legend.position="none")+
  facet_wrap(~days, nrow=5)
plot4
plot4 + coord_flip()

##########
dat2 <- dat_pheno
dat2 <- subset(dat2, Day == 14)
dat_7 <- subset(dat2, comment !=  "fungus")
write.csv(dat2, "dat2.csv")

is.factor(LRDI_quantification$LRILRDens)
is.factor(LRDI_quantification$Parent)
dat <- LRDI_quantification_rep
dat <- read.csv("LRDI_quantification_rep.csv")

#Run ANNOVA
attach(dat)
data(dat)
str(dat$dat)
tapply(dat$Parent, dat$LRILRDens, dat$mean) 
tapply(dat$Parent, dat$LRILRDens, dat$var)
boxplot(LRILRDens ~ Parent, data=dat)

lm.out = with(dat, lm(LRILRDens ~ Parent))
aov.out = aov(LRILRDens ~ Parent, data=dat)
oneway.test(LRILRDens ~ Parent, data=dat)
is.factor(dat$LRILRDens)
is.factor(dat$Parent)
dat2
#View(sapply(dat2, class))
dat$Parent <- factor(dat$Parent)
dat$LRILRDens <- as.factor(dat$LRILRDens)

aov.out
summary(aov.out)
TukeyHSD(aov.out)

summary.lm(aov.out)

model=lm( LRILRDens ~ Parent, data=crop.data)
ANOVA=aov(model)
TUKEY <- TukeyHSD(x=ANOVA, 'crop.data$Parent', conf.level=0.95)
plot(TUKEY , las=1 , col="brown")

library(multcompView)
install.packages('rcompanion')
library(rcompanion)
library(lsmeans)
library(multcomp)

marginal = lsmeans(model, ~ Parent)
pairs(marginal, adjust="tukey")
CLD = cld(marginal, alpha = 0.05, Letters = letters, adjust  = "tukey")
CLD

install.packages(c("AICcmodavg"))
library(AICcmodavg)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

crop.data <- read.csv("LRDI_quantification_slim.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric", "numeric", "numeric"))
summary(crop.data)
one.way <- aov(LRILRDens ~ Parent, data = crop.data)
summary(one.way)
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way
tukey.plot.aov<-aov(LRILRDens ~ Parent, data=crop.data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)
mean.rld.data <- crop.data %>%
  group_by(Parent) %>%
  summarise(
    LRD = mean(LRILRDens)
  )
mean.rld.data$group <- c("a","b","b","b","b")
mean.rld.data
one.way.plot <- ggplot(crop.data, aes(x = Parent, y = LRILRDens)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange')+
  theme_classic2() 
one.way.plot + geom_text(data=mean.rld.data, label=mean.rld.data$group, vjust = -8, size = 5)

###PRL version
summary(crop.data)
one.way <- aov(PRL ~ Parent, data = crop.data)
summary(one.way)
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way
tukey.plot.aov<-aov(PRL ~ Parent, data=crop.data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)
mean.rld.data <- crop.data %>%
  group_by(Parent) %>%
  summarise(
    PRL = mean(PRL)
  )
mean.rld.data$group <- c("a","b","b","b","b")
mean.rld.data
one.way.plot <- ggplot(crop.data, aes(x = Parent, y = PRL)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange')+
  theme_classic2() 
one.way.plot + geom_text(data=mean.rld.data, label=mean.rld.data$group, vjust = -8, size = 5)
####





#### Reran on subset alone:
dat_B
attach(dat_B)
data(dat_B)
str(dat_B)
tapply(construct, LRD, mean) 
tapply(construct, LRD, var)
tapply(construct, LRD, length)
boxplot(LRD ~ construct, data=dat_B)

lm.out = with(dat_B, lm(LRD ~ construct))
aov.out = aov(LRD ~ construct, data=dat_B)
oneway.test(LRD ~ construct, data=dat_B)
is.factor(LRD)
is.factor(construct)
is.numeric(LRD)
is.numeric(construct)

aov.out
summary(aov.out)
TukeyHSD(aov.out)
summary.lm(aov.out)

model=lm( dat_B$LRD ~ dat_B$construct )
ANOVA=aov(model)
TUKEY <- TukeyHSD(x=ANOVA, 'dat_B$construct', conf.level=0.95)
plot(TUKEY , las=1 , col="brown")
library(multcompView)
marginal = lsmeans(model, ~ construct)
pairs(marginal, adjust="tukey")
CLD = cld(marginal, alpha = 0.05, Letters = letters, adjust  = "tukey")
CLD


