library(ggthemes)
library(ggplot2)
library("viridis") 

dat_LRD <- read.csv("Isoswotch_LRD.csv")
dat_LRD <- subset(dat_LRD, comments !=  "contaminated")
dat_LRD <- subset(dat_LRD, switch !=  "none")
dat_LRD <- subset(dat_LRD, Lrpheno !=  "yes")
dat_LRD <- subset(dat_LRD, PRL > 40)
dat_LRD$switch_geno <- factor(dat_LRD$switch_geno, levels=c("Wt","Del3","Del5","Del7"),ordered=TRUE)

A_colors <- c("#000000","#FFCC00", "#FF9900","#F71735","#bdbdbd","#636363", "#0099FF", "#c51b8a","#0099FF","#0033CC","#a1d99b","#31a354","#bdbdbd","#636363")

#This plots all the data initially to ensure the data has been loaded in
plot1<- ggplot(dat_LRD, x=switch_geno) + 
  geom_boxplot(aes(x=switch_geno,y=LRD), outlier.size = 0, alpha = 0.5) 
plot1

plot1<- ggplot(dat_LRD, aes(x=switch_geno, y = as.numeric(LRD), color=switch_geno)) + 
  geom_boxplot(lwd=1, outlier.size = 0, alpha = 0.5) +
  geom_jitter(size =1, width = 0.1, alpha = 0.5) +
  #coord_cartesian(ylim = c(0.4, 1.25)) +
  ylab('LRD') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 12)+
  scale_colour_manual(values = A_colors)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="none")
  #facet_wrap(~Day, nrow=1) 
plot1

plot1_pheno<- ggplot(dat_LRD, aes(x= switch_geno, y = LRD)) + 
  geom_boxplot(lwd=1, outlier.size = 0, alpha = 0.7, aes(color=switch_geno)) +
  geom_jitter(size =2.5, width = 0.15, alpha = 0.8,aes(color=med21_genotype)) +
  #coord_cartesian(ylim = c(0.2, .8)) +
  ylab('LRD') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 10)+
  scale_colour_manual(values = A_colors)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="none")
plot1_pheno
plot1_pheno + facet_wrap(~med21_genotype)

####
dat_LRL <- read.csv("Isoswotch_LRL.csv")
#dat_LRL <- subset(dat_LRD, PRL > 40)
dat_LRL$Parent <- factor(dat_LRL$Parent, levels=c("904","903","902","901"),ordered=TRUE)


plot_LRL<- ggplot(dat_LRL, aes(x= Parent, y = LRL)) +
  geom_violin(lwd=1, aes(color=Parent))+
  #geom_boxplot(lwd=1, outlier.size = 0, alpha = 0.7, aes(color=Parent)) +
  geom_jitter(size =1, width = 0.2, alpha = 0.5,aes(color=med21_genotype)) +
  coord_cartesian(ylim = c(0, 40)) +
  ylab('Lateral Root Length') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 10)+
  scale_colour_manual(values = A_colors)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="none")
plot_LRL
plot_LRL+facet_wrap(~med21_genotype)

plot_LRL2<- ggplot(dat_LRL, aes(x= med21_genotype, y = LRL)) +
  geom_violin(lwd=1, aes(color=Parent))+
  geom_boxplot(width=.1, lwd=1, outlier.size = 0, alpha = 0.7, aes(color=Parent)) +
  #geom_jitter(size =1, width = 0.2, alpha = 0.5,aes(color=med21_genotype)) +
  #coord_cartesian(ylim = c(0, 40)) +
  ylab('Lateral Root Length') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 10)+
  scale_colour_manual(values = A_colors)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="none")
plot_LRL2
plot_LRL2+facet_wrap(~Parent, nrow=1)

####
####stats below
 ##########
dat <- dat_LRD
dat2 <- subset(dat2, Day == 14)
dat_7 <- subset(dat2, comment !=  "fungus")
write.csv(dat2, "dat2.csv")

is.factor(LRDI_quantification$LRILRDens)
is.factor(LRDI_quantification$Parent)
dat <- dat_LRD
dat <- read.csv("LRDI_quantification_rep.csv")

#Run ANNOVA
attach(dat)
data(dat)
str(dat$dat)
tapply(dat$Parent, dat$LRD, dat$mean) 
tapply(dat$Parent, dat$LRD, dat$var)
boxplot(LRD ~ Parent, data=dat)

lm.out = with(dat, lm(LRD ~ Parent))
aov.out = aov(LRD ~ Parent, data=dat)
oneway.test(LRD ~ Parent, data=dat)
is.factor(dat$LRD)
is.factor(dat$Parent)
dat2
#View(sapply(dat2, class))
dat$Parent <- factor(dat$Parent)
dat$LRD <- as.factor(dat$LRD)

aov.out
summary(aov.out)
TukeyHSD(aov.out)

summary.lm(aov.out)

model=lm( LRD ~ Parent, data=dat)
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

crop.data <- read.csv("Isoswotch_LRD.csv", header = TRUE, colClasses = c("factor","numeric","numeric", "factor", "factor", "factor","numeric", "numeric", "numeric","numeric", "factor", "factor", "factor", "factor", "factor"))
crop.data <- subset(crop.data, comments !=  "contaminated")
crop.data <- subset(crop.data, switch !=  "none")
crop.data <- subset(crop.data, Lrpheno !=  "yes")
crop.data <- subset(crop.data, PRL > 40)
summary(crop.data)
one.way <- aov(LRD ~ Parent, data = crop.data)
summary(one.way)
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way
tukey.plot.aov<-aov(LRD ~ Parent, data=crop.data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)
mean.rld.data <- crop.data %>%
  group_by(Parent) %>%
  summarise(
    LRD = mean(LRD)
  )
mean.rld.data$group <- c("b","ab","b","a")
mean.rld.data
one.way.plot <- ggplot(crop.data, aes(x = Parent, y = LRD)) +
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
mean.rld.data$group <- c("a","a","a","a")
mean.rld.data
one.way.plot <- ggplot(crop.data, aes(x = Parent, y = PRL)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange')+
  theme_classic2() 
one.way.plot + geom_text(data=mean.rld.data, label=mean.rld.data$group, vjust = -8, size = 5)
####
