library(ggthemes)
library(ggplot2)
library("viridis") 
library(dplyr)

dat_PR <- read.csv("PRLength.csv")
dat_LR <- read.csv("LRL.csv")
dat_PR_d14 <- subset(dat_PR, Day == 14)
dat_LR_d14 <- subset(dat_LR, Day == 14)
dat_LR_d14 <- subset(dat_LR_d14, Genotype != "5727-4")

dat_LRD <- read.csv("LRD_v2.csv")
dat_LRD <- subset(dat_LRD, Genotype != "5727-4")
dat_LRD$Genotype <- factor(dat_LRD$Genotype, levels=c("444","5730-3","5730-3 B","5727-10"),ordered=TRUE)

plot_PR<- ggplot(dat_LRD, aes(x = Genotype, y = PRL,color = as.factor(Genotype), show.legend = TRUE)) + 
  geom_boxplot(lwd=1, outlier.size = 0, alpha = 0.5) +
  geom_jitter(size =1, width = 0.1, alpha = 0.5) +
  ylab('PRL') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_manual(values = A_colors)+
  theme(legend.position="none")
plot_PR
ggsave("plot_PR_boxplot.pdf", plot = last_plot())

dat_LR_d14$Genotype <- factor(dat_LR_d14$Genotype, levels=c("444","5730-3","5730-3 B","5727-10"),ordered=TRUE)

A_colors <- c("#000000","#636363","#FF5733","#FF5733","#bdbdbd","#636363","#000000","#31A354", "#74C476","#00B050","#92D050")

plot_LRN<- ggplot(dat_LR_d14, aes(x = Genotype, y = LRL,color = as.factor(Genotype), show.legend = TRUE)) + 
  geom_boxplot(lwd=1, outlier.size = 0, alpha = 0.5) +
  geom_jitter(size =1, width = 0.1, alpha = 0.5) +
  ylab('LRL') +
  coord_cartesian(ylim = c(0,20))+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_manual(values = A_colors)+
  theme(legend.position="none")
plot_LRN
ggsave("plot_PRd14_boxplot.pdf", plot = last_plot())
###

##########
dat <- dat_LR_d14

is.factor(dat_LR_d14$LRL)
is.factor(dat_LR_d14$Genotype)

#Run ANNOVA
attach(dat)
data(dat)
str(dat$dat)
tapply(dat$Genotype, dat$LRL, dat$mean) 
tapply(dat$Genotype, dat$LRL, dat$var)
boxplot(LRL ~ Genotype, data=dat)

lm.out = with(dat, lm(LRL ~ Genotype))
aov.out = aov(LRL ~ Genotype, data=dat)
oneway.test(LRL ~ Genotype, data=dat)
is.factor(dat$LRL)
is.factor(dat$Genotype)
dat
#View(sapply(dat2, class))
dat$Genotype <- factor(dat$Genotype)
dat$LRL <- as.factor(dat$LRL)

aov.out
summary(aov.out)
TukeyHSD(aov.out)

summary.lm(aov.out)

model=lm( LRL ~ Genotype, data=crop.data)
ANOVA=aov(model)
TUKEY <- TukeyHSD(x=ANOVA, 'crop.data$Genotype', conf.level=0.95)
plot(TUKEY , las=1 , col="brown")

library(multcompView)
#install.packages('rcompanion')
library(rcompanion)
library(lsmeans)
library(multcomp)

marginal = lsmeans(model, ~ Genotype)
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
