library(ggthemes)
library(ggplot2)
library("viridis") 

dat_LRD <- read.csv("Isoswotch_LRD.csv")
dat_LRD <- subset(dat_LRD, comments !=  "contaminated")
dat_LRD <- subset(dat_LRD, switch !=  "none")
dat_LRD <- subset(dat_LRD, Lrpheno !=  "yes")
dat_LRD <- subset(dat_LRD, PRL > 40)
dat_LRD$switch_geno <- factor(dat_LRD$switch_geno, levels=c("Wt","Del3","Del5","Del7"),ordered=TRUE)

#This piece takes a data file and orders it based on the category after the money sign
#dat$genotype <- factor(dat$genotype, levels=c("Col-0","5057","299","409","406","414","411","408","407","slr"),ordered=TRUE)
#A_colors <- c("#424242","#FF5733", "#FF5733","#FF9900", "#FF9900", "#fa9fb5", "#c51b8a","#0099FF","#0033CC","#a1d99b","#31a354","#bdbdbd","#636363")
##########
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
####old below







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


#B_hyg <- c("Hyg",NA)
#dat_B <- subset(dat4t1, Selection %in% B_hyg)
dat_B <- subset(dat, Day ==  10)
dat_C <- subset(dat, days ==  11)
dat_D <- subset(dat, days ==  12)
dat_13 <- subset(dat, days ==  13)
dat_14 <- subset(dat, days ==  14)
#dat_B <- subset(dat_B, genotype !=  "5095-1")
#dat_B <- subset(dat_B, genotype !=  "5097-10")
#dat_B <- subset(dat_B, genotype !=  "5097-2")
#dat_B <- subset(dat_B, genotype !=  "5097-4")
#dat_B <- subset(dat_B, genotype !=  "5099-3")
#dat_B <- subset(dat_B, genotype !=  "5099-5")
#dat_B <- subset(dat_B, genotype !=  "5099-6")
#dat_B$genotype <- factor(dat_B$genotype, levels=c("Col-0","5057","299","409","406","414","411"#,"408","407","slr"),ordered=TRUE)
#dat_C$genotype <- factor(dat_C$genotype, levels=c("Col-0","5057","299","409","406","414","411"#,"408","407","slr"),ordered=TRUE)
#dat_D$genotype <- factor(dat_D$genotype, levels=c("Col-0","5057","299","409","406","414","411","408","407","slr"),ordered=TRUE)

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

#dat4 <- subset(dat4t1, Selection != "KanHyg" )
dat5 <- subset(dat, days == 14)
#dat5$genotype <- factor(dat5$genotype, levels=c("Col-0","5057","299","298","288","287","286","296","285","284","252","295","311","312","365","366","slr"),ordered=TRUE)
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

B_line <- c("5057","IAA14","TPLN188","slr","TPLN188-IAA14","TPLN188[H1SolvMuts]","TPLN188[H1LisHMuts]","TPLN188[H1->TAF5H1]","TPLN188[H1->ahelix]","TPLN188[[V145AE146AK148AK149A]]-IAA14","TPLN188[H1SolvMuts]-[V145AE146AK148AK149A]", "TPLN188[H1LisHMuts]-[V145AE146AK148AK149A]","TPLN188[H1->TAF5H1]-[V145AE146AK148AK149A]","TPLN188[H1->ahelix]-[V145AE146AK148AK149A]")
dat_V <- subset(dat8, construct %in% B_line)
dat_V$construct <- factor(dat_V$construct, levels=c("5057","IAA14","TPLN188","slr","TPLN188-IAA14","TPLN188[[V145AE146AK148AK149A]]-IAA14","TPLN188[H1SolvMuts]","TPLN188[H1SolvMuts]-[V145AE146AK148AK149A]","TPLN188[H1LisHMuts]","TPLN188[H1LisHMuts]-[V145AE146AK148AK149A]","TPLN188[H1->TAF5H1]","TPLN188[H1->TAF5H1]-[V145AE146AK148AK149A]","TPLN188[H1->ahelix]","TPLN188[H1->ahelix]-[V145AE146AK148AK149A]"),ordered=TRUE)
write.csv(dat_V,file="dat_V.csv")
dat_V <-read.csv("dat_V.csv")

plot3<- ggplot(dat_V, aes(as.factor(construct), y = LRD, color =as.factor(construct))) +   
  geom_boxplot(outlier.shape = NA, lwd=.65, alpha=0.1) +
  geom_jitter(size=.75, width=.2, color="black", alpha=0.5) +
  #ylim(c(0,0.5)) +
  ylab('LRD') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 10)+
  theme(legend.position="none")+
  scale_colour_manual(values = A_colors)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot3

A_colors <- c("#424242","#424242","#424242","#424242","#FFCC00", "#FF9900", "#fa9fb5", "#c51b8a","#0099FF","#0033CC","#a1d99b","#31a354","#bdbdbd","#636363")
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


##________
#mapping by time


plot_time <- ggplot(data = dat_E, mapping = aes(x = days, y = mean, color=as.factor(construct))) +
  geom_point(size=.5) + 
  geom_line() + 
  ylab("Mean LRD") + 
  xlab("Time (Days)") + 
  labs(color = "Genotype", title = "LRD TC") + 
  theme_classic(base_family = 'Arial Bold', base_size = 12) +
  geom_errorbar(aes(ymin=mean-(sd/sqrt(count)), ymax=mean+(sd/sqrt(count))), size = 0.2, width = 0.1) + 
  theme(legend.position="none")+
  #scale_color_brewer(palette = "Spectral") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) 
plot_time
plot_time+facet_wrap(~construct, nrow = 5)

summary(dat$LRD)
library(dplyr)
dat_E<- group_by(datb, construct, days) %>% 
  summarise(
    count = n(), 
    mean = mean(LRD, na.rm = TRUE),
    sd = sd(LRD, na.rm = TRUE),
    ymin = (mean-sd),
    ymax = (mean+sd),
    count = n()
  )

datb <- subset(dat, length_mm > 25)


#dat_D<- left_join(dat_C, dat_B)
#C_lines <- c("Col-0","5057","299","298","288","287","286","296","285","284","252","295","311"#,"312","365","366","slr")
#dat_E <- subset(dat_D, genotype %in% C_lines)
#dat_E$genotype <- factor(dat_E$genotype, levels=c("Col-0","5057","299","298","288","287","286"#,"296","285","284","252","295","311","312","365","366","slr"),ordered=TRUE)
#dat_E <- subset(dat_E, rl > 5)
#dat8 <- subset(dat5, rl > 25)
#
#B_line <- c("312","295","298","299","Col-0","slr","5057","252","365","366")
#dat_F <- subset(dat_E, genotype %in% B_line)
#dat_F$genotype <- factor(dat_F$genotype, levels=c("Col-0","5057","298","299","252","295","312"#,"365","366","slr"),ordered=TRUE)

#plotN188+ geom_vline(xintercept = 47, color = "blue", size= 1.25, alpha = 0.5)
#plotN188+ facet_wrap(~Rapa, ncol = 2)+ geom_vline(xintercept = 47, color = "blue", size= 1.25, alpha = 0.5)


###
##helixvistest
print(sequence[1, ])
data(H1_Seq)
draw_wheel("LSRELVFLILQFLDE")

draw_wheel("LSRELVFLILQFLDE", col = c("pink", "orange", "white", "black"),
           labels = TRUE, label.col = "blue")
draw_wheel("LSRELVFLILQFLDE",
           labels = TRUE, label.col = "black")

###
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("flowTime")
library(flowTime)
