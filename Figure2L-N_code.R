library(ggthemes)
library(ggplot2)
library("viridis") 
library(dplyr)

dat_PR <- read.csv("PRLength.csv")
dat_LR <- read.csv("LRLength.csv")
dat_PR_d14 <- subset(dat_PR, Day == 14)
dat_LR_d14 <- subset(dat_LR, Day == 14)
#dat3$genotype <- factor(dat3$genotype, levels=c("Col-0","5057","299","298","288","287","286","296","284","285","252","295","312","slr"),ordered=TRUE)
#dat4t1$genotype <- factor(dat4t1$genotype, levels=c("Col-0","5057","299","298","288","287","286","296","284","285","252","295","311","312","365","366","slr"),ordered=TRUE)
#dat4t2$genotype <- factor(dat4t2$genotype, levels=c("Col-0","5057","299","298","288","287","286","296","284","285","252","295","311","312","365","366","slr"),ordered=TRUE)


plot_PR<- ggplot(dat_PR, aes(x = Day, y = PR_mm,color = as.factor(Day), show.legend = TRUE)) + 
  geom_boxplot(lwd=.5, outlier.size = 0) +
  geom_jitter(size =.2, width = 0.2) +
  ylab('Primary ') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~as.factor(Genotype), nrow=1)+
  scale_color_brewer(palette = "Set1")
plot_PR
ggsave("plot_PR_boxplot.pdf", plot = last_plot())

plot_PR_14<- ggplot(dat_PR_d14, aes(x = Genotype, y = PR_mm,color = as.factor(Genotype), show.legend = TRUE)) + 
  geom_boxplot(lwd=.5, outlier.size = 0) +
  geom_jitter(size =.2, width = 0.1) +
  ylab('Primary ') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_brewer(palette = "Set1")
plot_PR_14
ggsave("plot_PRd14_boxplot.pdf", plot = last_plot())

summary(dat_PR_sum$dat_PR)
dat_PR_sum<- group_by(dat_PR,Genotype, Day) %>% 
  summarise(
    count = n(), 
    mean = mean(PR_mm, na.rm = TRUE),
    sd = sd(PR_mm, na.rm = TRUE),
    ymin = (mean-sd),
    ymax = (mean+sd),
    count = n()
  )

plot_PR_time <- ggplot(data = dat_PR_sum, mapping = aes(x = Day, y = mean, color = as.factor(Genotype))) +
  geom_point() + 
  geom_line() + 
  ylab("Mean Primary Root length (mm)") + 
  xlab("Time (Days)") + 
  labs(color = "Genotype", title = "PRL TC") + 
  theme_classic(base_size = 12) +
  geom_errorbar(aes(ymin=mean-(sd/sqrt(count)), ymax=mean+(sd/sqrt(count))), size = 0.2, width = 0.1) + 
  scale_fill_discrete(guide = FALSE) + 
  scale_color_brewer(palette = "Set1") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) 
plot_PR_time
ggsave("plot_PR_time.pdf", plot = last_plot())




plot_LRL<- ggplot(dat_LR, aes(x = Genotype, y = LRL_mm,color = as.factor(Day), show.legend = TRUE)) + 
  geom_boxplot(lwd=.5, outlier.size = 0) +
  geom_jitter(size =.2, width = 0.2) +
  ylab('Lateral Root Length') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~as.factor(Day), nrow=1)+
  scale_color_brewer(palette = "Set1")
plot_LRL
ggsave("plot_LRL_boxplot.pdf", plot = last_plot())


dat_LR_d14$Genotype <- factor(dat_LR_d14$Genotype, levels=c("5730-3","5727-4","5727-7","5727-10","5719-1"),ordered=TRUE)

A_colors <- c("#000000","#31A354", "#74C476","#00B050",   "#92D050")
dat_LR_14b <- subset(dat_LR_d14, Genotype != "5727-4")

plot_LR_14<- ggplot(dat_LR_14b, aes(x = Genotype, y = LRL_mm, show.legend = TRUE, color=construct), alpha=0.5) + 
  geom_boxplot(lwd=.75, outlier.size = 0, alpha = (0.5)) +
  geom_jitter(size =.5, width = 0.1,alpha=0.5) +
  coord_cartesian(ylim = c(0,30))+
  ylab('Lateral root length (mm)') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_manual(values = A_colors)
plot_LR_14
ggsave("plot_LRd14_boxplot_trim.pdf", plot = last_plot())

summary(dat_LR$LRL_mm)
dat_LRL_sum<- group_by(dat_LR,Genotype, Day) %>% 
  summarise(
    count = n(), 
    mean = mean(LRL_mm, na.rm = TRUE),
    sd = sd(LRL_mm, na.rm = TRUE),
    ymin = (mean-sd),
    ymax = (mean+sd),
    count = n()
  )

plot_LR_time <- ggplot(data = dat_LRL_sum, mapping = aes(x = Day, y = mean, color = as.factor(Genotype))) +
  geom_point() + 
  geom_line() + 
  ylab("Mean Lateral Root length (mm)") + 
  xlab("Time (Days)") + 
  labs(color = "Genotype", title = "LRL TC") + 
  theme_classic(base_size = 12) +
  geom_errorbar(aes(ymin=mean-(sd/sqrt(count)), ymax=mean+(sd/sqrt(count))), size = 0.2, width = 0.1) + 
  scale_fill_discrete(guide = FALSE) + 
  scale_color_brewer(palette = "Set1") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) 
plot_LR_time
ggsave("plot_LRL_time.pdf", plot = last_plot())


dat_LRD <- read.csv("PR_LRD.csv")

plot_LRD<- ggplot(dat_LRD, aes(x = Genotype, y = LRD,color = as.factor(Genotype), show.legend = TRUE)) + 
  geom_boxplot(lwd=.5, outlier.size = 0) +
  geom_jitter(size =.2, width = 0.2) +
  ylab('LRD') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~as.factor(Day), nrow=1)+
  scale_color_brewer(palette = "Set1")
plot_LRD
ggsave("plot_PR_boxplot.pdf", plot = last_plot())

dat_LRD_d14 <- subset(dat_LRD, Day == 14)
dat_LRD_d14$Genotype <- factor(dat_LRD_d14$Genotype, levels=c("5730-3","5727-4","5727-7","5727-10","5719-1"),ordered=TRUE)
dat_LRD_d14 <- subset(dat_LRD_d14, Genotype != "5727-10")

plot_LRD_14<- ggplot(dat_LRD_d14, aes(x = construct, y = LRD,color = construct, show.legend = TRUE)) + 
  geom_boxplot(lwd=.5, outlier.size = 0) +
  geom_jitter(size =.2, width = 0.1) +
  ylab('Lateral root density') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) +
  theme_classic(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_brewer(palette = "Set1")
plot_LRD_14
ggsave("plot_LRD-d14_boxplot_trim.pdf", plot = last_plot())

summary(dat_LRD$LRD)
dat_LRD_sum<- group_by(dat_LRD,Genotype, Day) %>% 
  summarise(
    count = n(), 
    mean = mean(LRD, na.rm = TRUE),
    sd = sd(LRD, na.rm = TRUE),
    ymin = (mean-sd),
    ymax = (mean+sd),
    count = n()
  )

plot_LRD_time <- ggplot(data = dat_LRD_sum, mapping = aes(x = Day, y = mean, color = as.factor(Genotype))) +
  geom_point() + 
  geom_line() + 
  ylab("Mean Lateral Root Density") + 
  xlab("Time (Days)") + 
  labs(color = "Genotype", title = "LRD TC") + 
  theme_classic(base_size = 12) +
  geom_errorbar(aes(ymin=mean-(sd/sqrt(count)), ymax=mean+(sd/sqrt(count))), size = 0.2, width = 0.1) + 
  scale_fill_discrete(guide = FALSE) + 
  scale_color_brewer(palette = "Set1") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) 
plot_LRD_time
ggsave("plot_LRD_time.pdf", plot = last_plot())




dat7 <- dat_V

#Run ANNOVA
attach(dat7)
data(dat7)
str(dat7)
tapply(genotype, LRD, mean) 
tapply(genotype, LRD, var)
tapply(genotype, LRD, length)
boxplot(LRD ~ genotype, data=dat7)

lm.out = with(dat7, lm(LRD ~ genotype))
aov.out = aov(LRD ~ genotype, data=dat7)
oneway.test(LRD ~ genotype, data=dat7)
is.factor(LRD)
is.factor(genotype)

aov.out
summary(aov.out)
TukeyHSD(aov.out)

summary.lm(aov.out)

model=lm( dat7$LRD ~ dat7$genotype )
ANOVA=aov(model)
TUKEY <- TukeyHSD(x=ANOVA, 'dat7$genotype', conf.level=0.95)
plot(TUKEY , las=1 , col="brown")

library(multcompView)
library(rcompanion)
library(lsmeans)
library(multcomp)

marginal = lsmeans(model, ~ genotype)
pairs(marginal, adjust="tukey")
CLD = cld(marginal, alpha = 0.05, Letters = letters, adjust  = "tukey")
CLD

#### Reran on subset alone:
dat_B
attach(dat_B)
data(dat_B)
str(dat_B)
tapply(genotype, LRD, mean) 
tapply(genotype, LRD, var)
tapply(genotype, LRD, length)
boxplot(LRD ~ genotype, data=dat_B)

lm.out = with(dat_B, lm(LRD ~ genotype))
aov.out = aov(LRD ~ genotype, data=dat_B)
oneway.test(LRD ~ genotype, data=dat_B)
is.factor(LRD)
is.factor(genotype)

aov.out
summary(aov.out)
TukeyHSD(aov.out)
summary.lm(aov.out)

model=lm( dat_B$LRD ~ dat_B$genotype )
ANOVA=aov(model)
TUKEY <- TukeyHSD(x=ANOVA, 'dat_B$genotype', conf.level=0.95)
plot(TUKEY , las=1 , col="brown")
library(multcompView)
marginal = lsmeans(model, ~ genotype)
pairs(marginal, adjust="tukey")
CLD = cld(marginal, alpha = 0.05, Letters = letters, adjust  = "tukey")
CLD


##________
#mapping by time


plot_time <- ggplot(data = dat_C, mapping = aes(x = dpg, y = mean, color = as.factor(TPL_type))) +
  geom_point(size=.5) + 
  geom_line() + 
  ylab("Mean LRD") + 
  xlab("Time (Days)") + 
  labs(color = "Genotype", title = "LRD TC") + 
  theme_classic(base_family = 'Arial Bold', base_size = 12) +
  geom_errorbar(aes(ymin=mean-(sd/sqrt(count)), ymax=mean+(sd/sqrt(count))), size = 0.2, width = 0.1) + 
  scale_fill_discrete(guide = FALSE) + 
  scale_color_brewer(palette = "Spectral") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), strip.background  = element_blank()) 
plot_time

summary(dat$LRD)
library(dplyr)
dat_C<- group_by(dat, TPL_type, dpg) %>% 
  summarise(
    count = n(), 
    mean = mean(LRD, na.rm = TRUE),
    sd = sd(LRD, na.rm = TRUE),
    ymin = (mean-sd),
    ymax = (mean+sd),
    count = n()
  )

