#Clear and set Directory
rm(list = setdiff(ls(), lsf.str()))
#load required packages
library(tidyverse)
library(reshape2)
library(data.table)
#for statistic
library(car)
library(dplyr)
library(broom)
library(ggpubr)
library(FSA)
#set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#load data
##structure data:
###Header: Experiment	Population	Treatment	Pellet	Time.h	Time	Activity	Spikes	Frequency	Run	Unique
###   chr           chr         chr     chr     chr   chr   chr         int     num     int   chr
###Row1: Experiment-601-5x-starved	N3	starved	no	288h	starved	Baseline	15	0.025	1	starved-no-Baseline
data<- read.csv(file= "data/N4-N3_all_frequency_Feb2024.csv", header = TRUE, sep = ";")
head(data)

#sort data
data$Treatment  = factor(data$Treatment , c("starved", "fed", "GSH", "glass"))
data$Time  = factor(data$Time , c("starved", "4h", "8h", "17h", "24h"))


#######################Analysis of spontaneous activity##################################################
base = data %>%
  filter(Activity %in% "Baseline")%>%
  filter(!(Time.h %in% c("8h", "17h", "24h")))%>%
  filter(!(Unique %in% "starved-glass-Baseline"))
head(base)

#sort data
base$Unique = as.factor(base$Unique)
levels(base$Unique)
base$Unique = factor(base$Unique, c("starved-no-Baseline", "fed-no-Baseline", "fed-pellet-Baseline", "GSH-no-Baseline"))

#color code
colors = c("#DCBBD7", #starved
           "#be29ec", #Fed #8A4992
           "#660066", #Fed +pellet
           "#efbbff", #GSh
           "#efbbff" #GSh
           )

#Boxplot frequenzy
ggplot(data = base, aes(Unique, Frequency , fill= Unique))+
  stat_boxplot(geom ='errorbar', 
               na.rm = TRUE, 
               lwd=1)+
  geom_boxplot(outlier.shape = NA,
               na.rm=TRUE,
               lwd=1)+
  geom_jitter(aes(Unique, Frequency , color= Treatment), width = .1, alpha= 0.15, size= 4)+
  scale_fill_manual(values = colors)+
  scale_color_manual(values = c("black", "black", "black", "black"))+
  theme(axis.text.x  = element_text(angle=45, vjust=1,hjust = 1, size=9))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 2),
        axis.ticks.length = unit(.25,"cm"),
        axis.ticks = element_line(size = 2),
        axis.text.x = element_text(size = 20), 
        axis.title.x =element_blank(),
        axis.text.y = element_text(size = 20), 
        axis.title.y =element_text(size = 20),
        strip.text.x = element_text(size = 12),
        legend.position = "none"
  )+
  ylab("N3: Frequency (Hz)")+
  coord_cartesian(ylim = c(0, 0.15))

##################statistical analysis#############################
#summarize data
base %>%
  group_by(Unique)%>%
  summarise(mean(Frequency), sd(Frequency))

#count n
base %>%
  group_by(Unique)%>%
  summarize(count = n())

# Levene's test with multiple independent variables
leveneTest(Frequency ~ Unique, data = base)

#testing for normal distribution
base %>%
  group_by(Unique) %>% 
  do(tidy(shapiro.test(.$Frequency)))#p-value>0.05 -> normal distributed

#ANOVA (equal variance and normal distributed)
# Compute the analysis of variance
res.aov <- aov(Frequency ~ Unique, data = base)
# Summary of the analysis
summary(res.aov)
#Multiple pairwise-comparison between the means of groups
TukeyHSD(res.aov)


#######################Analysis of activity during Feeding Response##################################################
feed = data %>%
  filter(Activity %in% "Feeding")%>%
  filter(!(Time.h %in% c("8h", "17h", "24h")))
head(feed)

#sort data
feed$Unique = as.factor(feed$Unique)
levels(feed$Unique)
feed$Unique = factor(feed$Unique, c("starved-no-Feeding", "fed-no-Feeding", "GSH-no-Feeding"))

#color code
colors = c("#DCBBD7", #starved
           "#be29ec", #Fed
           #"#660066", #Fed +pellet
           "#efbbff" #GSh
)


#Boxplot frequenzy
ggplot(data = feed, aes(Unique, Frequency , fill= Unique))+
  stat_boxplot(geom ='errorbar', 
               na.rm = TRUE, 
               lwd=1)+
  geom_boxplot(outlier.shape = NA,
               na.rm=TRUE,
               lwd=1)+
  geom_jitter(aes(Unique, Frequency , color= Treatment), width = .1, alpha= 0.15, size= 4)+
  scale_fill_manual(values = colors)+
  scale_color_manual(values = c("black", "black", "black", "black"))+
  theme(axis.text.x  = element_text(angle=45, vjust=1,hjust = 1, size=9))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 2),
        axis.ticks.length = unit(.25,"cm"),
        axis.ticks = element_line(size = 2),
        axis.text.x = element_text(size = 20), 
        axis.title.x =element_blank(),
        axis.text.y = element_text(size = 20), 
        axis.title.y =element_text(size = 20),
        strip.text.x = element_text(size = 12),
        legend.position = "none"
  )+
  ylab("N3: Frequency (Hz)")+
  coord_cartesian(ylim = c(0, 0.15)) 

#statistical analysis
#summarize data
feed %>%
  group_by(Unique)%>%
  summarise(mean(Frequency), sd(Frequency))

#count n
feed %>%
  group_by(Unique)%>%
  summarize(count = n())


# Levene's test with multiple independent variables
leveneTest(Frequency ~ Unique, data = feed)

#testing for normal distribution
feed %>%
  group_by(Unique) %>% 
  do(tidy(shapiro.test(.$Frequency)))#p-value>0.05 -> normal distributed

#all others one group no equal variance
kruskal.test(Frequency ~ Unique, feed)

pairwise.wilcox.test(feed$Frequency,feed$Unique, p.adjust="bonferroni")

dunnTest(Frequency ~ Unique,
         data=feed,
         method="bonferroni")

##############correlate spiking vs starvation#################
head(data)
#subset data
time = data %>%
  filter(Activity %in% "Baseline")%>%
  filter(Time %in% c("4h", "8h", "17h"#,"24h", "starved"#, ">48h"
                                  ))#%>%
  #filter(Run %in% c(2, 4))
head(time)

#sort data
time$Time = as.factor(time$Time)
levels(time$Time)
time$Time = factor(time$Time, c("4h", "8h", "17h"#, "24h", "starved"
                                ))

#color code
colors = c(#"#a99bae", #starved
           "#553860", #4h
           "#8c5d9e", #8h
           
           "#dacbe0", #17h
           "#abd9e9", 
           "#2c7bb6")

#Boxplot frequenzy
ggplot(data = time, aes(Time, Frequency , fill= Time))+
  stat_boxplot(geom ='errorbar', 
               na.rm = TRUE, 
               lwd=1)+
  geom_boxplot(outlier.shape = NA,
               na.rm=TRUE,
               lwd=1)+
  geom_jitter(aes(Time, Frequency , color= Time), width = .1, alpha= 0.15, size= 4)+
  scale_fill_manual(values = colors)+
  scale_color_manual(values = c("black", "black", "black", "black", "black"))+
  theme(axis.text.x  = element_text(angle=45, vjust=1,hjust = 1, size=9))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 2),
        axis.ticks.length = unit(.25,"cm"),
        axis.ticks = element_line(size = 2),
        axis.text.x = element_text(size = 20), 
        axis.title.x =element_blank(),
        axis.text.y = element_text(size = 20), 
        axis.title.y =element_text(size = 20),
        strip.text.x = element_text(size = 12),
        legend.position = "none"
  )+
  ylab("N3: Frequency (Hz)")+
  coord_cartesian(ylim = c(0, 0.15)) 


#line plot
# Standard error of the mean
ggplot(data= time, aes(x = Time, y = Frequency)) + 
  geom_jitter(aes(x = Time, y = Frequency, colour = Time), size = 4, alpha =0.4)+
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se= T, linewidth = 1, color= "grey", alpha= .2)+
  #geom_smooth(method = "lm")+
  stat_regline_equation(label.y = 0.08, aes(label = after_stat(rr.label)), linewidth = 7) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 1),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(linewidth = 1),
        axis.text.x = element_text(size = 24, angle = 45, hjust = 1), 
        axis.title.x =element_text(size = 24),
        axis.text.y = element_text(size = 24), 
        axis.title.y =element_text(size = 24)#,
        #legend.position="none"
  )+
  #ggtitle("Mean +/- SE")+
  ylab("Frequency (Hz)")+
  xlab("Hours post feeding")


#statistical analysis
#summarize data
time %>%
  group_by(Time)%>%
  summarise(mean(Frequency), sd(Frequency))

#count n
time %>%
  group_by(Time)%>%
  summarize(count = n())

# Levene's test with multiple independent variables
leveneTest(Frequency ~ Time, data = time)

#testing for normal distribution
time %>%
  group_by(Time) %>% 
  do(tidy(shapiro.test(.$Frequency)))#p-value>0.05 -> normal distributed

#ANOVA (equal variance and normal distributed)
# Compute the analysis of variance
#res.aov <- aov(Frequency ~ Unique, data = feed)
# Summary of the analysis
#summary(res.aov)
#Multiple pairwise-comparison between the means of groups
#TukeyHSD(res.aov)
library(FSA)

#all others one group no equal variance
kruskal.test(Frequency ~ Time, time)

pairwise.wilcox.test(time$Frequency,time$Time, p.adjust="bonferroni")

dunnTest(Frequency ~ Time,
         data=time,
         method="bonferroni")

