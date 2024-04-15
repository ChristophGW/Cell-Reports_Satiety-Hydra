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

#load data of neuronal activity after 4h, 8h, 16h and 24h post feeding of both neuronal populations
##data structure:
###Experiment     Population Treatment Pellet Time.h Time Activity Spikes  Frequency Run          Unique
###Experiment-745         N3       fed     no    17h  16h Baseline     33 0.05500000   2 fed-no-Baseline
data<- read.csv(file= "N4-N3_all_frequency_time-series_Feb2024.csv", header = TRUE, sep = ";")
head(data)

#sort data
data$Treatment  = factor(data$Treatment , c("starved", "fed", "GSH"))
data$Time  = factor(data$Time , c("starved", "4h", "8h", "16h", "24h"))

#select color code plotting data
colors = c("#a99bae", "#96a793",#starved
           "#8c5d9e", "#76b66b",#Fed
           "#553860", "#4b8341",#Fed +pellet
           "#dacbe0", "#dbecd8",#GSh
           "#abd9e9", 
           "#2c7bb6")


#plot data
#subset data
time = data %>%
  filter(Activity %in% "Baseline")%>%
  filter(Time %in% c("4h", "8h", "16h"#,"24h", "starved"#, ">48h"
                                  ))#%>%
  #filter(Run %in% c(2, 4))
head(time)

#sort data
time$Time = as.factor(time$Time)
levels(time$Time)
time$Time = factor(time$Time, c("4h", "8h", "16h"#, "24h", "starved"
                                ))

#color code
colors = c(#"#a99bae", "#96a793",#starved
           "#8c5d9e", "#76b66b",#Fed
           "#553860", "#4b8341",#Fed +pellet
           "#dacbe0", "#dbecd8",#GSh
           "#abd9e9", 
           "#2c7bb6")

#Boxplot frequenzy
ggplot(data = time, aes(Time, Frequency , fill= Population))+
  stat_boxplot(geom ='errorbar', 
               na.rm = TRUE, 
               lwd=1)+
  geom_boxplot(outlier.shape = NA,
               na.rm=TRUE,
               lwd=1)+
  geom_jitter(aes(Time, Frequency , color= Population), width = .1, alpha= 0.25, size= 4)+
  scale_fill_manual(values = colors)+
  scale_color_manual(values = colors)+
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
  ylab("Frequency (Hz)")+
  coord_cartesian(ylim = c(0, 0.15)) 



#statistical analysis
sub = time %>% filter(Population %in% "N4")#replace with N3 for analysis of neuronal population N3

#summarize data
sub %>%
  group_by(Time)%>%
  summarise(mean(Frequency), sd(Frequency))

#count n
sub %>%
  group_by(Time)%>%
  count(Time)

# Levene's test with multiple independent variables
leveneTest(Frequency ~ Time, data = sub)

#testing for normal distribution
sub %>%
  group_by(Time) %>% 
  do(tidy(shapiro.test(.$Frequency)))#p-value>0.05 -> normal distributed

#ANOVA (equal variance and normal distributed)
# Compute the analysis of variance
res.aov <- aov(Frequency ~ Time, data = sub)
# Summary of the analysis
summary(res.aov)
#Multiple pairwise-comparison between the means of groups
TukeyHSD(res.aov)

#all others one group no equal variance
kruskal.test(Frequency ~ Time, sub)

pairwise.wilcox.test(sub$Frequency,sub$Time, p.adjust="bonferroni")

dunnTest(Frequency ~ Time,
         data=sub,
         method="bonferroni")
