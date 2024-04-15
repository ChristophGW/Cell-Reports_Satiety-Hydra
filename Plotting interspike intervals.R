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
##structure of data:
###Experiment                       Intervals.time Time.post.feeding Starvation Run
###Experiment-01-N3-5x-12d-starved       16.44845              288h    Starved   3
data<- read.csv(file= "N3_Intervals_all_Jul23.csv", header = TRUE, sep = ";")
head(data)

#sort data
data$Starvation  = factor(data$Starvation , c("Starved", "Fed"))

#plot data
#color code
colors = c(#"#2B7BB8", #starved
           "#DCBBD7",
           "#8A4993", #Fed
           "#ACD9E9", #GSh
           "#abd9e9", 
           "#2c7bb6")

#summarize data
##take mean inter spike interval per polyp and time point
data.mean = data %>%
  group_by(Experiment, Starvation)%>%
  filter(Time.post.feeding %in% c("4h", ">48h", "288h", "Fed", "Starved") )%>%
  dplyr::summarise(Intervals.time = mean(Intervals.time, na.rm= T))
head(data.mean)

#plot data
#statistical comparison
my_comparison = list(c("Starved", "Fed"))

#Boxplot frequenzy
ggplot(data = data.mean, aes(Starvation, Intervals.time , fill= Starvation))+
  stat_boxplot(geom ='errorbar', 
               na.rm = TRUE, 
               lwd=1)+
  geom_boxplot(outlier.shape = NA,
               na.rm=TRUE,
               lwd=1)+
  geom_jitter(aes(Starvation, Intervals.time ,color= Starvation), width=0.1, alpha= 0.5, size= 4)+
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
        strip.text.x = element_text(size = 12)#,
        #legend.position = "none"
  )+
  ylab("Intervals (sec)")#+
  #facet_grid(~Run)


#save data
write.csv(data.mean, file = "N3_Intervals_all_mean_Jul23.csv", row.names = F)

#statastical analysis
#count n
data.mean %>%
  group_by(Starvation)%>%
  summarize(count = n())

#test for equal variance: Bartlett's test
bart <- bartlett.test(Intervals.time ~ Starvation, data = data.mean)
# Levene's test with multiple independent variables
leve <- leveneTest(Intervals.time ~ Starvation, data = data.mean)
bart #p-value>0.05 -> equal variance
leve #p-value>0.05 -> equal variance

#testing for normal distribution
data.mean %>%
  group_by(Starvation) %>% 
  do(tidy(shapiro.test(.$Intervals.time)))#p-value>0.05 -> normal distributed


#wilcox test
wilcox.test(Intervals.time ~ Starvation, data = data.mean)
