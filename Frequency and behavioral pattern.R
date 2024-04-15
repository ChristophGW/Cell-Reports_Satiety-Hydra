#Clear and set Directory
rm(list = setdiff(ls(), lsf.str()))

#load required packages
library(tidyverse)
library(dplyr)

#set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#define frame rate
fps = 24.5

#load data containing the spike data from the matlab script: "peakanalysis_batch.mat"
data<- read.csv(file= "N3_extrema_predictions_matlab_all_Experiment_Apr23.csv", header = TRUE, sep = ";")
#define last column name
column.name = colnames(data)[length(data)]

#add frame numbers to the data frame
data$Frames = 1:nrow(data)


#convert to long format
data <- gather(data, Extrema, values, Extrema1:all_of(column.name), factor_key=TRUE)
str(data)


#load data with the behavioral annotations
##data structure:
###Experiment             start   end behavior
### Experiment-745-5x-17h  9662 10330        1
#####-> the data frame contains the information of the frame when a behavior started (start) and ended (end), here the behaviors have specific numbers assigned (1-6)
behav<- read.csv(file= "N3_Behavior-annotation.csv", header = TRUE, sep = ";")
#only maintain the information of Experiment and number of recording
behav$Experiment <- str_extract(behav$Experiment, "Experiment-\\d+") 

#subset data to the experiment selected for spiking data (either Apr23 or Mai23)
behav = behav %>%
  #filter(Date %in% "23.05.2023")
  filter(Date %in% "17.04.2023")

#load meta data and information of treatment
IDs = read.csv(file = "ExperimentID.csv", sep = ";", header = TRUE)
#only maintain the information of Experiment and number of recording
IDs$Experiment <- str_extract(IDs$Experiment, "Experiment-\\d+")
#subset data to the experiment selected for spiking data (either Apr23 or Mai23)
IDs = IDs %>%
  #filter(Date %in% "23.05.2023")
  filter(Date %in% "17.04.2023")
head(IDs)

#merge data frames data and IDs by the extrema column
data = merge(data, IDs, by= c("Extrema"))
data$Activity = "Baseline"
head(data)

#add behavior annotation
# Join the two dataframes based on matching IDs
df_merged <- data %>%
  group_by(Experiment)%>%
  left_join(behav, by = "Experiment") %>%
  filter(Frames >= start & Frames <= end) %>%
  select(Experiment, Frames, behavior)

str(df_merged)

# Create separate columns for each annotation of behavior
df_final <- df_merged %>%
  pivot_wider(names_from = behavior, values_from = behavior, values_fill = 0)

# Replace 0-values with annotations
df_final <- data %>%
  left_join(df_final, by = c("Experiment", "Frames"))

#summarize all behavior columns into one column to identify regions without annotated behavior
# Define the columns to be considered for summarization
head(df_final)
columns_to_summarize <- c("1",
                          #"2", 
                          "3",
                          #"4", 
                          "5")

# Create a new column with 1 for rows with all NA values in specified columns and 0 otherwise
df_final$'0' <- ifelse(rowSums(is.na(df_final[, columns_to_summarize])) == length(columns_to_summarize), 1, 0)
head(df_final)


# Define the mapping between old and new column names
column_mapping <- c("0" = "Elongated",
                    "1" = "Contraction",
                    "2" = "Contracted",
                    "3" = "Elongating",
                    "4" = "Bending",
                    "5" = "Detachment",
                    "6" = "Detached")
column_mapping = as.data.frame(column_mapping)
column_mapping$names =row.names(column_mapping)

#rename the columns
renamed_df <- df_final %>%
  rename_at(as.vector(na.omit(column_mapping$names[match(names(df_final), column_mapping$names)])), 
            ~as.vector(na.omit(column_mapping$column_mapping[match(names(df_final), column_mapping$names)])))

# 0 to NA in specific columns
column.num = length(renamed_df)
select = c(colnames(renamed_df)[13:column.num])
renamed_df <- renamed_df %>%
  mutate(across(select, ~ifelse(. == 0, NA, .)))

# Create a sample data frame
# Specify the columns to be summarized
# Define the column positions and names
target_columns <- c(13:19)  # Specify the positions of the columns you want to check
target_column_names <- names(renamed_df)[target_columns]  # Get the names of the target columns

# Initialize an empty list to store the summary data frames
summary_list <- list()


# Loop through each target column position and name
for (i in seq_along(target_columns)) {
  col <- target_columns[i]
  col_name <- target_column_names[i]
  
  # Filter rows where the target column has a value and summarize/count spike
  summary_df <- renamed_df %>%
    filter(!is.na(.[, col])) %>%
    group_by(Experiment, Treatment, Time.post.feeding)%>%
    summarise(count_1 = sum(values == 1),
              num_rows = n_distinct(row_number()),
              column = col_name)
  
  # Append the summary data frame to the list
  summary_list[[col_name]] <- summary_df
}

# Combine the summary data frames from the list into a single data frame
summary_df <- bind_rows(summary_list)
head(summary_df)

summary = summary_df
summary$Time = summary$num_rows/fps
summary$Frequenz = summary$count_1/summary$Time
colnames(summary)[6] = "Behavior"
summary$Behavior = as.factor(summary$Behavior)
str(summary)
head(summary)
#rename behavior Contracted = Contraction, Detached = Elongated
summary$Behavior[summary$Behavior == "Contracted"] <- "Contraction"
summary$Behavior[summary$Behavior == "Detached"] <- "Elongated"

#rename treatment
#rename behavior Contracted = Contraction, Detached = Elongated
summary$Treatment[summary$Time.post.feeding == "4h"] <- "fed"
summary$Treatment[summary$Time.post.feeding == "8h"] <- "fed"
summary$Treatment[summary$Time.post.feeding == "24h"] <- "intermediate"
summary$Treatment[summary$Time.post.feeding == "17h"] <- "intermediate"
summary$Treatment[summary$Time.post.feeding == ">48h"] <- "starved"
summary$Treatment[summary$Time.post.feeding == "288"] <- "starved"

#filter data and only keep starved and fed
summary_final = summary %>%
  filter(Treatment %in% c("fed", "starved"))

#set color code
#color code
colors = c("#DCBBD7", #starved
           "#8A4992", #Fed #8A4992
           "#660066", #Fed +pellet
           "#efbbff" #GSh
)

#plot data
#Boxplot frequenzy
ggplot(data = summary_final, aes(Behavior, Frequenz , fill= Treatment))+
  stat_boxplot(geom ='errorbar', 
               na.rm = TRUE, 
               lwd=1)+
  geom_boxplot(outlier.shape = NA,
               na.rm=TRUE,
               lwd=1)+
  geom_point(aes(Behavior, Frequenz , color= Treatment), width=0.1, alpha= 0.5, size= 4)+
  scale_fill_manual(values = colors)+
  scale_color_manual(values = colors)+
  #scale_color_manual(values = c("black", "black", "black", "black"))+
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
  ylab("Frequency (Hz)")

#summarize data in mean and counts
summary_final %>%
  group_by(Behavior, Treatment)%>%
  summarise(mean= mean(Frequenz), count = n())


##################Statistical analysis###########################

data1 = summary_final %>%
  filter(Behavior %in% "Elongating")#select the behavior of interest
head(data1)
#calculate meand and sd
data1 %>% group_by(Treatment) %>% 
  summarise(median=median(Frequenz ), sd=sd(Frequenz ), n = n())

#statastical analysis
#test for equal variance: Bartlett's test
bart <- bartlett.test(Frequenz  ~ Treatment, data = data1)
# Levene's test with multiple independent variables
leve <- leveneTest(Frequenz  ~ Treatment, data = data1)
bart #p-value>0.05 -> equal variance
leve #p-value>0.05 -> equal variance

#testing for normal distribution
data1 %>%
  group_by(Treatment) %>% 
  do(tidy(shapiro.test(.$Frequenz )))#p-value>0.05 -> normal distributed

#ANOVA (equal variance and normal distributed)
# Compute the analysis of variance
res.aov <- aov(Frequenz  ~ Treatment, data = data1)

# Summary of the analysis
summary(res.aov)
#Multiple pairwise-comparison between the means of groups
TukeyHSD(res.aov)
t.test(Frequenz  ~ Treatment, data = data1)


data1 = data %>%
  filter(Behavior %in% "Elongating")
head(data1)
#calculate meand and sd
data1 %>% group_by(Treatment) %>% 
  summarise(median=median(Frequenz ), sd=sd(Frequenz ), n = n())

#statastical analysis
#test for equal variance: Bartlett's test
bart <- bartlett.test(Frequenz  ~ Treatment, data = data1)
# Levene's test with multiple independent variables
leve <- leveneTest(Frequenz  ~ Treatment, data = data1)
bart #p-value>0.05 -> equal variance
leve #p-value>0.05 -> equal variance

#testing for normal distribution
data1 %>%
  group_by(Treatment) %>% 
  do(tidy(shapiro.test(.$Frequenz )))#p-value>0.05 -> normal distributed

#ANOVA (equal variance and normal distributed)
# Compute the analysis of variance
res.aov <- aov(Frequenz  ~ Treatment, data = data1)

# Summary of the analysis
summary(res.aov)
#Multiple pairwise-comparison between the means of groups
TukeyHSD(res.aov)
t.test(Frequenz  ~ Treatment, data = data1)
