#Clear working space and environment
rm(list = setdiff(ls(), lsf.str()))

#load required packages
library(tidyverse)
library(R.matlab)
library(stringr)
library(readxl)
library(astsa)

#set working directory (directory where the file is saved)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()#show working directory

#set parameters for analysis
#here frame rate of recordings
fps = 24.45
#and total number of frames
frames = 14672

#load data: all xls files in working directory
##structure of files:
###first column: Frames
###(ROI approach) second column: mean fluorescence change of the whole population
###(Icy tracking of single neurons) second till last column: mean fluorescence of tracks (tracks#0, tracks#1 etc.)
files <- list.files(pattern = ".xls")#xls can be also exchanged with csv
files <- list.files(pattern = ".csv")

# Read and align the files in particular adapted for icy output
data <- files %>%
  setNames(nm = .) %>%
  map_dfr(
    ~ {
      # Read each file
      #df <- read_xls(.x, col_names = TRUE, guess_max = frames)#for xls files
      df <- read.csv(.x, header = T, sep = ",") #for csv
      
      # Add a temporary column for row numbers
      df <- df %>% mutate(row_number = row_number())
      
      # Create a new frame number column starting from 0 to 4500
      aligned_frame <- seq(1, frames, by = 1)
      
      # Create a data frame with aligned frame number and other columns
      result_df <- data.frame("Frames" = aligned_frame)
      
      # Merge the original data with the aligned frame number data
      result_df <- left_join(result_df, df, by = c("Frames" = "row_number"))
      
      return(result_df)
    },
    .id = "Experiment"
  )

# Remove column "Frame number" (old numbering from icy) from the data frame
data <- subset(data, select = - `Frame number`)#for icy
data <- subset(data, select = - `X`)#for ROI approach
#rename column with values in case of ROI approach
colnames(data)[3] = "Mean"
#split variable so maintain id of table/file
data$Experiment <- sub("*.xls","", data$Experiment)
data$Experiment <- sub("*.csv","", data$Experiment)
str(data)
head(data)


##convert to long format in case of icy tracked output, skip with ROI approach to normalize and smooth
#identify the last column
last.column = colnames(data)[length(data)]
#convert to long
data <- gather(data, Tracks, Values, `linked tracks#0`:all_of(last.column), factor_key=TRUE)
#clean naming of tracks and replace "#" with "."
data$Tracks <- sub("#",".", data$Tracks)
head(data)

###########Normalize and smooth data###############
#summarize data:take the mean of all tracks at each time point to get the mean neuronal population activity
data = data %>%
  group_by(Experiment, Frames)%>%
  summarize(Mean = mean(Values, na.rm= TRUE))
head(data)

#OPTIONAL: close gaps if needed and 
data <- data %>%
  group_by(Experiment) %>%
  mutate(lower_25_percentile = quantile(Mean, 0.15, na.rm = TRUE),
         filled = if_else(is.na(Mean),
                                             lower_25_percentile,
                                             Mean)) %>%
  select(-lower_25_percentile)
head(data)

#normalize data by quantile
data = data %>%
  group_by(Experiment)%>%
  mutate(Norm = (filled - quantile(filled, probs = c(.75), na.rm = T))/quantile(filled, probs = c(.75), na.rm = T))
head(data)

#OPTIONAL: detrend data
data = data%>%
  group_by(Experiment)%>%
  mutate(Detrend = detrend(Norm, order = 2, lowess = TRUE, lowspan = 1/3)) %>%
  ungroup()

#OPTIONAL: smooth data
data = data%>%
  group_by(Experiment)%>%
  mutate(smoothed = ksmooth(Frames, Norm, kernel = "normal", bandwidth = 1*fps)$y) %>%
  ungroup()


###########Visualize data###############
#plot data
ggplot(data= data)+  
  #geom_point(aes(Time, Norm, color = Experiment), size= .15)+
  geom_line(aes(Frames, Norm), linewidth = 0.15, alpha = 0.25, color = "black")+
  geom_line(aes(Frames, Detrend, color = Experiment), linewidth = 0.15)+
 
  #geom_line(aes(Time, smoothed), linewidth = 1, color = "black")+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 2),
        axis.ticks.length = unit(.25,"cm"),
        axis.ticks = element_line(linewidth = 1),
        axis.text.x = element_text(size = 14), 
        axis.title.x =element_text(size = 18), 
        axis.text.y = element_text(size = 14),
        #axis.text.y = element_blank(),
        axis.title.y =element_text(size = 18),
        #strip.text.x = element_text(size = 12),
        legend.position = "none",
        strip.text = element_blank(), 
        strip.background = element_blank()
  )+
  ylab("DeltaF")+
  xlab("Time (sec)")+
  #facet_wrap(~Experiment, ncol = 1, scales = "free_y")
  facet_wrap(~Experiment, scales = "free_y")

###########Convert to single matlab files##########
#first transform to wide format
data2 <- pivot_wider(
  data,
  id_cols = c("Frames"),
  names_from = Experiment,
  values_from = Norm
)

head(data2)

#convert to matlab
#save every experiment as its own file
for (i in 2:ncol(data2)) {
  df_traces = as.matrix(data2[i])
  Time = as.matrix(data2$Frames)
  file = colnames(data2)[i]
  filename= paste("matlab", file, ".mat", sep = "")
  writeMat(filename,dF_traces = df_traces, T= Time)
}

#continue with matlab script: peakanalysis_batch.mat
