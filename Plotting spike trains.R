#Clear and set Directory
rm(list = setdiff(ls(), lsf.str()))

#load required packages
library(tidyverse)
library(fs)

#set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#define folder path
folder_path = getwd()

#set frame rate of recording
fps= 24.44

#load all files which contain spike train data and contain "_spike-train_" in their name
# Function to find CSV files with "_spike-train_" in the name
find_spikes_csv_files <- function(folder_path) {
  # Get the absolute path of the folder
  folder_path <- fs::path(folder_path)
  
  # List all files in the folder and its subfolders
  all_files <- list.files(path = folder_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  
  # Filter files with "_spike-train_" in the name
  extrema_csv_files <- grep("/*_spike-train_", all_files, value = TRUE)
  
  return(extrema_csv_files)
}

# Function to load CSV files as separate data frames based on the extracted information in the function "find_spikes_csv_files"
load_csv_files_as_dataframes <- function(file_paths) {
  data_list <- list()
  
  for (i in seq_along(file_paths)) {
    # Generate the variable name (e.g., data1, data2, ..., datax)
    var_name <- paste0("data", i)
    
    # Load the CSV file into a data frame
    data_list[[var_name]] <- read.csv(file_paths[i], sep = ",", header = T)
  }
  
  return(data_list)
}

# Call the function to find CSV files with "extrema_" in the name
spikes_csv_files <- find_spikes_csv_files(folder_path)

# Load CSV files as separate data frames
data_frames_list <- load_csv_files_as_dataframes(spikes_csv_files)

# Merge the list of data frames into one dataframe using rbind and do.call
data <- do.call(rbind, data_frames_list)

#for plotting replace 0 with NA
data[data == 0] = NA

#filter data: fed= 4h and starved >48h post feeding
data = data %>%
  filter(Time.post.feeding %in% c("4h", ">48h", "288h"))

#sort data
data$Starvation = factor(data1$Starvation, c("starved", "fed"))
str(data)

#plot data
ggplot(data = data1, aes(x= Time, y= interaction(Experiment, Starvation), color = values))+
  geom_point(shape= "|", size= 3)+
  scale_color_gradient(na.value = NA)+
  theme(panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(),
       panel.background = element_blank(), 
       axis.line = element_line(colour = "black", size = 3),
       axis.ticks.length=unit(.25, "cm"),
       axis.ticks = element_line(size = 2),
       axis.text.x = element_text(size = 24, angle = 45, hjust = 1), 
       axis.title.x =element_text(size = 24),
       axis.text.y = element_blank(), 
       axis.title.y =element_text(size = 24),
       legend.position="none"
  )+
  ylab("Polyps")+
  xlab("Time (sec)")
