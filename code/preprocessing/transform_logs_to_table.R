library(dplyr)
library(dict)

##### Load text files ##### 
# set file path
hrb_us_hw_1_path <- "../HMM_Experiment/RAW_DATA/TW/HRB_HW/HRB1_HW.txt_fixed"

# create file connection
textfile <- file(description = hrb_us_hw_1_path, open = "r", blocking = "TRUE")

# create empty list to save event logs
lines <- list()

# read logs by line
repeat{
  line = readLines(textfile, n = 1) # Read 1 line from the file.
  if ("Experiment Start" %in% line){next} # if the line is start message, ignore
  if(identical(line, character(0))){break} # If the line is empty, exit.
  lines <- append(lines, line) # Otherwise, print and repeat next iteration.
}

# close file connection
close(textfile)

# remove first line (experiment start)
lines <- lines[-1]

# separate lines to events by "System Log" string
# if no action, a event will include 20 lines
for (i in seq(length(lines))) {
  # if the line include "System Log", then create a new event
  if (grepl("System Log", x =  lines[i]) == 1) {
    new_event = list()
    
    print(paste0("i = ", i))
    event_count = event_count + 1
    
    new_event
  }
  
  
}

print(event_count) # 600
