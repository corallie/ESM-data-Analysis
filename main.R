#' ____________________________________________________________________________
# 
# Main script of the project ESM-data-Analysis
# 
#' ____________________________________________________________________________
# 
# Description : 
# 
# Here is an overview of how to extract temporal information from an ESM 
# monitoring. 
# 
# You have to open this project in R in order to set your working directory at
# the right place where data and function can be read and where figures and 
# tables can be saved. 
# If your ESM monitoring is not sampled 5 times a day and with levels from 1 to
# 10, updates have to be done in functions files. 
# 
#' ____________________________________________________________________________
# 
# 
# Clean memory
rm(list = ls())
# 
# Close any graphic devices
graphics.off()
# 
# 
#' ____________________________________________________________________________
# 
# Source functions ====
# 
# 
# for segments 
source('./function/do_segments.R') 
source('./function/plot_segments.R') 
source('./function/table_segments.R') 
# 
# 
# for phases
source('./function/do_phases.R') 
source('./function/plot_phases.R')
source('./function/table_phases.R')
# 
# 
# for cycles 
source('./function/do_cycles.R') 
source('./function/plot_cycles.R')
source('./function/table_cycles.R')
# 
# 
#' ____________________________________________________________________________
# 
# DATA                                                                         ====
# 
# 
# Data folder is at : 
data_folder <- "./data/"
# 
# 
# Read data : 
simulated_data <- read.csv(paste0(data_folder, "simulated_NegA_group.csv"))
# 
# 
# Visualize data : 
plot_segments(data = simulated_data, #data.frame with all data
              id = 1) #id to plot
# 
# 
# If you want to save the last and next figures and table, 
# they will be in these folders :
figure_folder <- './figure/'
table_folder <- './table/'
# 
# 
# if not existing, create folders : 
if (!file.exists(figure_folder)) dir.create(figure_folder)
if (!file.exists(table_folder)) dir.create(table_folder)
# 
# 
# And save the data figure : 
plot_segments(data = simulated_data, 
              id = 1, 
              save_at = figure_folder)
# 
# 
#' ____________________________________________________________________________
# 
# SEGMENTS                                                                     ====
# 
# 
# Compute features of each segment : 
segments <- do_segments(data = simulated_data, 
                        cut_at_mean = FALSE) 
# and save : 
write.csv(segments, 
          file = paste0(data_folder, "segments.csv"), 
          row.names = FALSE)
# 
# 
# Do a table with a summary of segments features : 
summary_segments <- table_segments(segments = segments)
# and save :
write.csv(summary_segments, 
          file = paste0(table_folder, "summary_segments.csv"))
# 
# 
#' ____________________________________________________________________________
# 
# PHASES                                                                       ====
# 
# 
# Compute features of each phases : 
phases <- do_phases(data = simulated_data, 
                    segments = segments)
# and save : 
write.csv(phases, 
          file = paste0(data_folder, "phases.csv"), row.names = F)
# 
# 
# Visualize phases : 
plot_phases(data = simulated_data, 
            segments = segments, 
            phases = phases, 
            id = 1)
# and save : 
plot_phases(data = simulated_data, 
            segments = segments, 
            phases = phases, 
            id = 1, 
            save_at = figure_folder)
# 
# 
# Do table with a summary of phases features : 
summary_phases <- table_phases(phases = phases)
# and save :
write.csv(summary_phases, 
          file = paste0(table_folder, "summary_phases.csv"))
# 
# 
#' ____________________________________________________________________________
# 
# CYCLES ====
# 
# 
# Compute features of each cycles : 
cycles <- do_cycles(data = simulated_data)
# and save : 
write.csv(cycles, 
          file = paste0(data_folder, "cycles.csv"))
# 
# 
# Visualize cycles : 
plot_cycles(data = simulated_data, 
            cycles = cycles, 
            id = 1)
# and save : 
plot_cycles(data = simulated_data,
            cycles = cycles, 
            id = 1, 
            save_at = figure_folder)
# 
# 
# Do table with a summary of cycles features : 
summary_cycles <- table_cycles(cycles = cycles)
# and save :
write.csv(summary_cycles, 
          file = paste0(table_folder, "summary_cycles.csv"))
# 
# 
#' ____________________________________________________________________________