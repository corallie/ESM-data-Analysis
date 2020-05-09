#' ____________________________________________________________________________
# 
# Main script of the project
# 
# 
# 
# Nettoyage
rm(list = ls())
graphics.off()
# 
# 
# 
# Read data : 
data_folder <- "./data/"
data <- read.csv(paste0(data_folder, "simulated_NegA_group.csv"))
# 
# 
# Visualize data : 
source('./function/plot_segments.R')
plot_segments(data = data, id = 1)
# and save figure :
where_to_save_figs <- './figure/'
if (!file.exists(where_to_save_figs)) dir.create(where_to_save_figs)
plot_segments(data = data, id = 1, save_at = where_to_save_figs)
# 
# 
# 
#' ____________________________________________________________________________
# 
# SEGMENTS ====
# 
# Source functions :
source('./function/do_segments.R')
source('./function/table_segments.R')
# 
# Do segments without cut at mean : 
segments <- do_segments(data = data, cut_at_mean = FALSE)
# and save : 
write.csv(segments, file = paste0(data_folder, "segments.csv"), row.names = F)
# 
# Do segments with cut at mean : 
segments_cut <- do_segments(data = data, cut_at_mean = TRUE)
# and save : 
write.csv(segments_cut, file = paste0(data_folder, "segments_cut.csv"), row.names = F)
# 
# Do table with a summary of segments features : 
summary_segments <- table_segments(segments = segments)
# and save :
table_segments(segments = segments, save_at = data_folder)
# 
# 
#' ____________________________________________________________________________
# 
# PHASES ====
# 
# Source the function :
source('./function/do_phases.R') 
source('./function/table_phases.R')
# 
# Do phases : 
phases <- do_phases(data = data, segments = segments)
# and save : 
write.csv(phases, file = paste0(data_folder, "phases.csv"), row.names = F)
# 
# 
# Visualize phases : 
source('./function/plot_phases.R')
plot_phases(data = data, segments = segments, phases = phases, id = 1)
# and save : 
plot_phases(data = data, segments = segments, phases = phases, id = 1, 
            save_at = where_to_save_figs)
# 
# Do table with a summary of phases features : 
summary_phases <- table_phases(phases = phases)
# and save :
table_phases(phases = phases, save_at = data_folder)
# 
#' ____________________________________________________________________________
# 
# CYCLES ====
# 
# Source the function :
source('./function/do_cycles.R') 
source('./function/table_cycles.R')
# 
# Do segments without cut at mean : 
cycles <- do_cycles(segments_cut = segments_cut)
# and save : 
write.csv(cycles, file = paste0(data_folder, "cycles.csv"))
# 
# 
# Visualize cycles : 
source('./function/plot_cycles.R')
plot_cycles(data = data, segments_cut = segments_cut, cycles = cycles, id = 1)
# and save : 
plot_cycles(data = data, segments_cut = segments_cut, cycles = cycles, id = 1, 
            save_at = where_to_save_figs)
# 
# Do table with a summary of cycles features : 
summary_cycles <- table_cycles(cycles = cycles)
# and save :
table_cycles(cycles = cycles, save_at = data_folder)
# 
#' ____________________________________________________________________________