# Process data to generate a bar chart using counts

library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

#set working directory
path = 'C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/HMI_v1/'


# Some parameters
cnty <- "WAS"
# where the data are as feature classes
gdb <- "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/HMI_v1/HMI_v1.gdb"

# Prints feature classess in GDB
st_layers(gdb)

# fetch the data
# Clemson CE data
c_layer <- paste0(cnty, "_CE_HMI_mean")
c_ce <- st_read(dsn = gdb, layer = c_layer, stringsAsFactors = F, quiet = TRUE)

# Drop Geometry and results in a DF
st_geometry(c_ce) <- NULL

#Subsets by column the original DF
WAS_Pairs <- c_ce %>% select(CE_InstNum, HMI_mean, CEReas_1)

#Adds a column
WAS_Pairs['WAS_mean_round'] <- NA

#Checking Datatypes
str(WAS_Pairs)

#Converts the original HMI mean value to numeric
WAS_Pairs$WAS_mean_round <- as.numeric(as.character(WAS_Pairs$HMI_mean))

#Verifying that data type conversion has worked
str(WAS_Pairs)

#Round the mean value to tenths and write to the new column
WAS_Pairs$WAS_mean_round <- round(WAS_Pairs$WAS_mean_round, 1)

glimpse(WAS_Pairs)

WAS_Pairs$combo <- paste(WAS_Pairs$CEReas_1, WAS_Pairs$WAS_mean_round, sep=":")

glimpse(WAS_Pairs)

WAS_Pairs_cnts <- WAS_Pairs %>% count(combo) %>% arrange(desc(n))

glimpse(WAS_Pairs_cnts)

WAS_Pairs_cnts %>% mutate(gt = n > 10) %>% ggplot(aes(x = combo, y = n)) + geom_col(aes(fill = gt)) + 
  scale_fill_manual(values = c("grey", "#e27202"), c('Count > 10')) +
  theme(axis.text.x = element_text(angle = 45, size = rel(0.96), hjust = 1, vjust = 1)) + 
  labs(x= "Reason 1:Average HMI per CE", y = "Count", title = paste0(cnty, " - CE Reason 1 vs. HMI Average Counts"), subtitle = "") 
ggsave('C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/HMI_v1/results_plots/WAS_Pair_cnts.pdf', dpi = 300, width = 11.5, height = 8, units = "in")

write.csv(WAS_Pairs_cnts,'C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/results/WAS_Results.csv')


####################### Any and All Biologcal Reasons ###############################

#Subsets by column the original CE for Bio and HMI Data
WAS_Bio_reasons <- c_ce %>% filter_at(vars(CEReas_1,CEReas_2,CEReas_3,CEReas_4,CEReas_5,CEReas_6,CEReas_7,CEReas_8,CEReas_9,CEReas_10,CEReas_11,CEReas_12,CEReas_13,CEReas_14,CEReas_15),
                                      any_vars(. %in% c('2','3.2','4','5','6','7','8','9','10','14','15'))) %>% select(CEReas_1,CEReas_2,CEReas_3,CEReas_4,CEReas_5,CEReas_6,CEReas_7,CEReas_8,
                                                                                                                       CEReas_9,CEReas_10,CEReas_11,CEReas_12,CEReas_13,CEReas_14,CEReas_15,CE_InstNum, HMI_mean, BioFirst, BioFirst3, BioAny) %>% drop_na(HMI_mean)  

#Converts the original HMI mean value to numeric
WAS_Bio_reasons$WAS_HMI_mean <- as.numeric(WAS_Bio_reasons$HMI_mean)

glimpse(WAS_Bio_reasons$WAS_HMI_mean)

#Converts the original HMI mean value to numeric
hist(WAS_Bio_reasons$WAS_HMI_mean, border='black',
     col='Orange', main = paste0(cnty,' - HMI CE Mean for any Bio Reasons'), xlab = 'HMI CE Mean')

#HMI CE Hist with mean vertical bar. Horizontal Plot
fig <- ggplot(WAS_Bio_reasons,aes(WAS_HMI_mean)) + coord_flip() + geom_histogram(color="black", fill="orange", bins = 10) +
  labs(x= 'HMI CE Mean', y = 'Counts' ,title = paste0(cnty,' - HMI CE Mean for any Bio Reasons'), subtitle = "") +
  theme(plot.title = element_text(hjust = 0.5))

#Calculate the mean
WAS_mean <- mean(WAS_Bio_reasons$WAS_HMI_mean)

#peek
WAS_mean

#plot and save
fig+ geom_vline(aes(xintercept=WAS_mean), color="blue", linetype="dashed", size=1)

ggsave('C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/HMI_v1/results_plots/WAS_Hist_HMI_BioReasons_horiz.pdf', dpi = 300, width = 11.5, height = 8, units = "in")


#HMI CE Hist with mean vertical bar. Vertical Plot
fig <- ggplot(WAS_Bio_reasons,aes(WAS_HMI_mean)) + geom_histogram(color="black", fill="orange", bins = 10)

#Calculate the mean
WAS_mean <- mean(WAS_Bio_reasons$WAS_HMI_mean)

#peek
WAS_mean

#plot and save

fig+ geom_vline(aes(xintercept=WAS_mean), color="blue", linetype="dashed", size=1)+
  labs(x= 'HMI CE Mean', y = 'Counts' ,title = paste0(cnty,' - HMI CE Mean for any Bio Reasons'), subtitle = "") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/HMI_v1/results_plots/WAS_Hist_HMI_BioReasons_vertic.pdf', dpi = 300, width = 11.5, height = 8, units = "in")
