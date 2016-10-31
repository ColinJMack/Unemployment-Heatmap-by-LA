library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# read in data
UE_data <- read_csv("JSA_LA_UE_Data.csv",skip=7, col_names = TRUE)
View(UE_data)

################################################
# 1. DATA CLEANING
################################################

# 1. delete row 1
UE_data <- UE_data[2:33, ]

# 2. delete columns that have values instead of rates

# create a column index of columns 2 to 40 in 2 step jumps
col_index <- seq(from = 2, to=40, by= 2) # nos 2 to 40 (col numbers)

# remove these columns
UE_data <- UE_data[, -c(col_index)] 

# 3. now rename the remaining columns 
names(UE_data) # currently blank

# generate list of dates: starts with Jan 2015 to Aug 2016
dates <- seq(as.Date("2015/1/1"), by = "month", length.out = 20) # 20 = no of months till Aug 16

# now format
better_dates <- format(dates, "%b %y")

# ...and rename
names(UE_data)[2:21] <- better_dates 

# 4. rename column 1
names(UE_data)[1] <- "LA"

# 5. check structure of data
str(UE_data)
# change rates from character to numeric
UE_data[, 2:21] <- sapply(UE_data[, 2:21], as.numeric) 

################################################
# 2. VISUALISATION
################################################

# change from wide to long for ggplot
UE_data <- gather(UE_data, key=Year, value=UE, -LA)

# check
str(UE_data)

# factor the dates as a result of the gather function()
UE_data$Year <- factor(UE_data$Year, 
                       levels = c("Jan 15", "Feb 15", "Mar 15", "Apr 15", "May 15", "Jun 15", "Jul 15",
                                  "Aug 15", "Sep 15", "Oct 15", "Nov 15", "Dec 15", "Jan 16", "Feb 16",
                                  "Mar 16", "Apr 16", "May 16", "Jun 16", "Jul 16", "Aug 16",
                                  ordered=TRUE))

# convert LAs to factors
UE_data$LA <- as.factor(UE_data$LA) 
# reverse the levels so we get Aberdeen City first
UE_data$LA <- factor(UE_data$LA, levels = rev(levels(UE_data$LA))) 

# plot
ggplot(data=UE_data, aes(x=Year, y=LA))+
  geom_tile(aes(fill=UE))+
  scale_fill_gradient(low="White", high="red")

# save out - use the follow settings for larger plot/map
# ggsave("UE_heatmap.pdf", scale=3, dpi=400)




