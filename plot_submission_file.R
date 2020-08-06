library(ggplot2)
library(RCurl)
library(dplyr)

## submission_file reading
submission_file <- read.csv("/Users/gcgibson/sub531/2020-07-27-Umass-MechBayes-CORRECT.csv")
submission_file_point <- submission_file[submission_file$type == "point" & submission_file$target %in% paste0(1:4, " wk ahead cum death"),]

## read in covid data
x <- getURL("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Cumulative%20Deaths.csv")
y <- read.csv(text = x)
y_states <- y[!grepl("County",y$location_name) , ]
y_states_date_subset <- y_states[y_states$date <= "2020-07-27" &y_states$date >= "2020-05-27" ,]
## merge observed data and submission file
library(lubridate)
y_states_date_max_diffs_week <- y_states_date_subset %>% group_by(location,week = week(date))
y_states_date_max_diffs_week <- y_states_date_max_diffs_week %>% group_by(location,week) %>% summarize(value=tail(value,1))
y_states_date_max_diffs <- y_states_date_max_diffs_week %>% group_by(location) %>% summarize(diff=max(diff(value)))


# convert to date
y_states_date_subset$date <- as.Date(y_states_date_subset$date)
submission_file_point$target_end_date <- as.Date(submission_file_point$target_end_date)

unique_locations <- unique(submission_file_point$location )

for (location in unique_locations){
  plotdf <- ggplot(submission_file_point[submission_file_point$location == location,],aes(x=target_end_date,y=value)) + geom_point() + 
    geom_point(data=y_states_date_subset[y_states_date_subset$location ==location,],aes(x=date,y=value))+
    geom_text(data=y_states_date_max_diffs[y_states_date_max_diffs$location == location,],aes(x=tail(y_states_date_subset$date,1)[1],y=mean(y_states_date_subset$value)/2,label=paste0("Largest Weekly Diff ",diff)))+
    facet_wrap(~location,scales="free") + 
    geom_vline(xintercept=tail(y_states_date_subset$date,1))+theme(axis.text.x=element_blank()) + theme_bw()
   ggsave(filename = location,plot = plotdf,device = "png")
}
## get largest difference in historical data 



  ## merge in 

