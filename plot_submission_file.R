library(ggplot2)
library(RCurl)
library(dplyr)
## utility functions
date_in_week <- function(year, week, weekday){
  w <- paste0(year, "-W", sprintf("%02d", week), "-", weekday)
  ISOweek2date(w)
}
transform <- function(value,type){
  if (type == "cum"){
    return(value)
  } else{
    return (c(value[1],diff(value)))
  }
}
## submission_file reading
submission_file <- read.csv("dan_submission_file.csv")

for (type in c("cum","inc")){
    submission_file_point <- submission_file[submission_file$type == "point" & submission_file$target %in% paste0(1:4, " wk ahead ",type," death"),]
    
    
    submission_file_qauntile_upper <- submission_file[submission_file$quantile ==.975 & submission_file$target %in% paste0(1:4, " wk ahead ",type," death"),]
    submission_file_qauntile_lower <- submission_file[submission_file$quantile ==.025 & submission_file$target %in% paste0(1:4, " wk ahead ",type," death"),]
    submission_file_qauntile_upper <- submission_file_qauntile_upper[complete.cases(submission_file_qauntile_upper),]
    submission_file_qauntile_lower <- submission_file_qauntile_lower[complete.cases(submission_file_qauntile_lower),]
    
    
    submission_file_qauntile_upper$upper <- submission_file_qauntile_upper$value
    submission_file_qauntile_upper$lower <- submission_file_qauntile_lower$value
    
    
    
    
    library(dplyr)
    
    ## read in covid data
    x <- getURL("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Cumulative%20Deaths.csv")
    y <- read.csv(text = x)
    y_states <- y[!grepl("County",y$location_name) , ]
    y_states_date_subset <- y_states[y_states$date <= "2020-08-09" &y_states$date >= "2020-03-15" ,]
    ## merge observed data and submission file
    library(lubridate)
    y_states_date_max_diffs_week <- y_states_date_subset %>% group_by(location,week = week(date))
    y_states_date_max_diffs_week <- y_states_date_max_diffs_week %>% group_by(location,week) %>% summarize(value=tail(value,1))
    y_states_date_max_diffs <- y_states_date_max_diffs_week %>% group_by(location) %>% summarize(diff=max(diff(value)))
    
    
    # convert to date
    y_states_date_subset$date <- as.Date(y_states_date_subset$date)
    y_states_date_subset$week <- week(y_states_date_subset$date)
    submission_file_point$target_end_date <- as.Date(submission_file_point$target_end_date)
    y_states_date_subset_weekly <- y_states_date_subset %>% group_by(location,date = week(date)) %>% summarize(value=tail(value,2)[1])
    ## plot cumulative 
   
    y_states_date_subset_weekly$date<- date_in_week(year = 2020, week = y_states_date_subset_weekly$date +1 , weekday = 1) -2
    
    
    unique_locations <- unique(submission_file_point$location )
    plot_list <-list()
    plot_list_itr <- 1
    for (location in unique_locations){
      if (type=="cum"){
      plot_df <- ggplot(submission_file_point[submission_file_point$location == location,],aes(x=target_end_date,y=value)) + geom_point() + 
        geom_point(data=y_states_date_subset_weekly[y_states_date_subset_weekly$location ==location,],aes(x=as.Date(date),y=value))+
        geom_text(data=y_states_date_max_diffs[y_states_date_max_diffs$location == location,],aes(x=tail(as.Date(y_states_date_subset$date),1)[1],y=mean(submission_file_point[submission_file_point$location == location,]$value)/2,label=paste0(diff)))+
        facet_wrap(~location,scales="free") + 
        geom_vline(xintercept=tail(as.Date(y_states_date_subset$date),1))+theme(axis.text.x=element_blank()) + theme_bw()
       plot_df <- plot_df + geom_ribbon(data=submission_file_qauntile_upper[submission_file_qauntile_upper$location == location,],
                                       aes(x=as.Date(target_end_date),ymin=lower,ymax=upper),alpha=.5,col='cornflowerblue')
      } else{
        plot_df <- ggplot(submission_file_point[submission_file_point$location == location,],aes(x=target_end_date,y=value)) + geom_point() + 
          geom_point(data=y_states_date_subset_weekly[y_states_date_subset_weekly$location ==location,],aes(x=as.Date(date),y=c(value[1],diff(value))))+
          geom_text(data=y_states_date_max_diffs[y_states_date_max_diffs$location == location,],aes(x=tail(as.Date(y_states_date_subset$date),1)[1],y=mean(submission_file_point[submission_file_point$location == location,]$value)/2,label=paste0(diff)))+
          facet_wrap(~location,scales="free") + 
          geom_vline(xintercept=tail(as.Date(y_states_date_subset$date),1))+theme(axis.text.x=element_blank()) + theme_bw()
        plot_df <- plot_df + geom_ribbon(data=submission_file_qauntile_upper[submission_file_qauntile_upper$location == location,],
                                         aes(x=as.Date(target_end_date),ymin=lower,ymax=upper),alpha=.5,col='cornflowerblue')
      }
      
      plot_list[[plot_list_itr]] <- plot_df
      plot_list_itr <- plot_list_itr +1
      print (plot_list_itr)
    }
    ## get largest difference in historical data 
    library(gridExtra)
    
    pdf(paste0("plots_",type,".pdf"), onefile = TRUE)
    for (i in seq(length(plot_list))) {
      print(plot_list[[i]])  
    }
    dev.off()
}


