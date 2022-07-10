library(RSelenium) # Use browser with R-Code (e.g firefox)
library(rvest)    # Scrape data from html
library(stringr)    # Handle strings
library(tidyr)      # Data cleaning
library(dplyr)      # Data cleaning
library(readr)      # Read data in data 
library(lubridate)  # Date handling
library(devtools)   # R-Tools



# set work directory
setwd("~/proyectos/airfares-R")

# Upload data entry (Origin city, destination city)
flights <- read.csv('data.txt', 
                    header = TRUE,
                    sep = ',',
                    stringsAsFactors = FALSE)

# count number of rows
nbr_flights <- nrow(flights)

# URL sample to use in Kayak website
#test_url = paste0('https://www.kayak.es/flights/MAD-TCI/2022-01-04?sort=bestflight_a&fs=cfc=1')


# Set entry variables
origin <- flights[1]
destination <- flights[2]
departure_date <- today()+4
#departure_date <- today()+30 # let's always perform thirty days after today in order to keep affordable prices


# Build URL samples for cities. 
# to keep good performance we'll build 180 url's which get to 
# information from kayak for every 2 days.



# Function to combine two dataframes with different lengths
combine.df <- function(x, y) {
  rows.x <- nrow(x)
  rows.y <- nrow(y)
  if (rows.x > rows.y) {
    diff <- rows.x - rows.y
    df.na <- matrix(NA, diff, ncol(y))
    colnames(df.na) <- colnames(y)
    cbind(x, rbind(y, df.na))
  } else {
    diff <- rows.y - rows.x
    df.na <- matrix(NA, diff, ncol(x))
    colnames(df.na) <- colnames(x)
    cbind(rbind(x, df.na), y)
  }
}



# Start server
# server actually need to use a dockerized driver, hence,
# it's mandatory to install "docker tools" in local machine.
# Then, initialize driver up in local directory

# Note, if you are windows user please ensure you've installed WSL
# in your computer before using Docker Tools
# this script was actually run with Ubuntu WSL in Windows 10 Home Edition.





#for (i in 9:nbr_flights){
i = 8
  city_origin <- origin[i,1]
  city_dest <- destination[i,1]
  
  prices <-NULL
  stop <- NULL
  airlines <- NULL
  
  for (k in 1:77){
    url <-
      paste0('https://www.kayak.es/flights/',
             city_origin,
             '-',
             city_dest,
             '/',
             departure_date + k*2,
             '?sort=bestflight_a&fs=cfc=1')
    
    Sys.sleep(5)
    remDr <- remoteDriver(remoteServerAddr = "localhost", 
                          port = 4445L, 
                          browserName = "chrome")
    
    
    
    remDr$open(silent=TRUE)
    
    dep_date <- 
      departure_date + k*2 #every two days
    
    Sys.sleep(10) # sleep for 10 seconds
    
    remDr$navigate(url)
    
    # Go to url and hold for 10 seconds
    Sys.sleep(10)
    
    # Get source code and read html
    page <-
      remDr$getPageSource()
    
    
    text_xml <- 
      read_html(page[[1]])
    
    
    
    # Get prices
    prices <-
      text_xml %>%
      html_nodes('span.price-text') %>%
      html_text()%>%
      str_replace_all("[\n]" , "") %>%
      unlist(use.names = FALSE) %>%
      as.data.frame()
    
    
    
    
    # Get airlines
    airlines<-
      text_xml %>%
      html_nodes('span.codeshares-airline-names') %>%
      html_text()%>%
      str_replace_all("[\n]" , "") %>%
      unlist(use.names = FALSE) %>%
      as.data.frame()
    
    
    
    # Get kind of stops
    stop <- 
      text_xml %>%
      html_nodes('span.stops-text') %>%
      html_text()%>%
      str_replace_all("[\n]" , "") %>%
      unlist(use.names = FALSE) %>%
      as.data.frame()
    
    #set unique dataframe
    
    df_1 <- combine.df(prices,airlines)
    df_2 <- combine.df(df_1, stop)
    
    
    # Let's hold by 15 seconds
    Sys.sleep(15)
    
    # write txt file with every date 
    write.table(df_2, 
                file=paste0('~/proyectos/airfares-R/data_raw/',
                                                        city_origin,
                                                        '-',
                                                        city_dest,
                                                        '-',
                                                        dep_date,
                                                        '.txt'),
                                              row.names = FALSE,
                                              sep = ',')
    remDr$close()
    # remDr$quit()

  }
  # We hold by 10 more seconds
  Sys.sleep(10)
  
#}
# Repeat process for next destination

  
  

  
  
  

