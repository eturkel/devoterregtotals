# https://elections.delaware.gov/services/candidate/regtotals2012.shtml
#
#
#
print(date())

#set seed
set.seed(9)

#load necessary packages
library(tidyverse)
library(dplyr)
library(rvest)
library(httr)
library(magrittr)

#create objects to loop over
year <- seq(2010, 2019)
month <- c("01","02","03","04","05","06","07","08","09","10","11","12")
# meta dataset
data <- expand_grid(urls= c("https://elections.delaware.gov/reports/e70r2601_"), year, month) %>%
  mutate(urlname = paste0(urls, year, month, "01.shtml")) 

# fix the weird urls (different dates, different src)
metadata <- data %>%
  mutate(urlname = ifelse(year == 2012 & month == "06", "https://elections.delaware.gov/reports/e70r2601_20120611.shtml", urlname)) %>%
  mutate(urlname = ifelse(year == 2012 & month == "08", "https://elections.delaware.gov/reports/e70r2601_20120813.shtml", urlname)) %>%
  mutate(urlname = ifelse(year == 2010 & month == "03", "https://elections.delaware.gov/reports/e70r2601_20100303.shtml", urlname)) %>%
  mutate(urlname = ifelse(year == 2010 & month == "02", "https://elections.delaware.gov/reports/e70r2601.shtml", urlname))

#output folders
int_path <- file.path("..", "data", "monthly")

All_DE <-  lapply(metadata$urlname, function(x){
    print(paste("scraping url:", x))
    #define target uRL
    urlname <- x
    
    if(!http_error(urlname)) {
    #read-in table
    DE_2018_VoterRegMessy <- read_html(urlname) %>% 
      html_nodes(css = "pre") %>%
      html_text() %>%
      strsplit(split = "\n") %>%
      unlist() %>%
      .[. != ""]
    } else {
      message("could not find url")
      return(NA)
    }
    
    #fist do some hacky substitutions that will be needed later
    DE_2018_VoterRegMessy<-gsub(" OF  ","-OF-",DE_2018_VoterRegMessy)
    DE_2018_VoterRegMessy<-gsub("County Total", "County-Total",DE_2018_VoterRegMessy)
    
    #split up by county, first getting list numbers for each county
    kent.start<-grep("KENT COUNTY",DE_2018_VoterRegMessy)
    newcastle.start<-grep("NEW CASTLE COUNTY",DE_2018_VoterRegMessy)
    sussex.start<-grep("SUSSEX COUNTY",DE_2018_VoterRegMessy)
    statewide.start<-grep("STATEWIDE",DE_2018_VoterRegMessy)
    
    #now subset master list
    Kent<-DE_2018_VoterRegMessy[(kent.start+1):(newcastle.start-1)]
    NewCastle<-DE_2018_VoterRegMessy[(newcastle.start+1):(sussex.start-1)]
    Sussex<-DE_2018_VoterRegMessy[(sussex.start+1):(statewide.start-1)]
    Statewide<-DE_2018_VoterRegMessy[(statewide.start+1):length(DE_2018_VoterRegMessy)]
    
    #format and save Kent County
    # reading the fixed-width data. Width here looks like 12 for each column (changed to prevent overwrite of missing values)
    Kent.C<-read_fwf(Kent, col_positions = fwf_empty(Kent, col_names = c("District" , "Democrats", "Republicans",
                                                                     "Others", "Total"), skip = 1, n = 300), skip = 1)
    Kent.County<-Kent.C %>%
      separate(col = District, into = c("ED", "RD"), sep = "-OF-") %>%
      filter(!str_detect(ED, "[a-z]+")) %>%
      mutate(County = "Kent")
    
    #format and save New Castle County,  n given 300
    NewCastle.C<-read_fwf(NewCastle, col_positions = fwf_empty(NewCastle, col_names = c("District" , "Democrats", "Republicans",
                                                                                   "Others", "Total"), skip = 1, n = 300), skip = 1)
    NewCastle.County<-NewCastle.C %>%
      separate(col = District, into = c("ED", "RD"), sep = "-OF-") %>%
      filter(!str_detect(ED, "[a-z]+")) %>%
      mutate(County = "New Castle")

    #format and save Sussex County
    Sussex.C<-read_fwf(Sussex, col_positions = fwf_empty(Sussex, col_names = c("District" , "Democrats", "Republicans",
                                                                             "Others", "Total"), skip = 1, n = 300), skip = 1)
    
    Sussex.County<-Sussex.C %>%
      separate(col = District, into = c("ED", "RD"), sep = "-OF-") %>%
      filter(!str_detect(ED, "[a-z]+")) %>%
      mutate(County = "Sussex")
    
    #format and save statewide
    Statewide<-gsub("New Castle","New-Castle",Statewide)
    Statewide<-matrix(unlist(strsplit(Statewide, "\\s+")),ncol=5,byrow=TRUE)
    Statewide<-gsub("-OF-"," OF ",Statewide)
    Statewide<-gsub("County-Total", "County Total",Statewide)
    Statewide<-gsub("New-Castle", "New Castle",Statewide)
    Statewide<-gsub(",", "",Statewide) #Not Necessary
    Statewide<-as.data.frame(Statewide)
    write.table(Statewide,file.path(int_path, paste0("Statewide.",substr(x,49,56), ".csv")),sep=",",row.names=FALSE,col.names=FALSE)
    
    All_DE <- rbind(Kent.County, NewCastle.County, Sussex.County) %>%
      mutate(Url = x) 
    if(any(is.character(All_DE$Democrats), is.character(All_DE$Republicans), is.character(All_DE$Others))) {
      All_DE <- All_DE %>%
        mutate_at(vars(Democrats, Republicans, Others, Total), function(x) parse_number(x, trim_ws = TRUE))
    }
    #write.table(All_DE, file.path(int_path, paste0("All_DE.",substr(x,49,56),".csv")), sep=",", row.names=FALSE)
    
    All_DE
  })

#read intermediate and combine
full_DE <- All_DE[!is.na(All_DE)] %>%
  bind_rows()

full_data <- full_DE %>%
  left_join(metadata[,-1], by = c("Url" = "urlname"))

# write final data file
write_csv(full_data, file.path("data", "DE_2010_2019.csv"))

# Verify scrape:
# check if I can derive Statewide total for any year/month
full_data %>%
  group_by(County) %>%
  filter(month == "06", year == 2013) %>%
  count(wt = Democrats)

#The end
