################################################################################
# GOAL                                                                         #
################################################################################

# All functions for the script

### TODO:
# Add error handling for missing dates when scraping (read_table)
# Create function to write dataframes to .csv


################################################################################
# LIBRARIES                                                                    #
################################################################################

# load (un)necessary packages
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse
  ,rvest
  ,here
  ,glue)


################################################################################
# FUNCTIONS                                                                    #
################################################################################

# create_data_range

create_date_range <- 
  function(start_date, 
           end_date) {
    
    seq.Date(from = as.Date(start_date),
             to = as.Date(end_date),
             by = "months")
    
  }


# read_table

read_table <- 
  function(date) {
    
    clean_date <- 
      str_remove_all(date, "-")
    
    url <- 
      glue::glue("https://elections.delaware.gov/reports/e70r2601_{clean_date}.shtml")
    
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center[2]/pre') %>% #html_nodes acts as a selector
      html_text() %>%
      strsplit(split = "\n") %>%
      unlist() %>%
      .[. != ""]
    
  }


# extract_counties

extract_counties <- 
  function(.x) {
    
    .x <- 
      gsub(" OF ", "-OF-", .x)
    
    .x <- 
      gsub("County Total", "County-Total", .x)
    
    .x <- 
      gsub("New Castle", "New-Castle", .x)
    
    # split up by county, first getting list numbers for each county
    kent_start      <- grep("KENT COUNTY", .x)
    newcastle_start <- grep("NEW CASTLE COUNTY", .x)
    sussex_start    <- grep("SUSSEX COUNTY", .x)
    statewide_start <- grep("STATEWIDE", .x)
    
    # now subset master list
    Kent      <- .x[(kent_start):(newcastle_start - 1)]
    NewCastle <- .x[(newcastle_start):(sussex_start - 1)]
    Sussex    <- .x[(sussex_start):(statewide_start - 1)]
    Statewide <- .x[(statewide_start):length(.x)]
    
    return(list(Kent = Kent,
                NewCastle = NewCastle,
                Sussex = Sussex,
                Statewide = Statewide))
    
  }


# counties_to_df

county_to_df <- 
  function(x) {
    
    county_name <- x[1]
    
    x <- x[2:length(x)]
    
    county <-
      matrix(unlist(strsplit(x, "\\s+")), ncol = 5, byrow = TRUE)
    
    county_column_names <- county[1,]
    
    county <- county[-1,]
    county <- gsub("-", " ", county)
    county <- gsub(",", "", county) #Not Necessary
    
    county_df <- as.data.frame(county, stringsAsFactors = F)
    colnames(county_df) <- county_column_names
    
    
    county_df <- subset(county_df, !grepl("Total", county_df[, 1]))
    
    if (county_name != "STATEWIDE") {
      
      county_df <- 
        county_df %>% 
        mutate(County = county_name,
               ED = str_extract(District, "^[0-9]+"),
               RD = str_extract(District, "[0-9]+$")) %>% 
        select(County, ED, RD, Democrats, Republicans, Others, Total)
      
    }
    
    county_df <-
      county_df %>%
      mutate_at(vars(Democrats, Republicans, Others, Total), as.numeric)
    
    return(county_df)
    
  }

# get_voter_registration

get_voter_registration <-
  function(date) {
    
    voter_registration_raw <-
      read_table(date)
    
    Kent <- 
      counties_2017$Kent %>% 
      county_to_df()
    
    NewCastle <- 
      counties_2017$NewCastle %>% 
      county_to_df()
    
    Sussex <- 
      counties_2017$Sussex %>% 
      county_to_df()
    
    Statewide <- 
      counties_2017$Statewide %>% 
      county_to_df() %>% 
      mutate(Date = as.Date(date))
    
    counties <-
      Kent %>% 
      union_all(NewCastle) %>% 
      union_all(Sussex) %>% 
      mutate(Date = as.Date(date))
    
    return(list(Counties = counties,
                State = Statewide))
    
  }