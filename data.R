get_src <- function(){
  # locations <- read_csv("data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
  #   dplyr::select(
  #     `Country/Region`,
  #     Lat,
  #     Long
  #   ) %>% 
  #   group_by(
  #     `Country/Region`
  #   ) %>% 
  #   summarise(
  #     Lat = mean(Lat, na.rm=T),
  #     Long = mean(Long, na.rm=T)
  #   )
  # 
  # # (ggplot(locations, aes(x = Long, y = Lat, colour = `Country/Region`)) + geom_point()) %>% plotly::ggplotly()
  
  confirmed_set <- read_csv("data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
    dplyr::select(
      -Lat,
      -Long
    ) %>% 
    pivot_longer(
      cols = -c('Province/State', 'Country/Region'), 
      names_to = "Date", 
      values_to = "Confirmed Cases"
    ) %>% 
    mutate(
      Date = as.Date(Date, "%m/%d/%y")
    )
  
  # deaths_set <- read_csv("data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") %>%
  deaths_set <- read_csv("data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
    dplyr::select(
      -Lat,
      -Long
    ) %>% 
    pivot_longer(
      cols = -c('Province/State', 'Country/Region'), 
      names_to = "Date", 
      values_to = "Deaths"
    ) %>% 
    mutate(
      Date = as.Date(Date, "%m/%d/%y")
    )
  
  # recovered_set <- read_csv("data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") %>% 
  recovered_set <- read_csv("data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") %>% 
    dplyr::select(
      -Lat,
      -Long
    ) %>% 
    pivot_longer(
      cols = -c('Province/State', 'Country/Region'), 
      names_to = "Date", 
      values_to = "Recoveries"
    ) %>% 
    mutate(
      Date = as.Date(Date, "%m/%d/%y")
    )
  
  # Some countries do not track data against each state for all three of confirmed cases, deaths and recoveries
  # For any that do not, the state has to be converted to "Mainland"
  mainland_countries <- c()
  for(country in unique(confirmed_set$`Country/Region`)){
    convert_to_mainland = F
    if(any(is.na((confirmed_set %>% filter(`Country/Region` == country))$`Province/State`))){
      convert_to_mainland = T
    }
    if(any(is.na((deaths_set %>% filter(`Country/Region` == country))$`Province/State`))){
      convert_to_mainland = T
    }
    if(any(is.na((recovered_set %>% filter(`Country/Region` == country))$`Province/State`))){
      convert_to_mainland = T
    }
    
    if(convert_to_mainland){
      mainland_countries = c(mainland_countries, country)
    }
  }
  
  confirmed_set_country <- confirmed_set %>% 
    group_by(Date, `Country/Region`) %>% 
    summarise(`Confirmed Cases` = sum(`Confirmed Cases`, na.rm=T)) %>% 
    mutate(`Province/State` = "Mainland") %>% 
    ungroup()
  
  deaths_set_country <- deaths_set %>% 
    group_by(Date, `Country/Region`) %>% 
    summarise(Deaths = sum(Deaths, na.rm=T)) %>% 
    mutate(`Province/State` = "Mainland") %>% 
    ungroup()
  
  recovered_set_country <- recovered_set %>% 
    group_by(Date, `Country/Region`) %>% 
    summarise(Recoveries = sum(Recoveries, na.rm=T)) %>% 
    mutate(`Province/State` = "Mainland") %>% 
    ungroup()
  
  
  src <- confirmed_set %>% filter(!(`Country/Region` %in% mainland_countries)) %>% 
    left_join(
      deaths_set %>% filter(!(`Country/Region` %in% mainland_countries)),
      by = c(
        'Date', 'Province/State', 'Country/Region'
      )
    ) %>% 
    left_join(
      recovered_set %>% filter(!(`Country/Region` %in% mainland_countries)),
      by = c(
        'Date', 'Province/State', 'Country/Region'
      )
    ) %>% 
    bind_rows(
      confirmed_set_country %>% filter((`Country/Region` %in% mainland_countries)) %>% 
        left_join(
          deaths_set_country %>% filter((`Country/Region` %in% mainland_countries)),
          by = c(
            'Date', 'Province/State', 'Country/Region'
          )
        ) %>% 
        left_join(
          recovered_set_country %>% filter((`Country/Region` %in% mainland_countries)),
          by = c(
            'Date', 'Province/State', 'Country/Region'
          )
        )
    ) %>% 
    mutate(
      Removed = Recoveries + Deaths
    ) %>% 
    mutate(
      `Active Cases` = `Confirmed Cases` - Removed
    )
  
  
  ## Split out Hong Kong
  src$`Country/Region`[src$`Province/State` == "Hong Kong"] <- "Hong Kong"
  
  ## On the date of March 12, the number of cases increased by remarkably little, and was followed by a remarkably high increase on March 13. It can be assumed then that cases were simply not reported on March 12 and were followed up on March 13. For each country the geometric growth rate on these two days will be taken as the average of the two days.
  
  correct_confirmed <- function(
    Date,
    `Confirmed Cases`
  ){
    mar_11 <- `Confirmed Cases`[Date == as.Date("2020-03-11")]
    mar_12 <- `Confirmed Cases`[Date == as.Date("2020-03-12")]
    mar_13 <- `Confirmed Cases`[Date == as.Date("2020-03-13")]
    
    rate_12 <- mar_12/mar_11
    rate_13 <- mar_13/mar_12
    
    rate <- mean(c(rate_13, rate_12))
    
    `Confirmed Cases`[Date == as.Date("2020-03-12")] <- mar_11*rate
    `Confirmed Cases`[Date == as.Date("2020-03-13")] <- mar_11*rate*rate
    
    return(`Confirmed Cases`)
  }
  
  src <- src %>% 
    group_by(`Country/Region`, `Province/State`) %>% 
    arrange(Date) %>% 
    mutate(
      `Confirmed Cases` = correct_confirmed(Date, `Confirmed Cases`),
      `Growth Rate` = `Active Cases`/lag(`Active Cases`)
    ) %>% 
    ungroup()
  
  return(src)
}

get_country_data <- function(src){
  country_data <- src %>% 
    group_by(
      Date,
      `Country/Region`
    ) %>% 
    summarise(
      `Confirmed Cases` = sum(`Confirmed Cases`, na.rm=T),
      Deaths = sum(Deaths, na.rm=T),
      Recoveries = sum(Recoveries, na.rm=T),
      Removed = sum(Removed, na.rm=T),
      `Active Cases` = sum(`Active Cases`, na.rm=T)
    ) %>% 
    ungroup() %>% 
    group_by(`Country/Region`) %>% 
    arrange(Date) %>% 
    mutate(
      `Growth Rate` = `Active Cases`/lag(`Active Cases`)
    ) %>% 
    ungroup()
  
  return(country_data)
}

get_growth_rates <- function(country_data){
  growth_rates <- country_data %>% 
    group_by(
      `Country/Region`
    ) %>% 
    summarise(
      `Recent Growth` = mean(tail(`Growth Rate`, 5), na.rm = T),
      `Active Cases` = tail(`Active Cases`, 1),
      `Confirmed Cases` = tail(`Confirmed Cases`, 1)
    ) %>% 
    mutate(
      `Recent Growth` = ifelse(
        is.na(`Recent Growth`) | is.infinite(`Recent Growth`),
        1,
        ifelse(
          `Recent Growth` == 0,
          1,
          `Recent Growth`
        )
      )
    ) %>% 
    ungroup() %>% 
    # arrange(`Recent Growth`) %>% 
    arrange(`Country/Region`) %>% 
    mutate(
      `Country/Region` = factor(`Country/Region`, levels = `Country/Region`, ordered = T)
    ) %>%
    arrange(desc(`Recent Growth`)) %>% 
    mutate(
      `Recovery Factor` = log(`Confirmed Cases` / `Active Cases`)
    )
  
  # Set max recovery factor to the order of magnitude of the highest finite number
  highest_finite_recovery <- max(growth_rates$`Recovery Factor`[!is.infinite(growth_rates$`Recovery Factor`)], na.rm=T)
  highest_magnitude <- 10^ceiling(log10(highest_finite_recovery))
  growth_rates$`Recovery Factor`[is.infinite(growth_rates$`Recovery Factor`)] <- highest_magnitude
  growth_rates$`Recovery Factor`[growth_rates$`Recovery Factor` > highest_magnitude] <- highest_magnitude
  
  growth_threshold = 1.005
  active_threshold = 1000
  recovery_threshold = 1.5
  
  growth_rates$State <- NA
  growth_rates$State[(growth_rates$`Recovery Factor` >= recovery_threshold) & (growth_rates$`Recent Growth` <= growth_threshold) & growth_rates$`Active Cases` <= active_threshold] <- "Recovered"
  growth_rates$State[(growth_rates$`Recovery Factor` >= recovery_threshold) & (growth_rates$`Recent Growth` <= growth_threshold) & growth_rates$`Active Cases` > active_threshold] <- "Recovering"
  growth_rates$State[(growth_rates$`Recovery Factor` >= recovery_threshold) & (growth_rates$`Recent Growth` > growth_threshold) & growth_rates$`Active Cases` <= active_threshold] <- "Re-emergence"
  growth_rates$State[(growth_rates$`Recovery Factor` >= recovery_threshold) & (growth_rates$`Recent Growth` > growth_threshold) & growth_rates$`Active Cases` > active_threshold] <- "Slowed"
  growth_rates$State[(growth_rates$`Recovery Factor` < recovery_threshold) & (growth_rates$`Recent Growth` <= growth_threshold) & growth_rates$`Active Cases` <= active_threshold] <- "Preventing"
  growth_rates$State[(growth_rates$`Recovery Factor` < recovery_threshold) & (growth_rates$`Recent Growth` <= growth_threshold) & growth_rates$`Active Cases` > active_threshold] <- "Active Countermeasures"
  growth_rates$State[(growth_rates$`Recovery Factor` < recovery_threshold) & (growth_rates$`Recent Growth` > growth_threshold) & growth_rates$`Active Cases` <= active_threshold] <- "Outbreak"
  growth_rates$State[(growth_rates$`Recovery Factor` < recovery_threshold) & (growth_rates$`Recent Growth` > growth_threshold) & growth_rates$`Active Cases` > active_threshold] <- "Plague"
  growth_rates$State[growth_rates$`Active Cases` == 0] <- "Eradicated"
  
  growth_rates$State <- factor(growth_rates$State)
  
  return(growth_rates)
}