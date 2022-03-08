setwd("C:/Users/Yanjia/Desktop/research/BU Infection/covid/BU and CDC")

#will it work?

#########################
##### BU COVID DATA #####
#########################

BUcovid = read.csv("BU covid.csv")
str(BUcovid)

range(BUcovid$Negative)

range(BUcovid$Positive)

range(BUcovid$Results)

sum(BUcovid$Positive)

## convert str date into datetime 
Order.Date = as.Date(BUcovid$Order.Date, format = "%m/%d/%y")
BUcovid$Order.Date = Order.Date
BUcovid$ResultDate = as.Date(BUcovid$ResultDate, format = "%m/%d/%y")
str(BUcovid)

## Check if there is any crossover positive and negative
# table(BUcovid$Positive, BUcovid$Negative)

### negative has 2, 3, ... values

###############################
##### MA Death COVID DATA #####
###############################

deaths = read.csv("covid_deaths_usafacts.csv", header = TRUE)


deaths_MA = deaths[deaths$State == "MA",]
deaths_MA = as.data.frame(t(deaths_MA))

names(deaths[1:4])

col_name = c(names(deaths[1:4]), 'date', 'cases')



countysFIPS = unlist(unique(deaths_MA[1,]), use.names=FALSE)

countyName = unlist(unique(deaths_MA[2,]), use.names=FALSE) 

state = unlist(unique(deaths_MA[3,]), use.names=FALSE)

stateFIPS = unlist(unique(deaths_MA[4,]), use.names=FALSE) 
  
length(state)  



# row.names(deaths_MA)[1:5]
n = nrow(deaths_MA)-4

colum_fun <-  function(variable_name, uni_values, n_rep){
  
  variable_name = c()
  
  for (i in 1: 15){
    temp = c(rep( uni_values[i], n_rep))
    
    if (i == 1) {
      variable_name = temp
    } else  { variable_name =  c(variable_name,temp)
    
    }
  }
return(variable_name)
}

countys_FIPS = colum_fun(countys_FIPS,countysFIPS,n )
county_Name = colum_fun(county_Name, countyName, n)
state_Name =  colum_fun(state_Name, state, n)
state_FIPS = colum_fun(state_FIPS, stateFIPS, n)

length(state_FIPS)

deaths_MA[5:nrow(deaths_MA) ,5]



for (i in 1:length(countysFIPS)){
  cases = deaths_MA[5:nrow(deaths_MA), deaths_MA[2]==countysFIPS[i]]
  
}  

cases = deaths_MA[5:nrow(deaths_MA), deaths_MA[2, ]==countysFIPS[i]]

for (i in 1:length(countysFIPS)){
   temp = c(rep(countysFIPS[i], n))
  if (i == 1) {
    countys_FIPS = temp
  } else  { countys_FIPS =  c(countys_FIPS,temp)
  
  }
}



n*15*2



head(deaths_MA)
ncol(deaths_MA)
date = names(deaths[1,])[5:773]

date_death = as.Date(date)

deaths[1, 1:10]



head(deaths_MA[1:5, ])

county_name = deaths_MA[2,]

county_name



death_new = as.data.frame(t(deaths_MA))
str(death_new)

head(death_new)

library(dplyr)

test = deaths_MA %>% select(starts_with('x'))

date = names(test)


is.data.frame(deaths_MA)

str(date[1])


##################################################
##### MA COVID Incidence and Wastewater DATA #####
##################################################



cases = read.csv("covid19_wastewater/cases_by_county.csv")
str(cases)
# 
# library(anytime)
# 
# test = anytime(cases$date)
# cases["date_time"] = test

cases['date_time'] = as.Date(cases$date)

cases_MA = cases[cases$state=="MA", ]
table(cases_MA$state)


wastewater = read.csv("covid19_wastewater/wastewater_by_county.csv")
str(wastewater)


# wastewater["date_time"] = anytime(wastewater$sampling_week)

wastewater["date_time"] = as.Date(wastewater$sampling_week)




wastewater_MA = wastewater[wastewater$state=="MA",]
table(wastewater_MA$state)


cases_MA["Weekday"] = strftime(cases_MA$date, format = "%A")

head(cases_MA)

wastewater_MA["Weekday"] = strftime(wastewater_MA$sampling_week, format = "%A")

table(wastewater_MA$Weekday)

head(wastewater_MA)


county_names = names(table(cases_MA$name))


Berkshire_cases = cases_MA[cases_MA$name==county_names[1], ]

head(Berkshire_cases)

Berkshire_water = wastewater_MA[wastewater_MA$name ==county_names[1], ]



# install.packages("zoo")

library(zoo)


plots <- function (cases_data, water_data, county_name, cumula_day ){
  
  cases= cases_data[cases_data$name==county_name & cases_data$Weekday== 'Wednesday', ]
  
  prevalence = rollapply(cases$rolling_average_cases_per_100k_centered, cumula_day, FUN = sum)
  
  
  add = cases$rolling_average_cases_per_100k_centered[1:cumula_day-1]
  # add
  prevalence = c(add, prevalence)
  
  length(cases$rolling_average_cases_per_100k_centered)
  length(prevalence)
  
  water = water_data[water_data$name ==county_name, ]
  
  max = max(water$normalized_concentration_rolling_average)*1.05
  
  plot(cases$date_time, cases$rolling_average_cases_per_100k_centered,
       type = "l",
       col = 2,
       #labels = format(Berkshire$date_time, "%Y-%m"),
       ylim = c(0, max),
       #xlab = "Year",
       main = county_name,
       ylab = "Values")
  
  
  lines(water$date_time, water$normalized_concentration_rolling_average,
        type = "l",
        col = 3)
  
  lines(cases$date_time, prevalence,
        type = "l",
        col = 4)
  
  legend("topleft",                           # Add legend to plot
         c("Cases", "Wastewater", "Prevalence"),
         lty = 1,
         col = 2:4)
  
}

for (i in 1:6){
  
  plots(cases_MA, wastewater_MA, county_names[i], 7)

  # plots(cases_MA, wastewater_MA, county_names[1], 10)
}

# set the same length of the cases and wastewater

Berkshire_cases


start_date = max(range(Berkshire_cases$date_time)[1], range(Berkshire_water$date_time)[1])

end_date = min(range(Berkshire_cases$date_time)[2], range(Berkshire_water$date_time)[2])

date2 = Berkshire_water[Berkshire_water$date_time <= end_date,]$date_time

date3 = Berkshire_water[Berkshire_water$date_time <= end_date-2,]$date_time +2



cumula_day = 7
prevalence = rollapply(Berkshire_cases$rolling_average_cases_per_100k_centered, cumula_day, FUN = sum)
add = Berkshire_cases$rolling_average_cases_per_100k_centered[1:cumula_day-1]
# add
prevalence = c(add, prevalence)

Berkshire_cases['prevalence'] = prevalence

Berkshire_cases2 = Berkshire_cases[Berkshire_cases$date_time %in% date2, ]

Berkshire_water2 = Berkshire_water[Berkshire_water$date_time %in% date2, ]


Berkshire_cases3 = Berkshire_cases[Berkshire_cases$date_time %in% date3, ]

head(cases_MA)

# Berkshire_water3 = Berkshire_water[Berkshire_water$date_time %in% date2, ]


# cross-correlation function

# cor.test(Berkshire_cases2$prevalence, Berkshire_water2$normalized_concentration_rolling_average,method = "spearman")
# 
# cor.test(Berkshire_cases2$rolling_average_cases_per_100k_centered, Berkshire_water2$normalized_concentration_rolling_average,method = "spearman")
# 

#  same day for the week

ccfvalues = ccf(Berkshire_water2$normalized_concentration_rolling_average, Berkshire_cases2$prevalence ) 
ccfvalues


ccfvalues = ccf( Berkshire_water2$normalized_concentration_rolling_average, Berkshire_cases3$prevalence) 
ccfvalues

# cases are two days later with the wastewater date

ccfvalues = ccf(Berkshire_water2$normalized_concentration_rolling_average, Berkshire_cases2$rolling_average_cases_per_100k_centered ) 
ccfvalues

ccfvalues = ccf(Berkshire_water2$normalized_concentration_rolling_average, Berkshire_cases3$rolling_average_cases_per_100k_centered) 
ccfvalues



# install.packages('astsa')
library(astsa)

lag2.plot (Berkshire_cases2$rolling_average_cases_per_100k_centered, Berkshire_water2$normalized_concentration_rolling_average, 6)


lag2.plot ( Berkshire_water2$normalized_concentration_rolling_average,Berkshire_cases2$rolling_average_cases_per_100k_centered, -3:3 )
