
setwd("C:/Users/Yanjia/Desktop/research/BU Infection/covid/BU and CDC")

#############################################
###### Covid Cases ##########################
#############################################

cases_o = read.csv("town positive3_23.csv", header = TRUE)

town_cases = data.frame(cases_o)


  
c(nrow(town_cases), ncol(town_cases))

################## records counts

date = names(town_cases)[2:ncol(town_cases)]

sample_date[sample_date == "2020-10-23"]

sample_date = as.Date(substring(date, 2), format = "%m.%d.%Y")


length(sample_date)

Weekday = strftime(sample_date, format = "%A")

barplot(table(Weekday), main = 'Number of Reports from Covid')


###################### town names 

North_town = c("Boston","Waltham", "Lexington", "Bedford",
              "Burlington", "Wilmington", "Reading", "Wakefield","Stoneham",
              "Woburn", "Winchester", "Arlington", "Medford", "Melrose",
              "Malden", "Revere", "Everett", "Chelsea", "Somerville", "Belmont",
              "Watertown", "Cambridge", "Winthrop")

#"Brookline", "Newton",
 

South_town = c("Ashland", "Framingham", "Natick", "Wellesley", "Newton", "Needham",
               "Brookline", "Boston", "Dedham", "Westwood", "Norwood", "Walpole", "Milton",
               "Canton", "Stoughton", "Quincy", "Braintree", "Randolph", "Holbrook",
               "Weymouth", "Hingham")

c(length(North_town), length(South_town))

#### Q : for the cross over cities? 
#### Q : for counts < 5, assume it is 2.5 for better check.  

town_cases[town_cases == "<5"] = 2.5

##### check missing dates 


date_range = seq(min(sample_date), max(sample_date), by = 1) 
date_range[!date_range %in% sample_date]

Weekday = strftime(date_range, format = "%A")

date_list = data.frame("sample_date" = date_range,
                       "Weekday" = Weekday)

#### use the date range instead of the original dates


########### transpose the dataframe

cases = function(data_name, Town_Name, date_range){
   
   part_cases = data_name[data_name[,1] %in% Town_Name, ] 
   n = ncol(part_cases)
   date = names(part_cases)[2:n]
   case_date = as.Date(substring(date, 2), format = "%m.%d.%Y")
   
   col_names = c('sample_date', Town_Name)
   
   cases = data.frame(matrix(ncol=length(col_names), nrow= length(case_date)))
   
   colnames(cases) = col_names
   
   cases$sample_date = case_date
   # cases$Weekday = Weekday
   
   for (i in Town_Name ){
     
     cases[i] = as.numeric(unlist(part_cases[part_cases$X == i, 2:n], use.names = FALSE))
   }
   
   cases_all = merge(date_range, cases, by ='sample_date', all.x=TRUE)
   
   return(cases_all)
}


####### North

North_town_cases = cases(town_cases, North_town, date_list)

tail(North_town_cases)

sum(North_town_cases[,2:ncol(North_town_cases)] == 2.5)

range(North_town_cases$case_date)

tail(North_town_cases)

c(nrow(North_town_cases), ncol(North_town_cases))


# n = rep("NA", length(North_town))
# for (i in 1:length(North_town)){
#   n[i] = sum(is.na(North_town_cases[,i+2]))
# }


###### South 

South_town_cases = cases(town_cases, South_town, date_list)

sum(South_town_cases[,2:ncol(South_town_cases)] == 2.5)/(nrow(South_town_cases)*(ncol(South_town_cases-2)))


range(South_town_cases$case_date)

tail(South_town_cases)

c(nrow(South_town_cases), ncol(South_town_cases))

# n = rep("NA", length(South_town))
# for (i in 1:length(South_town)){
#   n[i] = sum(is.na(South_town_cases[,i+2]))
# }
# 
# South_nas = South_town_cases[is.na(South_town_cases$Ashland),names ]
# 
# missing = barplot(table(South_nas$Weekday), main="Borplot for Missing Dates",ylim = c(0, max(table(North_nas$Weekday))+5), ylab = "Frequency")
# 
# text(missing,table(South_nas$Weekday)+1, paste(table(South_nas$Weekday)) ,cex=1) 

### all the dates are the same North/ South

n_missing = 90
names = c("sample_date", "Weekday")

North_nas = North_town_cases[is.na(North_town_cases$Boston),names ]

missing = barplot(table(North_nas$Weekday), main="Borplot for Missing Dates",ylim = c(0, max(table(North_nas$Weekday))+5), ylab = "Frequency")

text(missing,table(North_nas$Weekday)+1, paste(table(North_nas$Weekday)) ,cex=1) 


######### implementing NAs 

implement = function(data_name, Town_name){
  
  test = data_name
  n = nrow(data_name)
  
  for (i in Town_name){
    for (j in 1:n){
      if (is.na(data_name[j, i]) & j-5 < 1 & j+5 <= n ){ 
        test[j,i] = round(mean(data_name[1:j+5, i], na.rm = T))
      }
      else if ( is.na(data_name[j, i]) & j-5 >= 1 & j+5 <= n){ 
        test[j,i] = round(mean(data_name[(j-5):j+5, i],na.rm = T))
      }
      else if ( is.na(data_name[j, i]) & j-5 >= 1 & j+5 > n){ 
        test[j,i] = round(mean(data_name[(j-5):n, i],na.rm = T))
      }
      else test[j, i] = data_name[j,i]
    }
  }
  
  return(test)
} 

North_town_miss = implement(North_town_cases, North_town)
tail(North_town_miss)

South_town_miss = implement(South_town_cases, South_town)
tail(South_town_miss)


###################### daily sum

North_sum = rowSums(North_town_miss[, 3:ncol(North_town_miss)])
South_sum = rowSums(South_town_miss[, 3:ncol(South_town_miss)])

North_South_case = date_list
North_South_case$North_sum =  North_sum
North_South_case$South_sum =  South_sum

length(North_sum)
nrow(date_list)
length(North_town_miss$sample_date)

North_town_miss$sample_date[North_town_miss$sample_date %in% date_list$sample_date]

North_town_miss[North_town_miss$sample_date == "2020-10-23",]

South_town_cases[South_town_cases$sample_date == "2020-10-23",]

library("reshape2")
library("ggplot2")

case_long = melt(North_South_case, id.vars = "sample_date") 

ggplot(case_long,                            # Draw ggplot2 time series plot
       aes(x = sample_date,
           y = value,
           col = variable)) +
  geom_line() + 
  ggtitle("Northern and Southern Daily Posivitive Cases")


########## get the daily wastewater

wastewater = read.csv("daily_wastewater325.csv")

## convert str date into datetime 

wastewater['Sample_date'] = as.Date(wastewater$Sample.Date, format = "%m/%d/%Y")
str(wastewater)
nrow(wastewater)

startdate = range(North_South_case$sample_date)[1]
enddate = range(North_South_case$sample_date)[2]

water_date = wastewater[wastewater$Sample_date >= startdate & wastewater$Sample_date <= enddate, 12]
water = wastewater[wastewater$Sample_date >= startdate & wastewater$Sample_date <= enddate, 2:3]

water = data.frame(water_date, water)
colnames(water) = c("sample_date", "South_water", "North_water") 
head(water)

sum(is.na(water))

water$South_water = as.numeric(water$South_water)
water$North_water = as.numeric(water$North_water)

water_long = melt(water, id.vars = "sample_date")
ggplot(water_long,                            # Draw ggplot2 time series plot
       aes(x = sample_date,
           y = value,
           col = variable)) +
  geom_line() + 
  ggtitle("Northern and Southern Wastewater")



case_waste = merge(x = North_South_case, y = water, all = TRUE)

case_waste$weekdays = strftime(case_waste$sample_date, format = "%A")

head(case_waste)

case_long = melt(case_waste[, 1:5], id.vars = "sample_date") 
head(case_long)

ggplot(case_long,                            # Draw ggplot2 time series plot
       aes(x = sample_date,
           y = value,
           col = variable)) +
  geom_line() + 
  ggtitle("Northern and Southern Daily Posivitive Cases and Wastewater")

########### North

North = data.frame(case_waste$sample_date, case_waste$North_sum, case_waste$North_water)
colnames(North) = c("sample_date", "Cases", "Wastewater")
North_long = melt(North, id.vars = "sample_date")

ggplot(North_long,                           
       aes(x = sample_date,
           y = value,
           col = variable)) +
  geom_line() + 
  ggtitle("Northern Daily Posivitive Cases and Wastewater")

########### South

South = data.frame(case_waste$sample_date, case_waste$South_sum, case_waste$South_water)
colnames(South) = c("sample_date", "Cases", "Wastewater")
South_long = melt(South, id.vars = "sample_date")

ggplot(South_long,                           
       aes(x = sample_date,
           y = value,
           col = variable)) +
  geom_line() + 
  ggtitle("Southern Daily Posivitive Cases and Wastewater")


############# ratio 

n = nrow(case_waste)
case_waste$South_ratio = rep(NA, n)

case_waste$North_ratio = rep(NA, n)
  
for (i in 1:n){
    if (case_waste$South_sum[i] == 0| is.na(case_waste$South_sum[i]) | is.na(case_waste$South_water[i]) ){
      case_waste$South_ratio [i] = 0
    }
    else case_waste$South_ratio[i] = case_waste$South_water[i]/case_waste$South_sum[i]
    
  }

for (i in 1:n){
  if (case_waste$North_sum[i] == 0| is.na(case_waste$North_sum[i]) | is.na(case_waste$North_water[i]) ){
    case_waste$North_ratio [i] = 0
  }
  else case_waste$North_ratio[i] = case_waste$North_water[i]/case_waste$North_sum[i]
  
}


ratio = data.frame(case_waste$sample_date, case_waste$North_ratio, case_waste$South_ratio ) 
colnames(ratio) = c("sample_date" ,"North_ratio", "South_ratio" )

ratio_long = melt(ratio, id.vars = "sample_date")

ggplot(ratio_long,                           
       aes(x = sample_date,
           y = value,
           col = variable)) +
  geom_line() +
  ggtitle("Ratio for Southern and Northern Wastewater and Daily Posivitive Cases")



#######################################################################
################# Turning points
#######################################################################

library(timeSeries)
library(xts)
library(pastecs)
library(splus2R)
library(TTR)

# waster_s = xts(case_waste$South_water, order.by= case_waste$sample_date, frequency=365)
# 
# is.xts(waster_s)

south_w = case_waste[!is.na(case_waste$South_water), "South_water"]

tp_waste = turnpoints(south_w)
summary(tp_waste)

Peaks <- function(x, level = 0.05) {
  if (!inherits(x, "turnpoints"))
    stop("x must be a 'turnpoints' object!")
  # Extract position and probability
  tp.pos <- x$tppos
  tp.proba <- x$proba
  # We have both peaks and pits. Keep only peaks
  keep <- 1:(x$nturns / 2) * 2
  if (x$firstispeak) keep <- keep - 1
  tp.pos <- tp.pos[keep]
  tp.proba <- tp.proba[keep]
  # Keep only peaks whose probability is lower than level
  return(tp.pos[tp.proba < level])
}


p.all = Peaks(tp_waste, level = 1)

plot(south_w, type="o", cex = 1/4, main = "Southern Wastewater and all peaks")
points(seq(south_w)[p.all], south_w[p.all], col = 2, cex = 1.5)

##### only must significant ones
p.50 = Peaks(tp_waste, level = 0.05)
points(seq(south_w)[p.50], south_w[p.50], col = 3, cex = 2)

mtext("Peaks at level = 50%", col = 3)

plot(tp_waste, level = 0.01, lhorz = TRUE, lcol = 2, llty = 2,
     type = "l", xlab = "data number", 
     ylab = "Level 1%", 
     main = "Information (turning points) ")

plot(south_w, type="o", cex = 1/4, main = "Southern Wastewater and all peaks")
lines(tp_waste)


south_w7 = SMA(south_w, n=7)
plot.ts(south_w7)

waster_s = ts(south_w, start = c(2020, as.numeric(format(startdate, "%j"))), frequency=365)

######## exponential smoothing

south_w_exp = HoltWinters(waster_s, beta = FALSE, gamma = FALSE) 

south_w_exp$fitted
south_w_exp$SSE
south_w_exp

plot(south_w_exp)











#################### 7 day rollcum 

library(zoo)

library(RcppRoll)


cumula_days = 7

#### limitation: NA as 0 better to be replaced by 7 days mean?

South_cumu7 = rollapply(case_waste$South_sum, cumula_days, function(x) sum(x, na.rm=T))
North_cumu7 = rollapply(case_waste$North_sum, cumula_days, function(x) sum(x, na.rm=T))


n = nrow(case_waste)

length(South_cumu7)

# South_cum = matrix(ncol=1, nrow = n)

cum = function(data_name, roll_days, length){
  
  cumu7 = rollapply(data_name,roll_days,function(x) sum(x, na.rm=T) )
  
  cumulate = rep(NA, length)
  
  for (i in 1:length){
    if (i == 1) {
      cumulate[i] = data_name[i]}
    
    else if (i > 1 & i <  roll_days ){
      cumulate[i] = cumulate[i-1] + data_name[i]
    }
    
    else cumulate[i]= cumu7[i- roll_days+1]
    
  }
  return(cumulate)
}

South_cum = cum(case_waste$South_sum, 7, nrow(case_waste)) 

North_cum = cum(case_waste$North_sum, 7, nrow(case_waste))

case_waste$South_cum = South_cum
case_waste$North_cum = North_cum


cumu = data.frame(case_waste$sample_date, case_waste$North_cum, case_waste$South_cum) 
colnames(cumu) = c("sample_date" ,"North_cum", "South_cum" )

cumu_long = melt(cumu, id.vars = "sample_date")

ggplot(cumu_long,                           
       aes(x = sample_date,
           y = value,
           col = variable)) +
  geom_line() +
  ggtitle("Southern and Northern 7 Days Cumulative Posivitive Cases")



case_waste$South_cum_ratio = rep(NA, n)

case_waste$North_cum_ratio = rep(NA, n)

for (i in 1:n){
  if (case_waste$South_cum[i] == 0| is.na(case_waste$South_cum[i]) | is.na(case_waste$South_water[i]) ){
    case_waste$South_cum_ratio[i] = 0
  }
  else case_waste$South_cum_ratio[i] = case_waste$South_water[i]/case_waste$South_cum[i]
  
}

for (i in 1:n){
  if (case_waste$North_cum[i] == 0| is.na(case_waste$North_cum[i]) | is.na(case_waste$North_water[i]) ){
    case_waste$North_cum_ratio[i] = 0
  }
  else case_waste$North_cum_ratio[i] = case_waste$North_water[i]/case_waste$North_cum[i]
  
}

cumu_ratio = data.frame(case_waste$sample_date, case_waste$North_cum_ratio, case_waste$South_cum_ratio) 
colnames(cumu_ratio) = c("sample_date" ,"North_cum_Ratio", "South_cum_Ratio" )

cumu_ratio_long = melt(cumu_ratio, id.vars = "sample_date")

ggplot(cumu_ratio_long,                           
       aes(x = sample_date,
           y = value,
           col = variable)) +
  geom_line() +
  ggtitle("Southern and Northern Wastewater / 7 Days Cumulative Posivitive Cases")


##############################################################################
############ lag 
case_waste_s = case_waste[!is.na(case_waste$South_water) & !is.na(case_waste$South_sum), ]

ccfvalues_s = ccf(case_waste_s$South_water, case_waste_s$South_sum )
ccfvalues_s  # (-5: 0.863, -4:0..794, -10:0.790 )

case_waste_n = case_waste[!is.na(case_waste$North_water) & !is.na(case_waste$North_sum), ]

ccfvalues_n = ccf(case_waste_n$North_water, case_waste_n$North_sum )
ccfvalues_n # (-8; 0.812, )


case_waste_sc = case_waste[!is.na(case_waste$South_water) & !is.na(case_waste$South_cum), ]

ccfvalues_sc = ccf(case_waste_sc$South_water, case_waste_sc$South_cum )
ccfvalues_sc #(-10: 0.921)

case_waste_nc = case_waste[!is.na(case_waste$North_water) & !is.na(case_waste$North_cum), ]

ccfvalues_nc = ccf(case_waste_nc$North_water, case_waste_nc$North_cum )
ccfvalues_nc  # (-10: 0.920)




