setwd("C:/Users/Yanjia/Desktop/research/BU Infection/covid/BU and CDC")

#will it work?



#################################
##### Daily Wastewater DATA #####
#################################
wastewater = read.csv("daily_wastewater.csv")

## convert str date into datetime 

wastewater['Sample_date'] = as.Date(wastewater$Sample.Date, format = "%m/%d/%Y")
str(wastewater)
nrow(wastewater)

plot(wastewater$Sample_date,
     wastewater$Southern..copies.mL.,
     type = "l",
     col = 2,
    # labels = format(wastewater$Sample_date, "%b %Y"),
     xlab = "Year",
     main = "Southern",
     ylab = "Values")

plot(wastewater$Sample_date,
     wastewater$Northern..copies.mL.,
     type = "l",
     col = 2,
     # labels = format(wastewater$Sample_date, "%b %Y"),
     xlab = "Year",
     main = "Northern",
     ylab = "Values")

library(ggplot2)
library(dplyr)


# south_p <- ggplot(wastewater, aes(x=Sample_date, y=Southern..copies.mL.)) +
#   geom_line() + 
#   xlab("")
# south_p
# 
# north_p <- ggplot(ratio_c[ratio_c$North_ratio != 0,], aes(x=case_date, y=North_ratio)) +
#   geom_line() + 
#   xlab("")
# north_p


# head(wastewater$Sample_date)
# 
# Southern1 = wastewater[which(wastewater$Southern..copies.mL.!=""),]
# nrow(Southern1)
# 
# Southern2 = Southern1[which(Southern1$Southern..copies.mL.!="ND"),]
# nrow(Southern2)
# 
# plot(Southern2$Sample_date,
#      Southern2$Southern..copies.mL.,
#      type = "l",
#      col = 2,
#      labels = format(Southern2$Sample_date, "%Y-%m"),
#      xlab = "Year",
#      main = "Southern",
#      ylab = "Values")

#############################################
###### Covid Cases ##########################
#############################################

cases = read.csv("covid_confirmed_usafacts.csv", header = TRUE)

cases_MA = cases[cases$State == "MA",]

str(cases_MA)

########### transpose the dataframe
n=ncol(cases_MA)

date_list = names(cases_MA)[5:n]

case_date = as.Date(substring(date_list, 2), format = "%Y.%m.%d")

# n=length(case_date)

countysFIPS = unlist(unique(cases_MA[, 1]), use.names=FALSE)

countyName = unlist(unique(cases_MA[, 2]), use.names=FALSE) 

state = unlist(unique(cases_MA[,3]), use.names=FALSE)

stateFIPS = unlist(unique(cases_MA[, 4]), use.names=FALSE) 

length(countyName)  

col_names = c('case_date', countyName)


county_cases = data.frame(matrix(ncol=length(col_names), nrow= length(case_date)))

for (i in 1:length(col_names)){
  if (i==1){ 
    county_cases[,i] = case_date }
  else county_cases[,i] = unlist(cases_MA[cases_MA$County.Name == countyName[i-1], 5:n], use.names = FALSE)
}

colnames(county_cases) = col_names

c(nrow(county_cases), ncol(county_cases))

tail(county_cases)

for (i in 2:ncol(county_cases))
  plot(county_cases$case_date, county_cases[,i],
       type = "l",
       col = 2,
       # labels = format(wastewater$Sample_date, "%b %Y"),
       xlab = "Year",
       main = countyName[i-1],
       ylab = "Values" )


########## get the daily new cases

newcases = data.frame(matrix(ncol=length(col_names), nrow= length(case_date)-1))


for (i in 1:length(col_names)){
  if (i==1) 
    newcases[,i] = case_date[2:length(case_date)]
  else newcases[,i] = diff(county_cases[,i])
}

colnames(newcases) = col_names

newcases["Weekday"] = strftime(newcases$case_date, format = "%A")
head(newcases)

for (i in 2:ncol(county_cases))
  plot(newcases$case_date, newcases[,i],
       type = "l",
       col = 2,
       # labels = format(wastewater$Sample_date, "%b %Y"),
       xlab = "Year",
       main = countyName[i-1],
       ylab = "Values" )

######## cum southern and northern

#southern: "Norfolk County "
#Northern: "Suffolk County " "Middlesex County " 

newcases['Southern'] = newcases$`Norfolk County `
newcases['Northern'] = newcases$`Suffolk County `+ newcases$`Middlesex County `

plot(newcases$case_date, newcases$Southern,
     type = "l",
     col = 2,
     # labels = format(wastewater$Sample_date, "%b %Y"),
     xlab = "Year",
     main = "Southern",
     ylab = "Values" )
lines(newcases$case_date,newcases$`Norfolk County `, type = 'l', col = 5)
legend("topleft",                           # Add legend to plot
       c("Southern Cases", "Norfolk"),
       lty = 1,
       col = c(2,5))

plot(newcases$case_date, newcases$Northern,
     type = "l",
     col = 2,
     # labels = format(wastewater$Sample_date, "%b %Y"),
     xlab = "Year",
     main = "Northern",
     ylab = "Values" )
lines(newcases$case_date, newcases$`Suffolk County `, type = 'l', col = 3)
lines(newcases$case_date, newcases$`Middlesex County `, type = 'l', col = 4)

legend("topleft",                           # Add legend to plot
       c("Northern Cases", "Suffolk", "Middlesex"),
       lty = 1,
       col = 2:4)


################################################
######## WC ratio ##############################
################################################

wastewater$Southern..copies.mL. = as.numeric(wastewater$Southern..copies.mL.)
wastewater$Northern..copies.mL. = as.numeric(wastewater$Northern..copies.mL.)


date_w = wastewater$Sample_date

date_c = newcases$case_date

n_c = length(date_c)
n_c

DateCom = date_c %in% date_w

ratio_c = newcases[DateCom, ]
nrow(ratio_c)

range(ratio_c$case_date)
range(wastewater$Sample_date)


ratio_c["South_W"] = wastewater$Southern..copies.mL.
ratio_c["North_W"] = wastewater$Northern..copies.mL.


######### Daily ratio

n = nrow(ratio_c)

ratio_c['South_ratio'] = rep(1, n)


for (i in 1:n){
  if (ratio_c$Southern[i] <= 0 | is.na(ratio_c$South_W[i])){
    ratio_c$South_ratio[i] = 0
  }
  else ratio_c$South_ratio[i] = ratio_c$South_W[i]/ratio_c$Southern[i]
  
}

ratio_c['North_ratio'] = rep(1, n) 
for (i in 1:n){
  if (ratio_c$Northern[i] <= 0 | is.na(ratio_c$North_W[i])){
    ratio_c$North_ratio[i] = 0
  }
  else ratio_c$North_ratio[i] = ratio_c$North_W[i]/ratio_c$Northern[i]
  
}



plot(ratio_c$case_date, ratio_c$South_ratio,
     type = "l",
     col = 2,
     # labels = format(wastewater$Sample_date, "%b %Y"),
     xlab = "Year",
     main = "Southern",
     ylab = "Ratio" )

lines(ratio_c$case_date,ratio_c$Southern, type = 'l', col = 3)
lines(ratio_c$case_date, ratio_c$South_W, type = 'l', col = 4)

legend("topleft",                           # Add legend to plot
       c(" Southern Ratio ", "Covid Cases", "Wastewater"),
       lty = 1,
       col = 2:4)


plot(ratio_c$case_date, ratio_c$Southern,
     type = "l",
     col = 2,
     # labels = format(wastewater$Sample_date, "%b %Y"),
     xlab = "Year",
     main = "Southern",
     ylab = "Values" )

lines(ratio_c$case_date, ratio_c$South_W, type = 'l', col = 3)

legend("topleft",                           # Add legend to plot
       c("Covid Cases", "Wastewater"),
       lty = 1,
       col = 2:3)

plot(ratio_c$case_date, ratio_c$Northern,
     type = "l",
     col = 2,
     # labels = format(wastewater$Sample_date, "%b %Y"),
     xlab = "Year",
     main = "Norththern",
     ylab = "Values" )

lines(ratio_c$case_date, ratio_c$North_W, type = 'l', col = 3)

legend("topleft",                           # Add legend to plot
       c("Covid Cases", "Wastewater"),
       lty = 1,
       col = 2:3)



plot(ratio_c$case_date, ratio_c$North_ratio,
     type = "l",
     col = 2,
     # labels = format(wastewater$Sample_date, "%b %Y"),
     xlab = "Year",
     main = "Northern",
     ylab = "Ratio" )


max(ratio_c$North_ratio)

cbind(ratio_c$case_date, ratio_c$Southern, ratio_c$South_W)

cbind(ratio_c$case_date, ratio_c$Northern, ratio_c$North_W, ratio_c$North_ratio)


ratio_0 = ratio_c[ratio_c$North_ratio != 0,]



south_p <- ggplot(ratio_c, aes(x=case_date, y=South_ratio)) +
  geom_line() + 
  xlab("")
south_p

north_p <- ggplot(ratio_c[ratio_c$North_ratio != 0,], aes(x=case_date, y=North_ratio)) +
  geom_line() + 
  xlab("")
north_p

############## get the daily lag 

library(astsa)

ratio_c_new = ratio_c[!is.na(ratio_c$South_W) & !is.na(ratio_c$Southern),]


ccfvalues_s = ccf(ratio_c_new$South_W, ratio_c_new$Southern ) 
ccfvalues_s


ratio_c_new_n = ratio_c[!is.na(ratio_c$North_W) & !is.na(ratio_c$Northern),]


ccfvalues_n = ccf(ratio_c_new_n$North_W, ratio_c_new_n$Northern ) 
ccfvalues_n








######### 7 day cumulative 
library(zoo)

cumula_days = 7

South_cumu = rollapply(ratio_c$Southern, cumula_days, FUN = sum)
North_cumu = rollapply(ratio_c$Northern, cumula_days, FUN = sum)

length(South_cumu)


head_s = matrix(ncol=1, nrow = n)

# add
for (i in 1:n){
  if (i == 1) {
   head_s[i] = ratio_c$Southern[i]}
  
  else if (i > 1 & i < cumula_days ){
    head_s[i] = head_s[i-1] + ratio_c$Southern[i]
  }
  
  else head_s [i]= South_cumu[i-cumula_days+1]
  
}

ratio_c$South_cum = head_s 

ratio_c['South_ratio_cum'] = rep(1, n) 

for (i in 1:n){
  if (ratio_c$South_cum[i] <= 0 | is.na(ratio_c$South_W[i])){
    ratio_c$South_ratio_cum[i] = 0
  }
  else ratio_c$South_ratio_cum[i] = ratio_c$South_W[i]/ratio_c$South_cum[i]
  
}

head_n = matrix(ncol=1, nrow = n)
# add
for (i in 1:n){
  if (i == 1) {
    head_n[i] = ratio_c$Northern[i]}
  
  else if (i > 1 & i < cumula_days ){
    head_n[i] = head_n[i-1] + ratio_c$Northern[i]
  }
  
  else head_n[i]= North_cumu[i-cumula_days+1]
  
}

ratio_c$North_cum= head_n 

ratio_c['North_ratio_cum'] = rep(1, n) 
for (i in 1:n){
  if (ratio_c$North_cum[i] <= 0 | is.na(ratio_c$North_W[i])){
    ratio_c$North_ratio_cum[i] = 0
  }
  else ratio_c$North_ratio_cum[i] = ratio_c$North_W[i]/ratio_c$North_cum[i]
  
}

south_p <- ggplot(ratio_c[ratio_c$South_ratio_cum != 0,], aes(x=case_date, y=South_ratio_cum)) +
  geom_line() + 
  xlab("")
south_p

north_p <- ggplot(ratio_c[ratio_c$North_ratio_cum != 0,], aes(x=case_date, y=North_ratio_cum)) +
  geom_line() + 
  xlab("")
north_p

################ 7 cum lag 

ratio_c_new_s = ratio_c[!is.na(ratio_c$South_W) & !is.na(ratio_c$South_cum),]


ccfvalues_s = ccf(ratio_c_new_s$South_W, as.numeric(ratio_c_new_s$South_cum ) )
ccfvalues_s

str(ratio_c_new_s)

ratio_c_new_n = ratio_c[!is.na(ratio_c$North_W) & !is.na(ratio_c$North_cum),]

ccfvalues_n = ccf(ratio_c_new_n$North_W, as.numeric(ratio_c_new_n$North_cum ) )
ccfvalues_n


south = data.frame(cbind(ratio_c$case_date, ratio_c$South_ratio, ratio_c$South_ratio_cum))

colnames(south) = c("case_date", "South_ratio", "South_ratio_cum")

north = data.frame(cbind(ratio_c$case_date, ratio_c$North_ratio, ratio_c$North_ratio_cum))
colnames(north) = c("case_date", "North_ratio", "North_ratio_cum")

library("reshape2")  
south_long <- melt(south, id.vars = "case_date")    # Reshaping data to long format
head(south_long)

south_long$case_date = as.Date(south_long$case_date)

north_long <- melt(north, id.vars = "case_date")    # Reshaping data to long format
head(north_long)

north_long$case_date = as.Date(north_long$case_date)

ggplot(south_long[south_long$value!=0, ],                            # Draw ggplot2 time series plot
       aes(x = case_date,
           y = value,
           col = variable)) +
geom_line() + 
ggtitle("Wastewater Southern Cases Ratio")

ggplot(north_long[north_long$value !=0,],                            # Draw ggplot2 time series plot
       aes(x = case_date,
           y = value,
           col = variable)) +
  geom_line() +
  ggtitle("Wastewater Northern Cases Ratio")

