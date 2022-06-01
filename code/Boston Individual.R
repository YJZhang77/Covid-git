
setwd("C:/Users/Yanjia/Desktop/research/BU Infection/covid/Covid-git/code")


#############################################
###### Covid Cases ##########################
#############################################

cases_o = read.csv("town positive3_23.csv", header = TRUE)

town_cases = data.frame(cases_o)



c(nrow(town_cases), ncol(town_cases))

################## records counts

date = names(town_cases)[2:ncol(town_cases)]

sample_date = as.Date(substring(date, 2), format = "%m.%d.%Y")

sample_date[sample_date == "2020-10-23"]



length(sample_date)

Weekday = strftime(sample_date, format = "%A")

barplot(table(Weekday), main = 'Number of Reports from Covid')


###################### town names 

North_town = c("Waltham", "Lexington", "Bedford",
               "Burlington", "Wilmington", "Reading", "Wakefield","Stoneham",
               "Woburn", "Winchester", "Arlington", "Medford", "Melrose",
               "Malden", "Revere", "Everett", "Chelsea", "Somerville", "Belmont",
               "Watertown", "Cambridge", "Winthrop")

#"Brookline", "Newton",

Boston = "Boston"

South_town = c("Ashland", "Framingham", "Natick", "Wellesley", "Newton", "Needham",
               "Brookline",  "Dedham", "Westwood", "Norwood", "Walpole", "Milton",
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
sum(North_town_cases[,3:ncol(North_town_cases)] == 2.5, na.rm = TRUE)

range(North_town_cases$sample_date)

c(nrow(North_town_cases), ncol(North_town_cases))


# n = rep("NA", length(North_town))
# for (i in 1:length(North_town)){
#   n[i] = sum(is.na(North_town_cases[,i+2]))
# }


####### Boston

Boston_cases = cases(town_cases, Boston, date_list)

###### South 

South_town_cases = cases(town_cases, South_town, date_list)

sum(South_town_cases[,2:ncol(South_town_cases)] == 2.5, na.rm =TRUE )

range(South_town_cases$sample_date)

tail(South_town_cases)

c(nrow(South_town_cases), ncol(South_town_cases))


###### Total number of <5 

n1 = sum(North_town_cases[,3:ncol(North_town_cases)] == 2.5, na.rm = TRUE)

n2 = sum(South_town_cases[,2:ncol(South_town_cases)] == 2.5, na.rm =TRUE )

n3 = sum(Boston_cases[, 3] == 2.5, na.rm = TRUE)

total = nrow(North_town_cases)*(ncol(North_town_cases)-2) + nrow(South_town_cases)*(ncol(South_town_cases)-2)+nrow(Boston_cases)

percentage = (n1+n2 + n3)/total
percentage


# n = rep("NA", length(South_town))
# for (i in 1:length(South_town)){
#   n[i] = sum(is.na(South_town_cases[,i+2]))
# }
# n
# 
# South_nas = South_town_cases[is.na(South_town_cases$Ashland), 1]
# 
# length(South_nas)


### all the dates are the same North/ South

South_nas = South_town_cases[is.na(South_town_cases$Ashland), 1]
n_missing = length(South_nas)
names = c("sample_date", "Weekday")

North_nas = North_town_cases[is.na(North_town_cases$Boston),names ]

## barplot

missing = barplot(table(North_nas$Weekday), main="Borplot for Covid Missing Dates",ylim = c(0, max(table(North_nas$Weekday))+5), ylab = "Frequency")

text(missing,table(North_nas$Weekday)*0.95, paste(table(North_nas$Weekday)) ,cex=1) 


######### implementing NAs with the average number of 10 

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

Boston_miss = implement(Boston_cases, Boston)


#### delete double "2020-10-23" 

North_town_miss_correct = North_town_miss[-c(115), ]

North_town_miss_correct[North_town_miss_correct$sample_date == "2020-10-23", ]

South_town_miss_correct = South_town_miss[-c(115), ]
South_town_miss_correct[South_town_miss_correct$sample_date == "2020-10-23", ]

Boston_miss_correct = Boston_miss[-c(115), ]
Boston_miss_correct[Boston_miss_correct$sample_date == "2020-10-23", ]


tail(North_town_miss_correct$Waltham)
###################### daily sum

North_sum = rowSums(North_town_miss_correct[, 3:ncol(North_town_miss)])
South_sum = rowSums(South_town_miss_correct[, 3:ncol(South_town_miss)])

North_South_case = date_list
North_South_case$North_sum =  North_sum
North_South_case$South_sum =  South_sum
North_South_case$South_sum =  South_sum

North_South_case$Boston =  Boston_miss_correct$Boston

names(North_South_case)


length(North_sum)
nrow(date_list)


library("reshape2")
library("ggplot2")

case = data.frame(North_South_case[, c(1,3,4)], "Boston" = North_town_miss_correct$Boston)

short_range = seq(as.Date("2021-12-01"), as.Date("2022-02-01"), by = 1) 

short_case = data.frame(North_South_case[North_South_case$sample_date %in% short_range,c(1,3,4)],
                        "Boston" = North_town_miss_correct[North_town_miss_correct$sample_date %in% short_range,"Boston"])


case_long = melt(case, id.vars = "sample_date") 

ggplot(case_long,                            # Draw ggplot2 time series plot
       aes(x = sample_date,
           y = value,
           col = variable)) +
  geom_line() + 
  ggtitle("Northern and Southern Daily Posivitive Cases")

short_range = seq(as.Date("2021-12-01"), as.Date("2022-02-01"), by = 1) 

short_case = data.frame(North_South_case[North_South_case$sample_date %in% short_range,c(1,3,4)],
                        "Boston" = North_town_miss_correct[North_town_miss_correct$sample_date %in% short_range,"Boston"])

short_case_long = melt(short_case, id.vars = "sample_date")
ggplot(short_case_long,                            # Draw ggplot2 time series plot
       aes(x = sample_date,
           y = value,
           col = variable)) +
  geom_line() + 
  ggtitle("Northern and Southern Daily Posivitive Cases During 2021-12-01 -- 2022-02-01 ")





######## check if they have the same date peaks and vallens

library(pracma)
library(ggrepel)


Dataset1 = North_South_case

peaks = function(Dataset1, Dataset2){
  
  indx_n = findpeaks(Dataset1$North_sum,threshold = 1)
  indx_s = findpeaks(Dataset1$South_sum, threshold = 1)
  indx_b = findpeaks(Dataset2$Boston, threshold = 1)
  
  sample_n = Dataset1[indx_n[,2], ]
  sample_s = Dataset1[indx_s[,2], ]
  sample_b = Dataset2[indx_b[,2],1:3]
  
  count3 = as.Date(intersect(intersect(sample_n[,1], sample_s[,1]),sample_b[,1]))
  
  count2 = as.Date(sort(union(union(setdiff(intersect(sample_n[,1], sample_s[,1]),sample_b[,1]),
                                    setdiff(intersect(sample_n[,1], sample_b[,1]),sample_s[,1])),
                              setdiff(intersect(sample_b[,1], sample_s[,1]),sample_n[,1]))))
  
  count1 = as.Date(sort(union(union(setdiff(setdiff(sample_n[,1], sample_s[,1]),sample_b[,1] ),
                                    setdiff(setdiff(sample_s[,1], sample_b[,1]),sample_n[,1])), 
                              setdiff(setdiff(sample_b[,1], sample_s[,1]),sample_n[,1]))))
  
  temp1 = c(count3, count2, count1)
  temp2 = c(rep("Three", length(count3)), rep("Two", length(count2)), rep("One", length(count1)))
  
  same_date = data.frame("Same_Date" = temp1, "Category" = temp2)
  
  all = list("NorthPeaks"=sample_n, "SouthPeaks"=sample_s, "BostonPeaks"=sample_b, "Distrbution"=same_date)
  
  return(all)
  
}

same = peaks(North_South_case, North_town_miss_correct)

same$Distrbution

same$NorthPeaks$Diff =  same$NorthPeaks$sample_date - dplyr::lag(same$NorthPeaks$sample_date, n = 1)

same$SouthPeaks$Diff =  same$SouthPeaks$sample_date - dplyr::lag(same$SouthPeaks$sample_date, n = 1)




plot(North_South_case$sample_date, North_South_case$North_sum,
     type = "l",
     col = 2,
     # labels = format(wastewater$Sample_date, "%b %Y"),
     xlab = "Date",
     main = "Northern, Southern, and Boston During 2021-12-01---2022-02-01",
     ylab = "Values" )
lines(North_South_case$sample_date, North_South_case$South_sum, type = 'l', col = 3)
lines(North_town_miss_correct$sample_date, North_town_miss_correct$Boston, type = "l", col = 4)


points(same$NorthPeaks$sample_date, same$NorthPeaks$North_sum, col = 2 )
points(same$SouthPeaks$sample_date, same$SouthPeaks$South_sum, col = 3 )
points(same$BostonPeaks$sample_date, same$BostonPeaks$Boston, col=4)

legend("topleft",                           # Add legend to plot
       c("Northern","Southern", "Boston"),
       lty = 1,
       col = c(2:4))

count = barplot(sort(table(same$Distrbution$Category), decreasing = TRUE),        
                ylim = c(0, max(table(same$Distrbution$Category))),
                # xlab = "Counts",
                ylab = "Number of Days",
                main = 'Number of Same Date Peaks from Northern, Southern, and Boston')

text(count,sort(table(same$Distrbution$Category), decreasing = TRUE)*0.95 , paste(sort(table(same$Distrbution$Category), decreasing = TRUE)) ,cex=1) 

#### peaks in weekdays

week_n = barplot(sort(table(same$NorthPeaks$Weekday), decreasing = TRUE),        
                 ylim = c(0, max(table(same$NorthPeaks$Weekday))),
                 # xlab = "Counts",
                 ylab = "Number of Peaks",
                 main = 'Number of Peaks from Northern')

text(week_n,sort(table(same$NorthPeaks$Weekday), decreasing = TRUE)*(0.95) , paste(sort(table(same$NorthPeaks$Weekday), decreasing = TRUE)) ,cex=1) 

week_s = barplot(sort(table(same$SouthPeaks$Weekday), decreasing = TRUE),        
                 ylim = c(0, max(table(same$SouthPeaks$Weekday))),
                 # xlab = "Counts",
                 ylab = "Number of Peaks",
                 main = 'Number of Peaks from Souththern')

text(week_s,sort(table(same$SouthPeaks$Weekday), decreasing = TRUE)*(0.95) , paste(sort(table(same$SouthPeaks$Weekday), decreasing = TRUE)) ,cex=1) 


week_b = barplot(sort(table(same$BostonPeaks$Weekday), decreasing = TRUE),        
                 ylim = c(0, max(table(same$BostonPeaks$Weekday))),
                 # xlab = "Counts",
                 ylab = "Number of Peaks",
                 main = 'Number of Peaks from Boston')

text(week_b,sort(table(same$BostonPeaks$Weekday), decreasing = TRUE)*(0.95) , paste(sort(table(same$BostonPeaks$Weekday), decreasing = TRUE)) ,cex=1) 

##### peaks dif in days
dif_n = barplot(table(same$NorthPeaks$Diff), 
                ylim = c(0, max(table(same$NorthPeaks$Diff))+5),
                # xlab = "Counts",
                ylab = "Number of Diffence in Days",
                main = 'Difference in Days Between Northern Cases Peaks')

text(dif_n,table(same$NorthPeaks$Diff)*(0.95) , 
     paste(table(same$NorthPeaks$Diff)) ,cex=1) 

dif_s = barplot(table(same$SouthPeaks$Diff), 
                ylim = c(0, max(table(same$SouthPeaks$Diff))+5),
                # xlab = "Counts",
                ylab = "Number of Diffence in Days",
                main = 'Difference in Days Between South Cases Peaks')

text(dif_s,table(same$SouthPeaks$Diff)*(0.95) , 
     paste(table(same$SouthPeaks$Diff)) ,cex=1) 

########### example for 2021-12-01, 2022-2-1 
#################

indx_n = findpeaks(short_case$North_sum,threshold = 1)

indx_s = findpeaks(short_case$South_sum,threshold = 1)
indx_b = findpeaks(short_case$Boston, threshold = 1)

sample_n = short_case[indx_n[,2], ]
sample_s = short_case[indx_s[,2], ]
sample_b = short_case[indx_b[,2], ]



count_3 = as.Date(intersect(intersect(sample_n[,1], sample_s[,1]),sample_b[,1]))


count_2 = as.Date(sort(union(union(setdiff(intersect(sample_n[,1], sample_s[,1]),sample_b[,1]),
                                   setdiff(intersect(sample_n[,1], sample_b[,1]),sample_s[,1])),
                             setdiff(intersect(sample_b[,1], sample_s[,1]),sample_n[,1]))))

count_1 = as.Date(sort(union(union(setdiff(setdiff(sample_n[,1], sample_s[,1]),sample_b[,1] ),
                                   setdiff(setdiff(sample_s[,1], sample_b[,1]),sample_n[,1])), 
                             setdiff(setdiff(sample_b[,1], sample_s[,1]),sample_n[,1]))))

fre_bar_same = sort( c(length(count_3), length(count_2), length(count_1)),decreasing = TRUE) 




count = barplot(fre_bar_same,names.arg=c("3", "2", "1"),        
                ylim = c(0, max(fre_bar_same)),
                xlab = "Counts",
                ylab = "Number of Days",
                main = 'Number of Same Date Peaks from Northern, Southern, and Boston')

text(count,fre_bar_same-0.5, paste(fre_bar_same) ,cex=1) 





short_ggplot = ggplot(short_case_long,                            # Draw ggplot2 time series plot
                      aes(x = sample_date,
                          y = value,
                          col = variable)) +
  geom_line() + 
  ggtitle("Northern and Southern Daily Posivitive Cases During 2021-12-01 -- 2022-02-01 ")



plot(short_case$sample_date, short_case$North_sum,
     type = "l",
     col = 2,
     ylim = c(0, max(short_case$North_sum,short_case$South_sum )),
     # labels = format(wastewater$Sample_date, "%b %Y"),
     xlab = "Date",
     main = "Northern, Southern, and Boston During 2021-12-01---2022-02-01",
     ylab = "Values" )
lines(short_case$sample_date, short_case$South_sum, type = 'l', col = 3)
lines(short_case$sample_date, short_case$Boston, type = "l", col = 4)
points(sample_n$sample_date, sample_n$North_sum, col = 2 )
points(sample_s$sample_date, sample_s$South_sum, col = 3 )
points(sample_b$sample_date, sample_b$Boston, col=4)
legend("topleft",                           # Add legend to plot
       c("Northern","Southern", "Boston"),
       lty = 1,
       col = c(2:4))

#######################################################################
########## get the daily wastewater
#######################################################################

wastewater = read.csv("daily_wastewater325.csv")

## convert str date into datetime 

wastewater['Sample_date'] = as.Date(wastewater$Sample.Date, format = "%m/%d/%Y")
str(wastewater)
nrow(wastewater)

water_date = wastewater[wastewater$Sample_date %in% date_list[,1], 12]

water = wastewater[wastewater$Sample_date %in%date_list[,1], c(2,3,12)]
colnames(water) = c("South_water","North_water","sample_date" ) 
tail(water)

water$South_water = as.numeric(water$South_water)
water$North_water = as.numeric(water$North_water)

sum(is.na(water))


###### Missings
water$Weekday = strftime(water$sample_date, format = "%A")

names = c("sample_date", "Weekday")

water_nas = water[is.na(water$North_water), names]

## barplot

missing_w = barplot(table(water_nas$Weekday), main="Borplot for Northern Wastewater Missing Dates",
                    ylim = c(0, max(table(water_nas$Weekday))+5), ylab = "Frequency")

text(missing_w,table(water_nas$Weekday)*0.95, paste(table(water_nas$Weekday)) ,cex=1) 


water_s_nas = water[is.na(water$South_water),names ]

missing_w_s = barplot(table(water_s_nas$Weekday), main="Borplot for Southern Wastewater Missing Dates",
                      ylim = c(0, max(table(water_s_nas$Weekday))+5), ylab = "Frequency")

text(missing_w_s,table(water_s_nas$Weekday)*0.95, paste(table(water_s_nas$Weekday)) ,cex=1) 




water_long = melt(water, id.vars = "sample_date")
ggplot(water_long,                            # Draw ggplot2 time series plot
       aes(x = sample_date,
           y = value,
           col = variable)) +
  geom_line() + 
  ggtitle("Northern and Southern Wastewater")



#### implement missings 
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

water_miss_correct = implement(water, c("South_water","North_water"))
sum(is.na(water_miss_correct))

names(water)
# water_miss_correct$Weekday = strftime(water_miss_correct$sample_date, format = "%A")
# 



#### 2021-12-01 -- 2022-2-1
short_range = seq(as.Date("2021-12-01"), as.Date("2022-02-01"), by = 1) 

short_water = water_miss_correct[water_miss_correct$sample_date %in% short_range, 1:3]
short_water_long = melt(short_water, id.vars = "sample_date")
ggplot(short_water_long,                            # Draw ggplot2 time series plot
       aes(x = sample_date,
           y = value,
           col = variable)) +
  geom_line() + 
  ggtitle("Northern and Southern Daily Posivitive Cases During 2021-12-01 -- 2022-02-01 ")



############# find peaks

peaks_water = function(Dataset){
  
  indx_n = findpeaks(Dataset$North_water, threshold = 1)
  indx_s = findpeaks(Dataset$South_water, threshold = 1)
  
  sample_n = Dataset[indx_n[,2], ]
  sample_s = Dataset[indx_s[,2], ]
  
  
  count2 = as.Date(intersect(sample_n$sample_date, sample_s$sample_date))
  
  count1 = as.Date(c(setdiff(sample_n$sample_date, sample_s$sample_date),setdiff(sample_s$sample_date, sample_n$sample_date)))
  
  temp1 = c(count2, count1)
  temp2 = c(rep("Two", length(count2)), rep("One", length(count1)))
  
  same_date = data.frame("Same_Date" = temp1, "Category" = temp2)
  
  all = list("NorthPeaks"=sample_n, "SouthPeaks"=sample_s, "Distrbution"=same_date)
  
  return(all)
  
}

same_water = peaks_water(water_miss_correct)
same_water$Distrbution


count_water = barplot(sort(table(same_water$Distrbution$Category), decreasing = TRUE),        
                      ylim = c(0, max(table(same_water$Distrbution$Category))),
                      # xlab = "Counts",
                      ylab = "Number of Days",
                      main = 'Number of Same Date Wastewater Peaks from Northern and Southern')

text(count_water,sort(table(same_water$Distrbution$Category), decreasing = TRUE)*0.95 , paste(sort(table(same_water$Distrbution$Category), decreasing = TRUE)) ,cex=1) 

#### peaks in weekdays

week_n_water = barplot(sort(table(same_water$NorthPeaks$Weekday), decreasing = TRUE),        
                       ylim = c(0, max(table(same_water$NorthPeaks$Weekday))),
                       # xlab = "Counts",
                       ylab = "Number of Peaks",
                       main = 'Number of Wastewater Peaks from Northern')

text(week_n_water,sort(table(same_water$NorthPeaks$Weekday), decreasing = TRUE)*(0.95) , paste(sort(table(same_water$NorthPeaks$Weekday), decreasing = TRUE)) ,cex=1) 

week_s_water = barplot(sort(table(same_water$SouthPeaks$Weekday), decreasing = TRUE),        
                       ylim = c(0, max(table(same_water$SouthPeaks$Weekday))),
                       # xlab = "Counts",
                       ylab = "Number of Peaks",
                       main = 'Number of Wastewater Peaks from Southern')

text(week_s_water,sort(table(same_water$SouthPeaks$Weekday), decreasing = TRUE)*(0.95) , paste(sort(table(same_water$SouthPeaks$Weekday), decreasing = TRUE)) ,cex=1) 

peaks_n = same_water$NorthPeaks[same_water$NorthPeaks$sample_date %in% short_range, ]
peaks_s = same_water$SouthPeaks[same_water$SouthPeaks$sample_date %in% short_range, ]

plot(short_water$sample_date, short_water$North_water,
     type = "l", ylim = c(0, max(short_water$North_water, short_water$South_water)),
     col = 2,
     # labels = format(wastewater$Sample_date, "%b %Y"),
     xlab = "Date",
     main = "Wastewater from Northern and Southern During 2021-12-01---2022-02-01",
     ylab = "Values" )

lines(short_water$sample_date, short_water$South_water, type = 'l', col = 3)

points(peaks_n$sample_date, peaks_n$North_water, col = 2 )
points(peaks_s$sample_date, peaks_s$South_water, col = 3 )

legend("topleft",                           # Add legend to plot
       c("Northern","Southern"),
       lty = 1,
       col = c(2,3))


#### Peak ranges 

same_water$NorthPeaks$Diff =  same_water$NorthPeaks$sample_date - dplyr::lag(same_water$NorthPeaks$sample_date, n = 1)

same_water$SouthPeaks$Diff =  same_water$SouthPeaks$sample_date - dplyr::lag(same_water$SouthPeaks$sample_date, n = 1)


dif_n_water = barplot(table(same_water$NorthPeaks$Diff), 
                      ylim = c(0, max(table(same_water$NorthPeaks$Diff))+5),
                      # xlab = "Counts",
                      ylab = "Number of Diffence in Days",
                      main = 'Difference in Days Between Northern Wastewater Peaks')

text(dif_n_water,table(same_water$NorthPeaks$Diff)*(0.95) , 
     paste(table(same_water$NorthPeaks$Diff)) ,cex=1) 


dif_s_water = barplot(table(same_water$SouthPeaks$Diff), 
                      ylim = c(0, max(table(same_water$SouthPeaks$Diff))+5),
                      # xlab = "Counts",
                      ylab = "Number of Diffence in Days",
                      main = 'Difference in Days Between Southern Wastewater Peaks')

text(dif_s_water,table(same_water$SouthPeaks$Diff)*(0.95) , 
     paste(table(same_water$SouthPeaks$Diff)) ,cex=1) 




#### test if these differences are significant or randomly distributed with the peaks

## chi-square test for the frequency (independent observations): 
######H_0:  proportion of number of peeks in everyday are the same

test_n = chisq.test(table(same_water$NorthPeaks$Weekday))

test_s = chisq.test(table(same_water$SouthPeaks$Weekday))

North = as.data.frame(table(same_water$NorthPeaks$Weekday))[,2]
South = as.data.frame(table(same_water$SouthPeaks$Weekday))[,2]
row_names = c("Friday", "Monday", "Saturday", "Sunday", "Thursday", "Tuesday", "Wednesday")

fretable = cbind(North,South)
rownames(fretable) = row_names

chi_ind = chisq.test(fretable)
chi_ind

#H_0: the row and the column variables of the contingency table are independent.



##################################################################333
########### merge cases and water
#######################################################################

case_waste = merge(x = North_South_case, y = water_miss_correct, all = TRUE)

write.csv(case_waste, file = "case_sum_wastewater.csv")


names(case_waste)
# case_waste$weekdays = strftime(case_waste$sample_date, format = "%A")

head(case_waste)

# case_long = melt(case_waste[, c(1,3,4,5,6)], id.vars = "sample_date") 
# head(case_long)
# 
# ggplot(case_long,                            # Draw ggplot2 time series plot
#        aes(x = sample_date,
#            y = value,
#            col = variable)) +
#   geom_line() + 
#   ggtitle("Northern and Southern Daily Posivitive Cases and Wastewater")
# 
# ########### North
# 
# North = data.frame(case_waste$sample_date, case_waste$North_sum, case_waste$North_water)
# colnames(North) = c("sample_date", "Cases", "Wastewater")
# North_long = melt(North, id.vars = "sample_date")
# 
# ggplot(North_long,                           
#        aes(x = sample_date,
#            y = value,
#            col = variable)) +
#   geom_line() + 
#   ggtitle("Northern Daily Posivitive Cases and Wastewater")
# 
# ########### South
# 
# South = data.frame(case_waste$sample_date, case_waste$South_sum, case_waste$South_water)
# colnames(South) = c("sample_date", "Cases", "Wastewater")
# South_long = melt(South, id.vars = "sample_date")
# 
# ggplot(South_long,                           
#        aes(x = sample_date,
#            y = value,
#            col = variable)) +
#   geom_line() + 
#   ggtitle("Southern Daily Posivitive Cases and Wastewater")
# 

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
# 
# library(timeSeries)
# library(xts)
# library(pastecs)
# library(splus2R)
# library(TTR)
# 
# # waster_s = xts(case_waste$South_water, order.by= case_waste$sample_date, frequency=365)
# # 
# # is.xts(waster_s)
# 
# south_w = case_waste[!is.na(case_waste$South_water), "South_water"]
# 
# tp_waste = turnpoints(south_w)
# summary(tp_waste)
# 
# Peaks <- function(x, level = 0.05) {
#   if (!inherits(x, "turnpoints"))
#     stop("x must be a 'turnpoints' object!")
#   # Extract position and probability
#   tp.pos <- x$tppos
#   tp.proba <- x$proba
#   # We have both peaks and pits. Keep only peaks
#   keep <- 1:(x$nturns / 2) * 2
#   if (x$firstispeak) keep <- keep - 1
#   tp.pos <- tp.pos[keep]
#   tp.proba <- tp.proba[keep]
#   # Keep only peaks whose probability is lower than level
#   return(tp.pos[tp.proba < level])
# }
# 
# 
# p.all = Peaks(tp_waste, level = 1)
# 
# plot(south_w, type="o", cex = 1/4, main = "Southern Wastewater and all peaks")
# points(seq(south_w)[p.all], south_w[p.all], col = 2, cex = 1.5)
# 
# ##### only must significant ones
# p.50 = Peaks(tp_waste, level = 0.05)
# points(seq(south_w)[p.50], south_w[p.50], col = 3, cex = 2)
# 
# mtext("Peaks at level = 50%", col = 3)
# 
# plot(tp_waste, level = 0.01, lhorz = TRUE, lcol = 2, llty = 2,
#      type = "l", xlab = "data number", 
#      ylab = "Level 1%", 
#      main = "Information (turning points) ")
# 
# plot(south_w, type="o", cex = 1/4, main = "Southern Wastewater and all peaks")
# lines(tp_waste)
# 
# 
# south_w7 = SMA(south_w, n=7)
# plot.ts(south_w7)
# 
# waster_s = ts(south_w, start = c(2020, as.numeric(format(startdate, "%j"))), frequency=365)
# 
# ######## exponential smoothing
# 
# south_w_exp = HoltWinters(waster_s, beta = FALSE, gamma = FALSE) 
# 
# south_w_exp$fitted
# south_w_exp$SSE
# south_w_exp
# 
# plot(south_w_exp)
# 
# 









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



#######################################################################################
############              ####################################################  
############ time series ############################################################
###########              ######################################################
###############################################################################
library(astsa)
library(TTR)
library(tseries)

library(forecast)
library(dLagM)
setwd("C:/Users/Yanjia/Desktop/research/BU Infection/covid/BU and CDC")

cases_wastewater = read.csv("case_sum_wastewater.csv", header = TRUE)[,c(-1)]
head(cases_wastewater)
tail(cases_wastewater)

range(cases_wastewater$sample_date)

#### first stage before December 2020

#https://www.gavi.org/vaccineswork/alpha-omicron-everything-you-need-know-about-coronavirus-variants-concern?gclid=CjwKCAjwu_mSBhAYEiwA5BBmf8ZfAcx5vHOPYBkN2Vajg5xtrFoIOeFWgUcHCYorxtqUCohGpBPFIhoCkT4QAvD_BwE


cases_wastewater$Boston

cases_wastewater["North_ratio"] = cases_wastewater$North_water/cases_wastewater$North_sum

cases_wastewater["South_ratio"] = cases_wastewater$South_water/cases_wastewater$South_sum


start1 = as.Date(range(cases_wastewater$sample_date)[1])
end1 = as.Date("2020-11-30")

stage1 = cases_wastewater[cases_wastewater$sample_date <= end1, ]

North_cases_1 = ts(stage1$North_sum,
                 start = c(2020, as.numeric(format(start1, "%j"))),
                 end = c(2020, as.numeric(format(end1, "%j"))),
                 frequency=365)

North_water_1 = ts(stage1$North_water, 
                 start = c(2020, as.numeric(format(start1, "%j"))),
                 end = c(2020, as.numeric(format(end1, "%j"))),
                 frequency=365)

North_ratio_1 = ts(stage1$North_ratio, 
                   start = c(2020, as.numeric(format(start1, "%j"))),
                   end = c(2020, as.numeric(format(end1, "%j"))),
                   frequency=365)

ts.plot(North_cases_1, North_water_1, type = "l",
        main = c(paste("Northern Covid Cases and Wastewater till 2020-11-30"),
                 paste("Before Alpha")),
        gpars=list(xlab="Date", ylab="Values", col= c(2,3)))
legend("topleft",                           # Add legend to plot
       c("Reported Cases", "Wastewater Copies"),
       lty = 1,
       col = c(2,3))

ts.plot(North_ratio_1, type = "l",
        main = c(paste("Northern Wastewater and Covid Cases Ratio till 2020-11-30"),
                 paste("Before Alpha")),
        gpars=list(xlab="Date", ylab="Wastewater/Covid Cases Ratio", col= 5))


North_cases_17= ts(SMA(stage1$North_sum, n=7),
                   start = c(2020, as.numeric(format(start1, "%j"))),
                   end = c(2020, as.numeric(format(end1, "%j"))),
                   frequency=365)

North_water_17= ts(SMA(stage1$North_water, n=7),
                   start = c(2020, as.numeric(format(start1, "%j"))),
                   end = c(2020, as.numeric(format(end1, "%j"))),
                   frequency=365)

ts.plot(North_cases_17, North_water_17, type = "l",
        main = c(paste("Northern Covid Cases and Wastewater till 2020-11-30"),
                 paste("Before Alpha")),
        gpars=list(xlab="Date", ylab="Values", col= c(2,3)))

legend("topleft",                           # Add legend to plot
       c("Reported Cases 7 days Avg.", "Wastewater Copies 7 days Avg."),
       lty = 1,
       col = c(2,3))


North_ratio_17= ts(SMA(stage1$North_water, n=7)/SMA(stage1$North_sum, n=7),
                   start = c(2020, as.numeric(format(start1, "%j"))),
                   end = c(2020, as.numeric(format(end1, "%j"))),
                   frequency=365)

North_ratio_17= ts(SMA(stage1$North_ratio, n=7),
                   start = c(2020, as.numeric(format(start1, "%j"))),
                   end = c(2020, as.numeric(format(end1, "%j"))),
                   frequency=365)

ts.plot(North_ratio_17, type = "l",
        main = c(paste("Northern Wastewater and Covid Cases Ratio till 2020-11-30"),
                 paste("7 days Average Before Alpha")),
        gpars=list(xlab="Date", ylab="Wastewater/Covid Cases Ratio", col= 5))


c1 = ccf(stage1$North_sum, stage1$North_water, ylab = "CCovF", type= "covariance")


#with freq=1

c1 = ccf(North_cases_17, North_water_17, ylab = "CCovF", type= "covariance")




info1 = c( c1[0]$lag[1], c1[0]$acf[1])



plot(stage1$North_sum,stage1$North_water, 
     main = "Reported Cases Vs. Wastewater Copie",
     xlab = "Reported Cases", ylab = "Wastewater Copies") 
legend('topleft', legend = info1)


#assumption  stationary

# first difference 


stage1$North_water %>% diff() %>% ggtsdisplay(main="")
stage1$North_sum %>% diff() %>% ggtsdisplay(main="")

##### adf test Null H: unit root exits

adf.test(North_cases_1)
adf.test(North_water_1)

adf.test(North_ratio_1)

north_water_s1 = stage1$North_water %>% diff()
north_cases_s1 = stage1$North_sum %>% diff()

length(north_water_s1)

south_water_s1 = stage1$South_water %>% diff()
south_cases_s1 = stage1$South_sum %>% diff()

adf.test(north_cases_s1)
adf.test(north_water_s1)
ccf(north_cases_s1,north_water_s1)

dlmfit1 = dlm(x = north_water_s1, y = north_cases_s1, q = 5)
dlmfit1_2 = dlm(x = north_water_s1[1:149], y = north_cases_s1[1:149], q = 4)

summary(dlmfit1)

GoF(dlmfit1, dlmfit1_2)

finiteDLMauto(x = north_water_s1, y = north_cases_s1,
              q.min = 2, q.max = 5, 
              model.type = "dlm", 
              error.type = "AIC", trace = FALSE)

north_cases_pred1 = forecast(dlmfit1, x = north_water_s1[149:152], h = 1,
                            interval = FALSE )


plot(c(1,2,3,4),north_cases_s1[149:152],
     type = "l",
     col = 2,
     # labels = format(wastewater$Sample_date, "%b %Y"),
     xlab = "Time",
     main = "Northern",
     ylab = "First Difference Values" )

lines(c(1,2,3,4),north_cases_pred1,
      type = "l",
      col = 2,)


legend("topleft",                           # Add legend to plot
       c("Y", "Fitted Values"),
       lty = 1,
       col = c(2,3))


fitted(dlmfit1)

#### Stage 2 befor the delta : 2020-12-01- 2021-04-30


start2 = as.Date("2020-12-01")
end2 = as.Date("2021-04-30")

stage2 = cases_wastewater[cases_wastewater$sample_date > "2020-11-30" & cases_wastewater$sample_date <= "2021-04-30" , ]

North_cases_2 = ts(stage2$North_sum,
                   start = c(2020, as.numeric(format(start2, "%j"))),
                   end = c(2021, as.numeric(format(end2, "%j"))),
                   frequency=365)

North_water_2 = ts(stage2$North_water, 
                   start = c(2020, as.numeric(format(start2, "%j"))),
                   end = c(2021, as.numeric(format(end2, "%j"))),
                   frequency=365)



ts.plot(North_cases_2, North_water_2, type = "l",
        main = c(paste("Northern Covid Cases and Wastewater 2020-12-1--2021-4-30"),
                 paste("Alpha & Beta")),
        gpars=list(xlab="Date", ylab="Values", col= c(2,3)))
legend("topright",                           # Add legend to plot
       c("Reported Cases", "Wastewater Copies"),
       lty = 1,
       col = c(2,3))

North_cases_27= ts(SMA(stage2$North_sum, n=7),
                   start = c(2020, as.numeric(format(start2, "%j"))),
                   end = c(2021, as.numeric(format(end2, "%j"))),
                   frequency=365)

North_water_27= ts(SMA(stage2$North_water, n=7),
                   start = c(2020, as.numeric(format(start2, "%j"))),
                   end = c(2021, as.numeric(format(end2, "%j"))),
                   frequency=365)

ts.plot(North_cases_27, North_water_27, type = "l",
        main = c(paste("Northern Covid Cases and Wastewater 2020-12-1--2021-4-30"),
                 paste("Alpha &n Beta")),
        gpars=list(xlab="Date", ylab="Values", col= c(2,3)))

legend("topright",                           # Add legend to plot
       c("Reported Cases 7 days Avg.", "Wastewater Copies 7 days Avg."),
       lty = 1,
       col = c(2,3))

North_cases_27= ts(SMA(stage2$North_sum, n=7),
                   start = c(2020, as.numeric(format(start2, "%j"))),
                   end = c(2021, as.numeric(format(end2, "%j"))),
                   frequency=365)

North_water_27= ts(SMA(stage2$North_water, n=7),
                   start = c(2020, as.numeric(format(start2, "%j"))),
                   end = c(2021, as.numeric(format(end2, "%j"))),
                   frequency=1)


c2= ccf(North_cases_2,North_water_2, ylab = "CCovF", type= "covariance")


ccf(stage2$North_sum,stage2$North_water, ylab = "CCovF", type= "covariance")


info2 = c( c2[0]$lag[1], c2[0]$acf[1])

plot(stage2$North_sum,stage2$North_water, 
     main = "Reported Cases Vs. Wastewater Copie",
     xlab = "Reported Cases", ylab = "Wastewater Copies") 
legend('topleft', legend = info2)

#assumption  stationary
# first difference 


stage2$North_water %>% diff() %>% ggtsdisplay(main="")
stage2$North_sum %>% diff() %>% ggtsdisplay(main="")

##### adf test Null H: unit root exits

adf.test(North_cases_2)
adf.test(North_water_2)

#adf.test(North_ratio_2)

north_water_s2 = stage2$North_water %>% diff()
north_cases_s2 = stage2$North_sum %>% diff()

length(north_water_s2)


adf.test(north_cases_s2)
adf.test(north_water_s2)
ccf(north_cases_s2,north_water_s2)

dlmfit2 = dlm(x = north_water_s2, y = north_cases_s2, q = 3)

dlmfit1_2 = dlm(x = north_water_s1[1:149], y = north_cases_s1[1:149], q = 4)

summary(dlmfit2)

GoF(dlmfit2)

finiteDLMauto(x = north_water_s2, y = north_cases_s2,
              q.min = 1, q.max = 5, 
              model.type = "dlm", 
              error.type = "AIC", trace = FALSE)




#### Stage 3 before Omicron Nov 2021

start3 = as.Date("2021-05-01")
end3 = as.Date('2021-10-31')

stage3 = cases_wastewater[cases_wastewater$sample_date > "2021-04-30" & cases_wastewater$sample_date <= "2021-10-31"  , ]


North_cases_3 = ts(stage3$North_sum,
                   start = c(2021, as.numeric(format(start3, "%j"))),
                   end = c(2021, as.numeric(format(end3, "%j"))),
                   frequency=365)
North_water_3 = ts(stage3$North_water, 
                   start = c(2021, as.numeric(format(start3, "%j"))),
                   end = c(2021, as.numeric(format(end3, "%j"))),
                   frequency=365)



ts.plot(North_cases_3, North_water_3, type = "l",
        main = c(paste("Northern Covid Cases and Wastewater 2021-5-1--2021-10-31"),
                 paste("Delta")),
        gpars=list(xlab="Date", ylab="Values", col= c(2,3)))
legend("topleft",                           # Add legend to plot
       c("Reported Cases", "Wastewater Copies"),
       lty = 1,
       col = c(2,3))



North_cases_37= ts(SMA(stage3$North_sum, n=7),
                   start = c(2021, as.numeric(format(start3, "%j"))),
                   end = c(2021, as.numeric(format(end3, "%j"))),
                   frequency=365)


North_water_37= ts(SMA(stage3$North_water, n=7),
                   start = c(2021, as.numeric(format(start3, "%j"))),
                   end = c(2021, as.numeric(format(end3, "%j"))),
                   frequency=365)


ts.plot(North_cases_37, North_water_37, type = "l",
        main = c(paste("Northern Covid Cases and Wastewater 2021-5-1--2021-10-31"),
                 paste("Delta")),
        gpars=list(xlab="Date", ylab="Values", col= c(2,3)))

legend("topleft",                           # Add legend to plot
       c("Reported Cases 7 days Avg.", "Wastewater Copies 7 days Avg."),
       lty = 1,
       col = c(2,3))



ccf(North_cases_3, North_water_3,20, main= "Reported Cases Vs. Wastewater Copie",
    ylab = "CCF")

c3= ccf(stage3$North_sum, stage3$North_water, ylab = "CCovF", type= "covariance")
info3 = c( c3[2]$lag[1], c3[2]$acf[1])

plot(stage3$North_sum[3:length(North_cases_3)],
     North_water_3[1:(length(North_cases_3)-2)], 
     main = "Reported Cases Vs. Wastewater Copie",
     xlab = "Reported Cases", ylab = "Wastewater Copies") 
legend('topleft', legend = info3)

plot(stage3$North_sum,
     North_water_3, 
     main = "Reported Cases Vs. Wastewater Copie",
     xlab = "Reported Cases", ylab = "Wastewater Copies") 


#assumption  stationary
# first difference 


stage3$North_water %>% diff() %>% ggtsdisplay(main="")
stage3$North_sum %>% diff() %>% ggtsdisplay(main="")

north_water_s3 %>% diff() %>% ggtsdisplay(main="")
##### adf test Null H: unit root exits

adf.test(North_cases_3)
adf.test(North_water_3)

#adf.test(North_ratio_2)

north_water_s3 = stage3$North_water %>% diff()
north_cases_s3 = stage3$North_sum %>% diff()


length(north_water_s3)


adf.test(north_cases_s3)
adf.test(north_water_s3)

ccf(north_cases_s3,north_water_s3)

dlmfit3 = dlm(x = stage, y = north_cases_s3, q = 5)

summary(dlmfit3)

GoF(dlmfit2)

finiteDLMauto(x = north_water_s2, y = north_cases_s2,
              q.min = 1, q.max = 5, 
              model.type = "dlm", 
              error.type = "AIC", trace = FALSE)



##### stage 4 Omicron 2021-11 - now

start4 = as.Date("2021-11-01")

stage4 = cases_wastewater[cases_wastewater$sample_date > "2021-10-31", ]


North_cases_4 = ts(stage4$North_sum,
                   start = c(2021, as.numeric(format(start4, "%j"))),
                   
                   frequency=365)
North_water_4 = ts(stage4$North_water, 
                   start = c(2021, as.numeric(format(start4, "%j"))),
                  
                   frequency=365)


ts.plot(North_cases_4, North_water_4, type = "l",
        main = "Northern Covid Cases and Wastewater 2021-11-1 - now",
        gpars=list(xlab="Date", ylab="Values", col= c(2,3)))
legend("topright",                           # Add legend to plot
       c("Reported Cases", "Wastewater Copies"),
       lty = 1,
       col = c(2,3))


North_cases_47= ts(SMA(stage4$North_sum, n=7),
                   start = c(2021, as.numeric(format(start4, "%j"))),
                   frequency=365)


North_water_47= ts(SMA(stage4$North_water, n=7),
                   start = c(2021, as.numeric(format(start4, "%j"))),
                   frequency=365)


ts.plot(North_cases_47, North_water_47, type = "l",
        main = c(paste("Northern Covid Cases and Wastewater 2021-11-1 - now"),
                 paste("Omicron")),
        gpars=list(xlab="Date", ylab="Values", col= c(2,3)))

legend("topright",                           # Add legend to plot
       c("Reported Cases 7 days Avg.", "Wastewater Copies 7 days Avg."),
       lty = 1,
       col = c(2,3))



c4 = ccf(stage4$North_sum, stage4$North_water, ylab = "CCovF", type= "covariance")

ccf(North_cases_4, North_water_4, main= "Reported Cases Vs. Wastewater Copie",
    ylab = "CCF")

info4 = c( c4[5]$lag[1], round(c4[5]$acf[1], 3))

plot(stage4$North_sum[6:length(North_cases_3)],
     stage4$North_water[1:(length(North_cases_3)-5)], 
     main = "Reported Cases Vs. Wastewater Copie",
     xlab = "Reported Cases", ylab = "Wastewater Copies") 


plot(stage4$North_sum,
     stage4$North_water, 
     main = "Reported Cases Vs. Wastewater Copie",
     xlab = "Reported Cases", ylab = "Wastewater Copies") 

legend('topleft', legend = info4)

#assumption  stationary
# first difference 
##### adf test Null H: unit root exits

adf.test(North_cases_4)
adf.test(North_water_4)

# stage4$North_water %>% diff() %>% ggtsdisplay(main="")
# stage4$North_sum %>% diff() %>% ggtsdisplay(main="")
# 
# 
# 
# #adf.test(North_ratio_2)
# 
# north_water_s4 = stage4$North_water %>% diff()
# north_cases_s4 = stage4$North_sum %>% diff()
# 
# 
# length(north_water_s3)
# 
# 
# adf.test(north_cases_s4)
# adf.test(north_water_s4)
# 
ccf(North_cases_4,North_water_4)

dlmfit4 = dlm(x = stage4$North_water, y = stage4$North_sum, q = 13)

summary(dlmfit4)

GoF(dlmfit2)

finiteDLMauto(x = north_water_s2, y = north_cases_s2,
              q.min = 1, q.max = 5, 
              model.type = "dlm", 
              error.type = "AIC", trace = FALSE)


################################
################ South ########
###############################

cases_wastewater$Boston

cases_wastewater["North_ratio"] = cases_wastewater$North_water/cases_wastewater$North_sum

cases_wastewater["South_ratio"] = cases_wastewater$South_water/cases_wastewater$South_sum

cases_wastewater$South_boston = cases_wastewater$Boston/2 + cases_wastewater$South_sum
head(cases_wastewater$South_boston)

start1 = as.Date(range(cases_wastewater$sample_date)[1])
end1 = as.Date("2020-11-30")

stage1 = cases_wastewater[cases_wastewater$sample_date <= end1, ]


South_cases_1 = ts(stage1$South_boston, 
                   start = c(2020, as.numeric(format(start1, "%j"))),
                   end = c(2020, as.numeric(format(end1, "%j"))),
                   frequency=365)

South_water_1 = ts(stage1$South_water, 
                   start = c(2020, as.numeric(format(start1, "%j"))),
                   end = c(2020, as.numeric(format(end1, "%j"))),
                   frequency=365)

# North_ratio_1 = ts(stage1$North_ratio, 
#                    start = c(2020, as.numeric(format(start1, "%j"))),
#                    end = c(2020, as.numeric(format(end1, "%j"))),
#                    frequency=365)

ts.plot(South_cases_1, South_water_1, type = "l",
        main = c(paste("Northern Covid Cases and Wastewater till 2020-11-30"),
                 paste("Before Alpha")),
        gpars=list(xlab="Date", ylab="Values", col= c(2,3)))
legend("topleft",                           # Add legend to plot
       c("Reported Cases", "Wastewater Copies"),
       lty = 1,
       col = c(2,3))



adf.test(South_cases_1)
adf.test(South_water_1)

South_cases_s1 = stage1$South_boston %>% diff()
stage1$South_boston %>% diff()%>% ggtsdisplay(main="")

South_water_s1 = stage1$South_water %>% diff()
stage1$South_water %>% diff()%>% ggtsdisplay(main="")

adf.test(South_cases_s1)
adf.test(South_water_s1)

ccf(South_water_s1, South_cases_s1)

dlmfit1 = dlm(x = South_cases_s1 , y = South_water_s1, q = 6)
summary(dlmfit1)

### Stage2 
start2 = as.Date("2020-12-01")
end2 = as.Date("2021-04-30")

stage2 = cases_wastewater[cases_wastewater$sample_date > "2020-11-30" & cases_wastewater$sample_date <= "2021-04-30" , ]

South_cases_2 = ts(stage2$South_boston,
                   start = c(2020, as.numeric(format(start2, "%j"))),
                   end = c(2021, as.numeric(format(end2, "%j"))),
                   frequency=365)

South_water_2 = ts(stage2$South_water, 
                   start = c(2020, as.numeric(format(start2, "%j"))),
                   end = c(2021, as.numeric(format(end2, "%j"))),
                   frequency=365)



ts.plot(South_cases_2, South_water_2, type = "l",
        main = c(paste("Northern Covid Cases and Wastewater 2020-12-1--2021-4-30"),
                 paste("Alpha & Beta")),
        gpars=list(xlab="Date", ylab="Values", col= c(2,3)))
legend("topright",                           # Add legend to plot
       c("Reported Cases", "Wastewater Copies"),
       lty = 1,
       col = c(2,3))



adf.test(South_cases_2)
adf.test(South_water_2)

South_cases_s2 = stage2$South_boston %>% diff()
stage2$South_boston %>% diff()%>% ggtsdisplay(main="")

South_water_s2 = stage2$South_water %>% diff()
stage2$South_water %>% diff()%>% ggtsdisplay(main="")

adf.test(South_cases_s2)
adf.test(South_water_s2)

ccf(South_water_s2, South_cases_s2)

dlmfit2 = dlm(x = South_cases_s2 , y = South_water_s2, q = 6)
summary(dlmfit2)

### Stage3

start3 = as.Date("2021-05-01")
end3 = as.Date('2021-10-31')

stage3 = cases_wastewater[cases_wastewater$sample_date >= start3 & cases_wastewater$sample_date <= end3 , ]

South_cases_3 = ts(stage3$South_boston,
                   start = c(2021, as.numeric(format(start3, "%j"))),
                   end = c(2021, as.numeric(format(end3, "%j"))),
                   frequency=365)

South_water_3 = ts(stage3$South_water, 
                   start = c(2021, as.numeric(format(start3, "%j"))),
                   end = c(2021, as.numeric(format(end3, "%j"))),
                   frequency=365)



ts.plot(South_cases_3, South_water_3, type = "l",
        main = c(paste("Northern Covid Cases and Wastewater 2020-12-1--2021-4-30"),
                 paste("Alpha & Beta")),
        gpars=list(xlab="Date", ylab="Values", col= c(2,3)))
legend("topright",                           # Add legend to plot
       c("Reported Cases", "Wastewater Copies"),
       lty = 1,
       col = c(2,3))





adf.test(South_cases_3)
adf.test(South_water_3)

South_cases_s3 = stage3$South_boston %>% diff()
stage3$South_boston %>% diff()%>% ggtsdisplay(main="")

South_water_s3 = stage3$South_water %>% diff()
stage3$South_water %>% diff()%>% ggtsdisplay(main="")

adf.test(South_cases_s3)
adf.test(South_water_s3)

ccf(South_water_s3, South_cases_s3)

dlmfit3 = dlm(x = South_water_s3, y =  South_cases_s3 , q = 15)
summary(dlmfit3)


### Stage4
start4 = as.Date("2021-11-01")

stage4 = cases_wastewater[cases_wastewater$sample_date > "2021-10-31", ]


South_cases_4 = ts(stage4$South_boston,
                   start = c(2021, as.numeric(format(start4, "%j"))),
                   
                   frequency=365)

South_water_4 = ts(stage4$South_water, 
                   start = c(2021, as.numeric(format(start4, "%j"))),
                   
                   frequency=365)


ts.plot(South_cases_4, South_water_4, type = "l",
        main = "Northern Covid Cases and Wastewater 2021-11-1 - now",
        gpars=list(xlab="Date", ylab="Values", col= c(2,3)))
legend("topright",                           # Add legend to plot
       c("Reported Cases", "Wastewater Copies"),
       lty = 1,
       col = c(2,3))


adf.test(South_cases_4)
adf.test(South_water_4)

South_cases_s4 = stage4$South_boston %>% diff()
stage4$South_boston %>% diff()%>% ggtsdisplay(main="")

South_water_s4 = stage4$South_water %>% diff()
stage4$South_water %>% diff()%>% ggtsdisplay(main="")

adf.test(South_cases_s4)
adf.test(South_water_s4)

ccf(South_water_s4, South_cases_s4)

dlmfit4 = dlm(x =South_water_s1  , y = South_cases_s4, q = 15)
summary(dlmfit4)









library(timeSeries)  


North_cases3 = SMA(North_cases, n=7)


ts.plot(North_cases3, North_cases, type = "l",main = "Northern Covid Cases",
        gpars=list(xlab="Date", ylab="Cases", col= c(2,3)))
legend("topleft",                           # Add legend to plot
       c("MA=7", "Reported"),
       lty = 1,
       col = c(2,3))


North_water3 = SMA(North_water, n=7) 
ts.plot(North_water3, North_water, type = "l",main = "Northern Wastewater",
        gpars=list(xlab="Date", ylab="Number of Copies/ml", col= c(2,3)))
legend("topleft",                           # Add legend to plot
       c("MA=7", "Reported"),
       lty = 1,
       col = c(2,3))


North_waterseries = ts(North_water3, 
                  start = c(2020,as.numeric(format(as.Date(cases_wastewater$sample_date[8]), "%j"))),
                  frequency=365)

North_caseseries = ts(North_cases3, 
                       start = c(2020,as.numeric(format(as.Date(cases_wastewater$sample_date[8]), "%j"))),
                       frequency=365)

ts.plot(North_waterseries,North_caseseries, type = "l",main = "Northern Wastewater and Cases",
        gpars=list(xlab="Date", ylab="values", col= c(2,4)))
legend("topleft",                           # Add legend to plot
       c("Wastewater", "Reported Cases"),
       lty = 1,
       col = c(2,4))


#### stationary 

library(tseries)

library(forecast)

##### adf test Null H: unit root exits

adf.test(North_cases3[8:length(North_cases3)], k= 6)

adf.test(North_cases, k= 4)



acf(North_cases3[8:length(North_cases3)], lag.max = 200)

North_cases3_for = auto.arima(North_cases3[8:length(North_cases3)])

acf(North_cases3_for$residuals, lag.max = 20)

Box.test(North_cases3_for$residuals, lag = 20, type = "Ljung-Box")


North_cases_for = auto.arima(North_cases)

acf(North_cases_for$residuals, lag.max = 20)

Box.test(North_cases_for$residuals, lag = 20, type = "Ljung-Box")




North_water3_for = auto.arima(North_water3[8:length(North_water3)])
acf(North_water3_for$residuals, lag.max = 20)

Box.test(North_water3_for$residuals, lag = 20, type = "Ljung-Box")






ccf(North_water, North_cases)

North_ccf = ccf(cases_wastewater$North_water, cases_wastewater$North_sum)
North_ccf
