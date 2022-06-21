
##############################################################################
############              ####################################################  
############ time series #####################################################
###########              #####################################################
##############################################################################
library(astsa)
library(TTR)
library(tseries)

library(forecast)
library(dLagM)

setwd("C:/Users/yzhang17/Desktop/Research/Covid/Covid-git")

source("code/utility.R")

cases_wastewater = read.csv("data/case_sum_wastewater.csv", header = TRUE)[,c(-1)]
head(cases_wastewater)
tail(cases_wastewater)

range(cases_wastewater$sample_date)

#### first stage before December 2020

#https://www.gavi.org/vaccineswork/alpha-omicron-everything-you-need-know-about-coronavirus-variants-concern?gclid=CjwKCAjwu_mSBhAYEiwA5BBmf8ZfAcx5vHOPYBkN2Vajg5xtrFoIOeFWgUcHCYorxtqUCohGpBPFIhoCkT4QAvD_BwE


cases_wastewater$Boston

cases_wastewater["North_ww.case.ratio"] = cases_wastewater$North_water/cases_wastewater$North_sum

cases_wastewater["South_ww.case.ratio"] = cases_wastewater$South_water/cases_wastewater$South_sum

cases_wastewater["water_sum"] = cases_wastewater$South_water + cases_wastewater$North_water

cases_wastewater["ww_boston.ratio"] = cases_wastewater$water_sum/cases_wastewater$Boston
cases_wastewater['case_sum'] = cases_wastewater$North_sum + cases_wastewater$South_sum + cases_wastewater$Boston
cases_wastewater["ww_case.ratio"] = cases_wastewater$water_sum/cases_wastewater$case_sum

col.names = names(cases_wastewater)[-c(1,2,8,9,11,12)]

##### different virus variation with different time frame

# 1) wild type (July 28, 2020 to November 31, 2020), 
# 2) alpha/beta (December 1, 2020, to April 30, 2021), 

# 3) delta (May 1, 2021, to October 31, 2021), 
# 4) omicron (December 1, 2021 to March 22, 2022) 

start.time = c(as.Date(range(cases_wastewater$sample_date)[1]),as.Date("2020-12-01"), as.Date("2021-05-01"), as.Date("2021-12-01") )
end.time = c(as.Date("2020-11-30"), as.Date("2021-04-30"),as.Date('2021-10-31'), as.Date(range(cases_wastewater$sample_date)[2]) )

stage.years = list()
stage.years[[1]] = c(2020, 2020)
stage.years[[2]] = c(2020, 2021)
stage.years[[3]] = c(2021, 2021)
stage.years[[4]] = c(2021, 2022)

stage.years[[1]][1]

stage.data = list()
stage.ts.data = list()

stage.adf.p =data.frame(matrix(NA, nrow = length(col.names), ncol = 4))

rownames(stage.adf.p) = col.names

for (i in 1:4){
  print(i)
  
  tmp.data = cases_wastewater[cases_wastewater$sample_date <= end.time[i] & cases_wastewater$sample_date > start.time[i], ]
  stage.data[[i]] = tmp.data
  print (end.time[i])
  
  timeseries.data = list()
  
  for (j in col.names){
    
    tmp.ts = ts(tmp.data[j],
                start = c(stage.years[[i]][1], as.numeric(format(start.time[i], "%j"))),
                end = c(stage.years[[i]][2], as.numeric(format(end.time[i], "%j"))),
                frequency=365)
    
    timeseries.data[[j]] = tmp.ts 
    
    # stationary test :Augmented Dickey-Fuller test, 
    # null hypothesis of a unit root of a univarate time series x 
    # (equivalently, x is a non-stationary time series, p>0.05)
    stage.adf.p[j,i] = adf.test(tmp.ts)$p.value 
    
    print(j)
  }
  
  stage.ts.data[[i]] = timeseries.data
  
}
length(stage.ts.data)

stage.data[[3]]$Boston




stage1.ccf = list()
stage1.ccf[["North"]] = ccf(stage.data[[1]]$North_water,stage.data[[1]]$North_sum, 
                            main = "Wild Type North WW vs. Cases",
                            ylab = "CCovF", type= "covariance")

stage1.ccf[["South"]] = ccf(stage.data[[1]]$South_water, stage.data[[1]]$South_sum, 
                            main = "Wild Type South WW vs. Cases",
                            ylab = "CCovF", type= "covariance")

stage1.ccf[["Boston"]] = ccf(stage.data[[1]]$water_sum, stage.data[[1]]$Boston, 
                             main = "Wild Type Overall WW vs. Boston Cases",
                             ylab = "CCovF", type= "covariance")


stage1.adf.result = matrix(NA, nrow = , )


result = adf.test(stage.ts.data[[3]]$ww_boston.ratio)






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
















