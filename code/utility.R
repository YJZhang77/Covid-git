
library(pracma)
library(ggrepel)


########### transpose the data frame and implement missing values with the average of (-5, +5)

cases.replace_missing = function(Data_Name, Town_Name, Date_Range){
  # Data_Name: the original data frame, each row represent the a town
  # Town_Name: the town name list for South and North, and Boston
  # Data_Range: is the data range from Junly 1s 2020 to March 22nd 2022
  
  part_cases = Data_Name[Data_Name[,1] %in% Town_Name, ] 
  n = ncol(part_cases)
  date = names(part_cases)[2:n]
  case_date = as.Date(substring(date, 2), format = "%m.%d.%Y")
  
  col_names = c('sample_date', Town_Name)
  
  tmp.cases = data.frame(matrix(ncol=length(col_names), nrow= length(case_date)))
  
  colnames(tmp.cases) = col_names
  
  tmp.cases$sample_date = case_date
  # cases$Weekday = Weekday
  
  for (i in Town_Name){
    
    tmp.cases[i] = as.numeric(unlist(part_cases[part_cases$X == i, 2:n], use.names = FALSE))
  }
  
  tmp = merge(Date_Range, tmp.cases, by ='sample_date', all.x=TRUE)
  
  n = nrow(tmp)
  case = tmp
  
  for (i in Town_Name){
    for (j in 1:n){
      if (is.na(tmp[j, i]) & j-5 < 1 & j+5 <= n ){ 
        case[j,i] = round(mean(tmp[1:j+5, i], na.rm = T))
      }
      else if ( is.na(tmp[j, i]) & j-5 >= 1 & j+5 <= n){ 
        case[j,i] = round(mean(tmp[(j-5):j+5, i],na.rm = T))
      }
      else if ( is.na(tmp[j, i]) & j-5 >= 1 & j+5 > n){ 
        case[j,i] = round(mean(tmp[(j-5):n, i],na.rm = T))
      }
      else case[j, i] = tmp[j,i] 
    }
  }
  
  if (nrow(case[case$sample_date=="2020-10-23",]) > 1) {case = case[-c(115),]}
  return(case)
}

### get the cleaned case data frame

case.data <- function(overeall_data, town.names){
  # overall.data: the original data frame, each row represent the a town
  # town.names: the data frame with south, north, and Boston names
  # date.list : the date time 
  
  overeall_data[overeall_data == "<5"] = 2.5
  
  date = names(overeall_data)[2:ncol(overeall_data)]
  
  sample_date = as.Date(substring(date, 2), format = "%m.%d.%Y")
  
  date_range = seq(min(sample_date), max(sample_date), by = 1) 
  date_range[!date_range %in% sample_date]
  
  Weekday = strftime(date_range, format = "%A")
  
  date_list = data.frame("sample_date" = date_range,
                         "Weekday" = Weekday)
  
  North_town_cases= cases.replace_missing(overeall_data, town.names[["North"]], date_list)
  
  South_town_cases = cases.replace_missing(overeall_data, town.names[["South"]], date_list)
  
  Boston_cases = cases.replace_missing(overeall_data, town.names[["Boston"]], date_list)
  
  North_sum = rowSums(North_town_cases[, 3:ncol(North_town_cases)])
  South_sum = rowSums(South_town_cases[, 3:ncol(South_town_cases)])
  
  All_case = date_list
  All_case$North_sum =  North_sum
  All_case$South_sum =  South_sum
  All_case$Boston =  Boston_cases$Boston
  
  return(All_case)
  
}




######## check if they have the same date peaks and vallens


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