
library(pracma)
library(ggrepel)


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



#### implement missing values with the average of (-5, +5)
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