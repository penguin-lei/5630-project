library(readxl)
library(arsenal)
library(glmnet)
library(scales)
library(xtable)
library(kableExtra)

descriptive = function(sheet_name, data, descriptive_variables)
{
  my_controls <- tableby.control(
    total = T,
    numeric.stats = c("meansd",
                      "medianq1q3",
                      "range",
                      "Nmiss2"),
    
    cat.stats = c("countpct",
                  "Nmiss2"),
    
    stats.labels = list(
      meansd= "Mean (SD)",
      medianq1q3= "Median (Q1, Q3)",
      range= "Min - Max",
      Nmiss2= "Missing"
    )
  )
  descriptive_formula = as.formula(paste(response_variable, '~', paste(descriptive_variables, collapse = '+'),sep = ''))
  table1 <- tableby(descriptive_formula, 
                    data=data,
                    control = my_controls)
  tab1 = as.data.frame(summary(table1))
  tab1[,1] = gsub('&nbsp;', '', tab1[,1])
  # write.csv(tab1, file = paste(as.character(sheet_name), '_descriptive.csv', sep = ''))
  print(summary(table1, digits = 1))
}

