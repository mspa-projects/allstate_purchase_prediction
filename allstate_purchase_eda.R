setwd("~/Google Drive/MSPA/454 (Advanced Modeling Techniques)/team project/allstate purchase prediction")

train = read.csv("allstate_purchase_prediction/train.csv")
test = read.csv("allstate_purchase_prediction/test_v2.csv")

library(dplyr)
library(tidyr)

allstate_train_tbl = tbl_df(train)
allstate_test_tbl = tbl_df(test)

# mark response variables as factors
allstate_train_cust$A = factor(allstate_train_cust$A)
allstate_train_cust$B = factor(allstate_train_cust$B)
allstate_train_cust$C = factor(allstate_train_cust$C)
allstate_train_cust$D = factor(allstate_train_cust$D)
allstate_train_cust$E = factor(allstate_train_cust$E)
allstate_train_cust$F = factor(allstate_train_cust$F)
allstate_train_cust$G = factor(allstate_train_cust$G)

# derive hour from time
allstate_train_tbl$hour = as.numeric(substr(allstate_train_tbl$time,1,2))
allstate_test_tbl$hour = as.numeric(substr(allstate_test_tbl$time,1,2))

# bin the hour - train data
allstate_train_tbl$hod = 
  ifelse(((allstate_train_tbl$hour >= 0) & (allstate_train_tbl$hour < 3)), "12am - 3am", 
  ifelse(((allstate_train_tbl$hour >= 3) & (allstate_train_tbl$hour < 6)), "3am - 6am",
  ifelse(((allstate_train_tbl$hour >= 6) & (allstate_train_tbl$hour < 9)), "6am - 9am",
  ifelse(((allstate_train_tbl$hour >= 9) & (allstate_train_tbl$hour < 12)), "9am - 12pm",
  ifelse(((allstate_train_tbl$hour >= 12) & (allstate_train_tbl$hour < 15)), "12pm - 3pm",
  ifelse(((allstate_train_tbl$hour >= 15) & (allstate_train_tbl$hour < 18)), "3pm - 6pm",
  ifelse(((allstate_train_tbl$hour >= 18) & (allstate_train_tbl$hour < 21)), "6pm - 9pm",
  ifelse(((allstate_train_tbl$hour >= 21)), "9pm - 12am",  
    "unknown"
  ))))))))

# bin the hour - test data
allstate_test_tbl$hod = 
  ifelse(((allstate_test_tbl$hour >= 0) & (allstate_test_tbl$hour < 3)), "12am - 3am", 
  ifelse(((allstate_test_tbl$hour >= 3) & (allstate_test_tbl$hour < 6)), "3am - 6am",
  ifelse(((allstate_test_tbl$hour >= 6) & (allstate_test_tbl$hour < 9)), "6am - 9am",
  ifelse(((allstate_test_tbl$hour >= 9) & (allstate_test_tbl$hour < 12)), "9am - 12pm",
  ifelse(((allstate_test_tbl$hour >= 12) & (allstate_test_tbl$hour < 15)), "12pm - 3pm",
  ifelse(((allstate_test_tbl$hour >= 15) & (allstate_test_tbl$hour < 18)), "3pm - 6pm",
  ifelse(((allstate_test_tbl$hour >= 18) & (allstate_test_tbl$hour < 21)), "6pm - 9pm",
  ifelse(((allstate_test_tbl$hour >= 21)), "9pm - 12am",  
    "unknown"
  ))))))))


# extract rows for purchases only (ie., rows with record_type == 1)
allstate_train_cust = filter(allstate_train_tbl, record_type == 1)

# draw some plots
library(e1071)
library(ggplot2)
library(lattice)

# scatter plots
p = qplot(age_oldest,age_youngest,colour=factor(A),data=allstate_train_cust)
p + scale_colour_manual(values=c("red","green","black"))

# histograms
histogram(~ hour | factor(A), data = allstate_train_cust,
          ref = TRUE,
          main = "Histogram of hour of purchase",
          xlab = "Hour"
          )

# bar plots
# interesting plots
barchart(day ~ n, 
         data = as.data.frame(allstate_train_tbl %>%
                                         select(day, A) %>%
                                         group_by(day, A) %>%
                                         summarise(n=n())), 
         groups = A, stack = TRUE, 
         main = list(label="Distribution of purchases per day",cex=1.6),
         xlab = "Total purchases",
         ylab = "Day of the week",
         auto.key = list(space='right', cex=1.2, title = "A")
  )

