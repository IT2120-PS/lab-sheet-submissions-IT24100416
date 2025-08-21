#1.
setwd("C:/Users/IT24100416/Desktop/IT24100416")
branch_data <- read.table("Exercise.txt", header = TRUE, sep = ",")
head(branch_data)

#2
str(branch_data)

#3
boxplot(branch_data$Sales_X1, main = "Boxplot of Sales",
        ylab = "Sales (X1)", col = "lightgreen", border = "darkgreen")

#4
summary(branch_data$Advertising_X2)
fivenum(branch_data$Advertising_X2)
IQR(branch_data$Advertising_X2)

#5
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- IQR(x)
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  outliers <- x[x < lower | x > upper]
  return(outliers)
}

find_outliers(branch_data$Years_X3)
