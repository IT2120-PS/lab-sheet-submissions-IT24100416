#Set working Directory
setwd("C:\\Users\\MSI\\Desktop\\IT24100416")

#Question 1
#Null Hypothesis (H0):
#Customers choose snacks A, B, C, and D equally ? each snack has the same chance of being chosen.

#Alternative Hypothesis (H1):
#Customers do not choose all snacks equally ? at least one snack is more or less popular than the others.

#Question2
# Observed data (number of purchases)
observed <- c(120, 95, 85, 100)

# Expected probabilities (equal probability for all snacks)
expected_prob <- rep(0.25, 4)

# Total number of purchases
total_purchases <- sum(observed)

# Expected counts for each snack (total purchases * 0.25 for each)
expected <- expected_prob * total_purchases

# Perform the chi-squared test
chisq_result <- chisq.test(observed, p = expected_prob)

# Output the chi-squared test result
chisq_result

#Question 3
#Since the p-value (0.08966) is more than 0.05, we do not reject the null hypothesis.
#This means there is no strong evidence that customers prefer one snack over another.
#So, we can accept the vending machine owner's claim that all four snack types (A, B, C, D) are chosen equally.


