#Setting the directory
setwd("C:\\Users\\MSI\\Desktop\\R")

#Importing the data set
data<-read.table("Data.txt",header=TRUE,sep=",")

#view the file in a separate window
fix(data)

#Attach the file into R.
attach(data)

#part1
#Renaming the variables
names(data)<-c("X1","X2")

#Attach the file into R again as we ranamed the variables.
attach(data)

#Obtain histogram for number of shareholders
hist(X2, main = "Histogram for Number of Shareholders")

#part2
histogram<-hist(X2,main="Histogram for Number of Shareholders",breaks = seq(130,270,length = 8),right = FALSE)

#check how much each argument inside "hist" command works using "help" command as follows
?hist

#part3
#Assign class limits of the frequency distribution into a variable called "breaks"
breaks<- round(histogram$breaks)

#Assigns class frequencies of the histogram into a variable called "freq"
freq<-histogram$counts

#Assign mid point of each class into a variable called "mids"
mids <- histogram$mids

#creating the variable called "Classes" for the frequency distribution
classes <- c()

#Creating a "for" loop to assign classes of the frequency distribution into "Classes" variable created above.
for(i in 1:length(breaks)-1){
  classes[i] <- paste0("[",breaks[i],",",breaks[i+1],")")
}

#"cbind" command used to merge the columns with same length
cbind(Classes = classes, Frequency = freq)

#Part4
#Draw frequency polygon to the same plot
lines(mids,freq)

#Draw frequency polygon in a new plot
plot(mids, freq,type = 'l',main = "Frequency Polygon for Shareholders",xlab = "Shareholders", ylab = "Frequency",ylim = c(0,max(freq)))

#Part5
#Using "cumsum" command we can get cumulative frequencies
cum.freq <- cumsum(freq)
new<-c()
#Using "for" loop to store cumulative frequncies in order to get the ogive
for(i in 1:length(breaks)){
  if(i==1){
    new[i]=0
  }else{
    new[i] = cum.freq[i-1]
  }
}

#Draw cumulative frequency polygon ina new plot
plot(breaks, new,type = 'l',main = "Cumalative Frequency Polygon for Shareholders",
     xlab = "Shareholders", ylab = "Cumulative Frequency", ylim = c(0,max(cum.freq)))

#Obtain upper limit of each class along with its cumulative in a table
cbind(Upper = breaks, CumFreq = new)


##Exercise
setwd("C:\\Users\\MSI\\Desktop\\R")
getwd()

Delivery_Times <- read.table("Exercise - Lab 05.txt", header = TRUE, sep = ",")
fix(Delivery_Times)
head(Delivery_Times)
str(Delivery_Times)


breaks_seq <- seq(20, 70, length.out = 10)  # Breaks: 20, 25.5556, ..., 70
hist(Delivery_Times$Delivery_Time_.minutes., breaks = breaks_seq, right = FALSE, 
     main = "Histogram of Delivery Times", xlab = "Delivery Time (minutes)", 
     ylab = "Frequency", col = "lightblue", border = "black")
print(breaks_seq)

# Optional: Get frequency table from hist for verification
hist_info <- hist(Delivery_Times$Delivery_Time_.minutes., breaks = breaks_seq, right = FALSE, plot = FALSE)
freq_table <- data.frame(Intervals = paste("[", round(breaks_seq[-length(breaks_seq)], 2), ", ", round(breaks_seq[-1], 2), ")", sep = ""),
                         Frequency = hist_info$counts)
print(freq_table)

# Compute cumulative frequencies
freq <- hist_info$counts  
cum_freq <- cumsum(freq)
# Create 'new' for ogive: starts at 0, then cumulative up to previous class
ogive_y <- c(0, cum_freq)  

# Upper bounds are the breaks
upper_bounds <- breaks_seq

# Plot the ogive
plot(upper_bounds, ogive_y, type = "o", pch = 19, col = "blue", 
     main = "Cumulative Frequency Polygon (Ogive) for Delivery Times", 
     xlab = "Upper Limit of Delivery Time (minutes)", ylab = "Cumulative Frequency", 
     ylim = c(0, max(ogive_y)))
abline(h = 0, col = "gray") 








































