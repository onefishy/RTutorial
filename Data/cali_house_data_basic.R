setwd('~/RTutorial/Data') #set working directory

#---------------   example function: read in data and get a few properties
read_file <- function(file_name){
  mydata <- read.csv(file_name) #read csv file as r dataframe
  #class(mydata) what is the type of 'mydata'
  #ncol(mydata) how many columns in 'mydata'
  #nrow(mydata) how many rows in 'mydata'
  #head(mydata) prints the first rows of 'mydata'
  #colnames(mydata) prints the column names of 'mydata'
  data_summary <- list(type=class(mydata), ncols=ncol(mydata), 
                       nrow=nrow(mydata), head=head(mydata), 
                       colnames=names(mydata))
  
  return(list(mydata, data_summary)) #returns a datafram, and summary of dataframe
}

returned_val <- read_file(file_name="California_Home_Prices_(2009).csv")
mydata <- returned_val[[1]]
data_summary <- returned_val[[2]]

#-----------------------   describing the data
sprintf("type of read-in data: %s", data_summary[[1]])
sprintf("data is %d by %d", data_summary[[3]], data_summary[[2]])
print("names of the columns are:")
col_titles <- paste(data_summary[[5]], collapse=', ')
print(col_titles)

sprintf("type of column: %s", class(mydata$Location))
sprintf("number of unique values in column: %d", nlevels(mydata$Location))
print("unique values in column: ")
print(levels(mydata$Location))

#-----------------------   filtering and descriptive stats/summary

#by location
data_by_city <- subset(mydata, Location=="Cambria")
print(head(data_by_city))

#by location and type of sale
data_by_city_sale <- subset(mydata, Location=="Cambria" & Status=="Short Sale")
print(head(data_by_city_sale))

#mean
datat_by_city_sale_mean <- colMeans(subset(data_by_city_sale, 
                                           select = c(Price, Bedrooms, Bathrooms, Sqft, Price.Sqft)), 
                                    na.rm = TRUE)
class(datat_by_city_sale_mean)
print("means of cambria listings (short sale)")
print(datat_by_city_sale_mean)

#range
datat_by_city_sale_range <- sapply(subset(data_by_city_sale, 
                                          select = c(Price, Bedrooms, Bathrooms, Sqft, Price.Sqft)), 
                                   range,
                                   na.rm = TRUE)

class(datat_by_city_sale_range)
print("means of cambria listings (short sale)")
print(datat_by_city_sale_range)

#mean and std for a specific column
sprintf("the mean for price: %f", mean(mydata$Price))
sprintf("the std for price: %f", sd(mydata$Price))

#summary of subset of data
summary(subset(data_by_city_sale, 
               select = c(Price, Bedrooms, Bathrooms, Sqft, Price.Sqft)))


#-----------------------   data visualization

#histogram of short sale prices compared with overall prices
hist(mydata$Price, breaks=40, col=rgb(1,0,0,0.5))
hist(subset(mydata, Status=="Short Sale")$Price, breaks=20, col=rgb(0,0,1,0.5), add=T)
box()

#histogram of foreclosure prices compared with overall prices
hist(mydata$Price, breaks=40, col=rgb(1,0,0,0.5))
hist(subset(mydata, Status=="Foreclosure")$Price, breaks=20, col=rgb(0,0,1,0.5), add=T)
box()

#histogram of regular prices compared with overall prices
hist(mydata$Price, breaks=40, col=rgb(1,0,0,0.5)) 
hist(subset(mydata, Status=="Regular")$Price, breaks=20, col=rgb(0,0,1,0.5), add=T)
box()

#scatter plot of regular prices vs sqft with possible regression lines
plot(mydata$Sqft, mydata$Price, main="Price vs Sqft", col=rgb(1,0,0,0.5),
     xlab="Sqft", ylab="Price", pch=19)

abline(a=0, b=100, col=rgb(0,0,1,0.5), lwd=2)
abline(a=100, b=300, col=rgb(0,0,1,0.5), lwd=2)

#scatter plot of regular prices vs sqft compared with foreclosure prices vs sqft
plot(mydata$Sqft, mydata$Price, main="Price vs Sqft", col=rgb(1,0,0,0.5),
     xlab="Sqft", ylab="Price", pch=19)
points(subset(mydata, Status=="Foreclosure")$Sqft, 
       subset(mydata, Status=="Foreclosure")$Price, 
       col=rgb(0,0,1,0.5), pch=19)

legend("topleft", legend=c("Overall","Foreclosure"), 
       cex = 0.9, text.width=1700,
       col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),
       pch=c(19,19))


