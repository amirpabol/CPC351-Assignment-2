# install and load the necessary package
library(ggplot2)
library(dplyr)
library(tidyverse)
library(magrittr)

# read in the data from the csv file
superStore <- read.csv("SuperStoreOrders.csv")

# function to remove the commas(,) and empty spaces from number field
# function to remove the commas(,) and empty spaces from number field

numberize <- function(input){
  input <- gsub(",", "", input)
  input <- gsub(" ", "", input)
  return(as.numeric(input))
}

# Convert the OrderDate variable to a date data type
superStore$order_date <- as.Date(superStore$order_date, format="%m/%d/%Y")

# Convert the ShipDate variable to a date data type
superStore$ship_date <- as.Date(superStore$ship_date, format="%m/%d/%Y")

# Convert the Profit and Sales variables to numeric data types
superStore$profit <- numberize(superStore$profit)
superStore$sales <- numberize(superStore$sales)
superStore$customer_name <- as.factor(superStore$customer_name)
superStore$state <- as.factor(superStore$state)
superStore$country <- as.factor(superStore$country)
superStore$region <- as.factor(superStore$region)
superStore$sub_category <- as.factor(superStore$sub_category)
superStore$category <- as.factor(superStore$category)
superStore$product_name <- as.factor(superStore$product_name)
superStore$order_priority <- as.factor(superStore$order_priority)
superStore$ship_mode <- as.factor(superStore$ship_mode)
superStore$market <- as.factor(superStore$market)
superStore$segment <- as.factor(superStore$segment)
superStore$order_id <- as.factor(superStore$order_id)


summary(superStore)

superStore <- slice(superStore, 1:1000)

#Q9a by Nicolas Chuang
#creating data frame with rows with Consumer only
consumer_segment <- superStore[superStore$segment == "Consumer", ]

#creating vector with only consumer's category
consumer_category <- consumer_segment$category

#plotting bar graph of total sales of category of consumers
ggplot(data.frame(consumer_category), aes(x=consumer_category)) +
  geom_bar() + ggtitle("Sales of category of Consumers") + 
  theme(plot.title = element_text(hjust = 0.5))+ labs(y="Sales",x="Category")
#Referred from https://stackoverflow.com/questions/21639392/make-frequency-histogram-for-factor-variables

#creating data frame with rows with Corporate only
corporate_segment <- superStore[superStore$segment == "Corporate", ]

#creating vector with only consumer's category
corporate_category <- corporate_segment$category

#plotting bar graph of total sales of category of Corporate
ggplot(data.frame(corporate_category), aes(x=corporate_category)) +
  geom_bar() + ggtitle("Sales of category of Corporate") + 
  theme(plot.title = element_text(hjust = 0.5))+ labs(y="Sales",x="Category")

#creating data frame with rows with Home Office only
home_office_segment <- superStore[superStore$segment == "Home Office", ]

#creating vector with only Home office's category
home_office_category <- home_office_segment$category

#plotting bar graph of total sales of category of Home Office
ggplot(data.frame(home_office_category), aes(x=home_office_category)) +
  geom_bar() + ggtitle("Sales of category of Home Office") + 
  theme(plot.title = element_text(hjust = 0.5))+ labs(y="Sales",x="Category")

#Q9b by Nicolas Chuang
#creating data frame which is grouped by sub category and total sales
total_sales <- superStore %>% group_by(sub_category) %>% 
  summarize(total_sales = sum(sales))

#arrange the data frame by descending order and insert into new data frame
sorted_sales <- total_sales %>% arrange(desc(total_sales))

#keep top 10 results 
sorted_sales <- head(sorted_sales,10)

#plot bar graph of top 10 sub categories by sale
barplot(ylim=c(0,35000), sorted_sales$total_sales, 
        names.arg=sorted_sales$sub_category, main="Top 10 sub categories by sales",
        ylab="Sales",xlab="Sub categories")

# Q9 C by Ikmal Hakim
# subset the data to only include rows with the "Furniture", "Office Supplies" and "Technology" categories
furniture_data <- subset(superStore, category == "Furniture")
officeSupplies_data <- subset(superStore, category == "Office Supplies")
technology_data <- subset(superStore, category == "Technology")

# create a table of the sub-categories
sub_category_counts_furn <- table(furniture_data$sub_category)
sub_category_counts_OS <- table(officeSupplies_data$sub_category)
sub_category_counts_Tech <- table(technology_data$sub_category) 

# view the table
sub_category_counts_furn
sub_category_counts_OS
sub_category_counts_Tech

# sort the sub-categories by frequency
sorted_sub_categories_Furn <- sort(sub_category_counts_furn, decreasing = TRUE)
sorted_sub_categories_OS <- sort(sub_category_counts_OS, decreasing = TRUE)
sorted_sub_categories_Tech <- sort(sub_category_counts_Tech, decreasing = TRUE)

# store the 3 most frequent sub-categories in a vector
most_frequent_sub_categories_Furn <- head(names(sorted_sub_categories_Furn), 3)
most_frequent_sub_categories_OS <- head(names(sorted_sub_categories_OS), 3)
most_frequent_sub_categories_Tech <- head(names(sorted_sub_categories_Tech), 3)

# view the vector
most_frequent_sub_categories_Furn
most_frequent_sub_categories_OS
most_frequent_sub_categories_Tech

# Q9 D by Ikmal Hakim
# create a table of the countries
country_counts <- table(superStore$country)

# sort the countries by frequency
sorted_countries <- sort(country_counts, decreasing = TRUE)

# store the top 10 countries in a vector
top_10_countries <- names(head(sorted_countries, 10))

# view the vector
top_10_countries

# create a barplot of the top 10 countries
barplot(sorted_countries[1:10], names.arg = top_10_countries, las = 2, ylim = c(0, 250))

#Q10 by Nicolas Chuang
#creating data frame with rows with Consumer only
consumer_segment <- superStore[superStore$segment == "Consumer", ]

#creating vector with only consumer's shipping mode
consumer_ship <- consumer_segment$ship_mode

#plot bar graph of count of shipping mode by consumers
ggplot(data.frame(consumer_ship), aes(x=consumer_ship)) +
  geom_bar() + ggtitle("Count of shipping mode by Consumers") + 
  theme(plot.title = element_text(hjust = 0.5))+ labs(y="Count",x="Ship mode")


corporate_segment <- superStore[superStore$segment == "Corporate", ]
corporate_ship <- corporate_segment$ship_mode
ggplot(data.frame(corporate_ship), aes(x=corporate_ship)) +
  geom_bar() + ggtitle("Count of shipping mode by Corporate") + 
  theme(plot.title = element_text(hjust = 0.5))+ labs(y="Count",x="Ship mode")

home_office_segment <- superStore[superStore$segment == "Home Office", ]
home_office_ship <- home_office_segment$ship_mode
ggplot(data.frame(home_office_ship), aes(x=home_office_ship)) +
  geom_bar() + ggtitle("Count of shipping mode by Home Office") + 
  theme(plot.title = element_text(hjust = 0.5))+ labs(y="Count",x="Ship mode")


summary(superStore)
# calculate the average value for each market
superStore_avg <- superStore %>%
  group_by(market) %>%
  summarize(Avg = mean(profit))

#Ssort the data frame by the average value in descending order
superStore_avg <- superStore_avg %>%
  arrange(desc(Avg))

# print bar graph of average profit of market
barplot(ylim=c(0,60), superStore_avg$Avg, 
        names.arg=superStore_avg$market, main="Top 10 market by average profit",
        ylab="Average profit",xlab="Market")

# Q10 by Ikmal Hakim
# extract the month from the order date column
superStore$order_month <- format(as.Date(superStore$order_date, "%Y-%m-%d"), "%B")


# create a vector of months in the desired order
month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# convert the order_month variable to a factor with the levels in the desired order
superStore$order_month <- factor(superStore$order_month, levels = month_order)


# create a table of the order counts by month
order_counts_by_month <- table(superStore$order_month)

# create a barplot of the order counts by month
ggplot(data.frame(order_counts_by_month), aes(x = Var1, y = Freq)) +
  geom_col() +
  labs(title = "Number of Orders by Month", x = "Month", y = "Number of Orders")

# calculate the total profit by country
profit_by_country <- aggregate(profit ~ country, data = superStore, sum)

# sort the countries by total profit
profit_by_country <- profit_by_country[order(profit_by_country$profit, decreasing = TRUE),]

# select the top 10 countries by total profit
top_10_countries_profit <- head(profit_by_country, 10)

# create a barplot of the profit by country
ggplot(top_10_countries_profit, aes(x = country, y = profit)) +
  geom_col() +
  labs(title = "Profit by Country", x = "Country", y = "Profit")
