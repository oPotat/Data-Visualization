# Load required packages
library(arules)
library(dplyr)
library(shinythemes)
library("ggplotgui")
library(devtools)
library("readxl")
library(shiny)
library(ggplot2)
library(scales)


options(max.print = 10000)  # Increase max.print limit


# Read transactions from CSV file
GRC <- read.csv("D:\\College Hell\\F‏irst Year Second Term\\Data Science\\Project Material\\grc.csv")


GRC_NoDup <- unique(GRC)

#Converting GRC to a data frame
GRC_df <- as(GRC_NoDup, "data.frame")

sum(duplicated(GRC_df))

is.na(GRC_df)
Check = sum(is.na(GRC_df))
na.omit(GRC_df)

# View the structure of the data frame
str(GRC_df)

Check # to see if there are any NA's in the list

col_names <- names(GRC_df)
col_names

# row_names <- rownames(GRC_df) 
# print(row_names)

# Convert to numeric NEEDED for Data Visualization
GRC_df$total <- as.numeric(GRC_df$total)
GRC_df$count <- as.numeric(GRC_df$count)
GRC_df$rnd <- as.numeric(GRC_df$rnd)
GRC_df$age <- as.numeric(GRC_df$age)

# Compare cash and credit totals (Using Pie Charts)

cash_total <- sum(GRC_df$total[GRC_df$paymentType == "Cash"])
credit_total <- sum(GRC_df$total[GRC_df$paymentType == "Credit"])

Cash_Credit_Comparison <- data.frame(
  PaymentType = c("Cash", "Credit"),
  Total = c(cash_total, credit_total)
)

Cash_Credit_Comparison$Percent <- round(Cash_Credit_Comparison$Total / sum(Cash_Credit_Comparison$Total) * 100, 4)
#function used to get the percent with 4 decimal points to tell the difference 

ggplot(Cash_Credit_Comparison, aes(x = "", y = Total, fill = PaymentType)) + # pie charts don't have x axis that's why it's empty
  geom_bar(width = 1, stat = "identity") + #creates the bar plot identity to use the actual values in total
  geom_text(aes(label = paste0(Percent, "%")), 
            position = position_stack(vjust = 0.5)) +  # this adds text for the percentages in the middle
  coord_polar("y", start = 0) + #converts the bar plot into a pie chart
  theme_void() + # removes all non-data
  labs(title = "Comparison of the total spending of cash and credit", 
       x = "Payment Type", y = "Total Spending") +
  scale_y_continuous(labels = percent_format())  # y-axis changes to percentages


# Comparison of age and sum of total spending (Using Scatter Plot)

Age_Total_Spending_Comparison <- GRC_df %>%
  group_by(age) %>%
  summarise(Total_Age_Spending = sum(total))  #groups the data by unique ages summarize calculates total spending for each age group

print(Age_Total_Spending_Comparison) # Checking if it prints the values correctly

ggplot(Age_Total_Spending_Comparison, aes(x = age, y = Total_Age_Spending)) +
  geom_point(color = "green") + #creates the points for the scatter plot
  labs(title = "Comparison of age and total spending", x = "Age", y = "Total Spent by Each Age")


# Showing each city's total spending 
unique_cities <- unique(GRC_df$city) # checking what cities are mentioned in the GRC
unique_cities # "Hurghada" "Aswan"  "Dakahlia"   "Sohag"  "Giza" "Gharbia"  "Cairo"  "Alexandria" "Port Said"  "Fayoum"

#Getting the total spending of each city
Hurghada_total <- sum(GRC_df$total[GRC_df$city == "Hurghada"])
Aswan_total <- sum(GRC_df$total[GRC_df$city == "Aswan"])
Dakahlia_total <- sum(GRC_df$total[GRC_df$city == "Dakahlia"])
Sohag_total <- sum(GRC_df$total[GRC_df$city == "Sohag"])
Giza_total <- sum(GRC_df$total[GRC_df$city == "Giza"])
Gharbia_total <- sum(GRC_df$total[GRC_df$city == "Gharbia"])
Cairo_total <- sum(GRC_df$total[GRC_df$city == "Cairo"])
Alexandria_total <- sum(GRC_df$total[GRC_df$city == "Alexandria"])
Port_Said_total <- sum(GRC_df$total[GRC_df$city == "Port Said"])
Fayoum_total <- sum(GRC_df$total[GRC_df$city == "Fayoum"])

City_Spending <- data.frame(
  City = c("Hurghada" ,"Aswan", "Dakahlia", "Sohag", "Giza", "Gharbia", 
           "Cairo", "Alexandria", "Port Said", "Fayoum"),
  Total = c(Hurghada_total, Aswan_total , Dakahlia_total, Sohag_total, Giza_total, Gharbia_total, 
            Cairo_total, Alexandria_total, Port_Said_total, Fayoum_total)
)

print(City_Spending)

ggplot(City_Spending, aes(x=reorder(City, -Total), y=Total)) + #reorder with -total makes it go from largest to smallest
  geom_bar(stat="identity", fill ="steelblue") + #creates the bar plot that can represent the actual values
  theme_bw() +
  labs(title = "City Spending", 
       x="City", y="Total Spending")


#Display the distribution of total spending (Using Histogram)

ggplot(GRC_df, aes(x=total)) +
  geom_histogram(binwidth=1, fill="black", color="black") + #bin width makes it take values like 1,2,3,4,5... bin width 10 wouldbe 0-10,10-20....
  labs(title="Display of distribution of Total Spending", x="Total Spending", y="Frequency")

  
#Display the distribution of total spending (Using BoxPlot)

ggplot(GRC_df, aes(x = "", y = total)) +
  geom_boxplot(fill = "white", color = "blue") + 
  labs(title = "Display of distribution of Total Spending", x = "", y = "Total Spending")

