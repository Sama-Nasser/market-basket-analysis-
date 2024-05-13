################################################################################################################
# Load the required library 'reader' for reading the dataset
library("reader")
 # Read the dataset using the specified path
data <- read.csv("C:/Users/Sama Naser/Downloads/grc.csv")  # Read the dataset
# Explore the dataset:
View(data)# View the dataset in a separate viewer window
head(data)# Display the first 6 rows of the dataset
tail(data)# Display the last 6 rows of the dataset
summary(data)# Display summary statistics of the dataset
class(data)# Get the class/type of the dataset
str(data)# Display the structure of the dataset, including the data types of each column
dim(data)# Display the dimensions (rows and columns) of the dataset
names(data)# Get the column names of the dataset
# Identify and handle duplicates:
duplicated(data)# Check for duplicated rows in the dataset
sum(duplicated(data))# Count the total number of duplicated rows in the dataset
data_clean <- unique(data)# Remove duplicates and create a cleaned version of the dataset
data_clean# Display the cleaned dataset
sum(duplicated(data_clean))# Check for duplicates again in the cleaned dataset
# Load the 'dplyr' library for data manipulation
library("dplyr")
data_cleaned <- distinct(data_clean)# Remove duplicates using 'distinct' function from 'dplyr' package
sum(duplicated(data_cleaned))# Check for duplicates in the cleaned dataset after using 'distinct'
View(data_cleaned)# View the cleaned dataset in a separate viewer window
# Data Structure
is.integer(data_cleaned$rnd)# Check if 'rnd' column contains integer values     \\  the output is: True
is.integer(data_cleaned$count)# Check if 'count' column contains integer values \\  the output is: True
is.integer(data_cleaned$total )# Check if 'total' column contains integer values  \\  the output is: True
is.integer(data_cleaned$age)# Check if 'age' column contains integer values    \\  the output is: True
is.character(data_cleaned$items)# Check if 'items' column contains character values     \\  the output is: True
is.character(data_cleaned$customer)# Check if 'customer' column contains character values   \\  the output is: True
is.character(data_cleaned$city)# Check if 'city' column contains character values    \\  the output is: True
is.character(data_cleaned$paymentType)# Check if 'paymentType' column contains character values    \\  the output is: True
#Finding missing values
is.na(data_cleaned)
sum(is.na(data_cleaned))
na.omit(data_cleaned)#If you insist on removing NA values
################################################################################################################
# VISUALIZATION
library("dplyr")
par(mfrow=c(2,2))  
comparison <- table(data_cleaned$paymentType)  
percent <- paste0(round(100 * comparison / sum(comparison)), "%") 
pie(comparison,labels = percent, col = c("skyblue", "pink"),main = "Cash and Credit Totals Comparison")  
legend("bottomleft",   legend = c("Cash", "Credit"), fill = c("skyblue", "pink") )


summarized_data <- data_cleaned %>%group_by(age) %>%summarize(total = sum(total))
my_colors <- rainbow(length(unique(summarized_data$age)))

plot(x = summarized_data$age, y = summarized_data$total,  main = "Age Total Spending", xlab = "Age",
     ylab = "Total Spending", col = my_colors,pch = 19)  

legend("topright", legend = levels(factor(summarized_data$age)),
       col = my_colors, pch = 19, title = "Age Groups")       

summarized <- data_cleaned %>%group_by(city) %>%summarize(total = sum(total)) %>% arrange(desc(total))  
my_colors <- rainbow(length(summarized$city)) 
barplot(summarized$total, names.arg = summarized$city,main = "City Total Spending",
        xlab = "City", ylab = "Total Spending",col = my_colors )  
legend("topright",legend = summarized$city, fill = my_colors, border = "black", title = "Cities") 


boxplot(x = data_clean$total,main = "Distribution of Total Spending",xlab = "Total",col = "lightblue") 




########################################################################################## 
# CLUSTERING
library("dplyr") #import the 'dplyr' package for data manipulation 
while (TRUE) { # Prompt the user to enter the number of clusters between 2 and 4
  clustering <- as.integer(readline('Enter the cluster number from 2 to 4: '))
  if (clustering >= 2 && clustering <= 4  ) { # Check if the entered number is within the specified range and not NA
    break # Exit the loop if the entered number is valid
  } else {print("You should enter an integer between 2 to 4 Please try again.")}} # Print a message if the entered number is invalid 
# Summarize total spending by customer and age
df <- data_clean %>% group_by(customer, age) %>% summarize(total_spending = sum(total))
kmeans_model <- kmeans(df[, c("age", "total_spending")], centers = clustering)# Perform K-means clustering
print(kmeans_model$centers)# Check the number of clusters generated
df$cluster <- kmeans_model$cluster# Add cluster information to the df data frame
# Create the data frame
display <- data.frame(
  customer = df$customer,
  age = df$age,
  Total_Spending = df$total_spending,
  cluster = df$cluster)
print(display)# Print the data frame
################################################################################################################
#ASSOCIATION
library("arules")
# Prompt the user to enter the minimum support value
min_support <- 0
while (min_support < 0.001 || min_support > 1) {
  min_support <- as.numeric(readline("Enter minimum support (between 0.001 and 1): "))   
  if (min_support < 0.001 || min_support > 1) {
    print("Please enter a number between 0.001 and 1")}}  # Print message if input is invalid
# Prompt the user to enter the minimum confidence value
min_confidence <- 0
while (min_confidence < 0.001 || min_confidence > 1) {
  min_confidence <- as.numeric(readline("Enter minimum confidence (between 0.001 and 1): "))  
  if (min_confidence < 0.001 || min_confidence > 1) {
    print("Please enter a number between 0.001 and 1") }}  # Print message if input is invalid
# Split the items in the 'data_cleaned' dataframe and convert to transactions
trans <- strsplit(data_cleaned$items, ",")  # Split the items
trans_data <- as(trans, "transactions") # Convert to transactions
# Mine association rules using the Apriori algorithm with user-defined parameters
rules <- apriori(trans_data, parameter = list(support = min_support, confidence = min_confidence , minlen = 2))
print(rules)  # Print the mined association rules
inspect(rules)  # Display the rules
inspect(head(rules))# Inspect the first 6 association rules
itemFrequencyPlot(trans_data, topN = 7 , type = "absolute")# Plot the item frequency in transactions


 

