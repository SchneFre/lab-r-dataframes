library(dplyr)
library(zoo)

# Load the sample_superstore dataset into a dataframe. 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Save the dataframe as superstore.
superstore <- read.csv("dataset/Sample - Superstore.csv")
# Display the first 10 rows of the dataframe using the head() function.
print(head(superstore))
# Use the str() function to inspect the structure of the dataframe. 
# What are the data types of the columns?
print(str(superstore))
# Row.ID, postal code, sales, quantity, dicount and profit are numerical
# the rest is string:

        #  $ Row.ID       : int  1 2 3 4 5 6 7 8 9 10 ...
        #  $ Order.ID     : chr  "CA-2016-152156" "CA-2016-152156" "CA-2016-138688" "US-2015-108966" ...
        #  $ Order.Date   : chr  "11/8/2016" "11/8/2016" "6/12/2016" "10/11/2015" ...
        #  $ Ship.Date    : chr  "11/11/2016" "11/11/2016" "6/16/2016" "10/18/2015" ...
        #  $ Ship.Mode    : chr  "Second Class" "Second Class" "Second Class" "Standard Class" ...
        #  $ Customer.ID  : chr  "CG-12520" "CG-12520" "DV-13045" "SO-20335" ...
        #  $ Customer.Name: chr  "Claire Gute" "Claire Gute" "Darrin Van Huff" "Sean O'Donnell" ...
        #  $ Segment      : chr  "Consumer" "Consumer" "Corporate" "Consumer" ...
        #  $ Country      : chr  "United States" "United States" "United States" "United States" ...
        #  $ City         : chr  "Henderson" "Henderson" "Los Angeles" "Fort Lauderdale" ...
        #  $ State        : chr  "Kentucky" "Kentucky" "California" "Florida" ...
        #  $ Postal.Code  : int  42420 42420 90036 33311 33311 90032 90032 90032 90032 90032 ...
        #  $ Region       : chr  "South" "South" "West" "South" ...
        #  $ Product.ID   : chr  "FUR-BO-10001798" "FUR-CH-10000454" "OFF-LA-10000240" "FUR-TA-10000577" ...
        #  $ Category     : chr  "Furniture" "Furniture" "Office Supplies" "Furniture" ...
        #  $ Sub.Category : chr  "Bookcases" "Chairs" "Labels" "Tables" ...
        #  $ Product.Name : chr  "Bush Somerset Collection Bookcase" "Hon Deluxe Fabric Upholstered Stacking Chairs, Rounded Back" "Self-Adhesive Address Labels for Typewriters by Universal" "Bretford CR4500 Series Slim Rectangular Table" ...
        #  $ Sales        : num  262 731.9 14.6 957.6 22.4 ...
        #  $ Quantity     : int  2 3 2 5 2 7 4 6 3 5 ...
        #  $ Discount     : num  0 0 0 0.45 0.2 0 0 0.2 0.2 0 ...
        #  $ Profit       : num  41.91 219.58 6.87 -383.03 2.52 ..

# Use the summary() function to get a summary of the dataframe. 
# What insights can you gather from the summary?
print(summary(superstore))
# sales: The mean is much higher than the median, indicating positive skewness — a few very large sales inflate the average.
# quantity: Most orders contain small quantities of products. Very few customers buy in bulk.
# Discount: A large number of orders receive no discount or a standard 20%.
# profit: The mean is higher than the median, but there are significant losses on some transactions. Many sales result in low or even negative profits.



#                       Sales              Quantity        Discount     
#                     Min.   :    0.444   Min.   : 1.00   Min.   :0.0000
#                     1st Qu.:   17.280   1st Qu.: 2.00   1st Qu.:0.0000
#                     Median :   54.490   Median : 3.00   Median :0.2000
#                     Mean   :  229.858   Mean   : 3.79   Mean   :0.1562
#                     3rd Qu.:  209.940   3rd Qu.: 5.00   3rd Qu.:0.2000
#                     Max.   :22638.480   Max.   :14.00   Max.   :0.8000
#      Profit
#  Min.   :-6599.978
#  1st Qu.:    1.729
#  Median :    8.666
#  Mean   :   28.657
#  3rd Qu.:   29.364
#  Max.   : 8399.976


# Extract the Sales column as a vector using the $ operator.
# print(superstore$Sales)
salesCln = superstore$Sales

# Subset the first 15 rows and the columns Order ID, Customer Name, and Sales.
subset_df <- superstore[1:15, c("Order.ID", "Customer.Name", "Sales")]
# print(head(subset_df))

# Use the nrow() and ncol() functions to determine the number of rows and columns in the dataframe.
print(nrow(superstore)) # 9994
print(ncol(superstore)) # 21

# Filter the dataframe to show only rows where the Profit is greater than 100.
profit_bigger_100 = superstore[superstore$Profit > 100, ]
# print(head(profit_bigger_100))

# Filter the dataframe to show only rows where the Category is "Furniture" and the Sales are greater than 500.
furniture_large_sales = superstore[superstore$Category == "Furniture" & superstore$Sales > 500, ]
# print(head(furniture_large_sales))

# Filter the dataframe to show only rows where the Region is "West" and the Quantity is greater than 5.
west_quantity_5 = superstore[superstore$Region == "West" & superstore$Quantity > 5, ]
# Add a new column called Profit Margin that calculates the profit margin as (Profit / Sales) * 100.
superstore$Profit.Margin  <- (superstore$Profit / superstore$Sales) * 100
# print(str(superstore))

#Modify the Sales column by rounding the values to 2 decimal places.
superstore$Sales <- round(superstore$Sales, digits=2)
print(head(superstore$Sales))

# Remove the Postal Code column from the dataframe using the subset() or select() function.
superstore <- superstore %>% select(-Postal.Code)
# print(str(superstore))

# Check for missing values in the dataframe using the is.na() function. Are there any missing values?
print(paste("Missing Values: ", any(is.na(superstore)))) # Missing Values:  FALSE" -> no missing values

# no missing values:
superstore_clean <- na.omit(superstore)
print(nrow(superstore_clean) == nrow(superstore))
# Replace any missing values in the Sales column with the mean of the Sales column using the na.fill() function.
superstore$Sales <- na.fill(superstore$Sales, fill = mean(superstore$Sales, nr.rm = TRUE))

# Group the dataframe by Region and calculate the total Sales and Profit for each region.
region_summary <- superstore %>%
  group_by(Region) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    Total_Profit = sum(Profit, na.rm = TRUE)
  )
print(region_summary)

# Create a new column called Discount Level that categorizes the Discount column
superstore$Discount.Level <- cut(
  superstore$Discount,
  breaks = c(-Inf, 0.2, 0.5, 1),
  labels = c("Low", "Medium", "High"),
  right = TRUE
)
print(table(superstore$Discount.Level))

# Sort the dataframe by Sales in descending order.
superstore_sorted <- superstore[order(superstore$Sales, decreasing = TRUE), ]