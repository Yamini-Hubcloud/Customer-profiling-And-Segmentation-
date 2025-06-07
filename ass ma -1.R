install.packages("readxl")
library(readxl)
CC<- read_excel("C:/Users/Admin/Desktop/MSc BA/7106 MA/Current Customers.xlsx")
View(CC)

install.packages(c("tidyverse", "lubridate", "dplyr", "cluster","readr", "factoextra","MASS","clustersim","fpc"))
library(tidyverse)
library(lubridate)
library(dplyr)
library(cluster)    # For clustering algorithms
library(factoextra) # For visualizing clusters
library(fbc)
library(clustersim)

# Display the first few rows
head(CC)

# Check data structure and data types
str(CC)

# Summary statistics
summary(CC)

# Change CustomerID to numeric
library(dplyr)
# Handle missing CustomerID values
CC <- CC%>%
  filter(!is.na(CustomerID))
CC$CustomerID <- as.numeric(CC$CustomerID)
CC$CustomerID[is.na(as.numeric(CC$CustomerID))]

# Missing plot
install.packages("DataExplorer")
library(DataExplorer)
plot_missing(CC)

CC <- na.omit(CC)
sum(is.na(CC))


# Convert InvoiceDate to datetime format
CC$InvoiceDate <- as.character(CC$InvoiceDate)

library(lubridate)

# Convert string to proper date-time format
CC$InvoiceDate <- ymd_hm(CC$InvoiceDate)

library(dplyr)
# Change format (adjust as needed)
CC$InvoiceDate <- as.Date(CC$InvoiceDate, format="%d/%m/%Y")

CC$InvoiceDate <- as.numeric(CC$InvoiceDate)
#Calculate Revenue for each transaction
CC$revenue <- CC$Quantity * CC$UnitPrice

# Calculate the number of distinct orders per customer (num_orders)
CC <- CC %>%
  group_by(CustomerID) %>%
  mutate(num_orders = n_distinct(InvoiceNo)) %>%
  ungroup()
CC$num_orders <- as.numeric(CC$num_orders)
#Visualize segment wise



# Load Packages and Set Seed
set.seed(40459080)

# Run hierarchical clustering with bases variable

seg_hclust <- hclust(dist(scale(cbind(CC$InvoiceNo,CC$Quantity,CC$InvoiceDate,CC$UnitPrice,CC$CustomerID,
    CC$married,CC$household_size,CC$income,CC$age,CC$Zip_Code,CC$Work,CC$Education,CC$num_orders,CC$revenue))), method="complete")

# Elbow plot for first 10 segments
x <- c(1:10)
sort_height <- sort(seg_hclust$height,decreasing=TRUE)
y <- sort_height[1:10]
plot(x,y); lines(x,y,col="pink")


# Run k-means with 6 segments
seg_kmeans <- kmeans(x = data.frame(CC$InvoiceNo,CC$Quantity,CC$InvoiceDate,CC$UnitPrice,CC$CustomerID,
CC$married,CC$household_size,CC$income,CC$age,CC$Zip_Code,CC$Work,CC$Education,CC$num_orders,CC$revenue), 6)

# Add segment number back to original data
segment = seg_kmeans$cluster
seg_result <- cbind(CC, segment)
View(seg_result)



library(readr)
# Export data to a CSV file

write.csv(seg_result, file = "C:\\Users\\Admin\\Documents\\seg_result.csv", row.names = FALSE)

# Get today's date
today <- Sys.Date()

# Calculate Recency for each customer
seg_result$recency_days <- as.numeric(difftime(today, seg_result$InvoiceDate, units = "days"))

# Find the most recent purchase date for each customer
recency <- aggregate(recency_days ~ CustomerID, data = seg_result, FUN = min)

# Calculate Recency, Frequency, and Monetary Value
library(dplyr)
RFM <- seg_result %>%
  group_by(CustomerID) %>%
  summarize(
    Recency = as.numeric(difftime(Sys.Date(), max(InvoiceDate), units = "days")),  # Days since last purchase
    Frequency = n_distinct(num_orders),  # Unique purchases (based on InvoiceNo)
    MonetaryValue = sum(Quantity * UnitPrice)  # Total spending
  ) %>%
  ungroup()


cc <- left_join(seg_resu, RFM, by = "CustomerID")

set.seed(1)
## How many levels for each
groups <- 5 ## This will use quintiles to sort and give 125 total groups


## Run RFM Analysis with Independent Sort
seg_result$recency_score <- ntile(seg_result$recency_days*-1, groups)
seg_result <- seg_result %>%
  filter(!is.na(num_orders))
seg_result$frequency_score <- ntile(seg_result$num_orders, groups)
seg_result$monetary_score <- ntile(seg_result$revenue, groups)
seg_result$rfm_score <- paste(seg_result$recency_score*100 + seg_result$frequency_score * 10 + seg_result$monetary_score)

## Run RFM Analysis with Sequential Sort
seg_result$recency_score_seq <- ntile(seg_result$recency_days*-1, groups)
r_groups <- NULL; rf_groups <- NULL; temp <- NULL ## Initialize empty matrices
for (r in 1:groups) {
  r_groups[[r]] <- filter(seg_result, seg_result$recency_score_seq == r)
  r_groups[[r]]$frequency_score_seq <- ntile(r_groups[[r]]$num_orders, groups)
  for (m in 1:groups) {
    rf_groups[[m]] <- filter(r_groups[[r]], r_groups[[r]]$frequency_score_seq == m)
    rf_groups[[m]]$monetary_score_seq <- ntile(rf_groups[[m]]$revenue, groups)
    temp <- bind_rows(temp, rf_groups[[m]])
  }	
}
rfm_result <- temp[order(temp$CustomerID),]
rfm_result$rfm_score_seq <- paste(rfm_result$recency_score_seq*100 + rfm_result$frequency_score_seq * 10 + rfm_result$monetary_score_seq)
View(rfm_result)
## Export RFM Results with Independent and Sequential Sort
write.csv(rfm_result, file = "C:\\Users\\Admin\\Desktop\\MSc BA\\7106 MA\\rfm_result.csv", row.names = FALSE)

# Customer profiling
# Customer profiling- Combine all metrics into one dataframe
rfm_result$Customer_profile <- case_when(
  rfm_result$rfm_score <= 1 ~ "VeryLow-value",        
  rfm_result$rfm_score <= 2 ~ "Low-value",     
  rfm_result$rfm_score <= 3 ~ "Medium-value",  
  rfm_result$rfm_score <= 4 ~ "High-value",      
  TRUE ~ "Very High-value"                              
)#####################################

library(MASS)
set.seed(1)

## Read in Segment Data and Classification Data 
seg <- read.csv("C:/Users/Admin/Desktop/MSc BA/7106 MA/rfm_result.csv") ## Choose segmentation_result.csv file
library(readxl)
class <- read_excel("C:/Users/Admin/Desktop/MSc BA/7106 MA/Prospect Customers.xlsx") ## Choose retail_classification.csv file

## Run Discriminant Analysis
fit <- lda(segment ~ married +household_size+income+ age	+Zip_Code+	Work+	Education, data = seg)
fit

## Check which Discriminant Functions are Significant
ldaPred <- predict(fit, seg)
ld <- ldaPred$x
anova(lm(ld[,1] ~ seg$segment))
anova(lm(ld[,2] ~ seg$segment))
anova(lm(ld[,3] ~ seg$segment))
anova(lm(ld[,4] ~ seg$segment))
anova(lm(ld[,5] ~ seg$segment))

## Check Disciminant Model Fit
pred.seg <- predict(fit)$class
tseg <- table(seg$segment, pred.seg)
tseg # print table
sum(diag(tseg))/nrow(seg) # print percent correct

## Run Classification Using Discriminant Function
class <- as.data.frame(class) 
pred.class <- predict(fit, class)$class
tclass <- table(pred.class)
tclass # print table

## Add Predicted Segment to Classification Data
class.seg <- cbind(class, pred.class)
write.csv(class.seg, file = "C:\\Users\\Admin\\Desktop\\MSc BA\\7106 MA\\class.seg.csv", row.names = FALSE)

View(class.seg)

