# Question 1 ====
# library(ggplot2)

# Set the working directory
setwd("/Users/farahafifah/R Programming/Assignment 2 CPC351")

# Read a CSV files using read.csv() function
studentperf <- read.csv("StudentsPerformance.csv")

# Print the summary of the imported data
summary(studentperf)

# a. Convert variables to suitable data type.
studentperf$gender <- as.factor(studentperf$gender)
studentperf$race.ethnicity <- as.factor(studentperf$race.ethnicity)
studentperf$parental.level.of.education <- as.factor(studentperf$parental.level.of.education)
studentperf$lunch <- as.factor(studentperf$lunch)
studentperf$test.preparation.course <- as.factor(studentperf$test.preparation.course)

# b. Identify the number of missing values in data
sum(is.na(studentperf))

# c. Remove variable 'lunch' from the data
drop <- c("lunch")
studentperf <- studentperf[,!(names(studentperf) %in% drop)]

# d. Rename the variables name
colnames(studentperf) <- c("STU_Gender", "STU_Ethnic", "PAR_Education", 
                           "PREP_CourseStatus", "Maths", "Reading", "Writing")

# Print the summary of the pre-processed data
summary(studentperf)

# Question 2 ====
# Pie chart of distribution of students according to ethnicity
table1 <- table(studentperf$STU_Ethnic)
freq <- paste(names(table1), "\n", table1, "\n/", round(100*table1/sum(table1),1), "%", ep="")
pie(table1, labels = freq,
    main="Distribution of Students According to Ethnicity")

# Bar chart of distribution of gender in each ethnic
ggplot(studentperf, aes(fill=STU_Gender, y=STU_Gender, x=STU_Ethnic)) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.y = element_blank()) +
  guides(fill = guide_legend(title = "Gender")) +
  ggtitle("Distribution of Gender in Each Ethnicity") +
  xlab("Ethnicity") +
  ylab("Gender")

# Bar chart of parental level education and course status
ggplot(studentperf, aes(fill=PAR_Education, y=PAR_Education, x=PREP_CourseStatus)) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.y = element_blank()) +
  guides(fill = guide_legend(title = "Parental Level \n of Education")) +
  ggtitle("Preparation Course Status and Parental Level of Education") +
  xlab("Preparation Course Status") +
  ylab("Parental Level of Education")

# Question 3 ====
# Distribution of marks for maths
histMath <- hist(studentperf$Maths, main="Distribution of Marks for Maths",
                 xlab="Marks", col="plum", border="purple")
text(histMath$mids,histMath$counts,labels=histMath$counts, adj=c(0.5, -0.5))

# Distribution of marks for reading
histReading <- hist(studentperf$Reading, main="Distribution of Marks for Reading",
                    xlab="Marks", col="lightblue", border="blue")
text(histReading$mids,histReading$counts,labels=histReading$counts, adj=c(0.5, -0.5))

# Distribution of marks for writing
histWriting <- hist(studentperf$Writing, main="Distribution of Marks for Writing",
                    xlab="Marks", col="pink", border="red")
text(histWriting$mids,histWriting$counts,labels=histWriting$counts, adj=c(0.5, -0.5))

# Performance of students from different ethnicity in each courses
# Box plot of performance of students in maths
ethnic_maths <- studentperf[order(studentperf$Maths),]
boxplot(Maths~STU_Ethnic,
        data=ethnic_maths,
        main="Performance of Students in Maths from Each Ethnicity",
        xlab="Ethnicity",
        ylab="Marks",
        col="plum",
        border="purple")

# Box plot of performance of students in reading
ethnic_reading <- studentperf[order(studentperf$Reading),]
boxplot(Reading~STU_Ethnic,
        data=ethnic_reading,
        main="Performance of Students in Reading from Each Ethnicity",
        xlab="Ethnicity",
        ylab="Marks",
        col="lightblue",
        border="blue")

# Box plot of performance of students in writing
ethnic_writing <- studentperf[order(studentperf$Writing),]
boxplot(Writing~STU_Ethnic,
        data=ethnic_writing,
        main="Performance of Students in Writing from Each Ethnicity",
        xlab="Ethnicity",
        ylab="Marks",
        col="pink",
        border="red")

# Question 8 =====
# (a) State the data type of each variable. 
#     Some of the variables are not defined with correct data type. 
#     Convert these variables such that they are with a suitable data type. 
#     Show the summary of the dataset.

# Load SuperStoresOrder.csv in R 
# Set the working directory
setwd("/Users/farahafifah/R Programming/Assignment 2 CPC351")

# Read a CSV files using read.csv() function
order <- read.csv("SuperStoreOrders.csv",header=T,sep=",")

# Install and load color packages
install.packages("viridis") 
library("viridis") 

# Data classes
str(order)

# Print the summary of the imported data
summary(order)

# Duplicate the data as backup (comparison)
order_new <- order

# Categorical 
order_new$customer_name <- as.factor(order_new$customer_name)
order_new$product_name <- as.factor(order_new$product_name)
order_new$order_id <- as.factor(order_new$order_id)
order_new$ship_mode <- as.factor(order_new$ship_mode)
order_new$segment <- as.factor(order_new$segment)
order_new$state <- as.factor(order_new$state)
order_new$market <- as.factor(order_new$segment)
order_new$region <- as.factor(order_new$region)
order_new$category <- as.factor(order_new$category)
order_new$country <- as.factor(order_new$country)
order_new$sub_category <- as.factor(order_new$sub_category)
order_new$order_priority <- as.factor(order_new$order_priority)
order_new$year <- as.factor(order_new$year)

# Numeric
order_new$sales <- as.numeric(order_new$sales) 

# Replace NAs with mean
order_new$sales[is.na(order_new$sales)] <- mean(order_new$sales,na.rm=TRUE) 

# Show the summary of order_new
summary(order_new)

str(order_new)


# (b) After variables are with their suitable data type. 
#     Maintain only the top 1000 instances and remove the others.

order_new_extracted <- order_new[1:1000,]
summary(order_new_extracted)


# Question 9 ====
# From the reduced data set, using an appropriate visualization:
# For question 9a, segments refer to the column "segment" and 
# category distributions is referring to the "category" column.
# For question 9b, the top 10 categories is referring 
# to the column "sub-category" - sales based

# (a) Group customer based on segments and determine the category distribution for each.

# Library ggplot2 to plot the graph
library("ggplot2")

# Library dplyr
library(dplyr)

# Create a new data      
segmentConsumer = order_new_extracted %>% group_by(country) %>%
  summarise(amount = length(country),
            .groups = 'drop')

# Group the consumer segment in category
consumerSegments <- order_new_extracted[order_new_extracted$segment == "Consumer", ]
consumerCategory <- consumerSegments$category

# Plot the graph
plot <- ggplot(data.frame(consumerCategory), aes(x=consumerCategory)) +
  geom_bar(fill="#EEAEE1", colour="#D32CB1") +
  ggtitle("Total of Customers in Consumer Segment") + 
  theme(plot.title = element_text(hjust = 0.5))+ labs(y="No of Customers",x="Category")
plot + theme_bw()

# Group the corporate segment in category
corporateSegment <- order_new_extracted[order_new_extracted$segment == "Corporate", ]
corporateCategory <- corporateSegment$category

# Plot the graph
plot <- ggplot(data.frame(corporateCategory), aes(x=corporateCategory)) +
  geom_bar(fill="#B7EAB4", colour="#0C4808") + 
  ggtitle("Total of Customers in Corporate Segment") + 
  theme(plot.title = element_text(hjust = 0.5))+ labs(y="No of Customers",x="Category")
plot + theme_bw()

# Group the home office segment in category
homeOfficeSegment <- order_new_extracted[order_new_extracted$segment == "Home Office", ]
homeOfficeCategory <- homeOfficeSegment$category

# Plot the graph
plot <- ggplot(data.frame(homeOfficeCategory), aes(x=homeOfficeCategory)) +
  geom_bar(fill="#ADCAEB", colour="#3382DA") + 
  ggtitle("Total of Customers in Home Office Segment") + 
  theme(plot.title = element_text(hjust = 0.5))+ labs(y="No of Customers",x="Category")
plot + theme_bw()

# (b) Find the Top 10 categories.

# Library dplyr
library(dplyr)

# Create a new data      
categories = order_new_extracted %>% group_by(sub_category) %>%
  summarise(amount = length(sub_category),
            .groups = 'drop')

# Sort descending by amount
categories <- categories[order(-categories$amount),]

# Extracted top 10 rows only
categories <- categories[1:10,]

# Bar chart
barplot(categories$amount, names.arg=categories$sub_category,
        xlab="Category",
        ylab="Amount",
        main="Top 10 Categories",
        col=rainbow(length(categories$amount))
)

# (c) For each category determine the 3 most frequent-bought sub-categories.

# Library dplyr
library(dplyr)

# Create a new data    
mostFrequentBought = order_new_extracted %>% group_by(category, sub_category) %>%
  summarise(amount = length(sub_category),
            .groups = 'drop')

# Group the furniture category
furnitureCategory <- mostFrequentBought[mostFrequentBought$category == "Furniture", ]

# Sort descending by amount
furnitureCategory <- furnitureCategory[order(-furnitureCategory$amount),]

# Extracted top 3 rows only
furnitureCategory <- furnitureCategory[1:3,]

# Create horizontal bar plot with labels
plot <- ggplot(furnitureCategory,
               aes(x = reorder(sub_category, amount),y = amount)) +
  geom_bar(stat = "identity")+
  geom_col(fill = "#AEF1E1", color = "#2ACEA6") +
  geom_text(aes(label = signif(amount)), nudge_y = 2) +
  labs(y = "Frequency", x = "Sub Categories")+
  coord_flip() +
  ggtitle("Top 3 Most Frequent Bought Sub Categories in Furniture")
plot + theme_bw()

# Group the office supplies category
officeSuppliesCategory <- mostFrequentBought[mostFrequentBought$category == "Office Supplies", ]

# Sort descending by amount
officeSuppliesCategory <- officeSuppliesCategory[order(-officeSuppliesCategory$amount),]

# Extracted top 3 rows only
officeSuppliesCategory <- officeSuppliesCategory[1:3,]

# Create horizontal bar plot with labels
plot <- ggplot(officeSuppliesCategory,
               aes(x = reorder(sub_category, amount),y = amount)) +
  geom_bar(stat = "identity")+
  geom_col(fill = "#B7A6E0", color = "#7A4AF3") +
  geom_text(aes(label = signif(amount)), nudge_y = 2) +
  labs(y = "Frequency", x = "Sub Categories")+
  coord_flip() +
  ggtitle("Top 3 Most Frequent Bought Sub Categories in Office Supplies")
plot + theme_bw()

# Group the technology category
technologyCategory <- mostFrequentBought[mostFrequentBought$category == "Technology", ]

# Sort descending by amount
technologyCategory <- technologyCategory[order(-technologyCategory$amount),]

# Extracted top 3 rows only
technologyCategory <- technologyCategory[1:3,]

# Create horizontal bar plot with labels
plot <- ggplot(technologyCategory,
               aes(x = reorder(sub_category, amount),y = amount)) +
  geom_bar(stat = "identity")+
  geom_col(fill = "#E2D9A6", color = "#B49D1E") +
  geom_text(aes(label = signif(amount)), nudge_y = 2) +
  labs(y = "Frequency", x = "Sub Categories")+
  coord_flip() +
  ggtitle("Top 3 Most Frequent Bought Sub Categories in Technology")
plot + theme_bw()

# (d) Where does most customer come from? Highlight the Top 10 countries.

# Library dplyr
library(dplyr)

# Create a new data      
mostCustomer = order_new_extracted %>% group_by(country) %>%
  summarise(amount = length(country),
            .groups = 'drop')

# Sort descending by amount
mostCustomer <- mostCustomer[order(-mostCustomer$amount),]

# Extracted top 10 rows only
mostCustomer <- mostCustomer[1:10,]

# Bar Chart
barplot(mostCustomer$amount, names.arg=mostCustomer$country,
        xlab="Category",
        ylab="Amount",
        main="Top 10 Countries",
        col=inferno(length(mostCustomer$amount))
)

# Question 10 ====

# Other than the above, show four more trends / patterns / 
# relationship that can be deduce from the data. 
# Explain your answer with appropriate visuals.

# 1. The Frequency of Ship Mode Used
# Library dplyr
library(dplyr)

# Create a new data      
shipping = order_new_extracted %>% group_by(ship_mode) %>%
  summarise(frequency = length(ship_mode),
            .groups = 'drop')

# Sort descending by amount
shipping <- shipping[order(-shipping$frequency),]

# Library ggplot2
library("ggplot2")

# Create bar plot with labels
plot<-ggplot(shipping,
             aes(x = reorder(ship_mode, -frequency),y = frequency)) +
  geom_bar(stat = "identity")+
  geom_col(fill = "#E59393", color = "#D62D2D") +
  geom_text(aes(label = signif(frequency)), nudge_y = 16) +
  labs(y = "Frequency", x = "Ship Mode")+
  ggtitle("The Frequency of Ship Mode Used")
plot + theme_bw()

# 2. Total Sales and Profit According Category

# Library dplyr
library(dplyr)

# Create a new data      
totalSales = order_new_extracted %>% group_by(category) %>%
  summarise(total = sum(sales),
            .groups = 'drop')

# Bar Chart
barplot(totalSales$total, names.arg=totalSales$category,
        horiz = TRUE,
        xlab="Total Sales",
        cex.names=0.9,
        main="Total Sales According Category",
        col=plasma(length(totalSales$total))
)


# 3. Top 5 Customer with The Most Shipping Cost Paid

# Library dplyr
library(dplyr)

# Create a new data      
mostPaidShipping = order_new_extracted %>% group_by(customer_name) %>%
  summarise(countCustomer = sum(shipping_cost),
            .groups = 'drop')

# Sort descending by amount
mostPaidShipping <- mostPaidShipping[order(-mostPaidShipping$countCustomer),]

# Extracted top 5 rows only
mostPaidShipping <- mostPaidShipping[1:5,]

# Bar Chart
barplot(mostPaidShipping$countCustomer, names.arg=mostPaidShipping$customer_name,
        xlab="Category",
        ylab="Amount",
        main="Top 5 Customer with The Most Shipping Cost",
        col=magma(length(mostPaidShipping$countCustomer))
)

# 4. Profit Gain According to Market List

# Library dplyr
library(dplyr)

# Create a new data      
gainProfitMarket = order_new_extracted %>% group_by(market) %>%
  summarise(sumProfitGain = sum(profit),
            .groups = 'drop')

# Bar Chart
barplot(gainProfitMarket$sumProfitGain, names.arg=gainProfitMarket$market,
        horiz = TRUE,
        xlab="Profit Gain",
        cex.names=0.9,
        main="Profit Gain According to Market List",
        col=inferno(length(gainProfitMarket$sumProfitGain))
)


