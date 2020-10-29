
# Duplicated Data

## 1. Identifying Duplicated Data

R checks for duplicates across rows through the duplicated() function. 

#### Identifying Duplicated Data Code Example 1.1


```R
## Example 
# ---
# Question: Identify duplicate data in the given dataframe
# ---
# OUR CODE GOES BELOW
# 

# Creating our vectors
# ---
# 
x1 <- c(2, 4, 5, 6)
x2 <- c(2, 3, 5, 6)
x3 <- c(2, 4, 5, 6)
x4 <- c(2, 4, 5, 6)

# Create a dataframe df from the above vectors
# ---
#
df <- data.frame(rbind(x1, x2, x3, x4))

# Then printing out this dataset 
df


```


```R
# Now lets find the duplicated rows in the dataset df 
# and assign to a variable duplicated_rows below
# ---
# 
duplicated_rows <- df[duplicated(df),]

# Lets print out the variable duplicated_rows and see these duplicated rows 
# ---
# OUR CODE GOES BELOW
#


```


```R
# Removing these duplicated rows in the dataset or 
# showing these unique items and assigning to a variable unique_items below
# ---
#
unique_items <- df[!duplicated(df), ]

# What about seeing what these unique items are?
# ---
#
unique_items


```


```R
# Now there is another way we can also remove duplicated rows 
# in the dataset or show the unique items;
# We simply use the unique() function
# ---
#
unique_items2 <- unique(df)

# After having assigned the unique items to the variable unique_items2, 
# we will now print out this variable and have a look at these unique items 
# ---
# OUR CODE GOES BELOW
# 


```


```R
## Challenge 
# ---
# Question: Display and delete the only duplicate records in the iris dataset below:
# ---
# OUR CODE GOES BELOW 
# 

# Showing the first 6 records in the iris dataset
# ---
# 
head(iris)

# Deleting duplicate records
# ---
# OUR CODE GOES BELOW 
# 


```


```R
## Challenge 
# ---
# Question: Drop duplicate records in the iris games dataset from the url 
# ---
# OUR CODE GOES BELOW
# 

# Importing the data.table
# ---
# 
library("data.table")

# Reading our dataset
# ---
# 
video_games <- fread('http://bit.ly/VideoGamesDataset')

# Previewing the first 6 records of the video games dataset
# ---
# 
head(video_games)

# Cleaning our dataset 
# ---
# OUR CODE GOES BELOW
# 


```
