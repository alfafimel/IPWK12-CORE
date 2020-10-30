IPWK12-CORE
================
Elizabeth Josephine
10/28/2020

# DEFINING THE QUESTION

## **a) Specifying the Question**

Determining which individuals are most likely to click on the ads of the
Kenyan entrepreneur

## **b) Defining the metrics for success**

Bivariates and univariate Exploratory data analysis

## **c) Understanding the context**

Determination of the audience the entrepreneur can target

## **d) Recording the Experimental Design**

1.  Define the question, the metric for success, the context,
    experimental design taken.
2.  Read and explore the given dataset.
3.  Find and deal with outliers, anomalies, and missing data within the
    dataset.
4.  Perform univariate and bivariate analysis.
5.  From your insights provide a conclusion and recommendation.

## **e) Relevance of the data**

The data used for this project is necessary for determining which
audience should be targeted

\[<http://bit.ly/IPAdvertisingData>\].

# EXPLORATORY DATA ANALYSIS

## DATA CHECKING

``` r
# loading libraries
library(relaimpo)
```

    ## Loading required package: MASS

    ## Loading required package: boot

    ## Loading required package: survey

    ## Loading required package: grid

    ## Loading required package: Matrix

    ## Loading required package: survival

    ## 
    ## Attaching package: 'survival'

    ## The following object is masked from 'package:boot':
    ## 
    ##     aml

    ## 
    ## Attaching package: 'survey'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     dotchart

    ## Loading required package: mitools

    ## This is the global version of package relaimpo.

    ## If you are a non-US user, a version with the interesting additional metric pmvd is available

    ## from Ulrike Groempings web site at prof.beuth-hochschule.de/groemping.

``` r
library(data.table)
library(ggplot2) # Data visualization
library(ggthemes) # Plot themes
library(plotly) # Interactive data visualizations
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:MASS':
    ## 
    ##     select

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(dplyr) # Data manipulation
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:MASS':
    ## 
    ##     select

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(psych) # Will be used for correlation visualization
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

    ## The following object is masked from 'package:boot':
    ## 
    ##     logit

``` r
# importing our data
# reading our data
df <- fread('http://bit.ly/IPAdvertisingData')
df
```

    ##       Daily Time Spent on Site Age Area Income Daily Internet Usage
    ##    1:                    68.95  35    61833.90               256.09
    ##    2:                    80.23  31    68441.85               193.77
    ##    3:                    69.47  26    59785.94               236.50
    ##    4:                    74.15  29    54806.18               245.89
    ##    5:                    68.37  35    73889.99               225.58
    ##   ---                                                              
    ##  996:                    72.97  30    71384.57               208.58
    ##  997:                    51.30  45    67782.17               134.42
    ##  998:                    51.63  51    42415.72               120.37
    ##  999:                    55.55  19    41920.79               187.95
    ## 1000:                    45.01  26    29875.80               178.35
    ##                               Ad Topic Line           City Male
    ##    1:    Cloned 5thgeneration orchestration    Wrightburgh    0
    ##    2:    Monitored national standardization      West Jodi    1
    ##    3:      Organic bottom-line service-desk       Davidton    0
    ##    4: Triple-buffered reciprocal time-frame West Terrifurt    1
    ##    5:         Robust logistical utilization   South Manuel    0
    ##   ---                                                          
    ##  996:         Fundamental modular algorithm      Duffystad    1
    ##  997:       Grass-roots cohesive monitoring    New Darlene    1
    ##  998:          Expanded intangible solution  South Jessica    1
    ##  999:  Proactive bandwidth-monitored policy    West Steven    0
    ## 1000:       Virtual 5thgeneration emulation    Ronniemouth    0
    ##                      Country           Timestamp Clicked on Ad
    ##    1:                Tunisia 2016-03-27 00:53:11             0
    ##    2:                  Nauru 2016-04-04 01:39:02             0
    ##    3:             San Marino 2016-03-13 20:35:42             0
    ##    4:                  Italy 2016-01-10 02:31:19             0
    ##    5:                Iceland 2016-06-03 03:36:18             0
    ##   ---                                                         
    ##  996:                Lebanon 2016-02-11 21:49:00             1
    ##  997: Bosnia and Herzegovina 2016-04-22 02:07:01             1
    ##  998:               Mongolia 2016-02-01 17:24:57             1
    ##  999:              Guatemala 2016-03-24 02:35:54             0
    ## 1000:                 Brazil 2016-06-03 21:43:21             1

``` r
# previewing the dataset
View(df)
```

``` r
# previewing the column names
colnames(df)
```

    ##  [1] "Daily Time Spent on Site" "Age"                     
    ##  [3] "Area Income"              "Daily Internet Usage"    
    ##  [5] "Ad Topic Line"            "City"                    
    ##  [7] "Male"                     "Country"                 
    ##  [9] "Timestamp"                "Clicked on Ad"

``` r
# previewing the dataset
class(df)
```

    ## [1] "data.table" "data.frame"

``` r
# previewing the head of the dataset
head(df, n = 5)
```

    ##    Daily Time Spent on Site Age Area Income Daily Internet Usage
    ## 1:                    68.95  35    61833.90               256.09
    ## 2:                    80.23  31    68441.85               193.77
    ## 3:                    69.47  26    59785.94               236.50
    ## 4:                    74.15  29    54806.18               245.89
    ## 5:                    68.37  35    73889.99               225.58
    ##                            Ad Topic Line           City Male    Country
    ## 1:    Cloned 5thgeneration orchestration    Wrightburgh    0    Tunisia
    ## 2:    Monitored national standardization      West Jodi    1      Nauru
    ## 3:      Organic bottom-line service-desk       Davidton    0 San Marino
    ## 4: Triple-buffered reciprocal time-frame West Terrifurt    1      Italy
    ## 5:         Robust logistical utilization   South Manuel    0    Iceland
    ##              Timestamp Clicked on Ad
    ## 1: 2016-03-27 00:53:11             0
    ## 2: 2016-04-04 01:39:02             0
    ## 3: 2016-03-13 20:35:42             0
    ## 4: 2016-01-10 02:31:19             0
    ## 5: 2016-06-03 03:36:18             0

``` r
# previewing the tail of the dataset
tail(df, n = 5)
```

    ##    Daily Time Spent on Site Age Area Income Daily Internet Usage
    ## 1:                    72.97  30    71384.57               208.58
    ## 2:                    51.30  45    67782.17               134.42
    ## 3:                    51.63  51    42415.72               120.37
    ## 4:                    55.55  19    41920.79               187.95
    ## 5:                    45.01  26    29875.80               178.35
    ##                           Ad Topic Line          City Male
    ## 1:        Fundamental modular algorithm     Duffystad    1
    ## 2:      Grass-roots cohesive monitoring   New Darlene    1
    ## 3:         Expanded intangible solution South Jessica    1
    ## 4: Proactive bandwidth-monitored policy   West Steven    0
    ## 5:      Virtual 5thgeneration emulation   Ronniemouth    0
    ##                   Country           Timestamp Clicked on Ad
    ## 1:                Lebanon 2016-02-11 21:49:00             1
    ## 2: Bosnia and Herzegovina 2016-04-22 02:07:01             1
    ## 3:               Mongolia 2016-02-01 17:24:57             1
    ## 4:              Guatemala 2016-03-24 02:35:54             0
    ## 5:                 Brazil 2016-06-03 21:43:21             1

``` r
# checking the structure of the data
str(df)
```

    ## Classes 'data.table' and 'data.frame':   1000 obs. of  10 variables:
    ##  $ Daily Time Spent on Site: num  69 80.2 69.5 74.2 68.4 ...
    ##  $ Age                     : int  35 31 26 29 35 23 33 48 30 20 ...
    ##  $ Area Income             : num  61834 68442 59786 54806 73890 ...
    ##  $ Daily Internet Usage    : num  256 194 236 246 226 ...
    ##  $ Ad Topic Line           : chr  "Cloned 5thgeneration orchestration" "Monitored national standardization" "Organic bottom-line service-desk" "Triple-buffered reciprocal time-frame" ...
    ##  $ City                    : chr  "Wrightburgh" "West Jodi" "Davidton" "West Terrifurt" ...
    ##  $ Male                    : int  0 1 0 1 0 1 0 1 1 1 ...
    ##  $ Country                 : chr  "Tunisia" "Nauru" "San Marino" "Italy" ...
    ##  $ Timestamp               : chr  "2016-03-27 00:53:11" "2016-04-04 01:39:02" "2016-03-13 20:35:42" "2016-01-10 02:31:19" ...
    ##  $ Clicked on Ad           : int  0 0 0 0 0 0 0 1 0 0 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
# checking the dimension/shape of the data
dim(df) # 1000 rows and 10 columns
```

    ## [1] 1000   10

``` r
# selecting needed columns
df <- subset(df, select = c("Daily Time Spent on Site", "Age", "Daily Internet Usage", "Male", "Country", "Clicked on Ad"))
colnames(df)
```

    ## [1] "Daily Time Spent on Site" "Age"                     
    ## [3] "Daily Internet Usage"     "Male"                    
    ## [5] "Country"                  "Clicked on Ad"

## DATA CLEANING

### Missing Values

``` r
# checking for missing values
sum(is.na(df))# there are no missing values in the data
```

    ## [1] 0

``` r
# displaying all rows from the dataset which don't contain any missing values 
na.omit(df)
```

    ##       Daily Time Spent on Site Age Daily Internet Usage Male
    ##    1:                    68.95  35               256.09    0
    ##    2:                    80.23  31               193.77    1
    ##    3:                    69.47  26               236.50    0
    ##    4:                    74.15  29               245.89    1
    ##    5:                    68.37  35               225.58    0
    ##   ---                                                       
    ##  996:                    72.97  30               208.58    1
    ##  997:                    51.30  45               134.42    1
    ##  998:                    51.63  51               120.37    1
    ##  999:                    55.55  19               187.95    0
    ## 1000:                    45.01  26               178.35    0
    ##                      Country Clicked on Ad
    ##    1:                Tunisia             0
    ##    2:                  Nauru             0
    ##    3:             San Marino             0
    ##    4:                  Italy             0
    ##    5:                Iceland             0
    ##   ---                                     
    ##  996:                Lebanon             1
    ##  997: Bosnia and Herzegovina             1
    ##  998:               Mongolia             1
    ##  999:              Guatemala             0
    ## 1000:                 Brazil             1

### Duplicates

``` r
# checking for duplicates
duplicated_rows <- df[duplicated(df),]
duplicated_rows # there are no duplicates in the data
```

    ## Empty data.table (0 rows and 6 cols): Daily Time Spent on Site,Age,Daily Internet Usage,Male,Country,Clicked on Ad

``` r
# showing these unique items and assigning to a variable unique_items below
unique_items <- df[!duplicated(df), ]
unique_items
```

    ##       Daily Time Spent on Site Age Daily Internet Usage Male
    ##    1:                    68.95  35               256.09    0
    ##    2:                    80.23  31               193.77    1
    ##    3:                    69.47  26               236.50    0
    ##    4:                    74.15  29               245.89    1
    ##    5:                    68.37  35               225.58    0
    ##   ---                                                       
    ##  996:                    72.97  30               208.58    1
    ##  997:                    51.30  45               134.42    1
    ##  998:                    51.63  51               120.37    1
    ##  999:                    55.55  19               187.95    0
    ## 1000:                    45.01  26               178.35    0
    ##                      Country Clicked on Ad
    ##    1:                Tunisia             0
    ##    2:                  Nauru             0
    ##    3:             San Marino             0
    ##    4:                  Italy             0
    ##    5:                Iceland             0
    ##   ---                                     
    ##  996:                Lebanon             1
    ##  997: Bosnia and Herzegovina             1
    ##  998:               Mongolia             1
    ##  999:              Guatemala             0
    ## 1000:                 Brazil             1

### Checking for outliers

``` r
# visualizing any existing outliers using a boxplot
df1 <- subset(df, select = c("Daily Time Spent on Site", "Age", "Daily Internet Usage", "Male", "Clicked on Ad"))

boxplot(df1)# there are no outliers in the data
```

![](IPWK12_CORE_DATA_CLEANING_AND_EDA_R_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# renaming columns for easy analysis
df1 <- df1 %>% rename(Daily_Time_Spent_on_Site = "Daily Time Spent on Site")
df1 <- df1 %>% rename(Daily_Internet_Usage = "Daily Internet Usage")
df1 <- df1 %>% rename(Clicked_on_Ad = "Clicked on Ad")

    
df1
```

    ##       Daily_Time_Spent_on_Site Age Daily_Internet_Usage Male Clicked_on_Ad
    ##    1:                    68.95  35               256.09    0             0
    ##    2:                    80.23  31               193.77    1             0
    ##    3:                    69.47  26               236.50    0             0
    ##    4:                    74.15  29               245.89    1             0
    ##    5:                    68.37  35               225.58    0             0
    ##   ---                                                                     
    ##  996:                    72.97  30               208.58    1             1
    ##  997:                    51.30  45               134.42    1             1
    ##  998:                    51.63  51               120.37    1             1
    ##  999:                    55.55  19               187.95    0             0
    ## 1000:                    45.01  26               178.35    0             1

``` r
colnames(df1)
```

    ## [1] "Daily_Time_Spent_on_Site" "Age"                     
    ## [3] "Daily_Internet_Usage"     "Male"                    
    ## [5] "Clicked_on_Ad"

# BIVARIATE AND UNIVARIATE ANALYSIS

## Univariate Analysis

### Measures of Central Tendency

``` r
# descriptive statistics
# these summaries will provide us with the measures of central tendencies of the numerical columns
summary(df1)
```

    ##  Daily_Time_Spent_on_Site      Age        Daily_Internet_Usage      Male      
    ##  Min.   :32.60            Min.   :19.00   Min.   :104.8        Min.   :0.000  
    ##  1st Qu.:51.36            1st Qu.:29.00   1st Qu.:138.8        1st Qu.:0.000  
    ##  Median :68.22            Median :35.00   Median :183.1        Median :0.000  
    ##  Mean   :65.00            Mean   :36.01   Mean   :180.0        Mean   :0.481  
    ##  3rd Qu.:78.55            3rd Qu.:42.00   3rd Qu.:218.8        3rd Qu.:1.000  
    ##  Max.   :91.43            Max.   :61.00   Max.   :270.0        Max.   :1.000  
    ##  Clicked_on_Ad
    ##  Min.   :0.0  
    ##  1st Qu.:0.0  
    ##  Median :0.5  
    ##  Mean   :0.5  
    ##  3rd Qu.:1.0  
    ##  Max.   :1.0

``` r
# Finding the mode of age
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

df1.Age.mode <- getmode(df$Age)
df1.Age.mode
```

    ## [1] 31

``` r
# Finding the median of age
df1.Age.median <- median(df$Age)
df1.Age.median 
```

    ## [1] 35

### Measures of Dispersion

``` r
# Since we cannot find the measures of dispersion for all the six
# columns at a go, i will work with age and daily internet usage
```

``` r
colnames(df1) 
```

    ## [1] "Daily_Time_Spent_on_Site" "Age"                     
    ## [3] "Daily_Internet_Usage"     "Male"                    
    ## [5] "Clicked_on_Ad"

``` r
# Finding the minimum code
df1.Daily_Internet_Usage.min <- min(df1$Daily_Internet_Usage)
df1.Daily_Internet_Usage.min
```

    ## [1] 104.78

``` r
# Finding the minimum code
df1.Age.min <- min(df1$Age)
df1.Age.min
```

    ## [1] 19

``` r
# Finding the maximum code
df1.Daily_Internet_Usage.max <- max(df1$Daily_Internet_Usage)
df1.Daily_Internet_Usage.max
```

    ## [1] 269.96

``` r
# Finding the maximum code
df1.Age.max <- max(df1$Age)
df1.Age.max
```

    ## [1] 61

``` r
# Finding the range code
df1.Daily_Internet_Usage.range <- range(df1$Daily_Internet_Usage)
df1.Daily_Internet_Usage.range
```

    ## [1] 104.78 269.96

``` r
# Finding the range code
df1.Age.range <- range(df1$Age)
df1.Age.range
```

    ## [1] 19 61

``` r
# Finding the quantile code
df1.Daily_Internet_Usage.quantile <- quantile(df1$Daily_Internet_Usage)
df1.Daily_Internet_Usage.quantile
```

    ##       0%      25%      50%      75%     100% 
    ## 104.7800 138.8300 183.1300 218.7925 269.9600

``` r
# Finding the quantile code
df1.Age.quantile <- quantile(df1$Age)
df1.Age.quantile
```

    ##   0%  25%  50%  75% 100% 
    ##   19   29   35   42   61

``` r
# Finding the variance code
df1.Daily_Internet_Usage.variance <- var(df1$Daily_Internet_Usage)
df1.Daily_Internet_Usage.variance
```

    ## [1] 1927.415

``` r
# Finding the variance code
df1.Age.variance <- var(df1$Age)
df1.Age.variance
```

    ## [1] 77.18611

``` r
# Finding the standard deviation code
df1.Daily_Internet_Usage.sd <- sd(df1$Daily_Internet_Usage)
df1.Daily_Internet_Usage.sd
```

    ## [1] 43.90234

``` r
# Finding the standard deviation code
df1.Age.sd <- sd(df1$Age)
df1.Age.sd
```

    ## [1] 8.785562

### Univariate Graphical

``` r
# creating a boxplot graph for the variable all the numerical variables
boxplot(df1)
```

![](IPWK12_CORE_DATA_CLEANING_AND_EDA_R_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
# fetching the columns
Daily_Internet_Usage <- df1$Daily_Internet_Usage

# fetching the frequency distribution
Daily_Internet_Usage_frequency <- table(Daily_Internet_Usage)

# plotting the bargraph
barplot(Daily_Internet_Usage)
```

![](IPWK12_CORE_DATA_CLEANING_AND_EDA_R_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
# fetching the columns
Age <- df1$Age

# fetching the frequency distribution
Age_frequency <- table(Age)

# plotting the bargraph
barplot(Age)
```

![](IPWK12_CORE_DATA_CLEANING_AND_EDA_R_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
# creating a boxplot graph for the variable all the numerical variables
colnames(df1)
```

    ## [1] "Daily_Time_Spent_on_Site" "Age"                     
    ## [3] "Daily_Internet_Usage"     "Male"                    
    ## [5] "Clicked_on_Ad"

``` r
# fetching the columns
hist(df1$Daily_Internet_Usage)
```

![](IPWK12_CORE_DATA_CLEANING_AND_EDA_R_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
# fetching the columns
hist(df1$Age)
```

![](IPWK12_CORE_DATA_CLEANING_AND_EDA_R_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

## Bivariate analysis

``` r
# assigning columns to variables
Age<- df1$Age

# finding the covariance
cov(Daily_Internet_Usage, Age)
```

    ## [1] -141.6348

``` r
# finding the correlation
cor(Age, Daily_Internet_Usage) # negative correlation coefficient
```

    ## [1] -0.3672086

### Graphical Techniques

``` r
# finding the correlation matrix
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
#rquery.cormat(df1, type="full")
res <- cor(df1)
round(res, 2)
```

    ##                          Daily_Time_Spent_on_Site   Age Daily_Internet_Usage
    ## Daily_Time_Spent_on_Site                     1.00 -0.33                 0.52
    ## Age                                         -0.33  1.00                -0.37
    ## Daily_Internet_Usage                         0.52 -0.37                 1.00
    ## Male                                        -0.02 -0.02                 0.03
    ## Clicked_on_Ad                               -0.75  0.49                -0.79
    ##                           Male Clicked_on_Ad
    ## Daily_Time_Spent_on_Site -0.02         -0.75
    ## Age                      -0.02          0.49
    ## Daily_Internet_Usage      0.03         -0.79
    ## Male                      1.00         -0.04
    ## Clicked_on_Ad            -0.04          1.00

``` r
# creating a scatterplot
plot(Age, Daily_Internet_Usage, xlab="Age", ylab="Daily_Internet_Usage")
```

![](IPWK12_CORE_DATA_CLEANING_AND_EDA_R_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
# creating a scatterplot
plot(Age, Daily_Internet_Usage, xlab="Age", ylab="DDaily_Time_Spent_on_Site")
```

![](IPWK12_CORE_DATA_CLEANING_AND_EDA_R_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->
\# CONCLUSIONS AND RECOMMENDATIONS \#\# Conclusions

``` r
# From the analysis above, we can conclude that:
# the people that clicked on the ads on the blog were aged between 19 yrs and 61 years old.
# their internet usage ranged between 104.8 to 270 units with the time spent on her blog ranging between 32 to 91 minutes.
# there were no outliers in the data which made it easier for analysis
# the graphs in the analysis were used to visualize the most important factors that influenced how the individuals that visited her site interacted with the posts and ads.
# there was a negative correlation between the age and the daily internet usage of the individuals.
# notably, the audiences were mostly middle aged and the young which explained the clicks on the site.
# it was also realized that the higher the age, the lower the time that was spent on the site.
```

## Recommendations

``` r
# From the analysis, i would recommend that the ads that are to be placed on the blog be relevant to the ages. Because only then will individuals click the ads.
# this is to say that they can minimize ads that relate to older individuals and maximize on the ads that the young and middle adolescents can relate with. 
```
