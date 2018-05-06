# loading sources
source("Documents/4th_Sem/Biz_Intel/BabsonAnalytics.R")

# loading libraries
library(readr)
library(gmodels)
library(arules)
library(arulesViz)
library(caret)

# loading data sets
df<-read.csv("Documents/4th_Sem/Biz_Intel/CaseReport2/Yelp.csv")

# Considering Unique IDs
df = sqldf("SELECT * FROM df 
           GROUP BY ID") # Takes Unique IDs (data pre-processing) (Step 0)

df$ID = NULL
elbowChart(df) # Finding out the optimum k-value (Step 1)

df = removeOutliers(df) # removing outliers (Step 3)

df_standardized = predict(preProcess(df,method=c("scale","center")),df) # (Step 4) : Normalization i.e., (contd. down)
                                    # subtracting each observation with mean and dividing by standard deviation
M = apply(df_standardized, 1, max) 
m = apply(df_standardized, 1, min)
outliers = df_standardized[M > 3 | m < -3, ] # Observing Outliers (Step 2) [They are insignificant in this particular (contd. down)
                                # dataset as they are only outliers for a single attribute and not for more than one attribute]
## Testing begin

elbowChart(outliers)
model = kmeans(outliers, 5)
aggregate(outliers, by=list(model$cluster), mean)

## Testing end

# make groups with standardized data
# but then show the cluster centers in 
# non-standardized (original) units
model = kmeans(df_standardized, 4) # Kmeans optimium clusters along with their centers (Step 5)
aggregate(df, by=list(model$cluster), mean) # See Excel for Ranking (Step 6)