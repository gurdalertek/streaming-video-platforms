
#all data
#######################################################################################
#scatterplot matrix - Support

library(readxl)
apriori <- read_excel("apriori_results_all - v01g.xlsx", 
                                       sheet = "Support Values")
View(apriori)
summary(apriori)
attach(apriori)

# load required library
library(ggplot2)

# create a data frame for the variables to be plotted
plotdata <- data.frame(Amazon_Support = apriori$Amazon_Support,
                       Apple_Support = apriori$Apple_Support,
                       Disney_Support = apriori$Disney_Support,
                       HBO_Support = apriori$HBO_Support,
                       Netflix_Support = apriori$Netflix_Support,
                       Paramount_Support = apriori$Paramount_Support)


library(car)
# select the columns to include in the scatterplot matrix
cols <- c("Amazon_Support", "Apple_Support", "Disney_Support", 
          "HBO_Support", "Netflix_Support", "Paramount_Support")

# create the scatterplot matrix
sp=scatterplotMatrix(apriori[cols], diagonal = "density")


# create a scatterplot matrix using ggplot2 for 2 streaming platforms
ggplot(plotdata,aes(x = Amazon_Support, y = Disney_Support)) + 
  geom_point() + 
  labs(x = "Amazon_Support", y = "Disney_Support") +
  facet_grid(. ~ ., scales = "free") 

###interactive##

library(ggplot2)
library(plotly)


# create a data frame for the variables to be plotted
plotdata <- data.frame(Amazon_Support = apriori$Amazon_Support,
                       Apple_Support = apriori$Apple_Support,
                       Disney_Support = apriori$Disney_Support,
                       HBO_Support = apriori$HBO_Support,
                       Netflix_Support = apriori$Netflix_Support,
                       Paramount_Support = apriori$Paramount_Support)

# create the scatterplot matrix using ggplot2
p <- ggplot(plotdata, aes(x = Amazon_Support, y = Disney_Support)) + 
  geom_point() + 
  labs(x = "Amazon_Support", y = "Disney_Support") +
  facet_grid(. ~ ., scales = "free") 

# convert the ggplot object to plotly
ggplotly(p)

# create the scatterplot matrix using car
sp <- scatterplotMatrix(apriori[cols], diagonal = "density")
sp <- scatterplotMatrix(apriori[cols], diagonal = "adaptiveDensity")

# convert the scatterplot matrix to plotly
ggplotly(sp)

install.packages("GGally")
library(GGally)

# create a data frame for the variables to be plotted
plotdata <- data.frame(Amazon_Support = apriori$Amazon_Support,
                       Apple_Support = apriori$Apple_Support,
                       Disney_Support = apriori$Disney_Support,
                       HBO_Support = apriori$HBO_Support,
                       Netflix_Support = apriori$Netflix_Support,
                       Paramount_Support = apriori$Paramount_Support)

# create the scatterplot matrix using GGally
ggpairs(plotdata)

###############################################################################################3

#scatterplot matrix - Confidence

library(readxl)
apriori<- read_excel("apriori_results_all - v01g.xlsx", 
                                       sheet = "Confidence Values")
View(apriori)
summary(apriori)
attach(apriori)

# load required library
library(ggplot2)

# create a data frame for the variables to be plotted
plotdata <- data.frame(Amazon_Confidence = apriori$Amazon_Confidence,
                       Apple_Confidence = apriori$Apple_Confidence,
                       Disney_Confidence = apriori$Disney_Confidence,
                       HBO_Confidence = apriori$HBO_Confidence,
                       Netflix_Confidence = apriori$Netflix_Confidence,
                       Paramount_Confidence = apriori$Paramount_Confidence)

# create a scatterplot matrix using ggplot2 for 2 streaming platforms
ggplot(plotdata, aes(x = Amazon_Confidence, y = Apple_Confidence)) + 
  geom_point() + 
  labs(x = "Amazon_Confidence", y = "Apple_Confidence") +
  facet_grid(. ~ ., scales = "free") 


library(car)
# read the data
apriori <- read_excel("apriori_results_all - v01g.xlsx", sheet = "Confidence Values")

# select the columns to include in the scatterplot matrix
cols <- c("Amazon_Confidence", "Apple_Confidence", "Disney_Confidence", 
          "HBO_Confidence", "Netflix_Confidence", "Paramount_Confidence")

# create the scatterplot matrix
scatterplotMatrix(apriori[cols], diagonal = "density")

# create a data frame for the variables to be plotted
plotdata <- data.frame(Amazon_Confidence = apriori$Amazon_Confidence,
                       Apple_Confidence = apriori$Apple_Confidence,
                       Disney_Confidence = apriori$Disney_Confidence,
                       HBO_Confidence = apriori$HBO_Confidence,
                       Netflix_Confidence = apriori$Netflix_Confidence,
                       Paramount_Confidence = apriori$Paramount_Confidence)

p <- ggplot(plotdata, aes(x = Disney_Confidence, y = HBO_Confidence)) + 
  geom_point() + 
  labs(x = "Disney_Confidence", y = "HBO_Confidence") +
  facet_grid(. ~ ., scales = "free") 

# convert the ggplot object to plotly
ggplotly(p)


# create the scatterplot matrix using GGally
ggpairs(plotdata)

######################################################################################
#parallel coordinates plot
library(GGally)

# convert the summary table to a dataframe
df <- as.data.frame(t(apriori[-1]))

# set the row names as a column in the dataframe
df$Platform <- row.names(df)

# remove any missing values
df <- na.omit(df)

# create the parallel coordinates plot
ggparcoord(df, columns = 1:6, groupColumn = "Platform", alphaLines = 0.5)

