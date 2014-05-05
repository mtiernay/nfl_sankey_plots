# Produce Sankey plots of the NFL play data


library(foreign)
library(plyr)
library(ggplot2)
library(reshape2)
library(RODBC)
library(Hmisc)
library(randomForest)
library(TraMineR)
library(cluster)
library(lubridate)
library(stringr)
library(data.table)
library(devtools)
require(rCharts)


# Read in the data
data <- read.csv("/Users/michaeltiernay-macbook/Desktop/2013_nfl_pbp_data_through_wk_4.csv", header=TRUE, stringsAsFactors=FALSE)

# Remove kick-offs and extra points (which are NA in the "down" field)
data <- data[complete.cases(data$down),]

# I need an "other" besides run and pass for first-downs and scores and turnovers
data$play <- "Run"
data$play[which(grepl("pass", data$description)==TRUE)] <- "Pass"
data$previous_play <- ""

###
# Here, I am formatting the data for the plots. It's not commented, so run each chunk if you care what it does.
# If not, just run it all and skip to where the Sankey plots below are produced.
data <- subset(data, down < 4)
j <- 0
for(i in 1:nrow(data)){
  
if(data$down[i] != 1) {data$drive[i] <- j
                        data$previous_play[i] <- data$play[i-1]
  }

else{j <- j+1
     data$drive[i] <- j
  }
}



data$pass <- as.numeric(grepl("pass", data$description))
data$run <- as.numeric(data$pass==0)

plot.data <- data.frame(source = character(14), target = character(14), value = numeric(14))

plot.data$source <- c("New Down", "New Down", 
                    "Pass on 1st Down", "Pass on 1st Down", "Pass on 1st Down",
                    "Run on 1st Down", "Run on 1st Down", "Run on 1st Down",
                    "Pass on 2nd Down", "Pass on 2nd Down", "Pass on 2nd Down",
                    "Run on 2nd Down", "Run on 2nd Down", "Run on 2nd Down")

plot.data$target <- c("Pass on 1st Down","Run on 1st Down",
                  "Pass on 2nd Down", "Run on 2nd Down", "Other",
                  "Pass on 2nd Down", "Run on 2nd Down", "Other",
                  "Pass on 3rd Down", "Run on 3rd Down", "Other",
                  "Pass on 3rd Down", "Run on 3rd Down", "Other")

# Fill in the numbers of the dataframe
first_down <- subset(data, down==1)
plot.data$value[1] <- mean(first_down$pass)
plot.data$value[2] <- mean(first_down$run)

second_down_from_pass <- subset(data, down==2 & previous_play=="Pass")
plot.data$value[3] <- sum(second_down_from_pass$pass)/sum(as.numeric(data$down==1))
plot.data$value[4] <- sum(second_down_from_pass$run)/sum(as.numeric(data$down==1))
plot.data$value[5] <- mean(first_down$pass) - plot.data$value[3] - plot.data$value[4]

second_down_from_run <- subset(data, down==2 & previous_play=="Run")
plot.data$value[6] <- sum(second_down_from_run$pass)/sum(as.numeric(data$down==1))
plot.data$value[7] <- sum(second_down_from_run$run)/sum(as.numeric(data$down==1))
plot.data$value[8] <- mean(first_down$run) - plot.data$value[6] - plot.data$value[7]

third_down_from_pass <- subset(data, down==3 & previous_play=="Pass")
plot.data$value[9] <- sum(third_down_from_pass$pass)/sum(as.numeric(data$down==2))
plot.data$value[10] <- sum(third_down_from_pass$run)/sum(as.numeric(data$down==2))
# Calculate the percentage of pass plays on 2nd down, then subtract the runs and passes on third to get the "other" category
plot.data$value[11] <-  plot.data$value[3] + plot.data$value[6] - plot.data$value[9] - plot.data$value[10]

third_down_from_run <- subset(data, down==3 & previous_play=="Run")
plot.data$value[12] <- sum(third_down_from_run$pass)/sum(as.numeric(data$down==2))
plot.data$value[13] <- sum(third_down_from_run$run)/sum(as.numeric(data$down==2))
plot.data$value[14] <- plot.data$value[4] + plot.data$value[7] - plot.data$value[12] - plot.data$value[13]

plot.data$value <- plot.data$value*100




#####################################################################################
# This is where the sankey plots are actually being produce
sankeyPlot <- rCharts$new()
sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')
sankeyPlot$setTemplate(script = "http://timelyportfolio.github.io/rCharts_d3_sankey/layouts/chart.html")

# If you have the rCharts files stored locally...
#sankeyPlot <- rCharts$new()
#sankeyPlot$setLib("C:/Program Files/R/R-2.15.3/library/rCharts_d3_sankey-gh-pages/rCharts_d3_sankey-gh-pages")
#sankeyPlot$setTemplate(script = "C:/Program Files/R/R-2.15.3/library/rCharts_d3_sankey-gh-pages/rCharts_d3_sankey-gh-pages/layouts/chart.html")

sankeyPlot$set(
  data = plot.data,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 960,
  height = 600
)
sankeyPlot


