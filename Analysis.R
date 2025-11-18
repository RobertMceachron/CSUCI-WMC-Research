library(tidyverse)
library(readxl)
data <- read_excel("data/WSE_data.xlsx")
tutdata <- read.csv("data/EXP_List.csv")

#################
# Data Cleaning #
#################

# Grabs only people who finished the survey
data <- data[data$Finished == "True",]

# Converts Pre-Post Scores to be numeric
data <- data %>%
  mutate(across(contains(c("Pre", "Post")), ~ as.numeric(.x)))

# Computes means for Pre-Post
data <- data %>%
  mutate(PreScore = rowMeans(across(starts_with("Pre"))))
data <- data %>%
  mutate(PostScore = rowMeans(across(starts_with("Post"))))

# Converts Tutor Name to Tutor Experience
EXPConverter <- if(data$Tutor == ) {
  # Need Identifier between both. Then if its equal then we replace
  # Need to 
  # tutdata$EXP
}


#################
# Data Analysis #
#################

# Runs a T-Test between the two groups
t.test(data$PreScore, data$PostScore, conf.level = 0.95)

# Moderation Analysis
model <- lm(data$PostScore ~ data$PreScore + data$TutorExp + data$PreScore:data$TutorExp, data = data)
summary(model)

#################
#   Data Vis.   #
#################