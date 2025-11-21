library(tidyverse) # Data Mutability
library(readxl) # Reads XLSX Files
library(car) # Homogeneity Tests
library(ggrain)


WSEdata <- read_excel("data/WSE_data.xlsx")
TUTdata <- read.csv("data/EXP_List.csv")

#################
# Data Cleaning #
#################

  # Grabs only people who finished the survey
    WSEdata <- WSEdata[WSEdata$Finished == "True",]
  
  # Converts Pre-Post Scores to be numeric
    WSEdata <- WSEdata %>%
      mutate(across(contains(c("Pre", "Post")), ~ as.numeric(.x)))
  
  # Computes means for Pre-Post
    WSEdata <- WSEdata %>%
      mutate(PreScore = rowMeans(across(starts_with("Pre"))))
    WSEdata <- WSEdata %>%
      mutate(PostScore = rowMeans(across(starts_with("Post"))))
  
  # Converts Tutor Name to Tutor Experience
    data <- merge(WSEdata, TUTdata[c("EXP","Tutor")], by = "Tutor", all.x = TRUE)
  
  # Changes "Training" to 0.5 Experience
    data$EXP <- as.numeric(data$EXP)
    data$EXP[is.na(data$EXP)] <- 0


#################
# Data Analysis #
#################
# Descriptive Statistics
##################################
  # Gender Plot
  GenCount <- as.data.frame(table(data$Gender))  # Create a count table for Gender
  
  # Rename the columns for clarity
  colnames(GenCount) <- c("Gender", "Count")
  
# Hypothesis #1 (Pre-Post Scores)
##################################
  # Computes Means for Each Group
    meanpre <- mean(data$PreScore)
    meanpost <- mean(data$PostScore)
    c("PreScore Mean is", meanpre, "PostScore Mean is", meanpost)
    
  # Runs a T-Test between the two groups
    t.test(data$PreScore, data$PostScore, conf.level = 0.95)

  # Assumptions
  #############
    # Normality
      shapiro.test(data$PreScore)    
        ggplot(data, aes(x=PreScore)) +
          geom_density(fill="#ffcccc", color="black", alpha=0.8)
      
      shapiro.test(data$PostScore)    
        ggplot(data, aes(x=PostScore)) +
          geom_density(fill="#ffcccc", color="black", alpha=0.8)
      
    # Homogeneity of Variance

        
  # Visualizations
  ################
    # Pivots Data to be Longer
      long_data <- pivot_longer(data, cols = c(PreScore, PostScore), names_to = "ScoreType", values_to = "Score")
    
    # Sets ScoreType as Factor so it shows up first
      long_data$ScoreType <- factor(long_data$ScoreType, levels = c('PreScore', 'PostScore'))    
    
    # Plot #1 (LamePlot)
      ggplot(long_data, aes(x = ScoreType, y = Score)) +
        geom_point(aes(color = as.factor(ResponseId)), size = 3) +
        geom_line(aes(group = ResponseId, color = as.factor(ResponseId)), size = 1) +
        labs(title = "Score Changes from Pre to Post",
             x = "Score Type",
             y = "Score") +
        theme_minimal() +
        theme(legend.position = "none")

    # Plot #2 (RainPlot)
      ggplot(long_data[long_data$ScoreType %in% c('PostScore', 'PreScore'),], 
             aes(ScoreType, Score, fill = ScoreType, label )) +
        geom_rain(alpha = .5, rain.side = 'f1x1', id.long.var = "ResponseId") +
        theme_classic() +
        scale_fill_manual(values=c("#ff503d", "dodgerblue")) +
        guides(fill = 'none', color = 'none') +
        labs(x = "Writing Self-Efficacy Scores", 
             y = "Score", 
             title = "Writing Self-Efficacy Before & After a Tutoring Session") + 
        theme(plot.background = element_rect(fill = "#E8E8E8"), 
              panel.background = element_rect(fill = "#ebe6e6"))
      # Add this in when we get more data    
        stat_summary(fun = mean, geom = "line", aes(group = EXP, color = EXP)) 
        
        
# Hypothesis #2 (Moderator of EXP)
##################################
  # Moderation Analysis
    model <- lm(data$PostScore ~ data$EXP + data$PreScore:data$EXP, data = data)
    summary(model)
    
  # Assumptions
  #############
    # Linearity
    
    # Homoscedasticity
