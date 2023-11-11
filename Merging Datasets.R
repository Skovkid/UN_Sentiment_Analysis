library(dplyr)

# Assuming you have three data frames: df1, df2, and df3

# df1 has columns 'country' and 'score'
final_sentiment_score


# df2 has columns 'Country' and 'AIScore'
AI_index

# df3 has columns 'sentiment' and 'Country'
agg_sentiment

# First, make sure that the country names are consistent across all data frames
# Convert all country names to a consistent case, here I am using title case
final_sentiment_score$country <- tools::toTitleCase(final_sentiment_score$country)
AI_index$Country <- tools::toTitleCase(AI_index$Country)
agg_sentiment$Country <- tools::toTitleCase(agg_sentiment$Country)


final_sentiment_score <- final_sentiment_score %>%
  rename(Country = country)

# Now, merge the data frames
# Start by merging the first two
merged_df <- left_join(AI_index,agg_sentiment, by = "Country")

# Now merge the third data frame
merged_df <- left_join(merged_df,final_sentiment_score , by = "Country")

# Replace NA values with 0 for summation
merged_df$score[is.na(merged_df$score)] <- 0
merged_df$sentiment[is.na(merged_df$sentiment)] <- 0

# Sum up the score and sentiment columns
merged_df$total_score <- rowSums(merged_df[, c("score", "sentiment")], na.rm = TRUE)

# At this point, merged_df contains all the data from the three original data frames
# You may need to address any NAs that result from the merge if there are countries that don't match up

#Remove Uruguay as a stat outlier
merged_df <- merged_df[merged_df$Country != "Uruguay", ]

#######################################################################Set scaling methods

#######################################  Min/Max


min_score <- min(merged_df$total_score)
max_score <- max(merged_df$total_score)
merged_df$scaled_total_score <- (merged_df$total_score - min_score) / (max_score - min_score) * 100

######################################### Z-score


merged_df$scaled_total_score_z <- scale(merged_df$total_score)


##############################################IQR-Range


IQR_value <- IQR(merged_df$total_score, na.rm = TRUE)
median_value <- median(merged_df$total_score, na.rm = TRUE)
merged_df$scaled_total_score_robust <- (merged_df$total_score - median_value) / IQR_value


# Now you can perform your multiple regression with sentiment as the dependent variable
# and AIScore, Share of Spending 2022, and GDP (PPP) as independent variables
# Here's a placeholder for the regression model


# Scatterplot with regression line 

########################################MIN/MAX Regress


lin_model <- lm(total_score ~ AIScore, data = merged_df)

    plot(merged_df$AIScore, merged_df$scaled_total_score, 
     main = "Scatterplot with Regression Line",
     xlab = "AI Score", ylab = "Scaled Total Score",
     pch = 19, frame = FALSE, col = "blue")
abline(model, col = "red")

summary(lin_model)
confint(lin_model)


##############################Z-score Regress


lin_model_Z <- lm(scaled_total_score_z ~ AIScore, data = merged_df)


plot(merged_df$AIScore, merged_df$scaled_total_score_z, 
     main = "Scatterplot with Regression Line",
     xlab = "AI Score", ylab = "Scaled Total Score",
     pch = 19, frame = FALSE, col = "blue")
abline(lin_model_Z, col = "red")

summary(lin_model_Z)
confint(lin_model_Z)

###############################IQR Regress

lin_model_Rob <- lm(scaled_total_score_robust ~ AIScore,data= merged_df)

plot(merged_df$AIScore, merged_df$scaled_total_score_robust, 
     main = "Scatterplot with Regression Line",
     xlab = "AI Score", ylab = "Scaled Total Score",
     pch = 19, frame = FALSE, col = "blue")
abline(lin_model_Rob, col = "red")

summary(lin_model_Rob)
confint(lin_model_Rob)



############################## NO SCALING

lin_model_No_Scl <- lm(total_score ~ AIScore, data = merged_df)

plot(merged_df$AIScore, merged_df$total_score, 
     main = "Scatterplot with Regression Line",
     xlab = "AI Score", ylab = "Scaled Total Score",
     pch = 19, frame = FALSE, col = "blue")
abline(lin_model_No_Scl, col = "red")

summary(lin_model_No_Scl)
confint(lin_model_No_Scl)
