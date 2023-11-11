#install.packages("syuzhet")
#install.packages("countrycode")

library(tidyverse)
library(tm)
library(syuzhet)  # or tidytext
library(countrycode)

#setwd("UN General Debate Corpus/Sentiment_analysis/")  # Set this to your directory

# This pattern matches files like "ABC_12_3456.txt"
file_names <- list.files(pattern = "^[A-Z]{3}_[0-9]{2}_[0-9]{4}.txt$")  
text_data <- lapply(file_names, function(file) {
  file_content <- readLines(file)
  paste(file_content, collapse = " ")  # Combine lines into one string per file
})
names(text_data) <- file_names


# Perform sentiment analysis on the text content of each file
sentiment_scores <- lapply(text_data, function(text) {
  score <- get_sentiment(text, method = "syuzhet")
  return(mean(score))  # Get the average sentiment score for each text
})

# Create a data frame for sentiment scores
sentiment_df <- data.frame(file = names(sentiment_scores), sentiment = unlist(sentiment_scores))

# Function to extract Country and Year from file names
extract_country_and_year <- function(file_name) {
  components <- unlist(strsplit(file_name, "_"))
  Country <- components[1]
  Year <- as.numeric(substr(components[3], 1, 4))
  return(data.frame(Country = Country, Year = Year))
}

country_year_df <- do.call(rbind, lapply(file_names, extract_country_and_year))

# Combine sentiment scores with country and year
sentiment_df <- cbind(sentiment_df, country_year_df)

# Assuming sentiment_df has 'Country' and 'sentiment' columns

# Aggregate sentiment scores for each country
agg_sentiment <- sentiment_df %>%
  group_by(Country) %>%
  summarize(sentiment = sum(sentiment, na.rm = TRUE)) %>%
  ungroup()

# Convert ISO country codes to country names
agg_sentiment$Country_Name <- countrycode(agg_sentiment$Country, "iso3c", "country.name")

# Check for NAs and possibly correct them
agg_sentiment <- agg_sentiment %>%
  filter(!is.na(Country_Name))

# Rename 'Country_Name' to 'Country' and remove the original 'Country' column
agg_sentiment <- agg_sentiment %>%
  select(-Country) %>%
  rename(Country = Country_Name)

# Create a bar chart with aggregated sentiment scores
ggplot(agg_sentiment, aes(x = Country, y = sentiment)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Country", y = "Aggregated Sentiment Score") +
  coord_flip()  # Flipped for better readability of country names

# Save the bar chart to a file
ggsave("agg_sentiment_chart.png", width = 12, height = 8)

    