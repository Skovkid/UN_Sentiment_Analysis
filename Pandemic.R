# Install and load required packages
# install.packages(c("tidyverse", "tidytext", "pdftools"))
library(tidyverse)
library(tidytext)

# Define the list of years
years <- c("2023","2022","2021", "2020")

# Initialize an empty data frame to store results
all_country_sentiments <- data.frame()

# Iterate over each year
for (year in years) {
  # Define the folder containing PDF files for the current year
  pdf_folder <- paste0("Negotiations/", year, "/")
  
  # Get the list of PDF files in the folder
  pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)
  
  # Iterate over each PDF file for the current year
  for (pdf_file in pdf_files) {
    # Open the PDF file and extract English text
    text <- pdftools::pdf_text(pdf_file) %>% paste(collapse = "\n")
    
    # Define a regular expression to match English text
    english_pattern <- "[a-zA-Z]+"
    
    # Use str_extract_all to extract English words
    english_text <- str_extract_all(text, english_pattern) %>%
      unlist()
    
    # All the countries in the world
    world_countries <- c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", 
                         "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", 
                         "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", 
                         "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", 
                         "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", 
                         "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", 
                         "Congo (Congo-Brazzaville)", "Costa Rica", "Cote d'Ivoire", "Croatia", "Cuba", 
                         "Cyprus", "Czechia (Czech Republic)", "Denmark", "Djibouti", "Dominica", 
                         "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", 
                         "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland", "France", 
                         "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", 
                         "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", 
                         "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", 
                         "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", 
                         "Kiribati", "Korea, North", "Korea, South", "Kosovo", "Kuwait", "Kyrgyzstan", 
                         "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", 
                         "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", 
                         "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", 
                         "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", 
                         "Mozambique", "Myanmar (formerly Burma)", "Namibia", "Nauru", "Nepal", 
                         "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Macedonia", 
                         "Norway", "Oman", "Pakistan", "Palau", "Palestine State", "Panama", 
                         "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", 
                         "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", 
                         "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", 
                         "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", 
                         "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Sudan", 
                         "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland", "Syria", 
                         "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", "Togo", 
                         "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", 
                         "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom of Great Britain 
                         and Northern Ireland",
                         "United States of America", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", 
                         "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe")
    
    # Section the data
    country_data <- list()
    current_country <- NULL
    current_words <- character()
    
    for (word in english_text) {
      if (word %in% world_countries) {
        if (!is.null(current_country)) {
          country_data[[current_country]] <- current_words
        }
        current_country <- word
        current_words <- character()
      } else {
        current_words <- c(current_words, word)
      }
    }
    
    if (!is.null(current_country)) {
      country_data[[current_country]] <- current_words
    }
    
    # Sentiment analysis
    bing_lexicon <- get_sentiments("bing")
    
    # Summarize sentiment scores for each country
    country_sentiments <- map_dfr(names(country_data), function(country) {
      country_words <- unlist(country_data[[country]])
      sentiment_scores <- inner_join(data.frame(text = country_words), bing_lexicon, by = c("text" = "word"))
      score <- sum(ifelse(sentiment_scores$sentiment == "positive", 1, -1), na.rm = TRUE)
      tibble(year = year, country = country, score = score)
    })
    
    # Append the results to the overall data frame
    all_country_sentiments <- bind_rows(all_country_sentiments, country_sentiments)
  }
}

# Rename "Ireland" to "UK" in the combined data frame
all_country_sentiments$country[all_country_sentiments$country == "Ireland"] <- "UK"

# Filter out rows with sentiment score = 0
filtered_country_sentiments <- all_country_sentiments %>%
  filter(score != 0)

# Create a line graph to visualize sentiment scores over time
ggplot(filtered_country_sentiments, aes(x = year, y = score, group = country, color = country)) +
  geom_line() +
  labs(title = "Sentiment Scores by Country Over Time",
       x = "Year",
       y = "Sentiment Score") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = rainbow(length(unique(filtered_country_sentiments$country))))


#Aggregated bar chart


# Combine sentiment scores for each country across different years
combined_country_scores <- filtered_country_sentiments %>%
  group_by(country) %>%
  summarize(score = sum(score))

# Create a bar chart for the combined sentiment scores
combined_plot <- ggplot(combined_country_scores, aes(x = reorder(country, score), y = score)) +
  geom_bar(stat = "identity", aes(fill = ifelse(score < 0, "Negative", "Positive")), color = "black") +
  geom_text(aes(label = abs(score), y = score + 15 * sign(score)), size = 3) +
  scale_fill_manual(values = c("Positive" = "blue", "Negative" = "red")) +
  labs(title = "Aggregated Combined Sentiment Scores by Country, 2020-2023",
       x = "Country",
       y = "Combined Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(panel.background = element_rect(fill = "white")) +  # Set background to white
  theme(plot.background = element_rect(fill = "white")) +  # Set background to white
  theme(legend.position = "none")  # Remove legend

# Save the combined bar chart as an image (you can adjust the file format and path)
ggsave(filename = "combined_bar_chart_pandemic.png", combined_plot)


#Make this into a scale

# Find the minimum and maximum scores
min_score <- min(combined_country_scores$score)
max_score <- max(combined_country_scores$score)

# Define the desired range
desired_min <- -100
desired_max <- 100

# Scale the scores to the desired range
combined_country_scores$scaled_score <- ((combined_country_scores$score - min_score) / (max_score - min_score)) * (desired_max - desired_min) + desired_min

# Now, combined_country_scores$scaled_score contains the scaled scores



##Hämta AI index och lägg som oberoende varibel mot den beroende variabeln, 

