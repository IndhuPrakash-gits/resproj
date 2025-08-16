# Step 1: Data Cleaning for "The Adventures of Sherlock Holmes"
## 1. Load Libraries
pacman::p_load(tidyverse, rvest, stringr, tidytext)


## 2. Scrape Text from URL
url <- "https://gutenberg.net.au/ebooks/c00011.html"
book_text_raw <- read_html(url) %>%
  html_elements("body") %>%
  html_text2()


## 3. Pre-processing and Initial Cleaning
book_tbl <- tibble(text = str_split(book_text_raw, "\\n")[[1]]) %>%
  # Remove any lines that are completely empty or just whitespace
  filter(str_trim(text) != "")


## 4. Isolate and Clean the Narrative Block
start_line <- which(str_detect(book_tbl$text, "TO Sherlock Holmes she is always the woman"))[1]
end_line <- which(str_detect(book_tbl$text, "THE END"))[1]

# To ensure the markers were found, otherwise stop with a clear message.
if (is.na(start_line) || is.na(end_line)) {
  stop("Could not find the start or end markers in the text.")
}

# Slice the tibble to get only the narrative, and then perform detailed cleaning.
narrative_clean <- book_tbl %>%
  slice(start_line:end_line) %>%
  # Use anchored filters to safely remove metadata
  filter(!str_detect(text, "^First published in The Strand Magazine")) %>%
  filter(!str_detect(text, "^First book appearance in")) %>%
  filter(!str_detect(text, "^PART (I|II|III)$")) %>%
  filter(!str_detect(text, "^THE END$"))


## 5. Separate Text by Story 
# This looks for a line starting with Roman numerals (I, V, X) followed by a period.
story_title_regex <- "^[IVX]+\\s*\\.\\s*"

stories_separated <- narrative_clean %>%
  mutate(
    # cumsum() creates the story ID. The first story gets ID 0.
    story_num = cumsum(str_detect(text, story_title_regex))
  ) %>%
  # Add 1 to make the story numbers 1-12 instead of 0-11.
  mutate(story_num = story_num + 1) %>%
  # Remove the title lines themselves.
  filter(!str_detect(text, story_title_regex))


## 6. Final Aggregation
sherlock_holmes_clean <- stories_separated %>%
  group_by(story_num) %>%
  summarise(
    text = paste(text, collapse = " ")
  ) %>%
  ungroup() %>%
  mutate(
    # Standardize to lowercase and clean up all excess whitespace.
    text = str_to_lower(text),
    text = str_replace_all(text, "\\s+", " ") %>% str_trim()
  )


## 7. Verification
cat("Final Output Structure:\n")
print(sherlock_holmes_clean)

cat(paste("\nTotal stories processed:", nrow(sherlock_holmes_clean), "\n\n"))

# Verify that story 2 is correct
cat("Start of Story 2 ('The Red-Headed League'):\n")
cat(str_sub(sherlock_holmes_clean$text[2], 1, 100))

# Verify that the last story is also correct
cat("\n\nStart of Story 12 ('The Copper Beeches'):\n")
cat(str_sub(sherlock_holmes_clean$text[12], 1, 100))

# Final clean data
write_csv(sherlock_holmes_clean, "sherlock_holmes_clean.csv")


# Rule-Based Segmentation for Holmes's Dialogue and Watson's Narration
# Load the clean
sherlock_holmes_clean <- read_csv("sherlock_holmes_clean.csv")

# Combine all 12 stories into a single block of text for processing.
full_text <- paste(sherlock_holmes_clean$text, collapse = " ")

## CORPUS A: EXTRACT HOLMES'S DIALOGUE

## 2. Define the Rule for Holmes's Dialogue
# This regex looks for a quote, followed by an attribution to Holmes.
holmes_identifiers <- "holmes|sherlock holmes|my friend|my companion|he"
verbs <- "said|cried|remarked|answered|murmured|asked|observed|continued|groaned|chuckled|roared|ejaculated|whispered|laughed"

# The regex combines these parts to find a quote followed by the attribution.
# () to create a "capturing group" for the dialogue itself.
holmes_dialogue_regex <- str_glue('(".*?")\\s*,?\\s*(?:{verbs})\\s+(?:{holmes_identifiers})')


## 3. Extract the Dialogue
holmes_dialogue_matches <- str_match_all(full_text, holmes_dialogue_regex)

# The results are in the second column of the match matrix.
holmes_dialogue <- holmes_dialogue_matches[[1]][, 2] %>%
  # Remove the quotation marks from the extracted text
  str_remove_all('"') %>%
  # Paste all the dialogue snippets together into a single corpus.
  paste(collapse = " ") %>%
  str_trim()


## CORPUS B: ISOLATE WATSON'S NARRATION

## 4. Define the Rule for All Dialogue
# This regex simply finds any text enclosed in double quotes.
all_dialogue_regex <- '"[^"]+"'


## 5. Extract the Narration
# Remove all dialogue from the full text, leaving Watson's narration.
watson_narration <- str_remove_all(full_text, all_dialogue_regex) %>%
  # Clean up any extra whitespace left behind.
  str_replace_all("\\s{2,}", " ") %>%
  str_trim()


## 6. Verification and Saving 
cat("--- Sherlock Holmes Dialogue Sample (Rule-Based) ---\n")
cat(str_sub(holmes_dialogue, 1, 500))

cat("\n\n--- Dr. Watson Narration Sample (Rule-Based) ---\n")
cat(str_sub(watson_narration, 1, 500))

# Save the two corpora as separate text files.
write_file(holmes_dialogue, "holmes_dialogue.txt")
write_file(watson_narration, "watson_narration.txt")

cat("\n\nRule-based segmentation complete.\n")


# Analysis: TF-IDF
holmes_text <- read_file("holmes_dialogue.txt")
watson_text <- read_file("watson_narration.txt")

corpora_tbl <- tibble(
  speaker = c("Holmes", "Watson"),
  text = c(holmes_text, watson_text)
)

## 2. Calculate TF-IDF 
# First, we need a tidy format: one word per row, with counts
# We will also remove stop words.
tidy_words <- corpora_tbl %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(speaker, word, sort = TRUE)

# Now, we use bind_tf_idf() to calculate the scores
book_tfidf <- tidy_words %>%
  bind_tf_idf(word, speaker, n) %>%
  arrange(desc(tf_idf))

## 3. Visualize the TF-IDF Results
# Plot of top 15 TF-IDF words for each speaker
ggplot(
  book_tfidf %>%
    group_by(speaker) %>%
    slice_max(tf_idf, n = 15) %>%
    ungroup(),
  aes(x = tf_idf, y = reorder_within(word, tf_idf, speaker), fill = speaker)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~speaker, scales = "free") +
  scale_y_reordered() +
  labs(
    title = "Top Words by TF-IDF: Holmes vs. Watson",
    # subtitle = "Note: This method only highlights words exclusive to one speaker",
    y = "Word",
    x = "TF-IDF Score"
  )




# Sentiment Analysis (Positive/Negative Polarity)
# Overall sentiment score for Holmes and Watson using the AFINN lexicon.
# AFINN lexicon
afinn_lexicon <- get_sentiments("afinn")


story_sentiment <- sherlock_holmes_clean %>%
  # Create a token (word) for each row, keeping the story number
  unnest_tokens(word, text) %>%
  # Sentiment value for each word
  inner_join(afinn_lexicon) %>%
  # Group by story number
  group_by(story_num) %>%
  # Average sentiment score for all rated words in the story
  summarise(average_sentiment = mean(value)) %>%
  ungroup()

print(story_sentiment)


## 3. Plot: Sentiment Arc 
ggplot(story_sentiment, aes(x = story_num, y = average_sentiment)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  # Add a horizontal line at y=0 to represent the neutral point
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = 1:12) + # Ensure all story numbers are shown on the x-axis
  labs(
    title = "Sentiment Arc of 'The Adventures of Sherlock Holmes'",
    subtitle = "Average AFINN sentiment score for each story",
    x = "Story Number",
    y = "Average Sentiment Score (Negative / Positive)"
  ) +
  theme_minimal()



# Tokenize the text, remove stop words, and join with the AFINN lexicon
sentiment_words <- corpora_tbl %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(afinn_lexicon)


## 3. Average Sentiment 
average_sentiment <- sentiment_words %>%
  group_by(speaker) %>%
  summarise(average_score = mean(value))

cat("--- Average Sentiment Scores ---\n")
print(average_sentiment)


## 4. Visualize Top Contributing Words

# Calculate the total contribution of each word (frequency * sentiment value)
word_contributions <- sentiment_words %>%
  count(speaker, word, value, sort = TRUE) %>%
  mutate(contribution = n * value)

# Visualize the top 10 positive and negative words for each speaker
ggplot(
  word_contributions %>%
    mutate(sentiment = if_else(contribution > 0, "Positive", "Negative")) %>%
    group_by(speaker, sentiment) %>%
    slice_max(abs(contribution), n = 10) %>%
    ungroup(),
  aes(x = contribution, y = reorder_within(word, contribution, list(speaker, sentiment)), fill = sentiment)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ speaker + sentiment, scales = "free_y", ncol = 2) +
  scale_y_reordered() +
  labs(
    title = "Top Words Contributing to Sentiment: Holmes vs. Watson",
    subtitle = "Contribution = Frequency of Word × AFINN Score",
    x = "Total Sentiment Contribution",
    y = "Word"
  )

