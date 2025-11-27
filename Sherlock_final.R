## Data Cleaning for "The Adventures of Sherlock Holmes"
## Load Libraries
pacman::p_load(tidyverse, rvest, stringr, tidytext, textdata, quanteda, quanteda.textstats, scales)


## Scrape Text from URL
url <- "https://gutenberg.net.au/ebooks/c00011.html"
book_text_raw <- read_html(url) %>%
  html_elements("body") %>%
  html_text2()


## Pre-processing and Initial Cleaning
book_tbl <- tibble(text = str_split(book_text_raw, "\\n")[[1]]) %>%
  # Remove any lines that are completely empty or just whitespace
  filter(str_trim(text) != "")


## Isolate and Clean the Narrative Block
start_line <- which(str_detect(book_tbl$text, "TO Sherlock Holmes she is always the woman"))[1]
end_line <- which(str_detect(book_tbl$text, "THE END"))[1]

# To ensure the markers were found, otherwise stop with a clear message.
if (is.na(start_line) || is.na(end_line)) {
  stop("Could not find the start or end markers in the text.")
}

# Slice the tibble to get only the narrative and then perform detailed cleaning.
narrative_clean <- book_tbl %>%
  slice(start_line:end_line) %>%
  # Use anchored filters to safely remove metadata
  filter(!str_detect(text, "^First published in The Strand Magazine")) %>%
  filter(!str_detect(text, "^First book appearance in")) %>%
  filter(!str_detect(text, "^PART (I|II|III)$")) %>%
  filter(!str_detect(text, "^THE END$"))


## Separate Text by Story 
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


## Final Aggregation
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


## Verification
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


# Text Segmentation (Watson's Narration vs. Holmes's Dialogue)

# Combine all 12 stories into a single block of text for processing.
full_text <- paste(sherlock_holmes_clean$text, collapse = " ")
full_text <- str_replace_all(full_text, "([\\.\\?!])([a-zA-Z])", "\\1 \\2")
full_text <- str_replace_all(full_text, "\\.", " ")


## CORPUS A: EXTRACT HOLMES'S DIALOGUE

# Define the Rule for Holmes's Dialogue
# DEFINE IDENTIFIERS (This part is correct)
verbs <- "said|cried|remarked|answered|murmured|asked|observed|continued|groaned|chuckled|roared|ejaculated|whispered|laughed|broke in|interrupted|thought|returned|inquired|ordered|agreed|suggested|ventured|smiling|reasoned"

# DEFINE THE NEW REGEX
holmes_identifiers <- "holmes|sherlock holmes|sherlock|my friend|my companion|he|we"

# 3. Define Regex Patterns
# Pattern A: Attribution AFTER
regex_after <- str_glue('(?i)^\\s*,?\\s*(?:{verbs})\\s+(?:{holmes_identifiers})')

# Pattern B: Attribution BEFORE
regex_before <- str_glue('(?i)(?:{holmes_identifiers})\\s+(?:{verbs}),?\\s*$')

# 4. Run the Segmentation Logic
speaker_identified <- sherlock_holmes_clean %>%
  # Normalize quotes
  mutate(text = str_replace_all(text, "[“”]", '"')) %>%
  # Add space to prevent parity flip
  mutate(text = paste(" ", text)) %>%
  separate_rows(text, sep = '"') %>%
  filter(str_trim(text) != "") %>%
  group_by(story_num) %>%
  mutate(chunk_type = if_else(row_number() %% 2 == 1, "narration", "dialogue")) %>%
  mutate(
    speaker = case_when(
      chunk_type == "dialogue" & str_detect(lead(text), regex_after) ~ "Holmes",
      chunk_type == "dialogue" & str_detect(lag(text), regex_before) ~ "Holmes",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

# Two final corpora.

# Corpus A: Holmes's Dialogue
holmes_dialogue <- speaker_identified %>%
  filter(speaker == "Holmes") %>%
  pull(text) %>% # pull() extracts the column as a vector
  paste(collapse = " ")

# Corpus B: Watson's Narration
watson_narration <- speaker_identified %>%
  filter(chunk_type == "narration") %>%
  pull(text) %>%
  paste(collapse = " ")


## Verification and Saving 

cat("--- Sherlock Holmes Dialogue Sample ---\n")
cat(str_sub(holmes_dialogue, 1, 500))

cat("\n\n--- Dr. Watson Narration Sample ---\n")
cat(str_sub(watson_narration, 1, 500))

# Save the two corpora as separate text files for the final analysis steps.
write_file(holmes_dialogue, "holmes_dialogue.txt")
write_file(watson_narration, "watson_narration.txt")

cat("\n\nSegmentation complete. Two files created: 'holmes_dialogue.txt' and 'watson_narration.txt'\n")


## Keyness and Sentiment Analysis
# This script compares the language of Holmes and Watson to identify
# characteristic words and emotional tone.

## NRC lexicon
textdata::lexicon_nrc()

holmes_text <- read_file("holmes_dialogue.txt")
watson_text <- read_file("watson_narration.txt")

# Combine into a single data frame for analysis
corpora_tbl <- tibble(
  speaker = c("Holmes", "Watson"),
  text = c(holmes_text, watson_text)
)


## Keyness Analysis
# Quanteda requires a special 'corpus' object.
corp <- corpus(corpora_tbl, text_field = "text", docid_field = "speaker")

# Now, we create a document-feature matrix (DFM), which is a table of word counts.
# Remove stop words and punctuation.
dfm_words <- corp %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  dfm()

# Find Words Characteristic
keyness_holmes <- textstat_keyness(dfm_words, target = "Holmes")
keyness_watson <- textstat_keyness(dfm_words, target = "Watson")

# Visualize the top 15 key words for each speaker
keyness_plot <- bind_rows(
  keyness_holmes %>% head(15) %>% mutate(speaker = "Holmes"),
  keyness_watson %>% head(15) %>% mutate(speaker = "Watson")
)

# Plot the results
ggplot(keyness_plot, aes(x = chi2, y = reorder_within(feature, chi2, speaker), fill = speaker)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~speaker, scales = "free") +
  scale_y_reordered() +
  theme_bw() +
  labs(
    title = "Most Characteristic Words: Holmes vs. Watson",
    subtitle = "Calculated using Chi-squared Keyness",
    y = "Word",
    x = "Chi-squared Value (Higher is more characteristic)"
  )


## Sentiment Analysis
# NRC lexicon for its emotional categories.
nrc_lexicon <- get_sentiments("nrc")

# Tokenize the corpora and join with the sentiment lexicon.
sentiment_analysis <- corpora_tbl %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_lexicon) %>%
  # Count how many words fall into each sentiment category for each speaker
  count(speaker, sentiment) %>%
  # Calculate the percentage for a fair comparison
  group_by(speaker) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()

# Visualize the emotional composition of their language
ggplot(sentiment_analysis, aes(x = sentiment, y = percent, fill = speaker)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  theme_bw() +
  labs(
    title = "Emotional Composition of Holmes's vs. Watson's Language",
    x = "Emotion",
    y = "Percentage of Words",
    fill = "Speaker"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Sentiment Analysis per Story

# Same logic as original sentiment analysis,
# but group by 'story_num' instead of 'speaker'.
story_sentiments <- sherlock_holmes_clean %>%
  # Tokenize the text of each story into words
  unnest_tokens(word, text) %>%
  
  # Join with the NRC lexicon
  inner_join(nrc_lexicon, by = "word") %>%
  
  # Count how many words fall into each sentiment for each story
  count(story_num, sentiment) %>%
  
  # Calculate percentages
  group_by(story_num) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  
  # Ensure stories are plotted in the correct order
  mutate(story_num = factor(story_num, levels = 1:12))

## Visualize the Results
# Show all 12 stories at once.
ggplot(story_sentiments, aes(x = sentiment, y = percent, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  
  # Create a 4x3 grid of charts for each story
  facet_wrap(~ story_num, ncol = 4) +
  
  # Format the y-axis as percentages
  scale_y_continuous(labels = percent_format()) +
  theme_bw() +
  labs(
    title = "Emotional Composition of Each Story",
    subtitle = "Based on the NRC Lexicon for all words in each story's text",
    x = "Emotion",
    y = "Percentage of Emotional Words"
  ) +
  
  # Rotate x-axis labels for readability
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# This tibble should have 12 rows, one for each story.
tryCatch({
  sherlock_holmes_clean <- read_csv("sherlock_holmes_clean.csv")
}, error = function(e) {
  stop("Error: 'sherlock_holmes_clean.csv' not found. 
       Please run the first data cleaning script to create this file.")
})


## Create Story-Level Corpora

# Create Holmes's corpus, grouped by story
holmes_by_story <- speaker_identified %>%
  filter(speaker == "Holmes") %>%
  group_by(story_num) %>%
  summarise(text = paste(text, collapse = " ")) %>%
  ungroup() %>%
  mutate(speaker = "Holmes") # Add speaker label

# Create Watson's corpus, grouped by story
watson_by_story <- speaker_identified %>%
  filter(chunk_type == "narration") %>%
  group_by(story_num) %>%
  summarise(text = paste(text, collapse = " ")) %>%
  ungroup() %>%
  mutate(speaker = "Watson") # Add speaker label

# Combine
corpora_by_story_tbl <- bind_rows(holmes_by_story, watson_by_story)


## Run Sentiment Analysis for each story

sentiment_by_story <- corpora_by_story_tbl %>%
  # Tokenize
  unnest_tokens(word, text) %>%
  # Join with NRC lexicon
  inner_join(nrc_lexicon, by = "word") %>%
  
  # Calculate Total Emotional Words per Story/Speaker 
  group_by(story_num, speaker) %>%
  mutate(total_emotional_words = n()) %>%
  ungroup() %>%
  
  # Filter Out Low Data (The Fix)
  # Only keep stories 7 emotional words.
  filter(total_emotional_words >= 7) %>%
  
  # Count and Calculate Percentages
  count(story_num, speaker, sentiment) %>%
  group_by(story_num, speaker) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()

cat("Analysis Complete. Sentiment data by story:\n")
print(sentiment_by_story)


## Visualize the Results

# Plot 1: Stacked Bar Chart (Good for composition)
# Shows the emotional "fingerprint" of each story,

ggplot(sentiment_by_story, aes(x = story_num, y = percent, fill = sentiment)) +
  geom_col(position = "stack") +
  # Facet by speaker to get two separate plots
  facet_wrap(~ speaker, ncol = 1) + 
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = 1:12) + # Ensure all 12 stories are on the x-axis
  labs(
    title = "Emotional Composition Across 12 Stories: Holmes vs. Watson",
    subtitle = "Sentiment percentage calculated within each story and speaker",
    x = "Story Number (1-12)",
    y = "Percentage of Emotional Words",
    fill = "Emotion"
  ) +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold"))


# Plot 2: Line Chart
# Filter for just the positive/negative sentiments for a cleaner plot
pos_neg_sentiment <- sentiment_by_story %>%
  filter(sentiment %in% c("positive", "negative"))

ggplot(pos_neg_sentiment, aes(x = story_num, y = percent, color = speaker, group = speaker)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  # Facet by sentiment to get separate plots for "positive" and "negative"
  facet_wrap(~ sentiment) + 
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Positive vs. Negative Sentiment Across 12 Stories",
    subtitle = "Comparing Holmes's dialogue to Watson's narration",
    x = "Story Number (1-12)",
    y = "Percentage of Emotional Words",
    color = "Speaker"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 12, face = "bold"))


## Granular Emotional Arc

# Plot 1: The Specific Hypothesis (Holmes's Anticipation vs. Watson's Fear)
# Filter the data to isolate only the specific emotions
hypothesis_data <- sentiment_by_story %>%
  filter(
    (speaker == "Holmes" & sentiment == "anticipation") |
      (speaker == "Watson" & sentiment == "fear")
  ) %>%
  # Combined label for the legend
  mutate(trace_label = paste(speaker, "-", sentiment))

ggplot(hypothesis_data, aes(x = story_num, y = percent, color = trace_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "The Detective vs. The Narrator: Emotional Arcs",
    subtitle = "Tracking Holmes's Anticipation against Watson's Fear across 12 Stories",
    x = "Story Number",
    y = "Percentage of Words",
    color = "Character Emotion"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14))


# Plot 2: The Full Granular View
core_emotions_only <- sentiment_by_story %>%
  filter(!sentiment %in% c("positive", "negative"))

ggplot(core_emotions_only, aes(x = story_num, y = percent, color = speaker)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  # Facet wrap creates 8 mini-charts, one for each emotion
  facet_wrap(~ sentiment, scales = "free_y", ncol = 4) +
  scale_x_continuous(breaks = c(1, 4, 8, 12)) + # Minimal x-axis labels to reduce clutter
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Granular Emotional Arcs: The Complete Picture",
    subtitle = "Comparing Holmes and Watson across all 8 NRC emotions",
    x = "Story Number",
    y = "Frequency (%)",
    color = "Speaker"
  ) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank()
  )

ggplot(core_emotions_only, aes(x = story_num, y = percent, color = speaker, fill = speaker)) +
  # se = FALSE removes the gray confidence shadow to keep it clean
  geom_smooth(method = "loess", se = FALSE, span = 0.8, linewidth = 1.2) +
  geom_point(size = 1.2, alpha = 0.3) + # Faint dots to show real data
  facet_wrap(~ sentiment, scales = "free_y", ncol = 4) +
  scale_x_continuous(breaks = c(1, 4, 8, 12)) + # Minimal x-axis labels to reduce clutter
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Emotional Trends: Holmes vs. Watson",
    subtitle = "Trend lines (Loess smoothing) highlighting general narrative arcs",
    y = "Frequency Percent"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank()
  )
