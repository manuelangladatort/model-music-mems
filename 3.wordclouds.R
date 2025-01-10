#' Wordclouds

# load libraries
library(tidyverse)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(scales)

# data: download data from google drive and add it in "data" folder within working directory
memory_data_umap <- read_rds('data/memory_data_umap.rds')
head(memory_data_umap)

################################################################################
# WORD CLOUDS (TF-IDF)
################################################################################
k = 6
select_cluster = 1

# Add cluster as document ID
tokenized_words <- memory_data_umap %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word") %>%
  count(cluster, word, sort = TRUE)

# Compute TF-IDF across all clusters
tfidf_scores <- tokenized_words %>%
  bind_tf_idf(word, cluster, n)

# Filter for cluster 5
cluster_5_tfidf <- tfidf_scores %>%
  filter(cluster == select_cluster) %>%
  arrange(desc(tf_idf))

# Generate word cloud
# wordcloud(words = cluster_5_tfidf$word, 
#           freq = cluster_5_tfidf$tf_idf, 
#           max.words = 50, 
#           random.order = FALSE, 
#           colors = brewer.pal(8, "Dark2"))


# Normalize TF-IDF scores (optional, for consistent scaling)
cluster_5_tfidf <- cluster_5_tfidf %>%
  mutate(scaled_tfidf = rescale(tf_idf, to = c(1, 100)))

# Generate the word cloud using wordcloud2
wordcloud2(data = cluster_5_tfidf[, c("word", "tf_idf")],
           size = 0.5,               # Control overall size
           shape = "circle",         # Shape of the word cloud
           fontFamily = "Arial",     # Font style
           color = "random-light",   # Light colors
           backgroundColor = "white") # Background color


# Save a word cloud for each cluster
library(webshot)
library(htmlwidgets)

# Loop through each cluster
for (i in 1:k) {
  cluster_tfidf <- tfidf_scores %>%
    filter(cluster == i) %>%
    arrange(desc(tf_idf)) %>%
    mutate(scaled_tfidf = rescale(tf_idf, to = c(1, 100))) 
    # %>% 
    # slice_max(order_by = tf_idf, n = 100) # Limit to top 100 words
  
  # Create the word cloud
  wc <- wordcloud2(data = cluster_tfidf[, c("word", "scaled_tfidf")],
                   size = 0.5,
                   shape = "circle",
                   fontFamily = "Arial",
                   color = brewer.pal(8, "Dark2"),
                   backgroundColor = "white")
  
  # Save the word cloud as an HTML file (temporary step)
  temp_html <- paste0("cluster_", i, "_wordcloud.html")
  htmlwidgets::saveWidget(wc, temp_html, selfcontained = TRUE)
  
  # Convert the HTML to PNG
  png_file <- paste0("cluster_", i, "_wordcloud.png")
  webshot(temp_html, file = png_file, vwidth = 800, vheight = 800)
  
  # Remove the temporary HTML file (optional)
  file.remove(temp_html)
}

