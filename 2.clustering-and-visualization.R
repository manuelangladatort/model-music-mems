#' Perform dimensionality reduction using UMAP, clustering, and visualization
#' Word embedding vectors are extracted from LLM (extraction_word_embedding.R)

# load libraries
library(umap)         
library(cluster)
library(factoextra)
library(tidyverse)


################################################################################
# SETUP
################################################################################
# data: download data from google drive and add it in "data" folder within working directory
memory_data <- read_rds('data/memory_data_with_embeddings.rds')
head(memory_data)

# convert the embedding column from lists to a matrix
embeddings <- do.call(rbind, memory_data$embeddings)
dim(embeddings)  # check structure


# set random seed reproducibility
set.seed(2025) 


# set up config for UMAP 
umap_config <- umap.defaults
umap_config$random_state <- 2025  # Set the random seed for UMAP
umap_config$n_neighbors <- 10  # Adjust for local/global balance
umap_config$min_dist <- 0.1   # Adjust for compactness of clusters
umap_config$n_components <- 2 # Reduce to 2 dimensions for clustering


################################################################################
# UMAP (to reduce large number of emedding features)
################################################################################
# Step 1: run UMAP for dimensional reduction
umap_res <- umap(embeddings, config = umap_config)
umap_data <- as.data.frame(umap_res$layout)
colnames(umap_data) <- c("UMAP1", "UMAP2")
head(umap_data)


################################################################################
# DETERMINE OPTIMAL NUMBER OF CLUSTERS
################################################################################
# Use silhouette score to find optimal clusters
sil_scores <- numeric()
for (k in 2:15) {
  cluster <- kmeans(umap_data, centers = k, nstart = 50)
  sil <- silhouette(cluster$cluster, dist(umap_data))
  sil_scores[k] <- mean(sil[, 3])
}

# Plot silhouette scores
plot(2:15, sil_scores[2:15], type = "b", pch = 19, xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score", main = "Silhouette Method")

# Choose the best k (e.g., highest silhouette score)
best_k <- which.max(sil_scores)

paste0("bests k = ", best_k,"; sil score = ", round(sil_scores[[best_k]],2)) 


################################################################################
# CLUSTERING
################################################################################

# Step 2: run clustering

k <- 6 # 13 and 6 are very close to 0.4: I  choose 6


# run k-means clustering
cluster <- kmeans(umap_data, centers = k, nstart = 50)
umap_data$cluster <- as.factor(cluster$cluster)

# validate
silhouette_score <- silhouette(cluster$cluster, dist(umap_data[, c("UMAP1", "UMAP2")]))
mean_silhouette_score <- mean(silhouette_score[, 3])
mean_silhouette_score # 0.4 


# add to original data
memory_data_umap <- memory_data %>% select(description)
memory_data_umap$UMAP1 <- umap_data$UMAP1
memory_data_umap$UMAP2 <- umap_data$UMAP2
memory_data_umap$cluster <- umap_data$cluster

# save 
write_rds(memory_data_umap, 'data/memory_data_umap.rds')

# Step 3: visualize
ggplot(memory_data_umap, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(size = 1, alpha = 0.7) +
  labs(
    title = "Semantic Embedding Space",
    x = "UMAP Dimension 1",
    y = "UMAP Dimension 2",
    color = "Cluster"
  ) +
  theme_minimal()

ggsave("figures/semantic_embedding_space.png", 
       width = 8, height = 6, dpi = 300,
       bg = "white")


# interactive visualization with Plotly
library(plotly)

p <- ggplot(memory_data_umap, aes(x = UMAP1, y = UMAP2, color = cluster, text = description)) +
  geom_point(size = 1, alpha = 0.7) +
  labs(
    title = "Interactive Semantic Embedding Space",
    x = "UMAP Dimension 1",
    y = "UMAP Dimension 2",
    color = "Cluster"
  ) +
  theme_minimal()

# Convert ggplot to plotly for interactivity
interactive_plot <- ggplotly(p, tooltip = "text")

interactive_plot

