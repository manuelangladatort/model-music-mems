#' Extracting word embedding vectors
#' Using OpenAI's most advanced version of the word embedding model (text-embedding-ada-002)

# load libraries
library(readxl)
require(openai) # wrapper for OpenAI API access
require(quanteda) # text processing


################################################################################
# SETUP
################################################################################
# your open ai API key should be stored in the ".Renviron" file located normally in the root directory
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
# print(OPENAI_API_KEY) # it should return your open ai key

# define the open ai model to use
open_ai_model <- 'text-embedding-ada-002' # state-of-the-art embeding model: https://openai.com/index/new-and-improved-embedding-model/


# data
data_path <- "data/FINAL_MEAMdataforanalysis.xlsx" # add here your local path to the data
data <- read_excel(data_path, sheet = "in")

# TODO: fix error
filtered_data <- data %>% filter(!is.na(description)) # filter out NAs

# memory descriptions
descriptions <- filtered_data$description
length(descriptions) # 1,454 descriptions


################################################################################
# EXTRACT WORD EMBEDDINGS
################################################################################
# initialize storage for embeddings
embedding_storage <- list()

# loop over each description and extract the embedding
for (i in 1:length(descriptions)) {
  tryCatch({
    embed <- openai::create_embedding(
      model = open_ai_model,
      input = descriptions[i]
    )
    embedding_storage[[i]] <- tibble(description = descriptions[i], embedding = embed$data$embedding)
    
    print(sprintf('Processed %d out of %d descriptions', i, length(descriptions)))
  }, error = function(e) {
    message(sprintf('Error processing description %d: %s', i, e$message))
  })
}

# combine all embeddings 
embedding_output <- bind_rows(embedding_storage)

# save embeddings only
write_rds(embedding_output, "data/memory_embeddings.rds")


# add to original data
memory_darta_with_embeddings <-  filtered_data %>% select(pptnum:important)

memory_darta_with_embeddings$input_embeddings <- NA  
memory_darta_with_embeddings$embeddings <- NA 

# loop through the embedding storage and fill embeddings for valid rows
for (i in seq_along(embedding_storage)) {
  if (!is.null(embedding_storage[[i]])) {
    # memory_darta_with_embeddings$embeddings[i] <- list(embedding_storage[[i]]) 
    memory_darta_with_embeddings$input_embeddings[i] <- embedding_storage[[i]][[1]]
    memory_darta_with_embeddings$embeddings[i] <- embedding_storage[[i]][[2]]  
  }
}

memory_darta_with_embeddings <- memory_darta_with_embeddings %>% filter(!is.na(embeddings)) # filter out NAs


# save
write_rds(memory_darta_with_embeddings, "data/memory_data_with_embeddings.rds")


