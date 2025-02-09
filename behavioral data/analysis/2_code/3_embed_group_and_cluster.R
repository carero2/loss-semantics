require(tidyverse)
require(embedR)
library(cluster)
source("2_code/_helpers.R")
library(ggplot2)
library(factoextra)
library(wordcloud2)
library(tidytext)
library(viridisLite)

set.seed(123)

df_exp = read_csv("1_data/loss_semantics_clean_spellcheck_normalized.csv")

# Reshape the data from wide to long format
df_exp <- df_exp %>%
  pivot_longer(cols = c(association, situation), 
               names_to = "type", 
               values_to = "word") %>%
  pivot_longer(cols = c(association_clean, situation_clean), 
               names_to = "type_clean", 
               values_to = "word_clean") %>%
  pivot_longer(cols = c(association_clean_normalized, situation_clean_normalized), 
               names_to = "type_clean_normalized", 
               values_to = "word_clean_normalized") %>%
  filter((type == "association" & type_clean == "association_clean" & type_clean_normalized == "association_clean_normalized") |
           (type == "situation" & type_clean == "situation_clean" & type_clean_normalized == "situation_clean_normalized")) %>%
  select(-c(type_clean, type_clean_normalized)) %>%
  arrange(prolific_id, type)

df_papers = read_csv("../../literature review/1_data/selected_papers_normalized.csv")

df_papers <- df_papers %>% separate_rows(normalized, sep = ", ") %>%
  mutate(normalized = na_if(normalized, "none"))


er_set_tokens(huggingface = , hard = TRUE)

read = T

if (read) {
  reticulate::use_virtualenv("pacmap")
  embed = readRDS("1_data/loss_semantics_embed_papers_exp_0702.RDS")
  groups = readRDS("1_data/loss_semantics_groups_95_papers_exp_0702.RDS")
  words_df = read_csv("1_data/WordsAndClusters_PapersAndExp.csv")
} else{
  elements = (c(df_exp$word_clean_normalized, df_papers$normalized))
  embed = er_embed(elements, api = "huggingface", model = "WhereIsAI/UAE-Large-V1")
  saveRDS(embed, "1_data/loss_semantics_embed_papers_exp_0702.RDS")
  mem.maxVSize(vsize = 100000)
  groups = embed %>% er_group(method = "fuzzy", threshold = .95)
  saveRDS(groups, "1_data/loss_semantics_groups_95_papers_exp_0702.RDS")
}

# umap dimensional reduction
proj = groups |> er_project(method = "umap", k = 2)

# elbow method to select k clusters
fviz_nbclust(proj, FUNcluster = hcut, method = "wss", k.max = 20) + 
  labs(subtitle = "Elbow Method for Optimal k")


# Also silhouette method
sil_scores <- function(k, object) {
  # Cluster assignment
  cluster_cut <- cutree(hcut(dist(object)), k)
  
  # Calculate silhouette scores
  ss <- silhouette(cluster_cut, dist(object))
  
  # Return the average silhouette width
  mean(ss[, 3])
}


# Range of k values to test
k_values <- 2:22
avg_sil_scores <- sapply(X=k_values, object=proj, FUN=sil_scores)

# Plot silhouette scores for different k
plot(k_values, avg_sil_scores, type = "b", pch = 19,
     xlab = "Number of clusters", ylab = "Average Silhouette Score", 
     main = "Silhouette Method for Optimal k", xaxt = "n")
axis(1, at = k_values, las = 2)

# 8 clusters
clust = proj |> er_cluster(method = "hclust", k = 9, verbose = T)
loss = er_frame(clust)

loss = loss %>% 
  mutate(most_frequent = sapply(group_texts, function(x) x %>% str_to_lower() %>% table() %>% sort(decreasing = T) %>% `[`(1) %>% names()))

clusters_dict = split(loss$most_frequent, loss$cluster) %>% sapply(function(x) paste0(x, collapse=", "))


# Save the reasoning of the clusters
output_file <- "1_data/reasoning_clusters.txt"
file_conn <- file(output_file, open = "a")

responses = c()
for(i in 1:length(clusters_dict)){
  
  words = clusters_dict[i]
  system = "You are a helpful assistant who assigns a single specific (non-generic) label to the following list of words and phrases capturing an aspect loss:\n\n{words}"
  instruct = glue::glue("In the context of type of loss, what label specifically (i.e., avoiding using general terms) captures the meaning of the following words and phrases in relation to others?\nBriefly reason through your answer providing some cases from the label. Avoid using 'Loss' or related terms (e.g., 'Losing') as solely label.\nAt the very end, return a single label in the following format:\nAnswer=[label]. Words and phrases:\n\n{words}")
  
  responses[i] = single_use_llama(system = system, instruct = instruct, token=)
  
  # Print the response in the console
  cat(i, "\n", responses[i], "\n\n")
  
  # Write to the file
  writeLines(paste(i, "\n", responses[i], "\n\n"), file_conn)
}
close(file_conn)


loss = loss %>% 
  mutate(cluster_name = case_when(
    cluster == 1 ~ "Competitive Failure",
    cluster == 2 ~ "Financial",
    cluster == 3 ~ "Missed Opportunities",
    cluster == 4 ~ "Impairment",
    cluster == 5 ~ "Emotional Distress",
    cluster == 6 ~ "Health-Related Losses",
    cluster == 7 ~ "Material Losses",
    cluster == 8 ~ "Deterioration",
    cluster == 9 ~ "Pet Bereavement",
    TRUE ~ NA))


words_df <- loss %>%
  unnest(group_texts) %>%
  rename(word = group_texts) %>%
  group_by(word) %>%
  summarise(text=unique(text),
            cluster=unique(cluster),
            cluster_name=unique(cluster_name),
            frequency=n(),
            group_dim_1 = unique(dim_1),
            group_dim_2 = unique(dim_2)) %>%
  mutate(orig = case_when(word %in% df_papers$normalized & word %in% df_exp$word_clean_normalized ~ "Papers, Experiment",
                          word %in% df_papers$normalized ~ "Papers",
                          TRUE ~ "Experiment")) %>%
  separate_rows(orig, sep = ", ")

words_df2 <- words_df %>% left_join(df_exp[, c("word_clean_normalized", "type")], by=c("word" = "word_clean_normalized"))%>%
  mutate(type = ifelse(orig == "Papers", NA, type))

write_csv(words_df, "1_data/WordsAndClusters_PapersAndExp.csv")
write_csv(words_df2, "1_data/WordsAndClusters_PapersAndExp2.csv")
