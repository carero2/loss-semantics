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



er_set_tokens(huggingface = "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", hard = TRUE)

read = T

if (read) {
  reticulate::use_virtualenv("pacmap")
  embed = readRDS("loss_semantics_embed_papers_exp.RDS")
  groups = readRDS("loss_semantics_groups_97_papers_exp.RDS")
} else{
  elements = (c(df_exp$word_clean_normalized, df_papers$normalized))
  embed = er_embed(elements, api = "huggingface", model = "WhereIsAI/UAE-Large-V1")
  saveRDS(embed, "loss_semantics_embed_papers_exp.RDS")
  groups = embed %>% er_group(method = "fuzzy", threshold = .97)
  saveRDS(groups, "loss_semantics_groups_97_papers_exp.RDS")
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
clust = proj |> er_cluster(method = "hclust", k = 8, verbose = T)
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
  system = glue::glue("You are a helpful assistant who assigns a single specific (non-generic) label to the following list of words and phrases capturing an aspect loss:\n\n{words}")
  instruct = "What label specifically captures the meaning of the words and phrases in relation to others on other aspects of loss. Briefly reason through your answer avoiding using 'Loss' as solely label and providing some cases from the label and a finally self-explanatory label. At the very end, return a single label in the following format: Answer=[label]."
  
  responses[i] = single_use_llama(system = system, instruct = instruct)
  
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
    cluster == 3 ~ "Deprivation",
    cluster == 4 ~ "Emotional Turmoil",
    cluster == 5 ~ "Career and Life Setbacks",
    cluster == 6 ~ "Detriment",
    cluster == 7 ~ "Separation",
    cluster == 8 ~ "Separation",
    TRUE ~ NA))


words_df = loss %>%
  unnest(group_texts) %>%
  rename(word = group_texts) %>%
  group_by(word) %>%
  summarise(text=unique(text),
            cluster=unique(cluster),
            cluster_name=unique(cluster_name),
            frequency=n(),
            group_dim_1 = unique(dim_1),
            group_dim_2 = unique(dim_2)) %>%
  mutate(orig = case_when(word %in% df_papers$normalized ~ "Papers",
                          word %in% df_papers$normalized & word %in% df_exp$word_clean_normalized ~ "Both",
                          TRUE ~ "Experiment"))

write_csv(words_df, "1_data/WordsAndClusters_PapersAndExp.csv")

# PLOTS


png("3_figures/embeddings_both.png", width = 1080, height = 720)
ggplot(words_df,
       aes(x=group_dim_1, y=group_dim_2, colour = factor(cluster_name))) +
  geom_point(alpha = 0.5, size = 5) +
  scale_color_viridis_d() +
  labs(colour="Clusters",
       title="Embeddings papers and experiment responses",
       x="Dimension 1",
       y="Dimension 2")+
  theme_bw(base_size=25)
dev.off()

png("3_figures/embeddings_papers.png", width = 1080, height = 720)
ggplot() +
  geom_point(data = words_df[words_df$orig == "Papers",],
             aes(x = group_dim_1, y = group_dim_2, colour = factor(cluster_name)),
             alpha = 0.5, size = 5) +
  geom_point(data = words_df[words_df$orig == "Experiment",],
             aes(x = group_dim_1, y = group_dim_2),
             colour = "grey",
             alpha = 0.03, size = 5) +
  scale_color_viridis_d() +
  labs(colour="Clusters",
       title="Embeddings papers",
       x="Dimension 1",
       y="Dimension 2")+
  theme_bw(base_size=25)
dev.off()


png("3_figures/embeddings_exp.png", width = 1080, height = 720)
ggplot() +
  geom_point(data = words_df[words_df$orig == "Experiment",],
             aes(x = group_dim_1, y = group_dim_2, colour = factor(cluster_name)),
             alpha = 0.5, size = 5) +
  geom_point(data = words_df[words_df$orig == "Papers",],
             aes(x = group_dim_1, y = group_dim_2),
             colour = "grey", 
             alpha = 0.03, size = 5) +
  scale_color_viridis_d() +
  labs(colour="Clusters",
       title="Embeddings experiment responses",
       x="Dimension 1",
       y="Dimension 2")+
  theme_bw(base_size=25)
dev.off()

words_df %>% group_by(orig, cluster_name) %>% summarise(n = n())


summary = df_exp %>%
  left_join(words_df, by = c("word_clean_normalized" = "word"))%>% 
  group_by(cluster_name, type) %>%
  summarise(n = n()) %>%
  na.omit()



summary$type <- plyr::revalue(summary$type, c("association" = "Association", "situation" = "Situation"))

width = 0.4
png("3_figures/exp_situations_associations.png", width = 1080, height = 550)
ggplot(summary, aes(x = as.factor(cluster_name))) +
  geom_col(data = subset(summary, type == "Association"), 
           aes(y = -n, fill = 'Association'), width = width) +
  geom_col(data = subset(summary, type == "Situation"), 
           aes(y = n, fill = 'Situation'), width = width) +
  coord_flip() +
  scale_fill_manual(values = c(Association = "#440154", Situation = "#FDE725"))+
  geom_text(aes(y = -20, label = cluster_name), size = 8, vjust = -1)+
  theme_bw(base_size=24)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.title=element_blank())+
  labs(y="Frequency",
       title = "Number of situations and associations per cluster")+
  scale_y_continuous(labels = abs)
dev.off()



# Cluster only papers
words_df_papers = words_df[words_df$orig == "Papers",]

# elbow method to select k clusters
fviz_nbclust(words_df_papers[c("group_dim_1", "group_dim_2")],
             FUNcluster = hcut, method = "wss", k.max = 20) + 
  labs(subtitle = "Elbow Method for Optimal k")


# Range of k values to test
k_values <- 2:22
avg_sil_scores <- sapply(X=k_values,
                         object=words_df_papers[c("group_dim_1", "group_dim_2")],
                         FUN=sil_scores)

# Plot silhouette scores for different k
plot(k_values, avg_sil_scores, type = "b", pch = 19,
     xlab = "Number of clusters", ylab = "Average Silhouette Score", 
     main = "Silhouette Method for Optimal k", xaxt = "n")
axis(1, at = k_values, las = 2)

# 4 clusters

hclust_result <- hcut(words_df_papers[c("group_dim_1", "group_dim_2")],
                      k = 10, stand = TRUE)

fviz_cluster(hclust_result, geom = "point")

words_df_papers$cluster_papers <- as.factor(hclust_result$cluster)


loss_papers = words_df_papers %>% group_by(text) %>%
  slice(which.max(frequency)) %>%
  ungroup()

clusters_papers_dict = split(loss_papers$word, loss_papers$cluster_papers) %>% sapply(function(x) paste0(x, collapse=", "))

# Save the reasoning of the clusters
output_file <- "1_data/reasoning_clusters_papers.txt"
file_conn <- file(output_file, open = "a")

responses = c()
for(i in 1:length(clusters_papers_dict)){
  
  words = clusters_papers_dict[i]
  system = glue::glue("You are a helpful assistant who assigns a single specific label to the following list phrases that capture the aspect of loss in a scientific paper:\n\n{words}")
  instruct = "What label specifically captures the meaning of all these phrases in the context of scientific papers about losses? Briefly reason through your answer and provide a self-explanatory label that represent most of the phrases and explain the types of losses that contain. At the very end, return a single label in the following format: Answer=[label]."
  
  responses[i] = single_use_llama(system = system, instruct = instruct)
  
  # Print the response in the console
  cat(i, "\n", responses[i], "\n\n")
  
  # Write to the file
  writeLines(paste(i, "\n", responses[i], "\n\n"), file_conn)
}
close(file_conn)

png("3_figures/embeddings_papers.png", width = 1080, height = 720)
ggplot(words_df_papers,
       aes(x = group_dim_1, y = group_dim_2, color = as.factor(cluster_papers))) +
  geom_point() +
  scale_color_viridis_d() +
  labs(colour="Clusters",
       title="Hierarchical Clustering for papers",
       x="Dimension 1",
       y="Dimension 2")+
  theme_bw(base_size=25)
dev.off()



# Get the number of unique clusters
n_clusters <- length(unique(words_df$cluster_name))

# Extract viridis colors for clusters
viridis_colors <- viridis(n_clusters)

# Create a color mapping for each cluster
color_mapping <- setNames(viridis_colors,
                          levels(as.factor(words_df$cluster_name)))

# Prepare the word data
wordcloud_papers <- words_df[words_df$orig == "Papers",] %>%
  rename(text_column = word) %>%
  unnest_tokens(word, text_column) %>%   # Tokenize the text into words
  filter(!word %in% stop_words$word) %>% # Remove stopwords
  mutate(word = gsub("[[:punct:]]", "", word)) %>% # Remove punctuation
  count(cluster_name, word, sort = TRUE) %>% # Count word frequencies
  mutate(color = color_mapping[as.factor(cluster_name)]) %>%
  filter(!word %in% c("losses", "loss", "lost", "losing", "lose"))


# Create the wordcloud with the mapped colors
wordcloud2(data = wordcloud_papers %>% select(word, n), rotateRatio = 0,
           color = unname(wordcloud_papers$color), fontFamily = "Arial",
           ellipticity = 0.9)


# Wordcloud exp
wordcloud_exp <- words_df[words_df$orig == "Experiment",] %>%
  rename(text_column = word) %>%
  unnest_tokens(word, text_column) %>%   # Tokenize the text into words
  filter(!word %in% stop_words$word) %>% # Remove stopwords
  mutate(word = gsub("[[:punct:]]", "", word)) %>% # Remove punctuation
  count(cluster_name, word, sort = TRUE) %>%  # Count word frequencies
  mutate(color = color_mapping[as.factor(cluster_name)]) %>%
  filter(!word %in% c("losses", "loss", "lost", "losing", "lose"))

wordcloud2(data = wordcloud_exp %>% select(word, n), rotateRatio = 0,
           color = unname(wordcloud_exp$color), fontFamily = "Arial",
           ellipticity = 0.9)
