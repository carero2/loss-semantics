library(scico)
library(dplyr)
library(ggplot2)

# PLOTS

words_df <- read_csv("1_data/WordsAndClusters_PapersAndExp.csv")
words_df2  <- read_csv("1_data/WordsAndClusters_PapersAndExp.csv")


png("3_figures/embeddings_clusters_papers.png", width = 1080, height = 720)
ggplot() +
  geom_point(data = words_df[words_df$orig == "Papers",],
             aes(x = group_dim_1, y = group_dim_2, fill = factor(cluster_name)),
             colour="black",pch=21,
             alpha = 0.5, size = 5, stroke = 0.1) +
  scale_colour_scico_d() +
  guides(colour = guide_legend(override.aes = list(alpha = 1)),
         fill = guide_legend(title = "Clusters"))+
  labs(colour="Clusters",
       title="Embeddings literature with clusters",
       x="Dimension 1",
       y="Dimension 2")+
  theme_bw(base_size=25)
dev.off()

png("3_figures/embeddings_clusters_exp_with_papers.png", width = 1080, height = 720)
ggplot() +
  geom_point(data = words_df[words_df$orig == "Experiment",],
             aes(x = group_dim_1, y = group_dim_2, fill = factor(cluster_name)),
             colour = "black", pch=21,
             alpha = 0.5, size = 5, stroke = 0.2) +
  geom_point(data = words_df[words_df$orig == "Papers",],
             aes(x = group_dim_1, y = group_dim_2),
             colour="grey",
             alpha = 0.3, size = 5) +
  scale_colour_scico_d() +
  guides(colour = guide_legend(override.aes = list(alpha = 1)),
         fill = guide_legend(title = "Clusters"))+
  labs(colour="Clusters",
       title="Embeddings survey with clusters",
       x="Dimension 1",
       y="Dimension 2")+
  theme_bw(base_size=25)
dev.off()




df_papers2 = df_papers[,c("normalized")] %>%
  left_join(words_df[, c("word", "cluster_name")], by = c("normalized" = "word"))

clusters_prop_papers <- df_papers2 %>% na.omit() %>%
  group_by(cluster_name) %>%
  summarise(n = n()) %>%
  mutate(n_total = sum(n),
         proportion = n / n_total)

df_exp2 = df_exp[,c("word_clean_normalized", "type")] %>%
  left_join(words_df[, c("word", "cluster_name")], by = c("word_clean_normalized" = "word"))

clusters_prop_exp <- df_exp2 %>% na.omit() %>%
  group_by(cluster_name) %>%
  summarise(n = n()) %>%
  mutate(n_total = sum(n),
         proportion = n / n_total)

combined_data <- bind_rows(
  clusters_prop_papers %>%
    mutate(source = "Papers"),
  clusters_prop_exp %>%
    mutate(source = "Experiments"))



# Reverse the order of the factor levels for 'source' and rename 'Papers' to 'Literature'
combined_data$source <- factor(combined_data$source, levels = c("Papers", "Experiments"))

png("3_figures/proportion_clusters.png", width = 1080, height = 700)
# Create the ggplot with updated colors, reversed order, and renamed 'Papers'
ggplot(combined_data, aes(x = cluster_name, y = proportion, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Papers" = "#1f77b4", "Experiments" = "#ff7f0e"),
                    labels = c("Papers" = "Literature", "Experiments" = "Survey")) +  # Rename 'Papers' to 'Literature'
  labs(x = "Cluster Name", y = "Proportion", title = "Proportion of Clusters") +
  guides(fill = guide_legend(title = "Source")) +
  theme_bw(base_size=25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
dev.off()



words_df2$origin = ifelse(words_df2$orig == "Papers", "Literature", words_df2$type)
words_df2$origin = ifelse(words_df2$origin == "association", "Association", words_df2$origin)
words_df2$origin = ifelse(words_df2$origin == "situation", "Situation", words_df2$origin)


png("3_figures/embeddings_all.png", width = 1080, height = 720)
ggplot() +
  geom_point(data = words_df2,
             aes(x = group_dim_1, y = group_dim_2, colour = origin),
             alpha = 0.3, size = 5) +
  scale_colour_scico_d(palette = "berlin") +
  labs(colour="Origin",
       title="Embeddings literature and experiment",
       x="Dimension 1",
       y="Dimension 2")+
  theme_bw(base_size=25)
dev.off()

png("3_figures/embeddings_papers.png", width = 900, height = 720)
ggplot() +
  geom_point(data = words_df2[words_df2$origin == "Papers", ],
             aes(x = group_dim_1, y = group_dim_2), colour = "#180B09",
             alpha = 0.3, size = 5) +
  geom_point(data = words_df2[words_df2$origin != "Papers", ],
             aes(x = group_dim_1, y = group_dim_2), colour = "grey",
             alpha = 0.01, size = 5) +
  labs(colour="Origin",
       title="Embeddings literature",
       x="Dimension 1",
       y="Dimension 2")+
  theme_bw(base_size=25)
dev.off()

png("3_figures/embeddings_situations.png", width = 900, height = 720)
ggplot() +
  geom_point(data = words_df2[words_df2$origin == "Situation", ],
             aes(x = group_dim_1, y = group_dim_2), colour="#FFACAC",
             alpha = 0.3, size = 5) +
  geom_point(data = words_df2[words_df2$origin != "Situation", ],
             aes(x = group_dim_1, y = group_dim_2), colour = "grey",
             alpha = 0.01, size = 5) +
  scale_colour_scico_d(palette = "berlin", begin = 0, end=2) +
  labs(colour="Origin",
       title="Embeddings situations",
       x="Dimension 1",
       y="Dimension 2")+
  theme_bw(base_size=25)
dev.off()

png("3_figures/embeddings_associations.png", width = 900, height = 720)
ggplot() +
  geom_point(data = words_df2[words_df2$origin == "Association", ],
             aes(x = group_dim_1, y = group_dim_2), colour="#9EB0FF",
             alpha = 0.3, size = 5) +
  geom_point(data = words_df2[words_df2$origin != "Association", ],
             aes(x = group_dim_1, y = group_dim_2), colour = "grey",
             alpha = 0.01, size = 5) +
  scale_colour_scico_d(palette = "berlin", begin = 1) +
  labs(colour="Origin",
       title="Embeddings associations",
       x="Dimension 1",
       y="Dimension 2")+
  theme_bw(base_size=25)
dev.off()


summary = df_exp2 %>%
  group_by(cluster_name, type) %>%
  summarise(n = n()) %>%
  na.omit() %>%
  group_by(type) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()



summary$type <- plyr::revalue(summary$type, c("association" = "Association", "situation" = "Situation"))

width = 0.4
png("3_figures/exp_situations_associations.png", width = 1080, height = 700)
ggplot(summary, aes(x = as.factor(cluster_name))) +
  geom_col(data = subset(summary, type == "Association"), 
           aes(y = -prop, fill = 'Association'), width = width) +
  geom_col(data = subset(summary, type == "Situation"), 
           aes(y = prop, fill = 'Situation'), width = width) +
  coord_flip() +
  scale_fill_manual(values = c(Association = "#440154", Situation = "#FDE725"))+
  geom_text(aes(y = 0, label = cluster_name), size = 8, vjust = -1)+
  theme_bw(base_size=24)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.title=element_blank())+
  labs(y="Frequency",
       title = "Number of situations and associations per cluster")+
  scale_y_continuous(labels = abs)
dev.off()

png("3_figures/embeddings_situations.png", width = 1080, height = 720)
ggplot() +
  geom_point(data = words_df2[words_df2$type != "situation",],
             aes(x = group_dim_1, y = group_dim_2),
             colour = "grey", 
             alpha = 0.03, size = 5) +
  geom_point(data = words_df2[words_df2$type == "situation",],
             aes(x = group_dim_1, y = group_dim_2, colour = factor(cluster_name)),
             alpha = 0.5, size = 5) +
  scale_colour_scico_d() +
  labs(colour="Clusters",
       title="Embeddings experiment responses",
       x="Dimension 1",
       y="Dimension 2")+
  theme_bw(base_size=25)
dev.off()

png("3_figures/embeddings_associations.png", width = 1080, height = 720)
ggplot() +
  geom_point(data = words_df2[words_df2$type != "association",],
             aes(x = group_dim_1, y = group_dim_2),
             colour = "grey", 
             alpha = 0.03, size = 5) +
  geom_point(data = words_df2[words_df2$type == "association",],
             aes(x = group_dim_1, y = group_dim_2, colour = factor(cluster_name)),
             alpha = 0.5, size = 5) +
  scale_colour_scico_d() +
  labs(colour="Clusters",
       title="Embeddings experiment responses",
       x="Dimension 1",
       y="Dimension 2")+
  theme_bw(base_size=25)
dev.off()




## WORDCLOUDS



# Extract viridis colors for clusters
scico_colors <- scico(length(unique(words_df$cluster_name)), palette = "batlow")

# Create a color mapping for each cluster
color_mapping <- setNames(scico_colors,
                          levels(as.factor(words_df$cluster_name)))

# Prepare the word data
wordcloud_papers <- df_papers2 %>%
  separate_rows(normalized, sep = " ")  %>% 
  rename(text_column = normalized) %>%
  unnest_tokens(word, text_column) %>%   # Tokenize the text into words
  filter(!word %in% stop_words$word) %>% # Remove stopwords
  mutate(word = gsub("[[:punct:]]", "", word)) %>% # Remove punctuation
  count(cluster_name, word, sort = TRUE) %>% # Count word frequencies
  mutate(color = color_mapping[as.factor(cluster_name)]) %>%
  filter(!word %in% c("losses", "loss", "lost", "losing", "lose"))


# Create the wordcloud with the mapped colors
png("3_figures/wordcloud_papers.png", width = 1080, height = 720)
wordcloud2(data = wordcloud_papers %>% select(word, n), rotateRatio = 0,
           color = unname(wordcloud_papers$color), fontFamily = "Arial",
           ellipticity = 0.9)
dev.off()


# Wordcloud exp
wordcloud_exp <- df_exp2 %>%
  na.omit() %>%
  rename(text_column = word_clean_normalized) %>%
  unnest_tokens(word, text_column) %>%   # Tokenize the text into words
  filter(!word %in% stop_words$word) %>% # Remove stopwords
  mutate(word = gsub("[[:punct:]]", "", word)) %>% # Remove punctuation
  count(cluster_name, word, sort = TRUE) %>% # Count word frequencies
  filter(n > 30) %>% 
  filter(!word %in% c("losses", "loss", "lost", "losing", "lose", "null"))  %>%
  mutate(color = color_mapping[as.factor(cluster_name)])

png("3_figures/wordcloud_exp", width = 1080, height = 720)
wordcloud2(data = wordcloud_exp %>% select(word, n), rotateRatio = 0,
           color = unname(wordcloud_exp$color), fontFamily = "Arial",
           ellipticity = 0.9)
dev.off()