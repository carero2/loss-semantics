library(tidyr)
# devtools::install_github("samuelae/associatoR")
library(associatoR)

df = read_csv("1_data/loss_semantics_clean_spellcheck_normalized.csv")

df$position = as.integer(df$position)

ar_import(df,
          participant = prolific_id,
          cue = wording,
          response = association_clean_normalized,
          response_vars = position) %>%
  ar_set_targets("cues") %>%
  ar_embed_targets() %>%
  ar_cluster_targets() %>%
  ar_project_embedding(n_dim = 2) %>%
  ar_plot_embedding(color_by = cluster,
                    proportion_labels = .5)

# Importing the data
ar_obj <- ar_import(data = df,
                    participant = prolific_id,
                    cue = wording,
                    response = association_clean,
                    response_vars = position)

ar_obj <- ar_normalize(ar_obj,case = "most_frequent",
                       punct = "all",
                       whitespace = "squish",
                       process_cues =TRUE)

ar_obj <- ar_set_targets(ar_obj, targets = "responses")

ar_obj <- ar_embed_targets(ar_obj,
                           method = "huggingface",
                           token = "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")

projection <- ar_project_embedding(ar_obj, method = "umap")

ar_obj <- ar_count_targets(ar_obj)

ar_obj <- ar_characterize_targets(ar_obj,
                                  characteristics = c("concreteness", "word_frequency"))



# Manually



words_df2 = words_df %>% group_by(text) %>% 
  slice_max(frequency, with_ties = FALSE) %>%
  mutate(word = gsub("[[:punct:]]", "", word))




# Label positions
pos = 15

# Plot
words_df2[words_df2$frequency > pos,] %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = group_dim_1,
                                         y = group_dim_2,
                                         label = word,
                                         size = frequency,
                                         color = cluster)) +
  ggplot2::geom_point(alpha = 0.4) +
  ggrepel::geom_text_repel(data = words_df2 %>% dplyr::slice((0:nrow(words_df2[words_df2$frequency > pos,]))[seq(0, 1, length = nrow(words_df2[words_df2$frequency > pos,])) <= 0.01]),
                           size = 3,  # You can adjust this size
                           nudge_x = 0.1,  # Adjust nudging for better label placement
                           nudge_y = 0.1,
                           max.overlaps = 60) +
  ggplot2::theme_void() +
  ggplot2::guides(size = "none", color = "none") +
  ggplot2::scale_size(range = c(0, 6))