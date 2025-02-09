library(tidyverse)
source("2_code/_helpers.R")

df = read_csv("1_data/loss_semantics_clean_spellcheck.csv")

# Associations
entries = unique(df$association_clean)
short_responses = sapply(entries,
                 function(x) length(unlist(strsplit(x, split = " "))) < 4)

system = "You are a helpful assistant with the task of summarizing sentences provided in the context of an association task. In the task, participants were presented with the cues  'loss', 'losing', or 'losses' and were asked to produce five association responses. Your goal is to produce an accurate summary of the given association."
instruct = "Given the context of the cue 'loss', 'losing', or 'losses', determine the summary of the association ‘{WORD}’. Limit your summary to 1-3 words that preserve the original meaning, and avoid using the word 'lose' or its derivatives. Respond only with the summary for '{WORD}' in this format: [summary]"

candidates = entries[!short_responses]
llama_resp = normalize_llama(candidates, sleep = .1, system = system, instruct = instruct, token = )
llama_dict = tolower(llama_resp) %>% str_remove_all("\n") %>% str_remove_all("^assistant *")
names(llama_dict) = entries[!correct]

dict = tibble(entries, shorter = correct) %>% 
  mutate(correct = case_when(
    correct ~ entries, 
    TRUE ~ llama_dict[entries])) %>% 
  pull(correct, entries)

df$association_clean_normalized = gsub("[^[:alnum:][:space:]'-]", "", tolower(dict[df$association_clean]))

# Situations
entries = unique(df$situation_clean)
correct = sapply(entries,
                 function(x) length(unlist(strsplit(x, split = " "))) < 4)

system = "You are a helper assisting with the task of summarizing sentences provided in the context of a situation retrieval task. In the situation retrieval task, participants are asked to produce situations related to loss."
instruct = "What is the summary of the phrase “{WORD}” using up to 3 words maintaining the original meaning in the context of loss-related situations? Only return the 1-3 words summary for '{WORD}' and use the following format: [summary]"

candidates = entries[!correct]
llama_resp = normalize_llama(candidates, sleep = .1, system = system, instruct = instruct, token = )
llama_dict = tolower(llama_resp) %>% str_remove_all("\n") %>% str_remove_all("^assistant *")
names(llama_dict) = entries[!correct]

dict = tibble(entries, shorter = correct) %>% 
  mutate(correct = case_when(
    correct ~ entries, 
    TRUE ~ llama_dict[entries])) %>% 
  pull(correct, entries)

df$situation_clean_normalized = gsub("[^[:alnum:][:space:]'-]", "", tolower(dict[df$situation_clean]))



write_csv(df, "1_data/loss_semantics_clean_spellcheck_normalized.csv")

