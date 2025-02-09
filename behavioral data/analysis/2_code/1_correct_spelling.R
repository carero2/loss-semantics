library(tidyverse)
source("2_code/_helpers.R")

df_long = read_csv("1_data/loss_semantics_clean.csv") %>% 
  pivot_longer(
    cols = association_1:situation_5,
    names_to = c(".value", "position"),
    names_pattern = "(association|situation)(_\\d+)") %>% 
  mutate(position = str_remove(position, "_") %>% as.integer()) %>%
  mutate(association = case_when(tolower(association) %in% c("na", "n/a", "none", "no", "nan") ~ NA,
                                 TRUE ~ association),
         situation = case_when(tolower(situation) %in% c("na", "n/a", "none", "no", "nan") ~ NA,
                               TRUE ~ situation),
         association = str_replace_all(association, "/", " or "),
         situation = str_replace_all(situation, "/", " or "))

# CLEANUP ASSOCIATIONS

entries = unique(df_long$association)
entries_clean = entries %>% sapply(clean_up) %>% unname()
correct = sapply(entries_clean, check_multi)

system = "You are a helpful assistant that provides the correct English spellings in the context of a free association task. In the task, participants were presented with the cues  'loss', 'losing', or 'losses' and were asked to produce five association responses. Your goal is to ensure accurate spelling of the given association."
instruct = "Given the context of the cue 'loss', 'losing', or 'losses', determine the correct English spelling for '{WORD}' preserving the original phrase. The word may be correctly or incorrectly spelled. Choose the appropriate capitalization. Respond only with the correct spelling for '{WORD}' in this format: [correct spelling]."

candidates = entries_clean[!correct]
print(length(candidates))
llama_resp = correct_llama(candidates, sleep = .1, system = system, instruct = instruct, token =)
llama_dict = tolower(llama_resp) %>% str_remove_all('\n|\\/|\\.|\\[|\\]"') %>%
  str_remove_all("^assistant *")
names(llama_dict) = entries[!correct]

dict = tibble(entries, hunspell = correct) %>% 
  mutate(correct = case_when(
    correct ~ entries, 
    TRUE ~ llama_dict[entries])) %>% 
  pull(correct, entries)

df_long$association_clean = dict[df_long$association]



# CLEANUP SITUATIONS

entries = unique(df_long$situation)
entries_clean = entries %>% sapply(clean_up) %>% unname()
correct = sapply(entries_clean, check_multi)

system = "You are a helpful assistant that provides the correct English spellings for words in the context of a situation retrieval task. In the task, participants were asked to recall situations where they have experienced a loss. Your goal is to ensure accurate spelling for the given situation."
instruct = "In the context of loss-related situations, determine the correct English spelling for '{WORD}' preserving the original phrase. The word may be correctly or incorrectly spelled. Choose the appropriate capitalization. Respond only with the correct spelling for '{WORD}' in this format: [correct spelling]."

candidates = entries_clean[!correct]
print(length(entries_clean[!correct]))
llama_resp = correct_llama(candidates, sleep = .1, system = system, instruct = instruct)
llama_dict = tolower(llama_resp) %>% str_remove_all('\n|\\/|\\.|\\[|\\]"') %>%
  str_remove_all("^assistant *")
names(llama_dict) = entries[!correct]

dict = tibble(entries, hunspell = correct) %>% 
  mutate(correct = case_when(
    correct ~ entries, 
    TRUE ~ llama_dict[entries])) %>% 
  pull(correct, entries)

df_long$situation_clean = dict[df_long$situation]



write_csv(df_long, "1_data/loss_semantics_clean_spellcheck.csv")



