library("tidyverse")

raw <- read_csv("1_data/raw_data/Real+losses_October+14,+2024_08.32.csv")

# fix variable names because of qualtrics assigning the same names multiple times
names(raw)[str_detect(names(raw),"^W")] = paste0("W", rep(1:5, 3), ".", rep(1:3, rep(5, 3)))
names(raw)[str_detect(names(raw),"^S[:digit:]")] = paste0("S", rep(1:5, 3), ".", rep(1:3, rep(5, 3)))


df = raw %>% 
  slice(-c(1:2)) %>% 
  group_by(prolific_id) %>% 
  filter(StartDate == min(StartDate)) %>% 
  mutate(association_1 = case_when(W1.1 != "" ~ W1.1, W1.2 != "" ~ W1.2, W1.3 != "" ~ W1.3, TRUE ~ NA),
         association_2 = case_when(W2.1 != "" ~ W2.1, W2.2 != "" ~ W2.2, W2.3 != "" ~ W2.3, TRUE ~ NA),
         association_3 = case_when(W3.1 != "" ~ W3.1, W3.2 != "" ~ W3.2, W3.3 != "" ~ W3.3, TRUE ~ NA),
         association_4 = case_when(W4.1 != "" ~ W4.1, W4.2 != "" ~ W4.2, W4.3 != "" ~ W4.3, TRUE ~ NA),
         association_5 = case_when(W5.1 != "" ~ W5.1, W5.2 != "" ~ W5.2, W5.3 != "" ~ W5.3, TRUE ~ NA),
         situation_1 = case_when(S1.1 != "" ~ S1.1, S1.2 != "" ~ S1.2, S1.3 != "" ~ S1.3, TRUE ~ NA),
         situation_2 = case_when(S2.1 != "" ~ S2.1, S2.2 != "" ~ S2.2, S2.3 != "" ~ S2.3, TRUE ~ NA),
         situation_3 = case_when(S3.1 != "" ~ S3.1, S3.2 != "" ~ S3.2, S3.3 != "" ~ S3.3, TRUE ~ NA),
         situation_4 = case_when(S4.1 != "" ~ S4.1, S4.2 != "" ~ S4.2, S4.3 != "" ~ S4.3, TRUE ~ NA),
         situation_5 = case_when(S5.1 != "" ~ S5.1, S5.2 != "" ~ S5.2, S5.3 != "" ~ S5.3, TRUE ~ NA),
         order = case_when(FL_21_DO != "" ~ FL_21_DO, FL_4_DO != "" ~ FL_4_DO, FL_24_DO != "" ~ FL_24_DO, TRUE ~ NA),
         first_block = ifelse(str_detect(order, "^Realsituation"), "situations", "associations"),
         wording = str_extract(order, "_[:alpha:]+") %>% str_remove("_")) %>% 
  select(which(!str_detect(names(.), "W[:digit:]|S[:digit:]|^FL"))) %>% 
  rename(
    duration_sec = `Duration (in seconds)`,
    loss3_win6 = loss_question_1,
    loss4_win6 = loss_question_2,
    loss5_win6 = loss_question_3,
    loss6_win6 = loss_question_4,
    loss7_win6 = loss_question_5,
    loss8_win6 = loss_question_6,
    loss9_win6 = loss_question_7,
    risk_question = risk_quesiton,
    age = demo1,
    gender = demo2,
    response_self_describe = demo2_4_TEXT,
    highest_education = demo3,
    employement_status = demo4,
    feedback = Q25,
    why_not_usage = `reason not`) %>% 
  mutate(gender_clean = case_when(
    gender %in% c("Male", "Female") ~ gender,
    gender == "Prefer to self-describe" & response_self_describe == "Man" ~ "Male",
    TRUE ~ "other")) %>%
  select(!c("IPAddress", "Status", "Progress", "Finished", "consent",
            "RecordedDate", "ResponseId", "RecipientLastName",
            "RecipientFirstName","RecipientEmail", "ExternalReference",
            "LocationLatitude", "LocationLongitude", "DistributionChannel",
            "UserLanguage", "Q_RecaptchaScore", "risk_quesiton_NPS_GROUP", "order"))


write_csv(df, '1_data/loss_semantics_clean.csv')

