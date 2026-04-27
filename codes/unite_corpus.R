ecb_speech_corpus_clean
ecb_speech_corpus_clean_old

ecb_speech_corpus_clean_old$DATE <- as.Date(ecb_speech_corpus_clean_old$DATE)

missing_2021_dates <- ecb_speech_corpus_clean_old %>%
  filter(format(DATE, "%Y") == "2021") %>%
  anti_join(
    ecb_speech_corpus_clean,
    by = "DATE"
  )


ecb_speech_corpus_clean_teste <- bind_rows(
  ecb_speech_corpus_clean,
  missing_2021_dates
) %>%
  arrange(DATE)

ecb_speech_corpus_clean <- ecb_speech_corpus_clean_teste

saveRDS(ecb_speech_corpus_clean, 'ecb_speech_corpus_clean.rds')

ecb_speech_corpus_clean %>% 
  filter(DATE >= as.Date('2010-01-01')) %>% 
  nrow()
