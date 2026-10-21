import pandas as pd

corpus = pd.read_csv('data/ecb_speech_corpus_clean.csv')
score = pd.read_csv('sentiment_scored_aggregated_full_final.csv')

score['LEN'] = corpus['TEXT'].str.len()
score['relative_finbert'] = score['sentiment_finbert'] * score['LEN']
score['relative_yiyanghkust'] = score['sentiment_yiyanghkust'] * score['LEN']

score.to_csv("data/sentiment_scored_aggregated_full_final.csv", index=False)
