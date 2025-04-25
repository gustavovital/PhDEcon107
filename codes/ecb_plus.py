import pandas as pd

corpus = pd.read_csv('/Users/gustavovital/Documents/GitHub/PhDEcon107/data/ecb_speech_corpus_clean.csv')
score = pd.read_csv('/Users/gustavovital/Documents/GitHub/PhDEcon107/data/sentiment_scored_aggregated.csv')

score['length'] = corpus['TEXT'].str.len()
score['relative_finbert'] = score['sentiment_finbert'] * score['length']
score['relative_yiyanghkust'] = score['sentiment_yiyanghkust'] * score['length']

score.to_csv("/Users/gustavovital/Documents/GitHub/PhDEcon107/data/sentiment_scored_aggregated.csv", index=False)
