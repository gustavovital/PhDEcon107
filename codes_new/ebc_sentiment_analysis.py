import pandas as pd
import torch
from transformers import AutoTokenizer, AutoModelForSequenceClassification
from tqdm import tqdm
import numpy as np

# Load full dataset
df = pd.read_csv("data/ecb_speech_corpus_clean.csv")
df['DATE'] = pd.to_datetime(df['DATE'])

# Define sentiment models
models = {
    "finbert": "ProsusAI/finbert",
    "yiyanghkust": "yiyanghkust/finbert-tone"
}

# Function to split text into chunks of max 512 tokens
def chunk_text(text, tokenizer, max_tokens=512):
    tokens = tokenizer.tokenize(str(text))
    chunks = []
    for i in range(0, len(tokens), max_tokens):
        chunk_tokens = tokens[i:i + max_tokens]
        chunks.append(tokenizer.convert_tokens_to_string(chunk_tokens))
    return chunks

# Function to extract sentiment scores
def get_sentiment_scores(texts, model_key, model_name):
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    model = AutoModelForSequenceClassification.from_pretrained(model_name)
    model.eval()

    # Resolve class indices from the model itself (case-insensitive),
    # instead of hard-coding positions. This makes the score correct
    # regardless of each model's label ordering.
    label2id = {label.lower(): idx for idx, label in model.config.id2label.items()}
    if "positive" not in label2id or "negative" not in label2id:
        raise ValueError(
            f"{model_name}: expected 'positive'/'negative' labels, "
            f"got {model.config.id2label}"
        )
    pos_idx = label2id["positive"]
    neg_idx = label2id["negative"]
    print(f"[{model_key}] {model_name} -> id2label={model.config.id2label} "
          f"(pos={pos_idx}, neg={neg_idx})")

    all_scores = []

    for text in tqdm(texts, desc=f"Processing with {model_key}"):
        chunks = chunk_text(text, tokenizer)
        scores = []

        for chunk in chunks:
            inputs = tokenizer(chunk, return_tensors="pt", truncation=True, max_length=512)
            with torch.no_grad():
                outputs = model(**inputs)

            probs = torch.nn.functional.softmax(outputs.logits, dim=1).numpy()[0]

            # score = P(positive) - P(negative); neutral gets weight 0
            score = probs[pos_idx] - probs[neg_idx]
            scores.append(score)

        # Guard against empty/whitespace speeches with no tokens
        all_scores.append(np.mean(scores) if scores else np.nan)

    return all_scores

# Apply to full DataFrame
for model_key, model_name in models.items():
    df[f"sentiment_{model_key}"] = get_sentiment_scores(df['TEXT'], model_key, model_name)

# Create monthly sentiment aggregation
df['MONTH'] = df['DATE'].dt.to_period('M')

monthly_sentiment = (
    df.groupby('MONTH')[[f"sentiment_{key}" for key in models.keys()]]
    .mean()
    .reset_index()
)

# Create final DataFrame without 'TEXT'
df_no_text = df.drop(columns=["TEXT"])

# Save results (optional)
df_no_text.to_csv("sentiment_scored_aggregated_full_final.csv", index=False)
monthly_sentiment.to_csv("monthly_sentiment_full_final.csv", index=False)

# Output to screen
print("Monthly sentiment:")
print(monthly_sentiment)