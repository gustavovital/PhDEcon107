import pandas as pd
import requests
from bs4 import BeautifulSoup
from tqdm import tqdm

# Carrega a base original
df = pd.read_csv("ecb_speeches_filtered.csv")

# Lista para armazenar os textos
texts = []

# Itera pelas linhas do DataFrame
for _, row in tqdm(df.iterrows(), total=len(df)):
    url = row["LINK"]

    try:
        response = requests.get(url, timeout=10)
        soup = BeautifulSoup(response.content, "lxml")

        # Achar main-wrapper > main > title + section
        main_wrapper = soup.find("div", id="main-wrapper")
        if not main_wrapper:
            texts.append("")
            continue

        main = main_wrapper.find("main")
        title_div = main.find("div", class_="title")
        section_div = main.find("div", class_="section")

        # h1 (título)
        h1 = title_div.find("h1") if title_div else None
        title_text = h1.get_text(strip=True) if h1 else ""

        # Conteúdo (h2 + p) na ordem
        content = []
        if section_div:
            for el in section_div.find_all(["h2", "p"], recursive=False):
                tag_text = el.get_text(strip=True)
                if tag_text:
                    content.append(tag_text)

        # Texto final concatenado
        full_speech_text = title_text + "\n" + "\n".join(content)
        texts.append(full_speech_text)

    except Exception as e:
        print(f"⚠️ Erro com URL: {url}\n→ {e}")
        texts.append("")

# Adiciona ao DataFrame e salva
df["TEXT"] = texts
missing = df["TEXT"].isna() | df["TEXT"].str.strip().eq("")
    
print(f"🔍 Entradas com TEXT ausente ou vazio: {missing.sum()}")

# Mostrar as linhas (opcional)
df[~missing].to_csv("ecb_speech_corpus_clean.csv", index=False)