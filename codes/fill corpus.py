"""
completar_corpus_2021.py
------------------------
Baixa o TEXT dos 448 discursos de 2021 (de ecb_2021_new_speeches.csv) e
completa o ecb_speech_corpus_clean.csv para 2021.

COMO RODAR (na pasta onde estao os arquivos):
    python .\completar_corpus_2021.py

Entradas:
    ecb_2021_new_speeches.csv        (LINKs de 2021, sem TEXT)
    data/ecb_speech_corpus_clean.csv (corpus atual)  <-- ajuste o caminho se preciso

Saidas:
    data/ecb_speech_corpus_clean_completo.csv   (corpus + 2021, nao sobrescreve)
    _texts_2021_cache.csv                        (cache; permite retomar)

Pode interromper e rodar de novo: ele retoma pelo cache.
Requisitos: pandas, lxml, selenium, tqdm
"""

import os, time
import pandas as pd
from lxml import html as lxml_html
from tqdm import tqdm
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By

NEW_CSV    = "data/ecb_2021_new_speeches.csv"
CORPUS_IN  = "data/ecb_speech_corpus_clean.csv"          # ajuste se estiver noutro caminho
CORPUS_OUT = "data/ecb_speech_corpus_clean_v2.csv"
CACHE      = "_texts_2021_cache.csv"

new = pd.read_csv(NEW_CSV)
new = new[new["LINK"].astype(str).str.startswith("http")].drop_duplicates("LINK")
print(f"Discursos de 2021 a baixar: {len(new)}")

# cache (retomar)
done = {}
if os.path.exists(CACHE):
    c = pd.read_csv(CACHE)
    done = dict(zip(c["LINK"], c["TEXT"].fillna("")))
    print(f"Cache encontrado: {len(done)} ja baixados")

todo = [l for l in new["LINK"] if l not in done]
print(f"Faltando baixar: {len(todo)}")

# ---------- extracao (mesma logica do corpus original, com fallback) ----------
def extract_text(tree):
    mw = tree.xpath('//div[@id="main-wrapper"]')
    if mw:
        m = mw[0].xpath('.//main'); main = m[0] if m else mw[0]
    else:
        m = tree.xpath('//main'); main = m[0] if m else tree
    tit = (main.xpath('.//div[contains(@class,"title")]//h1') or main.xpath('.//h1'))
    title = tit[0].text_content().strip() if tit else ""
    content = []
    sec = main.xpath('.//div[contains(@class,"section")]')
    if sec:
        for el in sec[0].xpath('./h2 | ./p'):
            t = el.text_content().strip()
            if t: content.append(t)
    if not content:
        for el in main.xpath('.//h2 | .//p'):
            t = el.text_content().strip()
            if t: content.append(t)
    return (title + "\n" + "\n".join(content)).strip()

if todo:
    opts = Options()
    opts.add_argument("--headless=new"); opts.add_argument("--no-sandbox")
    opts.add_argument("--disable-dev-shm-usage"); opts.add_argument("--window-size=1300,1000")
    driver = webdriver.Chrome(service=Service(), options=opts)
    cookies_ok = False
    buf = []
    for i, link in enumerate(tqdm(todo, desc="Baixando textos 2021")):
        text = ""
        for attempt in range(3):
            try:
                driver.get(link); time.sleep(2.0)
                if not cookies_ok:
                    try:
                        driver.find_element(By.CSS_SELECTOR,
                            'a.check.linkButtonLarge.floatLeft.highlight-medium').click()
                        time.sleep(1.2); cookies_ok = True
                    except Exception: pass
                text = extract_text(lxml_html.fromstring(driver.page_source))
                if text: break
            except Exception as e:
                if attempt == 2: print(f"\n  ! {link}: {e}")
                time.sleep(2)
        done[link] = text
        buf.append({"LINK": link, "TEXT": text})
        if len(buf) >= 25:   # salva cache a cada 25
            pd.DataFrame(list(done.items()), columns=["LINK","TEXT"]).to_csv(CACHE, index=False)
            buf = []
    driver.quit()
    pd.DataFrame(list(done.items()), columns=["LINK","TEXT"]).to_csv(CACHE, index=False)

# ---------- montar linhas novas no schema do corpus ----------
corpus = pd.read_csv(CORPUS_IN)
print(f"Corpus atual: {len(corpus)} linhas | colunas: {list(corpus.columns)}")

new = new.copy()
new["TEXT"] = new["LINK"].map(done)
new_ok = new[new["TEXT"].fillna("").str.strip() != ""].copy()
print(f"Textos baixados com sucesso: {len(new_ok)} | vazios (nao-texto): {len(new)-len(new_ok)}")

rows = pd.DataFrame({c: pd.NA for c in corpus.columns}, index=range(len(new_ok)))
for c in ["YEAR","DATE","AUTHOR","TITLE","LINK","TEXT"]:
    if c in corpus.columns:
        rows[c] = new_ok[c].values

# nao duplicar LINK ja existente no corpus
rows = rows[~rows["LINK"].isin(set(corpus["LINK"]))]

out = pd.concat([corpus, rows], ignore_index=True).drop_duplicates("LINK", keep="first")
if "DATE" in out.columns:
    out = out.assign(_d=pd.to_datetime(out["DATE"], errors="coerce")).sort_values("_d").drop(columns="_d")
out = out.reset_index(drop=True)

os.makedirs(os.path.dirname(CORPUS_OUT) or ".", exist_ok=True)
out.to_csv(CORPUS_OUT, index=False)
print(f"\nAdicionadas {len(rows)} linhas de 2021. Corpus final: {len(out)} linhas.")
print(f"Salvo: {CORPUS_OUT}")