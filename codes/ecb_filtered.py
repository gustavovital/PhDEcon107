from bs4 import BeautifulSoup
import pandas as pd
from datetime import datetime

# Ler o HTML
with open("ecb_final_page.html", "r", encoding="utf-8") as f:
    html = f.read()

soup = BeautifulSoup(html, "lxml")

blocks = soup.find_all("dl")

data = []

for block in blocks:
    children = block.find_all(["dt", "dd"], recursive=False)

    for i in range(0, len(children) - 1, 2):
        dt = children[i]
        dd = children[i + 1]

        # Data como string
        date_str = dt.get_text(strip=True)

        # Converter para datetime.date
        try:
            date_obj = datetime.strptime(date_str, "%d %B %Y").date()
        except ValueError:
            continue

        # Título e link
        a_tag = dd.select_one("div.title a")
        if not a_tag or a_tag["href"].endswith(".pdf"):
            continue

        title = a_tag.get_text(strip=True)
        link = "https://www.ecb.europa.eu" + a_tag["href"]

        # Autores
        author_tags = dd.select("div.authors li")
        authors = [li.get_text(strip=True) for li in author_tags] if author_tags else ["—"]
        authors_text = ", ".join(authors)

        # Adiciona ao dataset
        data.append({
            "DATE": date_obj,
            "AUTHOR": authors_text,
            "TITLE": title,
            "LINK": link
        })

# Criar DataFrame
df = pd.DataFrame(data)
df.index.name = "ID"

# Salvar CSV
df.to_csv("ecb_speeches_filtered.csv", index=True)

print("✅ DataFrame salvo em 'ecb_speeches_filtered.csv'")