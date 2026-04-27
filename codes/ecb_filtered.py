import glob
import pandas as pd
from datetime import datetime
from lxml import html as lxml_html

all_rows = []

html_files = sorted(glob.glob("data/ecb_final_page_*.html"))

for html_path in html_files:
    year = int(html_path.split("_")[-1].replace(".html", ""))
    print(f"Processing year {year}")

    with open(html_path, "r", encoding="utf-8") as f:
        page = f.read()

    doc = lxml_html.fromstring(page)

    # Cada item está em <dl> com pares <dt> (data) e <dd> (conteúdo)
    dls = doc.xpath("//dl")
    for dl in dls:
        dts = dl.xpath("./dt")
        dds = dl.xpath("./dd")
        n = min(len(dts), len(dds))

        for i in range(n):
            date_str = "".join(dts[i].itertext()).strip()
            try:
                date_obj = datetime.strptime(date_str, "%d %B %Y").date()
            except ValueError:
                continue

            # Título + link (evita PDF)
            a = dds[i].xpath('.//div[contains(@class,"title")]//a[1]')
            if not a:
                continue
            href = a[0].get("href", "").strip()
            if not href or href.endswith(".pdf"):
                continue
            title = "".join(a[0].itertext()).strip()
            link = "https://www.ecb.europa.eu" + href

            # Autores (se não houver, "—")
            author_nodes = dds[i].xpath('.//div[contains(@class,"authors")]//li')
            authors = [("".join(x.itertext()).strip()) for x in author_nodes]
            authors_text = ", ".join([x for x in authors if x]) if authors else "—"

            all_rows.append({
                "YEAR": year,
                "DATE": date_obj,
                "AUTHOR": authors_text,
                "TITLE": title,
                "LINK": link
            })

df = pd.DataFrame(all_rows)
df.index.name = "ID"
# df.head()
# df.tail()

df = df.drop_duplicates(subset="LINK", keep="first")

df["DATE"] = pd.to_datetime(df["DATE"], errors="coerce", dayfirst=True)
df = df.sort_values("DATE").reset_index(drop=True)

df = df[
    (df["DATE"] >= pd.Timestamp("2005-01-01")) &
    (df["DATE"] <  pd.Timestamp("2026-01-01"))
].reset_index(drop=True)

df.to_csv("data/ecb_speeches_filtered_raw.csv", index=True)
print("Saved: data/ecb_speeches_filtered_raw.csv")
