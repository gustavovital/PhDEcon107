from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
import time
import os

# =========================
# Setup do Selenium
# =========================
options = Options()
# options.add_argument("--headless")
options.add_argument("--no-sandbox")
options.add_argument("--disable-dev-shm-usage")

driver = webdriver.Chrome(service=Service(), options=options)

os.makedirs("data", exist_ok=True)

# =========================
# Loop por ano
# =========================
for year in range(2005, 2026):

    print(f"\n=== Processing year {year} ===")

    url = f"https://www.ecb.europa.eu/press/pubbydate/html/index.en.html?year={year}"
    driver.get(url)
    time.sleep(3)

    # Aceita cookies se necessário
    try:
        accept_button = driver.find_element(
            By.CSS_SELECTOR,
            'a.check.linkButtonLarge.floatLeft.highlight-medium'
        )
        accept_button.click()
        time.sleep(2)
    except:
        pass

    step = 1
    last_count = 0
    stagnant_steps = 0
    MAX_STAGNANT = 20   # número de scrolls sem crescimento antes de parar

    while True:
        print(f"Year {year} | Scroll step {step}")

        # Conta links carregados
        links = driver.find_elements(By.CSS_SELECTOR, "a")
        current_count = len(links)

        if current_count > last_count:
            last_count = current_count
            stagnant_steps = 0
        else:
            stagnant_steps += 1
            print(f"No new content ({stagnant_steps}/{MAX_STAGNANT})")

        # Scroll (NÃO ALTERADO)
        driver.execute_script("window.scrollBy(0, 400);")
        time.sleep(1)

        if stagnant_steps >= MAX_STAGNANT:
            print(f"All content loaded for year {year}")

            html = driver.page_source
            output_file = f"data/ecb_final_page_{year}.html"
            with open(output_file, "w", encoding="utf-8") as f:
                f.write(html)

            print(f"Saved: {output_file}")
            break

        step += 1

# =========================
# Finaliza
# =========================
driver.quit()
print("\nProcesso finalizado.")
