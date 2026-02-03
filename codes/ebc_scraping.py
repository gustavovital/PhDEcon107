from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
import time

# Setup do Selenium
options = Options()
# options.add_argument("--headless")
options.add_argument("--no-sandbox")
options.add_argument("--disable-dev-shm-usage")
driver = webdriver.Chrome(service=Service(), options=options)

# Abre a página
url = 'https://www.ecb.europa.eu/press/pubbydate/html/index.en.html?'
driver.get(url)
time.sleep(3)

# Aceita cookies se necessário
try:
    accept_button = driver.find_element(By.CSS_SELECTOR, 'a.check.linkButtonLarge.floatLeft.highlight-medium')
    accept_button.click()
    time.sleep(2)
except:
    pass

# PDF que estamos procurando
pdf_target = '/press/key/date/2018/html/ecb.sp180129.en.html'

# Scroll e busca
step = 1
while True:
    print(f"Scroll step {step}")
    
    # Captura o HTML da página
    html = driver.page_source

    # Verifica se contém o PDF alvo
    if pdf_target in html:
        print(f"Encontrado! Parando no passo {step}")
        with open("data/ecb_final_page_2.html", "w", encoding="utf-8") as f:
            f.write(html)
        break

    # Scroll e continua
    driver.execute_script("window.scrollBy(0, 400);")
    time.sleep(2)
    step += 1

print("Processo finalizado.")