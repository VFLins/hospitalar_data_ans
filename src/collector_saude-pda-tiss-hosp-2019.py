# deve ser executado pelo terminal:
# > python.exe C:/caminho/nomedestearquivo.py
from io import BytesIO
from bs4 import BeautifulSoup
from urllib.request import urlopen
from os import remove
from sqlalchemy import table
import requests, pandas, sqlite3

print("Encontrando dados...")
URL = "http://ftp.dadosabertos.ans.gov.br/FTP/PDA/TISS/HOSPITALAR/2019/"

HTML = BeautifulSoup(requests.get(URL).text, "html.parser")
UF_FOLDERS = []
DOWNLOAD_FILES = []

# guardando o endereço das subpastas
for folder in HTML.find_all("a"): 
    href = folder.get("href")
    if len(href) < 4: UF_FOLDERS.append(URL+href)

# usando o endereço das subpastas para obter os endereços dos arquivos
for subfolder in UF_FOLDERS:
    cur_folder = BeautifulSoup(requests.get(subfolder).text, "html.parser")
    for file in cur_folder.find_all("a"):
        href = file.get("href")
        if href.endswith(".zip"): DOWNLOAD_FILES.append(subfolder+href)

# variaveis que precisam de seu tipo declarado explicitamente
VAR_TYPES = {
    "TEMPO_DE_PERMANENCIA":"Int64", 
    "ID_EVENTO_ATENCAO_SAUDE":"Int64", 
    "CD_TABELA_REFERENCIA":"object",
    "CD_MUNICIPIO_BENEFICIARIO":"object",
    "CD_MUNICIPIO_PRESTADOR":"object",
    "UF_PRESTADOR":"object", 
    "CD_CARATER_ATENDIMENTO":"object",
    "CD_TIPO_INTERNACAO":"object",
    "CD_REGIME_INTERNACAO":"object",
    "CD_MOTIVO_SAIDA":"object",
    "IND_ACIDENTE_DOENCA":"object",
    "ANO_MES_EVENTO":"object", 
    "CD_PROCEDIMENTO":"object", 
    "CID_1":"object", "CID_2":"object", 
    "CID_3":"object", "CID_4":"object"}

db_con = sqlite3.connect("dataset.sqlite")
for table in range(len(DOWNLOAD_FILES)):
    if table == 0: continue
    if table % 2 != 0:
        progress = table / len(DOWNLOAD_FILES)*100
        print(f"\rCarregando dados {round(progress, 2)}%...", end=" ")
        # coletando arquivos em pares
        try: 
            file1 = BytesIO(urlopen(DOWNLOAD_FILES[table-1]).read())
            file2 = BytesIO(urlopen(DOWNLOAD_FILES[table]).read())
        except Exception as DownloadError:
            print(f"\n\nErro ao baixar tabelas {table-1} e/ou {table}:\n{DownloadError}\nsaindo...")
            db_con.close(); remove("dataset.sqlite")
            print("pressione enter para continuar"); input()

        table_cons = pandas.read_csv(file1, compression="zip", sep=";", decimal=",", dtype=VAR_TYPES)
        table_det = pandas.read_csv(file2, compression="zip", sep=";", decimal=",", dtype=VAR_TYPES)

        # unindo arquivos e formatando o campo de datas
        try:
            table_join = table_det.merge(table_cons, how="left", 
                  on=["ID_EVENTO_ATENCAO_SAUDE", "UF_PRESTADOR", "TEMPO_DE_PERMANENCIA", "ANO_MES_EVENTO"])
            table_join["ANO_MES_EVENTO"] = pandas.to_datetime(table_join["ANO_MES_EVENTO"], format="%Y-%m")
        except Exception as DataWranglingError:
            print(f"\n\nErro encontrado ao tratar os dados:\n{DataWranglingError}\nsaindo...")
            db_con.close(); remove("dataset.sqlite")
            print("pressione enter para continuar"); input()

        # armazenando uniao das tabelas em um banco de dados sqlite
        try:
            table_join.to_sql(name="pda_tiss_hosp", con=db_con, if_exists="append")
        except Exception as SQLiteWriteEror:
            print(f"\n\nErro encontrado ao armazenar os dados:\n{SQLiteWriteEror}\nsaindo...")
            db_con.close(); remove("dataset.sqlite")
            print("pressione enter para continuar"); input()
