import pandas as pd
from tqdm import tqdm

tqdm.pandas(desc="Processing...")

data = pd.read_csv("data\\usa_2.csv")

data = data.progress_applymap(lambda x: "" if pd.isna(x) else x)

data.to_csv("data/raw.csv", index = False)