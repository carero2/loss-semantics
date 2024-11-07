import pandas as pd
import glob
from datetime import datetime

# Get a list of all CSV files in a directory
csv_files = glob.glob(r'./1_data/*.csv')

li = []
i = 0

# Loop through all CSV files that start with "scopus"
for filename in csv_files:
    if "scopus" in filename:
        i += 1
        df = pd.read_csv(filename, index_col=None, header=0, low_memory=False)
        print(f"File {i}: {filename} {len(df)}")
        li.append(df)

# Concatenate all data into one DataFrame
frame = pd.concat(li, axis=0, ignore_index=True)

# Select only the columns we need
df = frame[["Title", "Source title", "Abstract", "Author Keywords"]]
df.index.name = 'ID'

# Select 200 random rows from the dataframe
df_sample = df.sample(n=1000, random_state=1)

df_sample["selected"] = None

# Save both files
current_date = datetime.now().strftime('%Y-%m-%d')
df.to_csv(f'./1_data/all_papers_{current_date}.csv', index=True)
df_sample.to_csv('./1_data/sample1000_papers.csv')