import pandas as pd
import requests

# wiki url 
url = 'https://en.wikipedia.org/wiki/List_of_serial_killers_by_number_of_victims'

# get content, extract tables
response = requests.get(url)
tables = pd.read_html(response.text)

# empty list for dataframes
data = []


# Append tables to list
for table in tables:
   data.append(table)

# Convert data to dataframe
df = pd.concat(data)

# Save DataFrame to CSV
df.to_csv(r'.\data\serial_killers.csv', index=False)