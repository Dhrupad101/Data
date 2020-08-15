# -*- coding: utf-8 -*-
"""
Created on Sun Jun 28 03:53:46 2020

@author: dhrup
"""

import requests
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np
from textblob import TextBlob
import nltk
import nltk.corpus

url = 'https://www.imdb.com/title/tt0111161/reviews?ref_=tt_urv'
response = requests.get(url)
html = response.text
    
soup = BeautifulSoup(html,'html.parser')

review_text = []

review_text_content = soup.find_all(class_ = 'text show-more__control')

for item in review_text_content :
    review_text.append(item.text)
    
print(review_text)

df = pd.DataFrame(review_text)
df.head()
