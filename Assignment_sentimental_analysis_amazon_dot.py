# -*- coding: utf-8 -*-
"""
Created on Wed Jul  1 01:37:46 2020

@author: dhrup
"""


"""
Amazon echo dot review 

"""
import re
from bs4 import BeautifulSoup as bs
import requests
import nltk
from nltk.corpus import stopwords
import matplotlib.pyplot as plt
from wordcloud import WordCloud

dot_review=[]

for i in range(1,30):
    dr=[]
    url='https://www.amazon.in/All-new-Echo-Dot-3rd-Gen/product-reviews/B07PKXJN7J/ref=cm_cr_getr_d_paging_btm_next_3?ie=UTF8&reviewerType=all_reviews&pageNumber='
    response = requests.get(url)
    dot_soup_1=bs(response.content,"html.parser")
    reviews = dot_soup_1.findAll(class_='a-size-base review-text review-text-content')
    for i in range(len(reviews)):
        dr.append(reviews[i].text)
        dot_review=dot_review+dr
        
with open("D:\\dotreviews.txt","w",encoding='utf8') as output:
    output.write(str(dot_review))

#joining all into 1 paraghraph
dot_string =" ".join(dot_review)

# Removing unwanted symbols incase if exists
dot_string = re.sub("[^A-Za-z" "]+"," ",dot_string).lower()
dot_string = re.sub("[0-9" "]+"," ",dot_string)

reviews_words = dot_string.split(" ")
stop_words = stopwords.words('english')

with open("D:\\stop.txt","r") as sw:
    stopwords = sw.read()

stopwords = stopwords.split("\n")    

temp = ["I","am","learing","Data","science","in","python"]
[i for i in temp if i is not "in"]

reviews_words = [w for w in reviews_words if not w in stopwords]

rev_sting = " ".join(reviews_words)

reviews_wc = WordCloud(
                      background_color='black',
                      width=1800,
                      height=1400
                     ).generate(rev_sting)

plt.imshow(reviews_wc)

with open("D:\\positive-words.txt","r") as pos:
  poswords = pos.read().split("\n")
  
with open("D:\\negative-words.txt","r") as neg:
  negwords = neg.read().split("\n")
  
#Choosing only the words present in postive & negative words
pos_in_pos = " ".join([w for w in reviews_words if w in poswords])
neg_in_neg = " ".join([w for w in reviews_words if w in negwords])

pos_wc = WordCloud(
                      background_color='white',
                      width=1800,
                      height=1400
                     ).generate(pos_in_pos)
neg_wc = WordCloud(
                      background_color='white',
                      width=1800,
                      height=1400
                     ).generate(neg_in_neg)
plt.imshow(pos_wc)
plt.imshow(neg_wc)

# Unique words 
unique_words = list(set(" ".join(dot_review).split(" ")))

unique_string =" ".join(unique_words)

unique_wc =WordCloud(collocations= False).generate(unique_string)
plt.imshow(unique_wc,)

