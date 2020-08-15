# -*- coding: utf-8 -*-
"""
Created on Fri Jul  3 04:06:14 2020

@author: dhrup
"""


import pandas as pd
from bs4 import BeautifulSoup as bs
import re
import matplotlib.pyplot as plt
from wordcloud import WordCloud as wc
from nltk.corpus import stopwords as sw
from selenium import webdriver
from webdriver_manager.chrome import ChromeDriverManager

browser = webdriver.Chrome(ChromeDriverManager().install())
page = "http://www.imdb.com/title/tt0944947/reviews?ref_=tt_urv"
#page = "http://www.imdb.com/title/tt6294822/reviews?ref_=tt_urv"
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import ElementNotVisibleException
browser.get(page)
import time
reviews = []
i=1
while (i>0):
   # i=i+25
    try:
        button = browser.find_element_by_xpath('//*[@id="load-more-trigger"]')
        button.click()
        time.sleep(5)
        ps = browser.page_source
        soup=bs(ps,"html.parser")
        rev = soup.findAll("div",attrs={"class","text"})
        reviews.extend(rev)
    except NoSuchElementException:
        break
    except ElementNotVisibleException:
        break
        

##### If we want only few recent reviews you can either press cntrl+c to break the operation in middle but the it will store 
##### Whatever data it has extracted so far #######
len(reviews)
len(list(set(reviews)))

print(reviews)

cleaned_reviews= re.sub("[^A-Za-z" "]+"," ",str(reviews)).lower()
f = open("D:\\GOT_reviews.txt","w")
f.write(cleaned_reviews)
f.close()

#with open("The_Post.text","w") as fp:
 #   fp.write(str(reviews))


len(soup.find_all("p"))

with open("D:\\GOT_reviews.txt","r") as gt:
    main = gt.read()


with open("D:\\stop.txt","r") as sw:
    stopwords =sw.read()
    
with open("D:\\positive-words.txt","r") as pw:
    postive = pw.read()
    
with open("D:\\negative-words.txt","r") as nw:
    negative = nw.read()

main_words = main.split(" ")
sw = stopwords.split("\n") 

main_words = [w for w in main_words if not w in sw]

pos_words =" ".join([w for w in main_words if w in postive])

wordcloud_pos = wc(
                      background_color='black',
                      width=1800,
                      height=1400
                     ).generate(pos_words)

plt.imshow(wordcloud_pos)

