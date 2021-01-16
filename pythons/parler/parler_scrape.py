## Based on https://colab.research.google.com/github/sbooeshaghi/parlertrick/blob/main/parler.ipynb

import glob
from bs4 import BeautifulSoup
import pandas as pd
import matplotlib.pyplot as plt

import nltk

nltk.download('punkt')
nltk.download('stopwords')

class HTMLDictifier:
    def __init__(self, meta_dict, fail_default=""):
        self.d = meta_dict
        self.f = fail_default
    def __call__(self, parsed_file):
        acc = {}
        for k, v in self.d:
            try:
                acc[k] = v(parsed_file)
            except AttributeError:
                acc[k] = self.f
        return acc

def dataframe_of_parler(parler_glob, dictifier):
    def seq_of_parler_posts():
        for idx, path in enumerate(glob.glob(parler_glob)):
            with open(path) as opened_post:
                yield dictifier(BeautifulSoup(opened_post, 'html.parser'))

    return pd.DataFrame.from_records(seq_of_parler_posts())

DEFAULT_PARLER_DICTIFIER = HTMLDictifier({
    "author_name": lambda parsed: parsed.find('span', {'class': 'author--name'}).text,
    "author_username": lambda parsed: parsed.find('span', {'class': 'author--username'}).text,
    "post_text": lambda parsed: parsed.find('div', {'class': 'card--body'}).find('p').text,
    "post_timestamp": lambda parsed: parsed.find('span', {'class': 'post--timestamp'}).text,
    "post_impressions": lambda parsed: parsed.find('span', {'class': 'impressions--count'}).text
})

def inplace_tokenize_posts(df, post_key="post_text", token_key="post_tokens"):
    def tokenize_post(post):
        return list(filter(lambda word: word.isalnum() and word not in nltk.corpus.stopwords.words('english'),
                           nltk.word_tokenize(post)))
    
    df[token_key] = df[post_key].apply(tokenize_post)

def freqdist_of_tokenized_posts(df, token_key="post_tokens", impression_key="post_impressions"):
    def merge_into_freqdist(freqdist, tokens, weight):
        for t in tokens:
            freqdist[t] += weight

    return reduce(lambda f, r: merge_into_freqdist(f, r[token_key], r[impression_key]),
                  df.iterrows(), nltk.Freqdist())

def plot_freqdist(freqdist):
    fd.plot(50, cumulative=False)

def pickle_freqdist(freqdist, pickle_path):
    pd.DataFrame.from_records(freqdist, columns=['token', 'freq']).to_pickle(pickle_path)
    
