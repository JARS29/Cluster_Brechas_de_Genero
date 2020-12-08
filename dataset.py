# -*- coding: utf-8 -*-
"""
Created on Mon Nov 30 11:51:50 2020

@author: Asus
"""
import pandas as pd 
df = pd.read_csv('doc_unimar.csv', sep=';')
df.columns=df.columns.str.replace('¿','')
df.columns=df.columns.str.replace(' ','')
df.columns=df.columns.str.replace('  ','')
df.columns=df.columns.str.replace('   ','')
df.columns=df.columns.str.replace('\t','')


df1 = pd.read_csv('est_unimar.csv', sep=';')
df1.columns=df1.columns.str.replace('¿','')
df1.columns=df1.columns.str.replace(' ','')
df1.columns=df1.columns.str.replace('  ','')
df1.columns=df1.columns.str.replace('   ','')
df1.columns=df1.columns.str.replace('\t','')

mergedStuff = pd.merge(df, df1, on=['Name'], how='inner')
mergedStuff.head()