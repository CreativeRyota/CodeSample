#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Mon Apr  9 23:18:08 2018

@author: Ryota
"""

import numpy as np
import pandas as pd
from pandas import Series, DataFrame

import matplotlib.pyplot as plt

#%% Read the data from Excel

df = pd.read_excel("file:///Users/Ryota/Python_data/Test.xlsx", "Sheet1")

#%% Confirming the data

df.describe()
df.head()
df.mean()

#%% Creating the subsets per company

agentBroker = df[df["Type of Company"]=="Agent / Broker"]
developer = df[df["Type of Company"]=="Developer"]
privateIndividual = df[df["Type of Company"]=="Private / Individual"]

#%% Check the missing values

agentBroker["Monthly Contract Value"].isnull().sum() # 4 out of 3855
valueAB = agentBroker["Monthly Contract Value"].dropna()

developer["Monthly Contract Value"].isnull().sum() # 0 out of 206
valueDV = developer["Monthly Contract Value"].dropna()

privateIndividual["Monthly Contract Value"].isnull().sum() # 0 out of 1738
valuePI = privateIndividual["Monthly Contract Value"].dropna()

#%% Difference in the distribution
fig = plt.figure()

ax = fig.add_subplot(1,3,1)
ax.hist(valueAB, normed=True, bins=20)
ax.set_title('Agent/Broker')
ax.set_xlabel('Contract Value')
ax.set_ylabel('freq')
vals = ax.get_yticks()
ax.set_yticklabels(['{:3.2f}%'.format(x*100) for x in vals])

ax = fig.add_subplot(1,3,2)
ax.hist(valueDV, normed=True, bins=20)
ax.set_title('Developer')
ax.set_xlabel('Contract Value')
vals = ax.get_yticks()
ax.set_yticklabels(['{:3.3f}%'.format(x*100) for x in vals])

ax = fig.add_subplot(1,3,3)
ax.hist(valuePI, normed=True, bins=20)
ax.set_title('Private/Individual')
ax.set_xlabel('Contract Value')
vals = ax.get_yticks()
ax.set_yticklabels(['{:3.1f}%'.format(x*100) for x in vals])

fig.legend()

fig.show()