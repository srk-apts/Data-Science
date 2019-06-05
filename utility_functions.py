
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import scipy as sp
import matplotlib.pyplot as plt
import seaborn as sns
import time
import pickle

from sklearn.utils import resample
from sklearn.neighbors import KNeighborsClassifier, KNeighborsRegressor
from sklearn.linear_model import LogisticRegression, LinearRegression, RidgeCV, LassoCV
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis, QuadraticDiscriminantAnalysis
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier, GradientBoostingClassifier
import statsmodels.api as sm
from statsmodels.api import OLS

from sklearn.model_selection import cross_val_score, train_test_split, KFold, GridSearchCV
from sklearn.metrics import accuracy_score, confusion_matrix, mean_squared_error, r2_score, log_loss
from sklearn.decomposition import PCA
from sklearn.preprocessing import Imputer, StandardScaler, PolynomialFeatures


# In[2]:


def scale_encode(fit_df, trans_df):
    num_cols = list(fit_df.columns[(fit_df.dtypes == 'float64') | (fit_df.dtypes == 'int64')])
    str_cols = list(set(fit_df.columns)^set(num_cols))
    
    df_scaled = trans_df[num_cols].copy()
    scaler = StandardScaler().fit(fit_df[num_cols])
    df_scaled = pd.DataFrame(scaler.transform(df_scaled), index=df_scaled.index, columns=num_cols)
    
    df_dummy = pd.get_dummies(trans_df[str_cols])
    
    df =  df_scaled.join(df_dummy)
    
    return(df)


# In[3]:


def set_outliers_to_null(df):
    
    num_cols = list(df.columns[(df.dtypes == 'float64') | (df.dtypes == 'int64')])
    str_cols = list(set(df.columns)^set(num_cols))

    for col in list(df.columns):
        if col in num_cols:
            cond1 = (df[col]<0)
            cond2 = (df[col].isna())
            cond3 = (df[col]>=(np.mean(df[col])+2*np.std(df[col]))) 
            cond4 = (df[col]<=(np.mean(df[col])-2*np.std(df[col])))
            df[col][cond1 | cond2 | cond3 | cond4] = None

        elif col in str_cols:
            cond1 = (df[col].isna())
            cond2 = (df[col]=='None')
            df[col][cond1 | cond2] = None
            
    return(df)

def handle_nas(df_impute, method='impute_mean'):
    
    df = set_outliers_to_null(df_impute)

    df = df.drop(columns=list(df.isnull().sum()[np.sum(df.isnull())>0.4*df.shape[0]].index))

    na_col = list(df.isnull().sum().sort_values(ascending=True)[np.sum(df.isnull())>0].index)
    num_cols = list(df.columns[(df.dtypes == 'float64') | (df.dtypes == 'int64')])
    str_cols = list(set(df.columns)^set(num_cols))

    if method == 'drop':
        df=df.dropna()
    elif method == 'impute_mean':
        for i in list(df[na_col].columns):
            if i in num_cols:
                df.loc[df[i].isnull(), i] = df[i].dropna().mean()
            elif i in str_cols:
                df.loc[df[i].isnull(), i] = df[i].dropna().mode()
    elif method == 'impute_predict':
        predictors = list(set(na_col)^set(df.columns))
        for i in na_col:
            print('loop started for %s...'%i)
            X = df[predictors]
            X_scaled = scale_encode(X, X)
            y = df.loc[~df[i].isnull(), i]
            X_train = X_scaled.loc[~df[i].isnull()]
            X_pred = X_scaled.loc[df[i].isnull()]
            print('   scaling done...')
            if i in num_cols:
                knn = KNeighborsRegressor(15).fit(X_train, y)
                print('   fit model...')
                display(df.loc[df[i].isnull(), i].shape)
                display(knn.predict(X_pred).shape)
                df.loc[df[i].isnull(), i] = knn.predict(X_pred)
            elif i in str_cols:
                knn = KNeighborsClassifier(15).fit(X_train, y)
                print('   fit model...')
                display(df.loc[df[i].isnull(), i].shape)
                display(knn.predict(X_pred).shape)
                df.loc[df[i].isnull(), i] = knn.predict(X_pred)
                predictors.append(i)
            print('   %s done...'%i)
            
    return(df)

