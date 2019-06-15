
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


def scale_numeric(fit_df, trans_df, cols):
    df_scaled = trans_df[cols].copy()
    scaler = StandardScaler().fit(fit_df[cols])
    df_scaled = pd.DataFrame(scaler.transform(df_scaled), index=df_scaled.index, columns=cols)
    return(df_scaled)

def scale_encode(fit_df, trans_df):
    num_cols = list(fit_df.columns[(fit_df.dtypes == 'float64') | (fit_df.dtypes == 'int64')])
    str_cols = list(set(fit_df.columns)^set(num_cols))
    
    if (len(num_cols)>0) & (len(str_cols)>0):
        df_scaled = scale_numeric(fit_df=fit_df, trans_df=trans_df, cols=num_cols)
        df_dummy = pd.get_dummies(trans_df[str_cols])
        df =  df_scaled.join(df_dummy)
    elif len(num_cols)==0:
        df = pd.get_dummies(trans_df[str_cols])
    elif len(str_cols)==0:
        df = scale_numeric(fit_df=fit_df, trans_df=trans_df, cols=num_cols)        
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

def impute_mean(df, na_cols):
    for i in na_cols:
        if i in num_cols:
            df.loc[df[i].isnull(), i] = df[i].dropna().mean()
        elif i in str_cols:
            df.loc[df[i].isnull(), i] = df[i].dropna().mode()            
    return(df)


def handle_nas(df_impute, method='impute_mean'):
    
    df = set_outliers_to_null(df_impute)

    df = df.drop(columns=list(df.isnull().sum()[np.sum(df.isnull())>0.4*df.shape[0]].index))

    na_cols = list(df.isnull().sum().sort_values(ascending=True)[np.sum(df.isnull())>0].index)
    num_cols = list(df.columns[(df.dtypes == 'float64') | (df.dtypes == 'int64')])
    str_cols = list(set(df.columns)^set(num_cols))
    predictors = list(set(na_cols)^set(df.columns))
    
    print('Imputing data for variables: %s \n Total number of features for imputing: %s \n Total Columns: %s'
          %(na_cols, len(na_cols), len(list(df.columns))))
    
    # if all cols have na's impute, one with least na's by mean
    if len(predictors) == 0:
        df = impute_mean(df=df, na_cols=na_cols[0])
        na_cols = list(df.isnull().sum().sort_values(ascending=True)[np.sum(df.isnull())>0].index)
        predictors = list(set(na_cols)^set(df.columns))

    if method == 'impute_mean':
        df = impute_mean(df=df, na_cols=na_cols)
    elif method == 'impute_predict':  
        
        for i in na_cols:
            t0 = time.time()
            print('loop started for %s...'%i)
            X = df[predictors]
            X_scaled = scale_encode(X, X)
            y = df.loc[~df[i].isnull(), i]
            X_train = X_scaled.loc[~df[i].isnull()]
            X_pred = X_scaled.loc[df[i].isnull()]
            print('   scaling done...')
            if i in num_cols:
                model = KNeighborsRegressor().fit(X_train, y)
                print('   model fit...')
                df.loc[df[i].isnull(), i] = model.predict(X_pred)
            elif i in str_cols:
                model = KNeighborsClassifier().fit(X_train, y)
                print('   model fit...')
                df.loc[df[i].isnull(), i] = model.predict(X_pred)
            predictors.append(i)
            print('   %s done...'%i)
            t1 = time.time()
            T = t1-t0
            print('   Time taken: %s'%T)            
            
    return(df)

