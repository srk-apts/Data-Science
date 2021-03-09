#!pip3 install scikit-learn
#!pip3 install statsmodels
#!pip3 install xgboost

import pandas as pd
import numpy as np
import scipy as sp
import matplotlib.pyplot as plt
import seaborn as sns
import time
import pickle
from IPython.display import display
import matplotlib.pylab as pylab
import itertools

# from sklearn import datasets
from sklearn.utils import resample
from sklearn.neighbors import KNeighborsClassifier, KNeighborsRegressor
from sklearn.linear_model import LogisticRegression, LogisticRegressionCV, LinearRegression, RidgeClassifier, LassoCV
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis, QuadraticDiscriminantAnalysis
from sklearn.tree import DecisionTreeClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier, GradientBoostingClassifier, ExtraTreesClassifier
from sklearn.svm import SVC
import statsmodels.api as sm
from statsmodels.api import OLS
import xgboost as xgb

from sklearn.model_selection import cross_val_score, train_test_split, KFold, GridSearchCV
from sklearn.inspection import permutation_importance
from sklearn.metrics import accuracy_score, confusion_matrix, mean_squared_error, r2_score, log_loss, classification_report
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler, PolynomialFeatures


def read_all_csv(path):
    fnames = glob.glob(path)
    fskip = []
    df = pd.read_csv(fnames[0], sep = ',', encoding='utf-8')
    
    for f in fnames[1:]:
        try:
            df = pd.concat([df, pd.read_csv(f, sep = ',', encoding='utf-8')])
        except:
            fskip.append(f)
            pass
    
    print("Skipped files list:")
    print(fskip)
    print("%s files read"%(len(fnames)-len(fskip)))
    
    return(df)

def scale_numeric(fit_df, trans_df, cols=None):
    num_cols = list(fit_df.columns[(fit_df.dtypes == 'float64') | (fit_df.dtypes == 'int64')])
    str_cols = list(set(fit_df.columns)^set(num_cols))
    if cols==None:
        cols = num_cols
        df_copy = trans_df[cols].copy()
        scaler = StandardScaler().fit(fit_df[cols])
        df_num = pd.DataFrame(scaler.transform(df_copy), index=df_copy.index, columns=cols)
        df_scaled = trans_df[str_cols].join(df_num)
    else:
        df_scaled = trans_df[cols].copy()
        scaler = StandardScaler().fit(fit_df[cols])
        df_scaled = pd.DataFrame(scaler.transform(df_scaled), index=df_scaled.index, columns=cols)
    return(df_scaled)

# def encode_categorical(df, cols=None):
#     num_cols = list(df.columns[(df.dtypes == 'float64') | (df.dtypes == 'int64')])
#     str_cols = list(set(df.columns)^set(num_cols))
#     if cols==None:
#         cols = str_cols
#         df_dummy = pd.get_dummies(df[cols], drop_first=True)
#         df_encoded = df[num_cols].join(df_dummy)
#     else:
#         df_encoded = pd.get_dummies(df[cols], drop_first=True)
#     return(df_encoded)

def scale_encode(fit_df, trans_df):
    num_cols = list(fit_df.columns[(fit_df.dtypes == 'float64') | (fit_df.dtypes == 'int64')])
    str_cols = list(set(fit_df.columns)^set(num_cols))

    if (len(num_cols)>0) & (len(str_cols)>0):
        df_scaled = scale_numeric(fit_df=fit_df, trans_df=trans_df, cols=num_cols)
        df_encoded = pd.get_dummies(trans_df[str_cols], drop_first=True).astype(float)
        df =  df_scaled.join(df_encoded)
    elif len(num_cols)==0:
        df = pd.get_dummies(trans_df[str_cols], drop_first=True).astype(float)
    elif len(str_cols)==0:
        df = scale_numeric(fit_df=fit_df, trans_df=trans_df, cols=num_cols)        
    return(df)



def set_outliers_to_null(df):
    
    num_cols = list(df.columns[(df.dtypes == 'float64') | (df.dtypes == 'int64')])
    str_cols = list(set(df.columns)^set(num_cols))

    for col in list(df.columns):
        if col in num_cols:
            cond1 = (df[col]<0)
            cond2 = (df[col].isna())
#             cond3 = (df[col]>=(np.mean(df[col])+3*np.std(df[col]))) 
#             cond4 = (df[col]<=(np.mean(df[col])-3*np.std(df[col])))
            df[col][cond1 | cond2] = None

        elif col in str_cols:
            cond1 = (df[col].isna())
            cond2 = (df[col]=='None')
            df[col][cond1 | cond2] = None
            
    return(df)

def cap_outliers(df, std_dev=3):
    num_cols = list(df.columns[(df.dtypes == 'float64') | (df.dtypes == 'int64')])
    str_cols = list(set(df.columns)^set(num_cols))
    df_copy = df.copy()

    for col in num_cols:
        cond1 = (df_copy[col]>=(np.mean(df_copy[col])+std_dev*np.std(df_copy[col]))) 
        cond2 = (df_copy[col]<=(np.mean(df_copy[col])-std_dev*np.std(df_copy[col])))
        df_copy[col][cond1] = np.mean(df_copy[col])+std_dev*np.std(df_copy[col])
        df_copy[col][cond2] = np.mean(df_copy[col])-std_dev*np.std(df_copy[col])
           
    return(df_copy)
    

def impute_mean(df, na_cols):
    num_cols = list(df.columns[(df.dtypes == 'float64') | (df.dtypes == 'int64')])
    str_cols = list(set(df.columns)^set(num_cols))
    for i in na_cols:
        if i in num_cols:
            df.loc[df[i].isnull(), i] = df[i].dropna().mean()
        elif i in str_cols:
            df.loc[df[i].isnull(), i] = df[i].dropna().mode()            
    return(df)


def handle_nas(df_impute, method='impute_mean', threshold=0.02):
    
    df = set_outliers_to_null(df_impute)

    df = df.drop(columns=list(df.isnull().sum()[np.sum(df.isnull())>0.7*df.shape[0]].index))
    
    # Impute mean for columns with very low % of NAs
    na_rows_in_each_col = np.sum(df.isnull())
    prop_na_each_col = na_rows_in_each_col/df.shape[0]
    thresh_cols = list(prop_na_each_col[prop_na_each_col <= threshold].keys())
    if len(thresh_cols)>0:
        df = impute_mean(df=df, na_cols=thresh_cols)
    
    na_cols = list(df.isnull().sum().sort_values(ascending=True)[np.sum(df.isnull())>0].index)
    num_cols = list(df.columns[(df.dtypes == 'float64') | (df.dtypes == 'int64')])
    str_cols = list(set(df.columns)^set(num_cols))
    predictors = list(set(na_cols)^set(df.columns))
    
    print('Imputing data for variables: %s \n Total number of features for imputing: %s \n Total Columns: %s'
          %(na_cols, len(na_cols), len(list(df.columns))))
    
    # if all cols have na's, impute one with least na's by mean
    if len(predictors) == 0:
        df = impute_mean(df=df, na_cols=na_cols[:1])
        na_cols = list(df.isnull().sum().sort_values(ascending=True)[np.sum(df.isnull())>0].index)
        predictors = list(set(na_cols)^set(df.columns))

    if method == 'impute_mean':
        df = impute_mean(df=df, na_cols=na_cols)
    elif method == 'impute_predict':  
        
        for i in na_cols:
            t0 = time.time()
            print('loop started for %s...'%i)
            X = df[predictors]
            print('using as predictors: %s'%predictors)
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
  
  
def preprocess(path, exc_cols, rows, target):
    df_subset_rows = pd.read_csv(path)[rows]
    X = df_subset_rows.drop(columns=exc_cols)
    y = df_subset_rows[target]
    X_cap = cap_outliers(X)
    X_scaled = scale_encode(fit_df=X_cap, trans_df=X_cap)
    X_train_scaled, X_test_scaled, y_train, y_test = train_test_split(X_scaled, y, test_size=0.3, random_state=43, stratify=y)
    return(X_train_scaled, X_test_scaled, y_train, y_test)


def preprocess_pred(path_fit, path_trans, exc_cols, rows_fit):    
    X_fit = pd.read_csv(path_fit)[rows_fit].drop(columns=exc_cols)
    X_fit_dropna = X_fit.dropna()
    inc_cols = list(X_fit_dropna.columns)
    X_trans = pd.read_csv(path_trans)[inc_cols]
    X_trans_dropna = X_trans.dropna()
    X_trans_cap = cap_outliers(X_trans_dropna.dropna())
    X_scaled = scale_encode(fit_df=X_fit_dropna, trans_df=X_trans_cap)
    return(X_scaled)

  
def preprocess_xgb(model, X_train, X_test, y_train, y_test):

    if (model == "no_hra_medical_model") | (model == "all_medical_model"):
        mapping = {"non_med": 0,
                   "ma": 1,
                   "gap": 2
                  }
        y_train = y_train.replace(mapping)
        y_test = y_test.replace(mapping)
    elif (model == "no_hra_pdp_model") | (model == "all_pdp_model"):
        mapping = {"no_pdp": 0,
                   "gap_pdp": 1,
                   "pdp_only": 2
                  }
        y_train = y_train.replace(mapping)
        y_test = y_test.replace(mapping)

    dtrain = xgb.DMatrix(X_train, label=y_train)
    dtest = xgb.DMatrix(X_test, label=y_test)
    
    return(dtrain, dtest)
  
  
def get_confusion_matrix(m, X_train, X_test, y_train, y_test):
    model = m.fit(X_train, y_train)    
    try:
        model = model.best_estimator_
        y_pred = model.predict(X_test)
    except:
        y_pred = model.predict(X_test)
    cf = confusion_matrix(y_test, y_pred)
    return(cf, model)    
  
  
def get_permutation_importance_plot(model_type, data_dict, m):
    
    model_name = model_type+"_"+m+".p"
    model_fit = pickle.load(open('models/%s'%model_name, mode='rb'))
    
    X_train_scaled, X_test_scaled, y_train, y_test = preprocess(path=datasets[m]["path"], 
                                                                exc_cols=datasets[m]["exc_cols"], 
                                                                rows=datasets[m]["rows"],
                                                                target=datasets[m]["target"])

    perm_imp = permutation_importance(estimator=model_fit, X=X_test_scaled, y=y_test, 
                                      n_repeats=5, scoring="balanced_accuracy")
    sorted_idx = perm_imp.importances_mean.argsort()

    fig, ax = plt.subplots()
    ax.boxplot(perm_imp.importances[sorted_idx].T,
               vert=False, labels=X_test_scaled.columns[sorted_idx])
    ax.set_title("Permutation Importances (test set)")
    fig.tight_layout()
    plt.show()