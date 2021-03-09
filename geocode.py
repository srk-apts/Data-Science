from IPython.display import display
from census import Census
import time
import us
import collections
import censusdata as cd
import censusgeocode
import pandas as pd
import numpy as np
import os
import ast
import requests
import time
import glob
import zipfile
from requests.adapters import HTTPAdapter
from requests.packages.urllib3.util.retry import Retry
from pyusps import address_information
import csv




def create_dict(df, interval=1000):
    df = df.dropna()
    batch_ind = np.concatenate((np.arange(interval, df.shape[0], interval), np.array([df.shape[0]])))
    df_list = []
    df_dict = {}
    for i in range(df.shape[0]):
        d = dict(df.iloc[i,])
        df_list.append(d)
    return(batch_ind, df_list)


def verify_addresses_usps(df, start=0, user_id='045AON003285'):
    
    val_ind, val_dict = create_dict(df, interval=5)
    
    counter = 0
    clean_dict=[]
    skip_ls = []
    
    for i in val_ind[start:]:
        
        strt = i-5
        stp = i
        try:
            dict_temp = address_information.verify(user_id, *val_dict[strt:stp])
        except:
            pass
        
        for j in range(len(dict_temp)):
            if type(dict_temp[j])==collections.OrderedDict: # type(d)==ValueError
                clean_dict.append(dict(dict_temp[j]))
                clean_dict[-1]['id'] = val_dict[strt+j]['id']
        
        counter += 1
        if counter%20:
            time.sleep(3)
            
    val_df = pd.DataFrame(clean_dict)
    return(val_df)


def get_geographies(df, batch_list):
    
    benchmark = ['Public_AR_Current', 'Public_AR_ACS2018','Public_AR_Census2010']
    vintage = ['Current_Current', 'ACS2018_ACS2018','Census2010_Census2010']
    address_dict = []

    for i in range(len(benchmark)):
        cg = censusgeocode.CensusGeocode(benchmark=benchmark[i], vintage=vintage[i])
        address_dict = address_dict+(cg.addressbatch(batch_list))

    address_df = pd.DataFrame(address_dict)
    address_match_df = address_df[address_df['match']==True]
    df_gb = address_match_df.groupby(by='id') 
    address_match_unique_df = pd.DataFrame(df_gb.max()).reset_index()
    
    id_ls = list(address_match_unique_df["id"])
    address_val_df = df[df["id"].isin(id_ls)==False].rename(columns={"street":"address"})
    
    return(address_match_unique_df, address_val_df)


def write_geo_batch_files(add_df, write_path, start_index=0):
    
    """
    This function takes a dataframe containing the addresses and gets the geocoding info from census API. 
    It also validates the addresses that errored out, with the USPS API and then re-runs those through the census API.
    
    add_df: DataFrame object containing following columns: ["id", "street", "city", "state", "zip"] ("id" is optional)
    write_path: Path where it will write the batch files (1000 rows each)
    start_index: This is just to de-bug in case the function errors out, so we don't have to start from the beginning.
    Look at the last index that was printed and pass it to this argument by dividing it by 1000.
    So index that was printed before the error occured was 245000, we should pass 245 to this argument when resuming. 
    
    """
    
    counter = 0
    
    index_list, address_list = create_dict(add_df)
    
    stop_index = len(index_list)
    add_skip = []
    
    for i in index_list[start_index:stop_index]:
        start = i-1000
        stop = i
        
        # create batch and get geographies
        batch_add_df = add_df[start:stop]
        batch_add_list = address_list[start:stop]
        try:
            address_match_unique_df, address_val_df = get_geographies(df=batch_add_df, batch_list=batch_add_list)
            
            # verify addresses that didn't return geocoding
            val_df = verify_addresses_usps(df=address_val_df)
            cols = {'address': 'street', 'state': 'state', 'city': 'city', 'zip5': 'zip', 'id': 'id'}
            val_add_df = val_df.rename(columns=cols)[cols.values()]

            # 2nd pass after validation addresses
            val_add_ind, val_add_dict_list = create_dict(val_add_df)
            val_add_2nd_pass, _ = get_geographies(df=val_add_df, batch_list=val_add_dict_list)

            # join the first and second pass DF and write it to a file
            address_df = pd.concat([address_match_unique_df, val_add_2nd_pass], axis=0).reset_index()
            address_df.to_csv(write_path%i, sep = ',', 
                              index=False, encoding='utf-8')
            
        except:
            add_skip.append(i)
            pass
        
        counter += 1
        if counter%5:
            time.sleep(2)
        print(i)
    
    print("Skipped batch numbers:")
    print(add_skip)
