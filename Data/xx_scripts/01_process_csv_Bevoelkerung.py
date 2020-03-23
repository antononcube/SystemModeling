#!/usr/bin/env python
# encoding: utf-8

"""
File Name   : 01_process_csv_Bevoelkerung
Project Name: SystemModeling
Description : process the file Data/00_raw/csv_Bevoelkerung/Zensus11_Datensatz_Bevoelkerung.csv
    to extract the correct columns and ids
Author      : ottor
Created     : 22.03.20 11:28
"""

# IMPORTS
import pandas as pd
import pathlib as pl
import numpy as np

# GLOBAL VARIABLES
ENCODING = 'latin_1'
PATH_TO_CSV_DATASET = pl.Path('../00_raw/csv_Bevoelkerung/Zensus11_Datensatz_Bevoelkerung.csv')
PATH_TO_OUTPUT = pl.Path('../01_processed/socioeconomics_districts.csv')
SELECTION = {'AGS_12': 'municipality_key',
             'RS_Land': 'state',
             'RS_RB_NUTS2': 'nuts2',
             'RS_Kreis': 'district',
             'RS_VB': 'municipality_agg',
             'RS_Gem': 'municipality',
             'Name': 'name',
             'Reg_Hier': 'hierarchy_index',
             'AEWZ': 'population',
             #'DEM_3.1': 'population',
             'DEM_3.2': 'male',
             'DEM_3.3': 'female',
             #'DEM_3.4': 'under10',
             'DEM_3.5': 'male_under10',
             'DEM_3.6': 'female_under10',
             #'DEM_3.7': '10to19',
             'DEM_3.8': 'male_10to19',
             'DEM_3.9': 'female_10to19',
             #'DEM_3.10': '20to29',
             'DEM_3.11': 'male_20to29',
             'DEM_3.12': 'female_20to29',
             #'DEM_3.13': '30to39',
             'DEM_3.14': 'male_30to39',
             'DEM_3.15': 'female_30to39',
             #'DEM_3.16': '40to49',
             'DEM_3.17': 'male_40to49',
             'DEM_3.18': 'female_40to49',
             #'DEM_3.19': '50to59',
             'DEM_3.20': 'male_50to59',
             'DEM_3.21': 'female_50to59',
             #'DEM_3.22': '60to69',
             'DEM_3.23': 'male_60to69',
             'DEM_3.24': 'female_60to69',
             #'DEM_3.25': '70to79',
             'DEM_3.26': 'male_70to79',
             'DEM_3.27': 'female_70to79',
             #'DEM_3.28': '80andover',
             'DEM_3.29': 'male_80andover',
             'DEM_3.30': 'female_80andover',
             'ERW_4.1': 'working_population',
             'ERW_4.2': 'agriculture_forests_fishery',
             #'ERW_4.3': 'production_industry',
             'ERW_4.4': 'mining_and_industry',
             'ERW_4.5': 'energy_water_waste',
             'ERW_4.6': 'construction',
             #'ERW_4.7': 'trade_tourism_traffic',
             'ERW_4.8': 'trade_carmechanics_tourism',
             'ERW_4.9': 'traffic_storage_communication',
             #'ERW_4.10': 'other_services',
             'ERW_4.11': 'finance_insurance',
             'ERW_4.12': 'property',
             'ERW_4.13': 'public_administration',
             'ERW_4.14': 'other_services',
             'ERW_4.15': 'unknown',
             }
CATEGORICAL_THRESHOLD = 10

# FUNCTIONS
def read_file(filename):
    """
    reads a the census 2011 dataset (www.zensus2011.de/) and performs multiple clean-ups
    """
    df = pd.read_csv(
            filename,
            sep=';',
            # convert the umlauts
            encoding=ENCODING,
            # make the first column the index
            index_col=0,
            # use only columns needed for further analysis
            usecols=SELECTION.keys(),
            # drop some missing value characters
            na_values=['/', '-'],
        )
    for col in df.columns:
        # some entries are bracketed, we remove these character to be able to transform them into numeric types later on
        df[col] = df[col].astype(str).str.replace('(', '').str.replace(')', '')
        # prepare the df.convert_dtypes() command, numbers -> np.float & rest -> object
        try:
            df[col] = df[col].astype(np.dtype('float'))
        except ValueError:
            df[col] = df[col].astype(object)
    # convert types accordingly and rename columns to something meaningful
    df = df.convert_dtypes().rename(SELECTION, axis=1)
    # this is categorical and used for subsetting later on
    df['hierarchy_index'] = df['hierarchy_index'].astype('category')
    return df

def select_hierarchy(df, hierarchy):
    """
    returns a subset of the dataset (based on hierarchy_index values)
    and removes all columns with less than two unique values
    """
    sel = df[df['hierarchy_index'] == hierarchy]
    return sel[sel.columns[sel.nunique() > 1]]


def write_to_csv(df, filename):
    """
    writes a pandas DataFrame to a csv file
    """
    df.to_csv(filename)

# CLASSES

# SCRIPT
if __name__ == '__main__':
    df = read_file(PATH_TO_CSV_DATASET)
    sel = select_hierarchy(df, df['hierarchy_index'].cat.categories[-1])
    write_to_csv(sel, PATH_TO_OUTPUT)
