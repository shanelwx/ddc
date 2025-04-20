#### Attempt to convert DDC.R to DDC.py
#### Quick conversion, needs to be looked through


import pandas as pd
import numpy as np
import os
import time

# Clear environment & Timer
# Start timer
start_time = time.time()

# Functions
# Radians to degree function
def rad(r):
    return r * np.pi / 180

# Set working directories
wd_db = "T:/DDC/R/csv/database"
wd_export = "T:/DDC/R/csv/exports/DDC"  # export location
wd_lfm = "M:/Final/ACQUIRE/Exports/OKD/LFM"
wd_input = "T:/DDC/R/csv/DDC"  # import location

# Pull survey and collar data from M drive
os.chdir(wd_lfm)
OKD_surv = pd.read_csv("LFM_OKD_MGA2020-53_sur.csv")
OKD_col = pd.read_csv("LFM_OKD_MGA2020-53_col.csv")

# Change directory to read inputs
os.chdir(wd_input)
# Geo input csvs
Pjt_surv = pd.read_csv("Projection survey.csv")
plan_csv = pd.read_csv("Planned Survey.csv")
targ_csv = pd.read_csv("Targets.csv")  # Change to all targets that team uses for LF?

# Initialize and clean dataframes
OKD_surv.columns = OKD_surv.columns.str.lower().str.replace(' ', '_')
OKD_col.columns = OKD_col.columns.str.lower().str.replace(' ', '_')
Pjt_surv.columns = Pjt_surv.columns.str.lower().str.replace(' ', '_')
plan_csv.columns = plan_csv.columns.str.lower().str.replace(' ', '_')
plan_csv.rename(columns={plan_csv.columns[0]: "bhid"}, inplace=True)
targ_csv.columns = targ_csv.columns.str.lower().str.replace(' ', '_')
targ_csv.rename(columns={targ_csv.columns[4]: "bhid"}, inplace=True)
Pjt_col = OKD_col

# QAQC check source
temp_check1 = Pjt_surv.sort_values(by='md', ascending=False).drop_duplicates(subset='bhid', keep='first')[['bhid', 'md']]
temp_check2 = targ_csv[['bhid', 'pad_id', 'target_id']]
temp_check3 = plan_csv.sort_values(by='md', ascending=False).drop_duplicates(subset='bhid', keep='first')[['bhid', 'md']]

check1 = temp_check1.merge(temp_check3, on='bhid', suffixes=('_proj', '_plan'))
check1.rename(columns={'md_proj': 'proj_maxmd', 'md_plan': 'plan_maxmd'}, inplace=True)

check2 = check1[check1['plan_maxmd'].isna()][['bhid']]
if not check2.empty:
    print("Missing in Planned Survey.csv")  # Bhids in pjt but not in planned csv
    raise ValueError(f"bhid - {', '.join(check2['bhid'].astype(str))}")

check3 = check1.merge(temp_check2, on='bhid', how='left')
check3 = check3[check3['target_id'].isna()][['bhid']]
if not check3.empty:
    print("No targets for")  # Bhids in pjt but not in target csv
    raise ValueError(f"bhid - {', '.join(check3['bhid'].astype(str))}")

check4 = check1.merge(temp_check2, on='bhid')
print("QAQC on source csvs completed, processing these holes...")  # QAQC message checkpoint
print(check4)

# 1. Produce projection holes - Joining contractors projection to actual surveys in db
# Filter
# Obtain latest md from OKD_surv
Pjt_merge_depth = OKD_surv.sort_values(by='depth', ascending=False).drop_duplicates(subset='bhid', keep='first')[['bhid', 'depth']]
# Merge OKD_surv with Pjt surv to remove outdated Pjt'ed surv
surv_join1 = Pjt_surv.merge(Pjt_merge_depth, on='bhid', how='left')
surv_join2 = surv_join1[['bhid', 'md', 'dip', 'gn_azi', 'depth']].copy()
surv_join2['filtering'] = surv_join2['md'] - surv_join2['depth']
surv_join2 = surv_join2[surv_join2['filtering'] > 0][['bhid', 'md', 'dip', 'gn_azi']]
surv_append = OKD_surv[['bhid', 'depth', 'azimuth', 'dip']].copy()
# Rename
surv_append.rename(columns={'depth': 'md', 'azimuth': 'gn_azi'}, inplace=True)

# Initialize Pjt_col EOH depth to max md from Pjt_surv
col_join1 = surv_join2.sort_values(by='md', ascending=False).drop_duplicates(subset='bhid', keep='first')[['bhid', 'md']]
Pjt_col_final = Pjt_col.merge(col_join1, on='bhid', how='left')
Pjt_col_final['eoh'] = np.where(Pjt_col_final['md'].isna(), Pjt_col_final['eoh'], Pjt_col_final['md'])
Pjt_col_final = Pjt_col_final.drop(columns=['md'])

# Final projection survey and collar, bind OKD_surv and Pjt_surv
Pjt_surv_final = pd.concat([surv_join2, surv_append]).groupby('bhid').apply(lambda x: x.sort_values(by='md')).reset_index(drop=True)