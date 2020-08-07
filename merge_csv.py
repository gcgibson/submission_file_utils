import pandas as pd

csv1=pd.read_csv("submission_files/incident/2020-07-26-UMass-MechBayes.csv.clean",dtype=str)
csv2=pd.read_csv("submission_files/cumulative/2020-07-26-UMass-MechBayes.csv.clean",dtype=str)
csv3=pd.read_csv("submission_files/county/2020-07-26-UMass-MechBayes.csv.clean",dtype=str)

total = csv1.append(csv2).append(csv3)

total.to_csv("2020-07-26-UMass-MechBayes.csv",index=False)
