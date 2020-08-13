import pandas as pd
date = "2020-08-09"
csv1=pd.read_csv("submission_files/incident/"+date+"-UMass-MechBayes.csv.clean",dtype=str)
csv2=pd.read_csv("submission_files/cumulative/"+date+"-UMass-MechBayes.csv.clean",dtype=str)
csv3=pd.read_csv("submission_files/county/"+date+"-UMass-MechBayes.csv.clean",dtype=str)

total = csv1.append(csv2).append(csv3)

total.to_csv(date + "-UMass-MechBayes.csv",index=False)
