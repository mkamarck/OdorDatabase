#python script
#This uses the api package chemspider to maybe spit out information about chemical compounds

#import the ChemSpider API package
from chemspipy import ChemSpider
import csv #for importing data
import os #for working directory
import pandas as pd #apparently this is better for making a working directory
import numpy as np #this is something else for dataframes

#define the chemSpider object with my security code 
cs = ChemSpider('6c2e700b-6a92-4551-9cc1-70f28c021f23')

#example of how to get a compound info from the ChemSpider ID
#compound = cs.get_compound(2157)
#print(compound.smiles)

#working directory info
#cwd = os.getcwd()
os.chdir('/Users/mkamarck/Documents/chemspipy') #change working directory

#figure out how to make a dataframe or matrix variable
f = open('SymriseOdorList_forChemSpiPy.csv')
csv_f = csv.reader(f)
for row in csv_f:
  print row
  
#  660002', 'ACETANISOLE CRYST.', '100-06-1', 'KETONE', 'S'
  #I can get chem ID from the CAS number
#for row in csv_f:
#	print row[3]
	
	
#import with pandas
chemData = pd.read_csv('SymriseOdorList_forChemSpiPy.csv')

chemCAS = pd.read_csv('Symrise.CAS.csv')


#chemCAS2 = chemCAS[:5] #trial


##assigns an empty variable to put CID into
chemCAS['csid'] = 0	
i = 0
#writes loop to put search csid into row
for index, row in chemCAS.iterrows():
	CAS = row[5]
	for result in cs.search(CAS):
		csid = result.csid
		chemCAS.ix[i, 'csid'] = csid
	i = i+1

#write output to csv
chemCAS.to_csv('chemCAS_csidList.csv')

#do the same thing with searching with words instead of with CAS numbers
chemNoCAS = pd.read_csv('Symrise.noCAS.csv')
chemNoCAS['csid'] = 0	
i = 0
#writes loop to put search csid into row
for index, row in chemNoCAS.iterrows():
	CAS = row[2]
	for result in cs.search(CAS):
		csid = result.csid
		chemNoCAS.ix[i, 'csid'] = csid
	i = i+1
	
#write to csv
chemNoCAS.to_csv('chemNoCAS_csidList.csv')


#how to search with chemspipy
#for result in cs.search('ACETANISOLE CRYST'):
#	print(result.csid)


#read back in final list which is the two chem lists combined
#then extract SMILE and MW information using the csid 
#boom

df = pd.read_csv("chem_cidList_total.csv")
#make columns for new data
df['MW'] = 0
df['SMILE'] = 0

#write a loop that fills in these variables
i = 0
for index, row in df.iterrows():
	csid = row[7]
	if csid != 0:
		compound = cs.get_compound(csid)
		MW = compound.molecular_weight
		SMILE = compound.smiles
		df.ix[i, 'MW'] = MW
		df.ix[i, 'SMILE'] = SMILE
		i = i + 1
	else:
		i = i + 1
		
#write to csv
df.to_csv('ChemID_MW_SMILES_everything.csv')

	
	
	
	
