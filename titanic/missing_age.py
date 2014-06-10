


import csv as csv
import numpy as np

csv_file_object = csv.reader(open('train.csv', 'rb')) 	# Load in the csv file
header = csv_file_object.next() 						# Skip the fist line as it is a header
data=[] 												# Create a variable to hold the data

for row in csv_file_object: 							# Skip through each row in the csv file,
    data.append(row[0:]) 								# adding each row to the data variable
data = np.array(data) 									# Then convert from a list to an array.

def expected_age(x):
	if x.find("Mr")!=-1:
		return 40
	elif x.find("Mrs")!=-1:
		return 35
	elif x.find("Miss")!=-1:
		return 20
	else:
		return 0

for item in data:
    if item[5]=="":
        item[5]=expected_age(item[3])



predictions_file = open("train_age.csv", "wb")
predictions_file_object = csv.writer(predictions_file)
predictions_file_object.writerow(header)	# write the column headers
														# or else if male,
predictions_file_object.writerows(data)			
												
predictions_file.close()

