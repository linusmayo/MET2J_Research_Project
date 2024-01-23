import json
import csv

letters = ("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

relevantKeys = []

for letter in letters:
     file = open(f"{letter}_people.json")
     people = json.load(file)
     for person in people:
        if ("ontology/birthDate" in person 
            or "ontology/birthYear" in person) and \
            ("ontology/instrument_label" in person) and \
            ("ontology/birthPlace_label" in person):
             relevantKeys.append(person)

total_list = []

for person in relevantKeys:
    person_new = {}
    person_new["Name"] = person["title"]
    if "ontology/birthDate" in person: 
        person_new["Date of Birth"] = person["ontology/birthDate"]
    elif "ontology/birthYear" in person:
        person_new["Date of Birth"] = person["ontology/birthYear"]
    if "ontology/birthPlace_label" in person: 
        person_new["Country"] = person["ontology/birthPlace_label"]
    elif "ontology/nationality_label" in person: 
        person_new["Country"] = person["ontology/nationality_label"]
    person_new["Instrument"] = person["ontology/instrument_label"] 
    
    total_list.append(person_new)

data_file = open('MET2J_filtered_instruments.csv', 'w', encoding= 'utf-8', newline='')
csv_writer = csv.writer(data_file)
 
count = 0
for data in total_list:
    if count == 0:
        header = data.keys()
        csv_writer.writerow(header)
        count += 1
    csv_writer.writerow(data.values())
 
data_file.close()