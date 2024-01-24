import json
import csv

letters = ("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

relevantKeys = []

for letter in letters:
     file = open(f"{letter}_people.json")
     people = json.load(file)
     for person in people:
        if ("ontology/birthYear" in person) and \
            ("ontology/instrument_label" in person) and \
            ("ontology/birthPlace_label" in person) and \
            ("ontology/genre_label" in person):
            relevantKeys.append(person)

print(len(relevantKeys))

total_list = []
US = ["United States", "Alaska", "Alabama", "Arkansas", "American Samoa", "Arizona", "California", "Colorado", "Connecticut", "District ", "of Columbia", "Delaware", "Florida", "Georgia", "Guam", "Hawaii", "Iowa", "Idaho", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Massachusetts", "Maryland", "Maine", "Michigan", "Minnesota", "Missouri", "Mississippi", "Montana", "North Carolina", "North Dakota", "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "Nevada", "New York", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Virginia", "Virgin Islands", "Vermont", "Washington", "Wisconsin", "West Virginia", "Wyoming"]

for person in relevantKeys:
    person_new = {}
    person_new["Name"] = person["title"]
    person_new["Date of Birth"] = person["ontology/birthYear"]
    if isinstance(person["ontology/birthPlace_label"], list): 
        for i in range(len(person["ontology/birthPlace_label"])):
            if person["ontology/birthPlace_label"][i] in US:
                person_new["US / US_state"] = person["ontology/birthPlace_label"][i]
    else: 
        if person["ontology/birthPlace_label"] in US: 
            person_new["US / US_state"] = person["ontology/birthPlace_label"]
    if isinstance(person["ontology/instrument_label"], list): 
        person_new["Instrument"] = person["ontology/instrument_label"][0]
    else:
        person_new["Instrument"] = person["ontology/instrument_label"] 
    if isinstance(person["ontology/genre_label"], list):
        person_new["Genre"] = person["ontology/genre_label"][0]
    else:
        person_new["Genre"] = person["ontology/genre_label"]
    
    total_list.append(person_new)

with open('MET2J_filtered_instruments.csv', 'w', encoding= 'utf-8', newline='') as data_file:
    csv_writer = csv.DictWriter(data_file, ["Name", "Date of Birth", "Country", "Instrument", "Genre"])
    csv_writer.writeheader()

    for data in total_list:
        csv_writer.writerow(data)