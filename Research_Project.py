import json
import datetime

letters = ("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

relevantKeys = []
total_list = []
instruments = ("Drums", "Guitar", "Piano", "Keyboard", "Voice", "Bass","Violin","Cello","Bass guitar")

for letter in letters:
     file = open(f"{letter}_people.json")
     people = json.load(file)
     for person in people:
        if ("ontology/birthDate" in person 
            or "ontology/birthYear" in person) and \
            ("ontology/instrument_label" in person) and \
            ("ontology/birthPlace_label" in person 
             or "ontology/nationality_label" in person):
             relevantKeys.append(person)

for person in relevantKeys:
    person_new = {}
    person_new["Name"] = person["title"]
    if "ontology/birthDate" in person: 
        person_new["Date of Birth"] = person["ontology/birthDate"]
    elif "ontology/birthYear" in person:
        person_new["Date of Birth"] = person["ontology/birthYear"]
    if "ontology/birthPlace" in person: 
        person_new["Country"] = person["ontology/birthPlace"]
    elif "ontology/nationality_label" in person: 
        person_new["Country"] = person["ontology/nationality_label"]
    person_new["Instrument"] = person["ontology/instrument_label"] 
    
    total_list.append(person_new)
        
print(len(total_list))