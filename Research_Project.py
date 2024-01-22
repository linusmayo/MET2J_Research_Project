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
            ("ontology/associatedMusicalArtist_label" in person
             or "ontology/profession_label" in person 
             or "ontology/instrument_label" in person 
             or "ontology/occupation_label" in person) and \
            ("ontology/birthPlace_label" in person 
             or "ontology/nationality_label" in person
             or "ontology/stateOfOrigin_label" in person):
             relevantKeys.append(person)
    
for person in relevantKeys:
    person_new = {}
    person_new["Name"] = person["title"]
    person_new["Instrument"] = person["ontology/instrument_label"]
    person_new["Country"] = person["ontology/stateOfOrigin_label"]
    total_list.append(person_new)
        
print(total_list)