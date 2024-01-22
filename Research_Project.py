import json

letters = ("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

relevantKeys = []

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

print(len(relevantKeys))