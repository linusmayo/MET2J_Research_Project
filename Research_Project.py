import json

letters = ("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

relevantKeys = []

for letter in letters:
     file = open(f"{letter}_people.json")
     people = json.load(file)
     for person in people:
         if ("ontology/birthDate" or "ontology/birthYear") and ("artist" or "ontology/associatedMusicalArtist_label" or "ontology/occupation_label" or "ontology/profession_label") and ("ontology/birthPlace_label" or "ontology/nationality_label" or "ontology/stateOfOrigin_label") and ("ontology/instrument_label" or "ontology/occupation_label") in dic:
             relevantKeys.append(person)