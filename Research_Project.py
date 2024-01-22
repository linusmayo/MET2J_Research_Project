import json
import datetime

letters = ("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

relevantKeys = []
total_dictionary = {}
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

        for instrument in instruments:
            if person["ontology/instrument_label" ] == instrument:
                                
                if (person["ontology/birthDate"] >= datetime.date(1/1/1900) and person["ontology/birthDate"] <= datetime.date(31/12/1999)): 
                    total_dictionary["20th Century"] = 

                if (person["ontology/birthDate"] >= datetime.date(1/1/2000) and person["ontology/birthDate"] <= datetime.date(31/12/2099)): 
                    total_dictionary["21st Century"] =
