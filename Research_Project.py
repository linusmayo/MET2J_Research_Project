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
         if ("ontology/birthDate" or "ontology/birthYear") and ("ontology/birthPlace_label" or "ontology/nationality_label" or "ontology/stateOfOrigin_label") and "ontology/instrument_label" in person:
            for instrument in instruments:
                if person["ontology/instrument_label" ] == instrument:
                        
                        if (person["ontology/birthDate"] >= datetime.date(1/1/1900) and person["ontology/birthDate"] <= datetime.date(31/12/1999)): 
                            total_dictionary["20th Century"] = 


                        if (person["ontology/birthDate"] >= datetime.date(1/1/2000) and person["ontology/birthDate"] <= datetime.date(31/12/2099)): 
                            total_dictionary["21st Century"] =