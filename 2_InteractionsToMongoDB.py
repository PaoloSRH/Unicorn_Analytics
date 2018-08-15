import json
import lxml.etree
from pymongo import MongoClient
import os
import pandas as pd

mongoClient = MongoClient()
db = mongoClient.mails 
collection = db.interactions

df = pd.read_excel('CategoryMapping.xlsx')

dir_path = os.path.dirname(os.path.realpath(__file__))

xml_file = os.path.join(dir_path, 'omq_public_email_data', 'omq_public_interactions.xml')
e = lxml.etree.parse(xml_file)

for interaction in e.find('interactions').findall('interaction'):
    metadata = interaction.find('metadata')
    print('Processing interaction '+metadata.find('id').text+'...')
    json_str = '{'
    json_str += '"businessScenario":"'+metadata.find('businessScenario').text+'", '
    json_str += '"id":"'+metadata.find('id').text+'", '
    json_str += '"date":"'+metadata.find('date').text+'", '
    json_str += '"categories":['
        
    #process categories
    categories = metadata.find('category').text.split(',');
    for category in categories:
        json_str += '{"id":"'+category+'", "text":"'+interaction.find('text').xpath('./relevantText[re:test(@goldCategory, "(^|.*,)'+category+'(,.*|$)")]', namespaces={'re': 'http://exslt.org/regular-expressions'})[0].text.replace('\\', '\\\\').replace('\n', '\\n').replace('\r', '\\r').replace('"', '\\"')+'"}, '
    json_str = json_str[:-2]+'], '
    
    json_str += '"keyword":"'+metadata.find('keyword').text+'", '
    
    #add the complete message to the "text" attribute of the interaction
    completeMessage = interaction.find('text').text
    for relevantText in interaction.find('text').findall('relevantText'):
        completeMessage += relevantText.text+relevantText.tail
    completeMessage = completeMessage.replace('\\', '\\\\').replace('\n', '\\n').replace('\r', '\\r').replace('"', '\\"');
    json_str += '"text":"'+completeMessage+'"}'
    
    #print(json_str)
    collection.insert_one(json.loads(json_str))
    print('Done!')
print('Run completed!')
