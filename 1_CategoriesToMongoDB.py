import json
import xml.etree.ElementTree
from pymongo import MongoClient
import os

mongoClient = MongoClient()
db = mongoClient.mails 
collection = db.categories

dir_path = os.path.dirname(os.path.realpath(__file__))

xml_file = os.path.join(dir_path, 'omq_public_email_data', 'omq_public_categories.xml')
e = xml.etree.ElementTree.parse(xml_file).getroot()

for categoryGroup in e.find('categories').findall('categoryGroup'):
    print('Processing categoryGroup '+categoryGroup.get('id')+'...')
    json_str = '{"id":"'+categoryGroup.get('id')+'", "categories":['
    
    for category in categoryGroup.findall('category'):
        #print(category.get('id'))
        
        textCount = category.get('textCount')
        if textCount is None:
            textCount = ''
            
        json_str = json_str+'{"id":"'+category.get('id')+'", "textCount":"'+textCount+'", "text":"'+category.text.strip('\n').rstrip().replace('"', '\\"')+'"},'
    json_str = json_str[:-1]+']}'
    #print(json_str)
    collection.insert_one(json.loads(json_str))
    print('Done!')
print('Run completed!')

# to get a categoryGroup based on the categoryId:
# db.categories.find({"categories":{$elemMatch:{"id":"63"}}})
