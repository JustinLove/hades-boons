# coding=utf-8
import sjson
import json
import luadata

with open("D:/Games/Epic Games/Hades/Content/Game/Text/en/HelpText.en.sjson", 'r', encoding="utf8") as sj:
    data = sjson.loads(sj.read())
    with open("output/HelpText.en.json", "w", encoding='utf8') as j:
        j.write(json.dumps(data, indent=2))
    luadata.write("output/HelpText.en.lua", data, encoding='utf-8', indent="\t")

with open("D:/Games/Epic Games/Hades/Content/Game/Animations/GUIAnimations.sjson", 'r', encoding="utf8") as sj:
    data = sjson.loads(sj.read())
    with open("output/GUIAnimations.json", "w", encoding='utf8') as j:
        j.write(json.dumps(data, indent=2))
    luadata.write("output/GUIAnimations.lua", data, encoding='utf-8', indent="\t")
