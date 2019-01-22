import random

durchlauefe = 10000
kandidaten = ["Marty","Peter","Dieter","Klaus"]
string_file = ""


text_file = open("Output.xml", "w")
string_file += "<VOTERS>\n"
for x in range(0,durchlauefe):
    string_file += "<VOTER>\n"
    for y in range(0,len(kandidaten)):   
        rnd = random.randint(0,5)
        string_file += "<VOTE CANDIDATE=\""+kandidaten[y]+"\" RATING=\""+str(rnd)+"\"></VOTE>\n"
    string_file += "</VOTER>\n"
    text_file.write(string_file)
    string_file = ""
string_file += "</VOTERS>\n"


text_file.write(string_file)
text_file.close()
