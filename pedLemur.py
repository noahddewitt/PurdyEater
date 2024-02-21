import re

#For a given vector of line names (supplied as one-column tsv),recursively climbs
#Historic pedigree tree to find all related lines
#Then prints them out in the "proper" format.

inLineFile_name = ""
inPedFile_name = ""
outPedFile_name = ""

inLines = []

inLineFile = open(inLineFile_name, "r")
inLineList = inLineFile.readlines()
inLineList = inLineList[1:len(inLineList)]

for line in inLineList:
    lineMatch = re.match(r'^(.+)\n$', line)
    inLines.append(lineMatch.group(1))
    #some lines need to be hyphenated
    lineNameMatch = re.match(r'^([a-zA-Z]+)(\d+)', lineMatch.group(1))
    
    if lineNameMatch is not None:
        inLines.append(lineNameMatch.group(1) + "-" +  lineNameMatch.group(2))

pedFile = open(inPedFile_name, "r")

pedList = ped.readlines()
pedList = pedList[1:len(pedList)]

#Match each line to populate dictionary with the pedigree
#Each progeny has parent(s) saved in an array

pedDict = {}

for line in pedList:
    lineMatch = re.match(r'^(.+)\t(.+)\n$', line)
    
    if lineMatch is not None:
        prog = lineMatch.group(1)
        prnt = lineMatch.group(2)
        if prog not in pedDict:
            pedDict[prog] = [prnt]
        else:
            pedDict[prog].append(prnt)

reducedPedDict = {}

def getParents(lineName):
    if lineName in pedDict:
        print(lineName)
        parents = pedDict[lineName]
        if lineName not in reducedPedDict:
            reducedPedDict[lineName] = parents
        for prnt in parents:
            getParents(prnt)

for line in inLines:
    getParents(line)

with open(outPedFile_Name, "a") as outFile:
    outFile.write("Line\tParent1\tParent2\n")
    for lineName in reducedPedDict.keys():
        linePrnts = reducedPedDict[lineName]
        if len(linePrnts) == 2:
            outFile.write(lineName + "\t" + linePrnts[0] + "\t" + linePrnts[1] + "\n")
        elif len(linePrnts) == 1:
            outFile.write(lineName + "\t" + linePrnts[0] + "\tNA\n")
