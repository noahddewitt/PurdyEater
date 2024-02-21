library(readxl)

famDF <- read_excel("/data/FieldAnalyses/24/Family_Predictions/24Nurs_Families.xlsx")

pedVec <- famDF$Pedigree

#Now, so we can use it later, let's write a function to parse this thing. 
#We have to deal with, basically, either a 2 way or three way cross. Simplifies things a bit.

#Three wway crosses will have the form:
#A//B/C
#OR
#LADDDDD (A/B) / C
#OR
#A / LADDDDD (B/C)

#We could format this as a three-column format, in which case we'd move LADDDD to
#Its own row with a B/C ped. 

#To make averaging MP value together simpler, let's keep that as a separate set in the resulting DF

#Part of parsed pedigree issues is those of the form:
#LA21200 (LA12275LDH-56 / GA15VDH---18LE43F)/LA21202(LA12275LDH-56 / KWS246)
#Need to write additional if/then for double-F1 top cross. 

split_ped <- function(crossString, pedString) {
  #Get rid of all white spaces, alon with ",F1"s you see in top crosses
  cleanString <- gsub("\\s*", "", pedString)
  cleanString <- gsub("\\,F1", "", cleanString)
  
 
  #TODO -- after checking it's a complex cross, recursively build out.
  #Probly need to split the below code out into a separate function and call it
  #From within a while loop
  if (grepl("\\/[0-9]\\/", cleanString)) {
    #what is the highest number of those "/n/" complex crosses
    crossLevels <- regmatches(cleanString,gregexpr("\\/[0-9]\\/",cleanString))[[1]]
    crossLevels <- gsub("/", "", crossLevels)
    max_crossLevel <- max(as.numeric(crossLevels))
  }
 
  #We can't just "inflate" a BC component into a Purdy, because would have to re-order
  #Instead, we can replace the BC part of pedigree with a generated accession name, and create
  #a 3-column pedigree record for that BC part
  #Still likely have to call this when a) pedigree is a simple BC or b) as part of simplifying complex purdy
  if (grepl("\\*", Pedigree)) {
    pedString <- convert_back_cross()
  }
  
  #If it's not complex cross (>2) deal with any top crosses
  if(grepl("\\(.*\\/.*\\)|\\/\\/", cleanString)) {
    if (grepl("\\(.*\\/.*\\)", cleanString)) {
      pedRoot <- gsub("\\(.*\\/.*\\)", "", cleanString)
      
      p1 <- gsub("[A-Z,a-z,0-9,-]+\\(.*\\)", "", cleanString)
      p1 <- gsub("\\/", "", p1) #I think by doing it two-step removes issue with directionality
      
      #p1 will be empty if it's a four-way cross.
      if (p1 == "") {
        
        str1 <- gsub("\\)/.*$", "", cleanString) #Parenth prevents stripping from p1's sub-ped
        str2 <- gsub("^.*\\)/", "", cleanString)
        
        f1_1Name <- regmatches(str1, regexpr("[A-Z,a-z,0-9,-]+\\(", str1, perl=TRUE))
        f1_1Name <- gsub("\\(", "", f1_1Name) 
        
        f1_2Name <- regmatches(str2, regexpr("[A-Z,a-z,0-9,-]+\\(", str2, perl=TRUE))
        f1_2Name <- gsub("\\(", "", f1_2Name) 
        
        f1_1P1 <- gsub("^.*\\(", "", gsub("\\/.*$", "", str1))
        f1_1P2 <- gsub("^.*\\/", "", str1)
        
        f1_2P1 <- gsub("^.*\\(", "", gsub("\\/.*$", "", str2))
        f1_2P2 <- gsub("\\)$", "", gsub("^.*\\/", "", str2))
        
        newPedDF <- data.frame(Id = c(f1_1Name, f1_2Name, crossString),
                               P1 = c(f1_1P1, f1_2P1, f1_1Name),
                               P2 = c(f1_1P2, f1_2P2, f1_2Name),
                               Gen = c("F1", "F1", "Root"),
                               baseStr = c(pedString, pedString, pedString))
        
        return(newPedDF)
        
      } else {
        f1Name <- regmatches(cleanString, regexpr("[A-Z,a-z,0-9,-]+\\(", cleanString, perl=TRUE))
        f1Name <- gsub("\\(", "", f1Name) 
        
        pedF1 <- regmatches(cleanString, regexpr("\\(.*\\)", cleanString, perl=TRUE))
        pedF1 <- gsub("\\(|\\)", "", pedF1)
        f1P1 <- gsub("\\/.*$", "", pedF1)
        f1P2 <- gsub("^.*\\/", "", pedF1)
        
        newPedDF <- data.frame(Id = c(f1Name, crossString),
                               P1 = c(f1P1, p1),
                               P2 = c(f1P2, f1Name),
                               Gen = c("F1", "Root"),
                               baseStr = c(pedString, pedString))
        
        return(newPedDF)
      }
    } else {
      p1 <- gsub("\\/\\/.*$", "", cleanString)
      p2 <- gsub("^.*\\/\\/", "", cleanString)
      
      isF1 <- grepl("/", c(p1, p2))
      
      if (sum(isF1) == 1) {
        
        pF1 <- c(p1, p2)[which(isF1)]
        p1 <- c(p1, p2)[which(!isF1)]
        
        f1P1 <- gsub("\\/.*$", "", pF1)
        f1P2 <- gsub("^.*\\/", "", pF1)
          
        f1Name <- gsub("\\/", "@", pF1)
        
        newPedDF <- data.frame(Id = c(f1Name, crossString),
                               P1 = c(f1P1, p1),
                               P2 = c(f1P2, f1Name),
                               Gen = c("F1", "Root"),
                               baseStr = c(pedString, pedString))
        
        return(newPedDF)
        
      } else if (sum(isF1) == 2) {
        f1_1Name <- paste0(crossString, "_F1_1")
        f1_2Name <- paste0(crossString, "_F1_2")
        
        f1_1P1 <- gsub("\\/.*$", "", p1)
        f1_1P2 <- gsub("^.*\\/", "", p1)
        
        f1_2P1 <- gsub("\\/.*$", "", p2)
        f1_2P2 <- gsub("^.*\\/", "", p2)
        
        newPedDF <- data.frame(Id = c(f1_1Name, f1_2Name, crossString),
                               P1 = c(f1_1P1, f1_2P1, f1_1Name),
                               P2 = c(f1_1P2, f1_2P2, f1_2Name),
                               Gen = c("F1", "F1", "Root"),
                               baseStr = c(pedString, pedString, pedString))
        
        return(newPedDF)
        
      } else {
        return("ERROR")
      }
    }
  } else {
    p1 <- gsub("\\/.*$", "", cleanString)
    p2 <- gsub("^.*\\/", "", cleanString)
    return(data.frame(Id = crossString, P1 = p1, P2 = p2, Gen = "Root", baseStr = pedString))
  }
}

#Converts backcrosses to move BC information into ped tree -- so:
#The place to do this is in the simple pedigrees -- we want to only run this function when we get to the backcross level!
#So Genotype | P1 | 2*P2 -> Genotype | P1_BC1 | P2
#                           P1_BC1   |  P1    | P2
#backXConv <- function(prog, donPar, recPar, numFirst) {
convert_back_cross <- function(backCrossString) {
  #Identify backcross 
  p1 <- gsub("\\/(.*)", "", backCrossString)
  p2 <- gsub("(.*)\\/", "", backCrossString)
  
  if (grepl("\\*", p1) & (!grepl("\\*", p2))) {
    pRec <- p1
    pDon <- p2
  } else if ((!grepl("\\*", p1)) & grepl("\\*", p2)) {
    pRec <- p2
    pDon <- p1
  } else {
    return("Error: Invalid BC string")
  }
  
  #Technically this allows us to parse incorrect Purdy notation pedigrees
  #Hypothetically, you could have a BC over 9. Not that I've seen that myself...
  if (grepl("^\\d{1,2}\\*", pRec)) {
    bcMatch <- regmatches(pRec,regexec("(.+)\\*(\\d)$",pRec))[[1]]
    bcNum <- as.numeric(bcMatch[2])
    pBC <- bcMatch[3]
  } else if (grepl("\\*\\d{1,2}$", pRec)) {
    bcMatch <- regmatches(pRec,regexec("(.+)\\*(\\d)$",pRec))[[1]]
    bcNum <- as.numeric(bcMatch[3])
    pBC <- bcMatch[2]
  } else {
    return("Error: Invalid BC String")
  }
  
  oldName <- paste0(pDon, "@", pBC)
  
  #I simply don't believe that any meaningful information on female/male
  #ends up preserved in BC notation
  newPed <- data.frame(lineName = oldName, parentOne = pDon, parentTwo = pBC)
  
  for (i in c(2:bcNum)) {
    newName <- c(paste0(oldName, "@", pBC))
    newPed <- rbind(newPed, c(lineName = newName, parentOne = oldName, parentTwo = pBC))
    
    oldName <- newName
  }
  #newPed[bcNum,1] <- prog
  return(newPed)
}

pedList <- mapply(split_ped, crossString = famDF$Fam_ID, pedString = famDF$Pedigree)

pedDF <- do.call("rbind", pedList)

###Lot of overlap. Save the important.

pedDF <- select(pedDF, Id, P1, P2) %>% distinct() %>% 
  arrange(Id)


dupIds <- unique(pedDF$Id[duplicated(pedDF$Id)])
pedDF_dups <- filter(pedDF, Id %in% dupIds)

#W/ bug fixes, only a couple, and it's just issues with synonyms.

pedDF <- filter(pedDF, !is.na(P1))
pedDF <- filter(pedDF, !(Id == "LA19456" & P2 == "GA-18LE43"), 
                !(Id == "LA17109" & P1 == "GA12E6DGROW3500"))

unique(pedDF$Id[duplicated(pedDF$Id)])

write_csv(pedDF, "Nurs24_parsedPeds_draftAug1523.csv")
