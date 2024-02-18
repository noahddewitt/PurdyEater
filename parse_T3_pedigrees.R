library(BrAPI)

setwd("/data/pedigree/purdy_eater")

wheat <- getBrAPIConnection("T3/Wheat")

#Gets ALL germplasm
resp <- wheat$get("/germplasm", 
                  page="all", pageSize="1000")


saveRDS(resp, "All_T3Public_Germplasm.rds")

fieldsOfInterest <- c("germplasmName", "species", "defaultDisplayName", "synonyms", "pedigree",
                      "accessionNumber", "germplasmDbId", "germplasmPUI")

germDF <- data.frame(matrix(ncol = length(fieldsOfInterest), nrow = 1000 * length(resp$response)))
colnames(germDF) <- fieldsOfInterest

counter <- 1
for (page in resp$content) {
  for (accession in page$result$data) {
    for (field in fieldsOfInterest) {
      if (length(unlist(accession[field])) == 1) {
        germDF[counter, field] <- unlist(accession[field])
      } else if (length(unlist(accession[field])) > 1) {
        germDF[counter, field] <- paste(unlist(accession[field]), collapse = ", ")
      } else {
        germDF[counter, field] <- NA
      }
    }
    counter <- counter + 1
  }
  print(paste0("Accession ", counter, " parsed!"))
}

write.csv(germDF, "accession_records_T3Public_Feb1724.csv", row.names = F)

#There aren't any more than two-way crosses in here.
#I think that's because these are pedigree db outputs.

####Approximate workflow
##A -- Build initial database so that it "works with" T3
#get all T3 accessions... reconstruct 3-column
#get my 3-column
#To compare the two, we have to run a synonym search for ALL in my db...
#We should also look out for T3 ones
#Add all in, remove ones with exact match or imperfect match for manual curation
#Apply synonym table to both....

##B -- Build tool for bringing new germplasm in.