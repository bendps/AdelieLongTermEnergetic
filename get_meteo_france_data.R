library(httr)
library(R.utils)

for (year in 1998:2024) {
  for(month in c(01,02,10,11,12)){
    # The URL to download data from
    url <- paste0("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/Archive/synop.", year, month,".csv.gz")
    
    # The filename to save it as
    compressed_file <- paste0("data_", year, month, ".csv.gz")
    output_file <- paste0("data_", year, month, ".csv")
    
    # Send a GET request to download the file
    response <- GET(url)
    
    # Save the content to a file
    writeBin(content(response, "raw"), output_file)
    
    gunzip(compressed_file, destname = output_file, remove = TRUE)
    cat("File saved as", output_file, "\n")
    
  }
}

