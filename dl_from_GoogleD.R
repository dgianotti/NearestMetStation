dl_from_GoogleD <- function(output, key) {  
  ## Arguments:
  ## output = output file name
  ## key = Google document key
  ## Note: File must be shareable!
  
  require(RCurl)
  bin <- getURL(paste0("https://docs.google.com/uc?export=download&id=", key))
  con <- file(output, open = "wb")
  writeBin(bin, con)
  close(con)
  message(noquote(paste(output, "read into", getwd())))                        
}