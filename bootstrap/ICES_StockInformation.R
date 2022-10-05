
library(icesTAF)
library(icesSD)
library(jsonlite)


url <- "https://sid.ices.dk/services/odata3/StockListDWs3?$filter=ActiveYear%20eq%202022"
out <- fromJSON(url)$value

write.taf(out, file = "SD_2022.csv", quote = TRUE)
