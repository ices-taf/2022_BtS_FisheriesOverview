# wd: bootstrap/data/ICES_nominal_catches

library(icesTAF)
taf.library(icesFO)

hist <- load_historical_catches()
write.taf(hist, file = "ICES_historical_catches.csv", quote = TRUE)

official <- load_official_catches()
write.taf(official, file = "ICES_2006_2019_catches.csv", quote = TRUE)




load_preliminary_catches <- function (year){
        url<- "https://data.ices.dk/rec12/download/2021preliminaryCatchStatistics.zip"
        tmpFilePrelimCatch <- tempfile(fileext = ".zip")
        download.file(url, destfile = tmpFilePrelimCatch, mode = "wb", quiet = TRUE)
        out <- out <- read.csv(unz(tmpFilePrelimCatch, "2021preliminaryCatchStatistics.csv"),
                               stringsAsFactors = FALSE,
                               header = TRUE,
                               fill = TRUE,
                               na.strings = c("...", "-", "ns", "."))
}

prelim <- load_preliminary_catches(2021)
names(prelim)
colnames(prelim) <- c("Year","AphiaID","Species.Latin.Name","Species_Latin_Name","Area","Country","AMS_Catch","BMS_Catch")  

write.taf(preliminary, file = "ICES_preliminary_catches.csv", quote = TRUE)