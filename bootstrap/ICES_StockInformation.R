
library(icesTAF)
library(icesSD)
library(jsonlite)



sid <- load_sid(2022)

write.taf(sid, file = "SD_2022.csv", quote = TRUE)
