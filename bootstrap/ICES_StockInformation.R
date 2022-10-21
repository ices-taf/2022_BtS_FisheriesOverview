
library(icesTAF)
library(icesSD)
library(jsonlite)



sid <- load_sid(2022)
# sid2 <- load_sid(2021)

write.taf(sid, file = "SD_2022.csv", quote = TRUE)
