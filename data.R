
# Initial formatting of the data

library(icesTAF)
library(icesFO)
library(icesSAG)
library(icesSD)
library(dplyr)

mkdir("data")

# load species list
species_list <- read.taf("bootstrap/initial/data/FAO_ASFIS_species/species_list.csv")
sid <- read.taf("bootstrap/initial/data/ICES_StockInformation/sid.csv")
effort$sub.region <- tolower(effort$sub.region)
unique(effort$sub.region)


# 1: ICES official cath statistics

hist <- read.taf("bootstrap/data/ICES_nominal_catches/ICES_historical_catches.csv")
official <- read.taf("bootstrap/data/ICES_nominal_catches/ICES_2006_2017_catches.csv")
prelim <- read.taf("bootstrap/data/ICES_nominal_catches/ICES_preliminary_catches.csv")

catch_dat <-
  format_catches(2022, "Baltic Sea",
    hist, official, prelim, species_list, sid)

write.taf(catch_dat, dir = "data", quote = TRUE)


# 2: STECF effort and landings

effort <- read.taf("bootstrap/initial/data/FDI effort by country.csv", check.names = TRUE)
names(effort)
effort$Sub.region <- tolower(effort$Sub.region)
unique(effort$Sub.region)
effort_BtS <- dplyr::filter(effort, grepl("27.3.b|27.3.c.22|27.3.d.24|27.3.d.25|27.3.d.28.2|
                                          27.3.d.29|27.3.d.26|27.3.d.27|27.3.b.23|27.3.d.28.1|27.3.d.30|27.3.d.31|27.3.d.32", Sub.region))





landings1 <- read.taf("bootstrap/initial/data/Landings_2014.csv", check.names = TRUE)
landings2 <- read.taf("bootstrap/initial/data/Landings_2015.csv", check.names = TRUE)
landings3 <- read.taf("bootstrap/initial/data/Landings_2016.csv", check.names = TRUE)
landings4 <- read.taf("bootstrap/initial/data/Landings_2017.csv", check.names = TRUE)
landings5 <- read.taf("bootstrap/initial/data/Landings_2018.csv", check.names = TRUE)
landings6 <- read.taf("bootstrap/initial/data/Landings_2019.csv", check.names = TRUE)
landings7 <- read.taf("bootstrap/initial/data/Landings_2020.csv", check.names = TRUE)
landings <- rbind(landings1, landings2, landings3, landings4, landings5, landings6, landings7)
names(landings)
landings$Sub.region <- tolower(landings$Sub.region)
landings_BtS <- dplyr::filter(landings, grepl("27.3.b|27.3.c.22|27.3.d.24|27.3.d.25|27.3.d.28.2|
                                          27.3.d.29|27.3.d.26|27.3.d.27|27.3.b.23|27.3.d.28.1|27.3.d.30|27.3.d.31|27.3.d.32", Sub.region))

# need to group gears, Sarah help.
unique(landings_BtS$Gear.Type)
unique(effort_BtS$Gear.Type)

landings_BtS <- dplyr::mutate(landings_BtS, gear_class = case_when(
        grepl("TBB", Gear.Type) ~ "Beam trawl",
        grepl("DRB|DRH", Gear.Type) ~ "Dredge",
        grepl("GNS|GND|GTN|LHP|LLS|FPN|GTR|FYK|LLD|SDN", Gear.Type) ~ "Static/Gill net/LL",
        grepl("OTT|OTB|PTB|SSC|SB", Gear.Type) ~ "Otter trawl/seine",
        grepl("PTM|OTM|PS", Gear.Type) ~ "Pelagic trawl/seine",
        grepl("FPO", Gear.Type) ~ "Pots",
        grepl("NK", Gear.Type) ~ "other",
        is.na(Gear.Type) ~ "other",
        TRUE ~ "other"
)
)

effort_BtS <- dplyr::mutate(effort_BtS, gear_class = case_when(
        grepl("TBB", Gear.Type) ~ "Beam trawl",
        grepl("DRB|DRH", Gear.Type) ~ "Dredge",
        grepl("GNS|GND|GTN|LHP|LLS|FPN|GTR|FYK|LLD|SDN", Gear.Type) ~ "Static/Gill net/LL",
        grepl("OTT|OTB|PTB|SSC|SB", Gear.Type) ~ "Otter trawl/seine",
        grepl("PTM|OTM|PS", Gear.Type) ~ "Pelagic trawl/seine",
        grepl("FPO", Gear.Type) ~ "Pots",
        grepl("NK", Gear.Type) ~ "other",
        is.na(Gear.Type) ~ "other",
        TRUE ~ "other"
)
)


# effort <- read.taf("bootstrap/data/STECF_effort.csv", check.names = TRUE)
# 
# landings <- read.taf("bootstrap/initial/data/STECF_landings.csv", check.names = TRUE)
# 
# frmt_effort <- format_stecf_effort(effort)
# frmt_landings <- format_stecf_landings(landings)
# 
# write.taf(frmt_effort, dir = "data", quote = TRUE)
# write.taf(frmt_landings, dir = "data")


# 3: SAG

sag_BtS <- read.taf("SAG_complete_BtS.csv")
# sag_refpts <- read.taf("SAG_refpts.csv")
sag_status_BtS <- read.taf("SAG_status_BtS.csv")
sid <- read.taf("SD_2022.csv")

clean_sag <- format_sag(sag_complete, sid)
clean_status <- icesFO::format_sag_status(status, 2022, "Baltic Sea")

unique(clean_sag$StockKeyLabel)


# remove some stocks
Baltic_Out_stocks <-  c("sal.27.32", "sal.27.22-31", "ele.2737.nea", "trs.27.22-32", "fle.27.2425")
clean_sag <- dplyr::filter(clean_sag, !StockKeyLabel %in% Baltic_Out_stocks)
#pending
clean_status <- dplyr::filter(clean_status, !StockKeyLabel %in% Baltic_Out_stocks)

write.taf(clean_sag, dir = "data")
write.taf(clean_status, dir = "data", quote = TRUE)
