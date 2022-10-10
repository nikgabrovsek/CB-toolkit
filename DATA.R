#DATA IMPORT CODE
#I use US based data, namely main three macroeconomic indicators: GDP, INFLATION and, INTEREST RATE

library(fredr)

fredr::fredr_set_key("ce20dbad212f7e3deb52657da4227e0e")

GDP <- fredr("GDPC1")
GDP <- GDP[,c(1,3)]

CPI <- fredr("CPALCY01USQ661N")
CPI <- CPI[,c(1,3)]

RATE <- fredr("IR3TIB01USQ156N")
RATE <- RATE[,c(1,3)]

DATA <- merge(GDP, CPI, by = "date")
DATA <- merge(DATA, RATE, by = "date")
colnames(DATA) <- c("DATE", "GDP", "CPI", "RATE")

#Compute growth rates

library(dplyr)


DATA <- DATA %>% 
  mutate(GDP = (log(GDP) - lag(log(GDP), 4))) %>%
  mutate(CPI = (log(CPI) - lag(log(CPI), 4))) %>%
  mutate(RATE = RATE / 100)

#Get rid of first four observations

DATA <- DATA[-c(1:4),]

# Save it to csv file
write.csv(DATA, "DATA.csv", row.names = FALSE, quote = FALSE)







