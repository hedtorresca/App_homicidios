library(readxl)
library(tidyverse)
library(xlsx)

datos <- read_excel("IHME-GBD_2017_DATA.xlsx")
datos2 <- read_excel("IHME-GBD_2017_DATA-2.xlsx")
datos3 <- full_join(datos, datos2, by = c("measure_id", "measure_name", "location_id", "location_name", "sex_id", "sex_name", "age_id", "age_name", "cause_id", "cause_name", "year"))

names(datos)
muertesviolentas <- datos3  %>% group_by_(.dots = list("measure_name",  "location_name"  )) %>%
    summarise(Rate_lower = sum(lower), Rate_val = sum(val), Rate_upper = sum(upper), Count_lower = sum(lower2), Count_val = sum(val2), Count_upper = sum(upper2)) %>% select( "measure_name",  "location_name"   , "Rate_val", "Count_val" ,"Rate_lower"  , "Rate_upper" ,"Count_lower" , "Count_upper"   ) %>% arrange(desc(Rate_val))   
muertesviolentas

data.frame(muertesviolentas)

write.xlsx(data.frame(muertesviolentas), file= "muertesviolentas.xlsx", sheetName = "Hoja1")


