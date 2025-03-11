library(dslabs)
library(tidyverse)
data(heights)
data(murders)
heights <- heights |> mutate(género=case_when(sex=="Female"~"Femenino",
                                              sex=="Male"~"Masculino"),
                             alturacm=height*2.54)
head(heights)
murders <- murders |> mutate(region=case_when(region=="Northeast"~"Noreste",
                                              region=="South"~"Sur",
                                              region=="North Central"~"el Midwest",
                                              region=="West"~"Oeste",
                                              region=="Southeast"~"Sureste"),
                             tasa=total/population*100000)
head(heights)

#Exportar ambos a csv estándar
write.csv(heights, "heights.csv")
write.csv(murders, "murders.csv")

#Exportar a CSV separado por punto y coma
write.csv2(heights, "heights_pc.csv")
write.csv2(murders, "murders_pc.csv")

# Guardar en formato tab-separated (.tsv)
write.table(heights, file = "heights.tsv", sep = "\t", row.names = FALSE)
write.table(murders, file = "murders.tsv", sep = "\t", row.names = FALSE)

#Guardar valores separados por espacios en blanco en formato txt
write_delim(heights, "heights.txt", delim=" ")
write_delim(murders, "murders.txt", delim=" ")

write_excel_csv(murders, file = "murders_excel.csv")

#usando write_xlsx
library(writexl)

library(haven)

# Guardar para SPSS (.sav)
write_sav(heights, path = "heights.sav")
write_sav(murders, path = "murders.sav")

# Guardar para STATA (.dta)
write_dta(heights, path = "heights.dta")
write_dta(murders, path = "murders.dta")
