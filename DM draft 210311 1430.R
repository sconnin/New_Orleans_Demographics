#DRAFT 210311 1133
#OVERARCHING PRINCIPAL: Lose no information, and trade readability for tidyness. Repeat procedures wherever possible. Use tidyverse and Base R wherever possible.
#GOAL: Try to apply the framework to each table, starting with the biggest:
##transpose
##fix rownames (move to col, remove nas, fill)
##fix colnames (rename them all to address nested naming structure from original table)
##drop empty rows
##drop empty cols
##convert to numeric

library(tidyverse)
library(readxl)

xlsx_src <- "C:/Users/dmosc/OneDrive/Documents/academic/CUNY SPS/DATA 607/Proj2/NO_short_data.xlsx"

S1A14 <- read_excel(xlsx_src, range = "A14:C18")
S1A22 <- read_excel(xlsx_src, range = "A22:U48")
S1A53 <- read_excel(xlsx_src, range = "A53:B68")
S1A72 <- read_excel(xlsx_src, range = "A72:J88")
S1A91 <- read_excel(xlsx_src, range = "A91:I104")
S1A108 <- read_excel(xlsx_src, range = "A108:I110")

###S1A22###
#TRANSPOSE
S1A22 <- S1A22 %>%
  t() %>%
  data.frame()

#FIX ROW NAMES
S1A22 <- rownames_to_column(S1A22)
S1A22$rowname <- gsub('.[0-9]+', NA, S1A22$rowname)
S1A22 <- fill(S1A22, rowname)

#FIX COLNAMES
S1A22 <- rename(S1A22, "parish" = rowname,
                "year" = X1,
                "f_wht_not_hisp" = X3, 
                "f_blk_not_hisp" = X4, 
                "f_hisp_any" = X5, 
                "f_asn_not_hisp" = X6, 
                "age_btw_0_4" = X9, 
                "age_btw_5_9" = X10, 
                "age_btw_10_14" = X11, 
                "age_btw_15_19" = X12, 
                "age_btw_20_24" = X13, 
                "age_btw_25_29" = X14, 
                "age_btw_30_34" = X15, 
                "age_btw_35_39" = X16, 
                "age_btw_40_44" = X17, 
                "age_btw_45_49" = X18, 
                "age_btw_50_54" = X19, 
                "age_btw_55_59" = X20, 
                "age_btw_60_64" = X21, 
                "age_btw_65_69" = X22, 
                "age_btw_70_74" = X23, 
                "age_btw_75_79" = X24, 
                "age_btw_80_84" = X25, 
                "age_geq_85" = X26)

#DROP EMPTY ROWS AND COLUMNS
S1A22 <- S1A22[-c(1),-c(3,8,9)]
S1A22 <- remove_rownames(S1A22)

#CONVERT TO NUMERIC
S1A22[2:length(S1A22[1,])] <- sapply(S1A22[2:length(S1A22[1,])], as.numeric)

###S1A14###

#TRANSPOSE
S1A14 <- S1A14 %>%
  t() %>%
  data.frame()

#FIX ROW NAMES
S1A14 <- rownames_to_column(S1A14)
S1A14 <- cbind(parish = "Orleans", S1A14)

#FIX COLNAMES
S1A14 <- rename(S1A14, "year" = rowname,
                "wht_not_hisp" = X2, 
                "blk_not_hisp" = X1, 
                "hisp_any" = X3, 
                "asn_not_hisp" = X4)

#DROP EMPTY ROWS AND COLUMNS
S1A14 <- S1A14[-c(1),-c(3,5)]
S1A14 <- remove_rownames(S1A14)

#CONVERT TO NUMERIC
S1A14[2:length(S1A14[1,])] <- sapply(S1A14[2:length(S1A14[1,])], as.numeric)

###S1A53###

#FIX ROW NAMES

S1A53 <- cbind(parish = "Orleans", S1A53)


#FIX COLNAMES
S1A53 <- rename(S1A53, 
                "year" = Year,
                "blk_not_hisp" = ...2)

#CONVERT TO NUMERIC
S1A53[2:length(S1A53[1,])] <- sapply(S1A53[2:length(S1A53[1,])], as.numeric)

###S1A72###
S1A72 <- pivot_longer(S1A72, `Orleans`:`New Orleans Metro`)

#FIX COLNAMES
S1A72 <- rename(S1A72, "parish" = name,
                "year" = ...1,
                "hisp_any" = value)

#DROP EMPTY ROWS AND COLUMNS AND REARRANGE
S1A72 <- S1A72[-c(1:9),]
S1A72 <- S1A72[,c(2,1,3)]


#CONVERT TO NUMERIC
S1A72[2:length(S1A72[1,])] <- sapply(S1A72[2:length(S1A72[1,])], as.numeric)

###S1A91###

#TRANSPOSE
S1A91 <- S1A91 %>%
  t() %>%
  data.frame()

#FIX ROW NAMES
S1A91 <- rownames_to_column(S1A91)
S1A91$rowname <- gsub('.[0-9]+', NA, S1A91$rowname)
S1A91 <- fill(S1A91, rowname)

#FIX COLNAMES
S1A91 <- rename(S1A91, "parish" = rowname,
                "year" = X1,
                "f_hisp_cub" = X3, 
                "f_hisp_dom" = X4,
                "f_hisp_mex" = X5,
                "f_hisp_pr" = X6,
                "f_hisp_hon" = X7,
                "f_hisp_gua" = X8,
                "f_hisp_nic" = X9,
                "f_hisp_sal" = X10,
                "f_hisp_ca" = X11,
                "f_hisp_sa" = X12,
                "f_hisp_oth" = X13)

#DROP EMPTY ROWS AND COLUMNS
S1A91 <- S1A91[-c(1,3,5,7,9),-3]
S1A91 <- remove_rownames(S1A91)

#CONVERT TO NUMERIC
S1A91[2:length(S1A91[1,])] <- sapply(S1A91[2:length(S1A91[1,])], as.numeric)

###S1A108###

#TRANSPOSE
S1A108 <- S1A108 %>%
  t() %>%
  data.frame()

#FIX ROW NAMES
S1A108 <- rownames_to_column(S1A108)
S1A108$rowname <- gsub('.[0-9]+', NA, S1A108$rowname)
S1A108 <- fill(S1A108, rowname)

#FIX COLNAMES
S1A108 <- rename(S1A108, "parish" = rowname,
                "year" = X1,
                "age_btw_0_18" = X2)

#DROP EMPTY ROWS AND COLUMNS
S1A108 <- S1A108[-c(1),]
S1A108 <- remove_rownames(S1A108)

#CONVERT TO NUMERIC
S1A108[2:length(S1A108[1,])] <- sapply(S1A108[2:length(S1A108[1,])], as.numeric)

###combined###
#combined <- data.frame()

combined <- 
  full_join(S1A14, S1A22, by = c("parish", "year")) %>%
  full_join(S1A53, by = c("parish", "year")) %>%
  full_join(S1A72, by = c("parish", "year")) %>%
  full_join(S1A91, by = c("parish", "year")) %>%
  full_join(S1A108, by = c("parish", "year"))