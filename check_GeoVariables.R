
install.packages("gdata")
install.packages("readxl")
install.packages("openxlsx")
library(gdata)
library(wihoja)
library(readxl)
library(openxlsx)


country <- "IT"
year <- 2019
limit <- 1000


download.file("https://ec.europa.eu/eurostat/documents/345175/501971/EU-28-LAU-2019-NUTS-2016.xlsx ", destfile="correspondence.xlsx")
corresp_table <- read_excel("correspondence.xlsx" , sheet = country)
colnames(corresp_table) <- substr(str_replace_all(colnames(corresp_table) , " " , "_") , 1 , 11)
dim(corresp_table)
NUTS_table <- as.data.frame(table(corresp_table$NUTS_3_CODE))
colnames(NUTS_table) <- c("NUTS_3_CODE", "Freq")
NUTS_table$NUTS_3_CODE_m <- NUTS_table$NUTS_3_CODE
dim(NUTS_table)
View(NUTS_table)

View(geo_query)
View(NUTS_check_table)


NUTSCHECK_country <- function(country , year=2019 , limit=99999) {
        corresp_table <- read_excel("correspondence.xlsx" , sheet = country)
        colnames(corresp_table) <- substr(str_replace_all(colnames(corresp_table) , " " , "_") , 1 , 11)
        NUTS_table <- as.data.frame(table(corresp_table$NUTS_3_CODE))
        colnames(NUTS_table) <- c("NUTS_3_CODE", "Freq")
        NUTS_table$NUTS_3_CODE_m <- NUTS_table$NUTS_3_CODE
        
        query_text <- paste0("SELECT idprovince, province, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE ((year_grab_date=",year,") AND (idcountry='",country,"')) ORDER BY RAND() LIMIT ",limit)
        geo_query <- get_data(query_text)
        geo_query <- geo_query[idprovince!=""]
        geo_query$NUTS_3_CODE_m <- geo_query$idprovince
        
        NUTS_check_table <- merge(geo_query , NUTS_table , all=TRUE)
        NUTS_check_table$RedFlag <- 0
        NUTS_check_table$RedFlag[NUTS_check_table$NUTS_3_CODE!=NUTS_check_table$idprovince] <- 1
        NUTS_check_table$RedFlag[is.na(NUTS_check_table$NUTS_3_CODE)==TRUE] <- 1
        
        NUTS_problems_table <- NUTS_check_table[NUTS_check_table$RedFlag == 1 , ]
        output <- list(dim(geo_query) , dim(NUTS_check_table) , sum(NUTS_check_table$RedFlag) ,  NUTS_problems_table)
        return(output)
}

countrylist <- c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE")
NUTSCHECK_results <- lapply(countrylist , NUTSCHECK_country)

printfct_NUTSCHECK <- function(i) {
        a <- countrylist[[i]]
        b <- NUTSCHECK_results[[i]][[3]]
        c <- c(a,b)
        return(print(c))
}

length <- as.matrix(1:length(countrylist))
summary_NUTSCHECK <- t(as.matrix(apply(length,1,printfct_NUTSCHECK)))
summary_NUTSCHECK <- as.data.frame(summary_NUTSCHECK)
colnames(summary_NUTSCHECK) <- c("country" , "red_flags")
View(summary_NUTSCHECK)


   
     
LAUCHECK_country <- function(country, year=2019, limit=99999) {
        corresp_table <- read_excel("correspondence.xlsx" , sheet = country)
        colnames(corresp_table) <- substr(str_replace_all(colnames(corresp_table) , " " , "_") , 1 , 11)
        LAU_table <- as.data.frame(table(corresp_table$LAU_CODE))
        colnames(LAU_table) <- c("LAU_CODE", "Freq")
        LAU_table$LAU_CODE_m <- LAU_table$LAU_CODE
       
        query_text <- paste0("SELECT idcity, city, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE ((year_grab_date=",year,") AND (idcountry='",country,"')) ORDER BY RAND() LIMIT ",limit)
        geo_query <- get_data(query_text)
        geo_query <- geo_query[idcity!=""]
        geo_query$LAU_CODE_m <- geo_query$idcity
        
        LAU_check_table <- merge(geo_query , LAU_table , all=TRUE)
        LAU_check_table$RedFlag <- 0
        LAU_check_table$RedFlag[LAU_check_table$LAU_CODE!=LAU_check_table$idcity] <- 1
        LAU_check_table$RedFlag[is.na(LAU_check_table$LAU_CODE)==TRUE] <- 1
        LAU_problems_table <- LAU_check_table[LAU_check_table$RedFlag == 1 , ]
        output <- list(dim(geo_query) , dim(LAU_check_table) , sum(LAU_check_table$RedFlag) ,  LAU_problems_table)
        return(output)
}


countrylist <- c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE")
LAUCHECK_results <- lapply(countrylist , LAUCHECK_country)

printfct_LAUCHECK <- function(i) {
        a <- countrylist[[i]]
        b <- LAUCHECK_results[[i]][[3]]
        c <- c(a,b)
        return(print(c))
}

length <- as.matrix(1:length(countrylist))
summary_LAUCHECK <- t(as.matrix(apply(length,1,printfct_LAUCHECK)))
summary_LAUCHECK <- as.data.frame(summary_LAUCHECK)
colnames(summary_LAUCHECK) <- c("country" , "red_flags")
View(summary_LAUCHECK)

x<-as.data.frame(summary_NUTSCHECK)
str(summary_LAUCHECK)

###export summary results
wb <- createWorkbook("GeoVariables_Checks")
addWorksheet(wb , "NUTSCHECK")
writeData(wb , "NUTSCHECK" , summary_NUTSCHECK)
addWorksheet(wb, "LAUCHECK")
writeData(wb , "LAUCHECK" , summary_LAUCHECK)
saveWorkbook(wb , "GeoVariables_Checks" , overwrite = TRUE)


saveWorkbook(wb , "//net1.cec.eu.int/Homes/02/marcgab/My Documents/GeoVariables_Checks.xlsx" , overwrite = TRUE)
write.csv(summary_NUTSCHECK, "GeoVariables_Checks.xlsx")

