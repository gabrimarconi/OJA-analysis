


### packages and libraries

#install.packages("gdata")
#install.packages("readxl")
library(gdata)
library(wihoja)
library(readxl)
library(tidyverse)


### establish connection to Athena

source("Olo_connection.R")


### download Eurostat territorial classifications

#download.file("https://ec.europa.eu/eurostat/documents/345175/501971/EU-28-LAU-2019-NUTS-2016.xlsx ", destfile="correspondence.xlsx")
download.file("https://ec.europa.eu/eurostat/documents/345175/501971/EU-28_LAU_2017_NUTS_2013.xlsx ", destfile="correspondence.xlsx")


### set-up a function to check whether idprovince codes have an exact NUTS3 code match

NUTSCHECK_country <- function(country , year=2019 , limit=99999) {
        #import NUTS classification and generate a table ready for a merge (including a merging key which name ends in "_m")
        corresp_table <- read_excel("correspondence.xlsx" , sheet = country)
        colnames(corresp_table) <- substr(str_replace_all(colnames(corresp_table) , " " , "_") , 1 , 11)
        NUTS_table <- as.data.frame(table(corresp_table$NUTS_3_CODE))
        colnames(NUTS_table) <- c("NUTS_3_CODE", "Freq")
        NUTS_table$NUTS_3_CODE_m <- NUTS_table$NUTS_3_CODE
        
        #import data from OJA dataset and prepare the table for merge
        query_text <- paste0("SELECT idprovince, province, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE ((year_grab_date=",year,") AND (idcountry='",country,"')) ORDER BY RAND() LIMIT ",limit)
        geo_query <- get_data(query_text)
        geo_query <- geo_query[idprovince!=""]
        geo_query$NUTS_3_CODE_m <- geo_query$idprovince
        
        #merge the two tables from the NUTS classification and from the OJA dataset
        NUTS_check_table <- merge(geo_query , NUTS_table , all=TRUE)
        
        #create a red flag variable indicating if a match for the OJA observation is not found in the NUTS classification, and create a table containing red-flagged observations
        NUTS_check_table$RedFlag <- 0
        NUTS_check_table$RedFlag[NUTS_check_table$NUTS_3_CODE!=NUTS_check_table$idprovince] <- 1
        NUTS_check_table$RedFlag[is.na(NUTS_check_table$NUTS_3_CODE)==TRUE] <- 1
        NUTS_problems_table <- NUTS_check_table[NUTS_check_table$RedFlag == 1 , ]
        
        #generate an output list
        output <- list(dim(geo_query) , dim(NUTS_check_table) , sum(NUTS_check_table$RedFlag) ,  NUTS_problems_table)
        return(output)
}


### apply the previous function to all European countries

countrylist <- c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE")
NUTSCHECK_results <- lapply(countrylist , NUTSCHECK_country)


### show and export summary output

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
write.csv(summary_NUTSCHECK, "GeoVariables_NUTSChecks.csv")


### repeat for LAUs/cities the procedure that has been set up for NUTS/provinces  
     
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

write.csv(summary_LAUCHECK, "GeoVariables_LAUChecks.csv")

### example of a line of command to see the detailed table with red flags

View(NUTSCHECK_results[[7]][[4]])

View(summary_NUTSCHECK)






















### PS: LAUcheck for older files
# the only change is the following line within the function: LAU_table <- as.data.frame(table(corresp_table$LAU2_NAT_CO))

#download.file("https://ec.europa.eu/eurostat/documents/345175/501971/EU-28_LAU_2016 ", destfile="correspondence.xlsx")
#download.file("https://ec.europa.eu/eurostat/documents/345175/501971/EU-28_2015.xlsx ", destfile="correspondence.xlsx")
#download.file("https://ec.europa.eu/eurostat/documents/345175/501971/EU-28_2014.xlsx ", destfile="correspondence.xlsx")
download.file("https://ec.europa.eu/eurostat/documents/345175/501971/EU-28_2013.xlsx ", destfile="correspondence.xlsx")




LAUCHECK_country <- function(country, year=2019, limit=99999) {
        corresp_table <- read_excel("correspondence.xlsx" , sheet = country)
        colnames(corresp_table) <- substr(str_replace_all(colnames(corresp_table) , " " , "_") , 1 , 11)
        LAU_table <- as.data.frame(table(corresp_table$LAU2_NAT_CO))
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

write.csv(summary_LAUCHECK, "GeoVariables_LAUChecks.csv")

sum(as.numeric(summary_LAUCHECK$red_flags))
