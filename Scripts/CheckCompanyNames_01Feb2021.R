


#libraries and data connection
#install.packages("wihoja")
library(wihoja)
open_oja_db()
#install.packages("tidyverse")
library(tidyverse)

#country <- "IT"
#year <- 2019
#lim <- 50000
#query <- paste("SELECT DISTINCT companyname, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE ((year_grab_date=" , year , ") AND (idcountry='" , country , "')) LIMIT " , lim)
#query <- paste("SELECT DISTINCT companyname, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE ((year_grab_date=2019) AND (idcountry='",country,"')) LIMIT 10000")
#companies_names_query <- query_athena(query)


###creating a table with company names' frequency, sorted by frequency or company name
#query and deduplication
companies_names_query <- query_athena("SELECT companyname, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE idcountry='IT' ORDER BY RAND()  LIMIT 1000000")
#companies_names_query <- query_athena("SELECT companyname, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE ((year_grab_date=2019) AND (month_grab_date=1 OR month_grab_date=2 OR month_grab_date=3) AND (idcountry='LU')) ORDER BY RAND()  LIMIT 1000000")
dim(companies_names_query)
companies_names_query$dup <- ifelse(duplicated(companies_names_query$general_id), 1, 0)
companies_names_query <- companies_names_query[companies_names_query$dup==0]
#background checks
table(companies_names_query$dup)
dim(companies_names_query)

#creating a table with company names' frequency, sorted by frequency or company name
companies_names_dataframe <- as.data.frame(table(companies_names_query$companyname))
colnames(companies_names_dataframe) <- c("companyname","Freq")
companies_names_dataframe <- arrange(companies_names_dataframe , desc(Freq))
companies_names_dataframe_bynames <- arrange(companies_names_dataframe , companyname)
str(companies_names_dataframe)


#doing some standardisation of company names and dropping empty company names
companies_names_dataframe$companyname <- str_to_lower(companies_names_dataframe$companyname)
#companies_names_dataframe$companyname <- gsub(",|;|.","",companies_names_dataframe$companyname)
companies_names_dataframe$companyname <- str_trim(companies_names_dataframe$companyname)
companies_names_dataframe$companyname <- gsub(" ","_",companies_names_dataframe$companyname)
companies_names_dataframe$notgood <- ifelse(companies_names_dataframe$companyname=="",1,0)
companies_names_dataframe <- companies_names_dataframe[companies_names_dataframe$notgood != 1 , -3]
dim(companies_names_dataframe)

#applying the job agency filter
staff_agencies <- read.csv("Data/staff_agencies_IT.csv" , sep = ";")
blacklist <- staff_agencies[staff_agencies$exact != "exact" , 2]
blacklist_exact <- staff_agencies[staff_agencies$exact == "exact" , 2]
#filteredout <- filter(companies_names_dataframe, str_detect(companies_names_dataframe$companyname, paste(blacklist, collapse = '|')) | (companies_names_dataframe$companyname == paste(blacklist_exact, collapse = '|')) )
length(blacklist)
filteredout <- cbind.data.frame(0,0)[-1,]
colnames(filteredout) <- c("companyname" , "Freq")
for(i in 1:length(blacklist)) {
  filteredout <- rbind(filteredout , filter(companies_names_dataframe, str_detect(companies_names_dataframe$companyname, blacklist[i]) ) )
  companies_names_dataframe <- filter(companies_names_dataframe, str_detect(companies_names_dataframe$companyname, blacklist[i] , negate = TRUE))
  }
for(i in 1:length(blacklist_exact)) {
  filteredout <- rbind(filteredout, filter(companies_names_dataframe, blacklist_exact[i] == companies_names_dataframe$companyname) )
  companies_names_dataframe <- filter(companies_names_dataframe, blacklist_exact[i] != companies_names_dataframe$companyname)
}
filteredout <- arrange(filteredout , desc(Freq))
dim(filteredout)
dim(companies_names_dataframe)
#the following commands would be equivalent to the previous loops but do not work with long strings as conditions
#filteredout <- filter(companies_names_dataframe, str_detect(companies_names_dataframe$companyname, paste(blacklist, collapse = '|')) | sub(paste(blacklist_exact, collapse = '|'),"",companies_names_dataframe$companyname) == "" )
#companies_names_dataframe <- mutate(companies_names_dataframe, companyname = replace(companyname, str_detect(companies_names_dataframe$companyname, paste(blacklist, collapse = '|')) | sub(paste(blacklist_exact, collapse = '|'),"",companies_names_dataframe$companyname) == "", NA))
#companies_names_dataframe <- companies_names_dataframe[!is.na(companies_names_dataframe$companyname) , ]


# generating a table of number of companies having x ads
companies_freqtable <- as.data.frame(table(companies_names_dataframe$Freq))
colnames(companies_freqtable) <- c("ads_per_company" , "n_companies")
#ensuring that the variables of this table are numeric. NB: as.numeric does not work well for the variable ads_per_company, so I have to get the numeric value in a different way (through a merge)
companies_names_dataframe$ads_per_company <- as.factor(companies_names_dataframe$Freq)
companies_freqtable <- merge(companies_freqtable , companies_names_dataframe[duplicated(companies_names_dataframe$ads_per_company) == FALSE , -1])[ , -1]
colnames(companies_freqtable) <- c("n_companies" , "ads_per_company")
companies_freqtable$n_companies <- as.numeric(companies_freqtable$n_companies)
str(companies_freqtable)
#calculating the cumulative number of ads for the x biggest company names
companies_freqtable <- arrange(companies_freqtable , desc(ads_per_company))
companies_freqtable$tot_ads <- companies_freqtable$n_companies * companies_freqtable$ads_per_company
companies_freqtable$cum_prop_ads <- 100 * cumsum(companies_freqtable$tot_ads) / sum(companies_freqtable$tot_ads)
companies_freqtable$cum_prop_companies <- 100 * cumsum(companies_freqtable$n_companies) / sum(companies_freqtable$n_companies)
companies_freqtable$cum_n_companies <- cumsum(companies_freqtable$n_companies)
head(companies_freqtable)




### print and view output

#print output
write.csv(companies_freqtable , "Data/companies_freqtable_IT.csv")
write.csv(companies_names_dataframe , "Data/companies_names_dataframe_IT.csv")
write.csv(filteredout , "Data/filteredout.csv")
#cumulative distribution of ads and company names
View(companies_freqtable)
# list of company names by number of ads and alphabetical order
View(companies_names_dataframe)
View(companies_names_dataframe_bynames)
#total number of distinct company names found (with an upper bound due to the limit of obs allowed)
sum(companies_freqtable$n_companies)
#total number of distinct company and of job ads that are filtered out
sum(filteredout$Freq)
sum(companies_names_dataframe$Freq)
#filteredout and blacklists
View(filteredout)
blacklist
blacklist_exact





### these charts are not used anymore

ggplot(data = companies_freqtable) + 
  geom_point(mapping = aes(x = n_companies, y = ads_per_company))

ggplot(data = companies_by_name_table) + 
  geom_point(mapping = aes(x = Var1, y = Freq)) +
  ylim(1, 10)

ggplot(data = companies_by_name_table) + 
  geom_point(mapping = aes(x = Var1, y = Freq)) +
  ylim(100,NA)








### check if agency and company posts differ systematically in some ways

general_query <- query_athena("SELECT companyname, general_id, grab_date, idesco_level_4, idesco_level_3, idcity, idprovince, idsector, idcategory_sector FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE idcountry='IT' ORDER BY RAND()  LIMIT 1000000")
dim(general_query)
general_query$dup <- ifelse(duplicated(general_query$general_id), 1, 0)
general_query$companyname <- str_to_lower(general_query$companyname)
#companies_names_dataframe$companyname <- gsub(",|;|.","",companies_names_dataframe$companyname)
general_query$companyname <- str_trim(general_query$companyname)
general_query$companyname <- gsub(" ","_",general_query$companyname)
general_query$notgood <- ifelse(general_query$companyname=="",1,0)
general_query <- general_query[general_query$notgood != 1 , ]
View(general_query)


filteredout_m <- as.data.frame(table(filteredout$companyname))
filteredout_m$Freq <- 1
colnames(filteredout_m) <- c("companyname", "filteredout")
keep_m <- companies_names_dataframe[companies_names_dataframe$Freq>109 , ]
keep_m <- as.data.frame(table(keep_m$companyname))
keep_m$Freq <- 1
colnames(keep_m) <- c("companyname", "keep")


DF <- merge(general_query, filteredout_m, all.x=TRUE)
DF <- merge(DF, keep_m, all.x=TRUE)
dim(DF)

DF <- group_by(DF , companyname)
sumstats_by_company <- summarise(DF , tot_n=n(), nd_esco4=n_distinct(idesco_level_4), nd_esco3=n_distinct(idesco_level_3), tot_dups=sum(dup), nd_city=n_distinct(idcity), nd_prov=n_distinct(idprovince), nd_sect=n_distinct(idcategory_sector), nd_grab=n_distinct(grab_date),  filteredout=median(filteredout), keep=median(keep))
sumstats_by_company <- arrange(sumstats_by_company , desc(tot_n))
sumstats_by_company$filteredout[sumstats_by_company$keep==1] <- 0
sumstats_by_company$r_dup <- 100*sumstats_by_company$tot_dups / sumstats_by_company$tot_n
sumstats_by_company$r_esco4 <- 100*sumstats_by_company$nd_esco4 / sumstats_by_company$tot_n
sumstats_by_company$r_grab <- 100*sumstats_by_company$nd_grab / sumstats_by_company$tot_n
sumstats_by_company$r_sect <- 100*sumstats_by_company$nd_sect / sumstats_by_company$tot_n


### summary stats by variable "filteredout", which is equal to 1 if the observation has been labelled as agency, 0 for company, NA for not labelled yet

sumstats_by_company <- sumstats_by_company[sumstats_by_company$tot_n>29 , ]
DF2 <- group_by(sumstats_by_company , filteredout)
summarise(DF2, tot_n=n_distinct(companyname), sd_esco4=sd(r_esco4), r_esco4=mean(r_esco4), sd_m_esco4=sd(nd_esco4) , m_esco4=mean(nd_esco4), sd_city=sd(r_city), r_city=mean(r_city), sd_dup=sd(r_dup), r_dup=mean(r_dup))

###cross filteredout and other indicators through filters
prova <- sumstats_by_company[sumstats_by_company$r_esco4>50 , ]
table(prova$filteredout)
prova <- sumstats_by_company[sumstats_by_company$r_esco4<1 , ]
table(prova$filteredout)

prova <- sumstats_by_company[sumstats_by_company$r_city>50 , ]
table(prova$filteredout)
prova <- sumstats_by_company[sumstats_by_company$r_city<1 , ]
table(prova$filteredout)


### cross through charts (after recoding filteredout=-1 for observations that have not yet been labelled as company or agency)

sumstats_by_company <- mutate(sumstats_by_company, filteredout = replace(filteredout, is.na(filteredout), -1))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_esco4, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_city, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_dup, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_sect, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_sect, y = filteredout)) +
  xlim(0,5)

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_grab, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_dup, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_dup, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_dup, y = filteredout))


### given the discriminative power of r_sect, some more cross-tabulation can help
dim(sumstats_by_company)
prova <- sumstats_by_company[sumstats_by_company$r_sect>3 & sumstats_by_company$r_sect<100 & sumstats_by_company$tot_n>99 , ]
table(prova$filteredout)
prova <- sumstats_by_company[sumstats_by_company$r_sect<1 , ]
table(prova$filteredout)

filteredout_sect <- as.data.frame(sumstats_by_company$companyname[sumstats_by_company$r_sect>3])
colnames(filteredout_sect) <- "companyname"
dim(filteredout_sect)
filteredout_sect$filteredout_sect <- 1

companies_names_dataframe <- merge(companies_names_dataframe, filteredout_sect, all.x=TRUE)
companies_names_dataframe <- companies_names_dataframe[companies_names_dataframe$filteredout_sect!=1 , ]

sumstats_by_company$n_sect <- sumstats_by_company$r_sect * sumstats_by_company$tot_n / 100

(t(sumstats_by_company$tot_n)%*%sumstats_by_company$tot_n)^-1%*%t(sumstats_by_company$tot_n)%*%sumstats_by_company$n_sect

