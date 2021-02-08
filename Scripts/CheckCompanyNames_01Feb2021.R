


#libraries and data connection
library(wihoja)
open_oja_db()
install.packages("tidyverse")
library(tidyverse)

#country <- "IT"
#year <- 2019
#lim <- 50000
#query <- paste("SELECT DISTINCT companyname, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE ((year_grab_date=" , year , ") AND (idcountry='" , country , "')) LIMIT " , lim)
#query <- paste("SELECT DISTINCT companyname, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE ((year_grab_date=2019) AND (idcountry='",country,"')) LIMIT 10000")
#companies_names_query <- query_athena(query)


###creating a table with company names' frequency, sorted by frequency or company name
#query and deduplication
#companies_names_query <- query_athena("SELECT companyname, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE ((year_grab_date=2019) AND (idcountry='IT')) ORDER BY RAND()  LIMIT 1000000")
companies_names_query <- query_athena("SELECT companyname, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE ((year_grab_date=2019) AND (month_grab_date=1 OR month_grab_date=2 OR month_grab_date=3) AND (idcountry='IT')) ORDER BY RAND()  LIMIT 1000000")
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
View(companies_names_dataframe)

#doing some standardisation of company names and dropping empty company names
companies_names_dataframe$companyname <- str_to_lower(companies_names_dataframe$companyname)
companies_names_dataframe$companyname <- gsub(",|;|\\.","",companies_names_dataframe$companyname)
companies_names_dataframe$companyname <- str_trim(companies_names_dataframe$companyname)
companies_names_dataframe$notgood <- ifelse(companies_names_dataframe$companyname=="",1,0)
companies_names_dataframe <- companies_names_dataframe[companies_names_dataframe$notgood != 1 , -3]

#applying the job agency filter
blacklist <- c("cross border talents","kelly services","hays","manpower","randstad","glecruit","egor","michael page","ray human capital","adecco","tempo-team ","industria criativa","select","randstad","eotim","approach people","gigroup","vertex","inditex careers","multitempo","rhmais","multipessoal","go work","iman","robert walters","wipjobs recruitment","global employability","expatica","alliance recruitment agency","new horizons","npaworldwide","airswift","experis","badenoch & clark","Robert Half","Huxley","recruit","recruitment","recruiting","talent","job","staff","staffing","hire","hiring")
filteredout <- filter(companies_names_dataframe, str_detect(companies_names_dataframe$companyname, paste(blacklist, collapse = '|')))
companies_names_dataframe <- mutate(companies_names_dataframe, companyname = replace(companyname, str_detect(companies_names_dataframe$companyname, paste(blacklist, collapse = '|')), NA))
companies_names_dataframe <- companies_names_dataframe[!is.na(companies_names_dataframe$companyname) , ]


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

#cumulative distribution of ads and company names
View(companies_freqtable)
# list of company names by number of ads and alphabetical order
View(companies_names_dataframe)
View(companies_names_dataframe_bynames)
#total number of distinct company names found (with an upper bound due to the limit of obs allowed)
sum(companies_freqtable$n_companies)

View(filteredout)










ggplot(data = companies_freqtable) + 
  geom_point(mapping = aes(x = n_companies, y = ads_per_company))

ggplot(data = companies_by_name_table) + 
  geom_point(mapping = aes(x = Var1, y = Freq)) +
  ylim(1, 10)

ggplot(data = companies_by_name_table) + 
  geom_point(mapping = aes(x = Var1, y = Freq)) +
  ylim(100,NA)



















#querying the dataset
company_names_it <- query_athena("select companyname from estat_dsl2531b_oja.ft_document_en_v8 where idcountry='IT' order by rand() limit 1000")
View(company_names_it)















































