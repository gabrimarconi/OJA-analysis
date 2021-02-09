

#install.packages("DBI")
library(reticulate)
library(DBI)
library(RAthena)

url<-"http://169.254.169.254/latest/meta-data/iam/security-credentials/ESTAT.DSL2531B.DefaultRole"
aws<-jsonlite::fromJSON(url)
reticulate::use_condaenv("RAthena")
con <- dbConnect(RAthena::athena(),
                 region_name="eu-west-1",
                 aws_access_key_id = aws$AccessKeyId,
                 aws_secret_access_key = aws$SecretAccessKey,
                 aws_session_token = aws$Token,
                 s3_staging_dir="s3://ecdataplatform-prod-estat-dsl2531b-athena-results-eu-west-1/",
                 work_group = "ESTAT.DSL2531B.WorkGroup"
)


get_data <- function(query){
    my_data <- dbGetQuery(con, query)
  return(my_data)
}


#query <-  "SELECT * FROM estat_dsl2531b_oja.ft_document_en_v8 ORDER BY RAND() LIMIT 10;"

#documents1232ws <- get_data(query)

#View(documents1232ws)

#prova <- dbGetQuery(con, "SELECT DISTINCT idprovince, province FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE ((year_grab_date=2019) AND (idcountry='IT')) LIMIT 10000")
#View(prova)

