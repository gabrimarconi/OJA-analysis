library(wihoja)

query_athena("")
n

query_athena("select * from ")
query_athena("select * from ")
query_athena("select * from ")

query_athena("select * from estat_dsl2531b_oja.ft_document_en_v8 limit 10")

oja <- query_athena("select * from estat_dsl2531b_oja.ft_document_en_v8 limit 10")

company_names_lu <- query_athena("select distinct companyname from estat_dsl2531b_oja.ft_document_en_v8 where idcountry='LU' limit 1000")

View(company_names_lu)

oja <- get_oja()

oja <- get_oja(idcountry = "LU")

noja <- get_oja_count(idcountry = "LU", lang = "pt", group_by = "sourcecountry")
noja

noja <- get_oja_count(group_by = "source")
noja

get_oja_count()

nojv <- get_ojv_count(idcountry= "LU")
get_ojv_count()


