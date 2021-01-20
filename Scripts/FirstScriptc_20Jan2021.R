prova <- query_athena("select distinct companyname from estat_dsl2531b_oja.ft_document_en_v8 where idcountry='LU' limit 1000")


prova <- query_athena("select * from estat_dsl2531b_oja.ft_document_en_v8 where year_grab_date=2018 limit 1000")
n_prova <- query_athena("select COUNT(*) AS n_query FROM estat_dsl2531b_oja.ft_document_en_v8 where year_grab_date=2018 limit 1000")
n_prova

prova <- query_athena("select * from estat_dsl2531b_oja.ft_document_en_v8 where ((NOT year_grab_date=2018) AND (month_grab_date=7) ) limit 1000")

n_prova <- query_athena("select COUNT(*) AS n_query FROM estat_dsl2531b_oja.ft_document_en_v8 where year_grab_date=2018 group by general_id limit 10")
dim(n_prova)
str(n_prova)
is.numeric(n_prova)
is.integer(n_prova)
max(n_prova)
head(n_prova)
n_prova$n_prova[n_prova>10]
as.numeric(n_prova)
n_prova <- as.data.frame(n_prova)
