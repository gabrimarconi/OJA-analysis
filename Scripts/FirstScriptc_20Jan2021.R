prova <- query_athena("select distinct companyname from estat_dsl2531b_oja.ft_document_en_v8 where idcountry='LU' limit 1000")


prova <- query_athena("select * from estat_dsl2531b_oja.ft_document_en_v8 where year_grab_date=2018 limit 1000")
dim(prova)
View(prova)
unique(prova[year_grab_date==2018]$month_grab_date)
n_prova <- query_athena("select COUNT(*) AS n_query FROM estat_dsl2531b_oja.ft_document_en_v8 where year_grab_date=2018 limit 1000")
n_prova



n_7_2018 <- query_athena("select COUNT(*) AS n_query FROM estat_dsl2531b_oja.ft_document_en_v8 where ((year_grab_date=2018) AND (month_grab_date=7) ) limit 1000")
n_7_2018

prova <- query_athena("select * from estat_dsl2531b_oja.ft_document_en_v8 where ((year_grab_date=2018) AND (month_grab_date=3) ) limit 100")

n_prova <- query_athena("select COUNT(*) AS n_query FROM estat_dsl2531b_oja.ft_document_en_v8 where year_grab_date=2018 group by general_id limit 1000")
dim(prova)
View(prova)
is.numeric(n_prova$n_query)
table(n_prova$n_query)









tabid<-function(mo,ye) {
  myquery<-paste("select COUNT(*) AS n_query FROM estat_dsl2531b_oja.ft_document_en_v8 where ((year_grab_date=",as.character(ye),") AND (month_grab_date=",as.character(mo),") ) group by general_id limit 100000000")
  mydata<-query_athena(myquery)
  mytable<-as.data.frame(table(mydata$n_query))
  colnames(mytable)<-c("id_occurrences", "freq")
  return(mytable)
  }



tabid_ye <- function(ye) {
  simple_tabid <- function(mo) {
    mytable <- tabid(mo,ye)
    return(mytable)
  }
  vec_1_12<-as.matrix(c(1:12))
  mylist <- apply(vec1_12,1,simple_tabid)
  return(mylist)
}

prova <- tabid_ye(2019)
prova[[7]]

n_prova2<-tabid(8,2018)
View(n_prova2)

vec1_12<-as.matrix(c(1:12))
dim(vec1_12)


argmo <- function(ye) {
  
}

as.numeric(substr(moye(2010)[5],1,2))


prova1 <- tabid(1,2019)
prova2 <- tabid(2,2019)
provalist <- list(prova1, prova2)
provalist[[2]]

colnames(n_prova2)<-c("id_occurrences", "freq")
dim(n_prova2)
dim(as.data.frame(n_prova2))

paste("me",as.character(2018),sep = "_")<-dim(prova)

View(n_prova2)

myquery<-paste("select COUNT(*) AS n_query FROM estat_dsl2531b_oja.ft_document_en_v8 where ((year_grab_date=","2018",") AND (month_grab_date=","8",") ) group by general_id limit 1000")
print(myquery)
n_prova3<-query_athena(myquery)
dim(n_prova3)

dim(as.data.frame(unique(n_prova$n_query)))
is.numeric(as.data.frame(unique(n_prova$n_query)))
as.numeric(as.data.frame(unique(n_prova$n_query)))

dim(n_prova)
str(n_prova)
is.numeric(n_prova)
is.integer(n_prova)
max(n_prova)
head(n_prova)
n_prova$n_prova[n_prova>10]
as.numeric(n_prova)
n_prova <- as.data.frame(n_prova)
