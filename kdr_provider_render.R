library(quarto)
library(tidyverse)

load("G:/Press Ganey II/Reports/Ad Hoc/DEEP DIVE/Key Driver Reports/data/provider_data.Rdata")

x <- "Physician"

y <- "O7"

##Running kdr reports for those providers who meet the specified criteria

runners <- data %>%
  filter(provider_type == x ) %>%
  filter(varname == y ) %>%
  group_by(name,npi) %>%
  summarise(tbscore=sum(top_box)/n()*100,n=n()) %>% #calculate topbox
  filter(n>29, tbscore<100 )

t<- nrow(runners)

npis <- runners %>%
  pull(npi) %>%
  as.character()

names <- runners %>%
  pull(name) %>%
  as.character()

reports_01<-
  tibble(
    input="kdr_provider.qmd",
    output_file = str_glue("{names}.html"),
    execute_params=map(npis,~list(npi=.))
  )

reports_01<-reports_01%>%
  slice(1:t)

pwalk(reports_01,quarto_render)


##Running kdr for those providers with n <29 but not perfect ltr.

smallsamples <- data%>%
  filter(provider_type == x )%>%
  filter(varname == y ) %>%
  group_by(name,npi) %>%
  summarise(tbscore=sum(top_box)/n()*100,n=n()) %>% #calculate topbox
  filter(n<30 & tbscore < 100)

p<- nrow(smallsamples)

npis2 <- smallsamples %>%
  pull(npi) %>%
  as.character()

names2 <- smallsamples %>%
  pull(name) %>%
  as.character()

reports_02<-
  tibble(
    input="kdr_provider.qmd",
    output_file = str_glue("{names2}.html"),
    execute_params=map(npis2,~list(npi=.))
  )

reports_02<-reports_02%>%
  slice(1:p)

pwalk(reports_02,quarto_render)


##Running kdr for those providers with perfect ltr.

perfectltr <- data%>%
  filter(provider_type == x )%>%
  filter(varname == y ) %>%
  group_by(name,npi) %>%
  summarise(tbscore=sum(top_box)/n()*100,n=n()) %>% #calculate topbox
  filter(tbscore == 100)

r<- nrow(perfectltr)

npis3 <- perfectltr %>%
  pull(npi) %>%
  as.character()

names3 <- perfectltr %>%
  pull(name) %>%
  as.character()

reports_03<-
  tibble(
    input="kdr_provider_perfectltr.qmd",
    output_file = str_glue("{names3}.html"),
    execute_params=map(npis3,~list(npi=.))
  )

reports_03<-reports_03%>%
  slice(1:r)

pwalk(reports_03,quarto_render)

