install.packages(c('data.table','highcharter','dplyr','tibble','stringr','tidyr'))
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-yau/3/R")

library(dplyr)
library(data.table)
library(highcharter)
library(h2o)
library(tibble)
library(stringr)
library(tidyr)

df = fread('900_houses.csv',encoding = 'UTF-8',na.strings = '')

glimpse(df)

df %>% rownames_to_column() %>% 
  filter(!V7 %in% c("mülkiyyətçi","vasitəçi (agent)")) %>% as_tibble()->problems

df= df[-c(659,695,744),]

df$V3 = df$V3 %>% gsub(pattern = "[A-z]|²",replacement = '') %>% str_squish()

df$V6 = df$V6 %>% gsub(pattern = "AZN| ",replacement = '') %>% str_squish()

df$ID = c(1:nrow(df))

df[,c(10:19)] = lapply(df[,c(10:19)], function(i) str_squish(i))

df %>% select(V10:V19) %>% gather() %>% count(value) %>% arrange(desc(n)) %>% drop_na() %>% head(15)

#ll=dcast(gather(df[,c(10:20)], id="ID", key = key,value = value, -ID), ID ~ value, drop=FALSE) 
ll=dcast(melt(df[,c(10:20)], id="ID", factorsAsStrings=FALSE), ID ~ value, drop=FALSE)


total = df[,c(1:9,20)] %>% left_join(ll[1:134])

total[!colnames(total) %in% c('ID')] ->total
total =subset(total, str_length(total$V9)>=10)

total$V9=str_replace_all(total$V9,"Avqust","August")
total$V9=str_replace_all(total$V9,"İyul","July")
total$V9 %>% unique()
total$V9 = total$V9 %>% as.Date(dates, format = "%d %B %Y")
total= total %>% separate(col = V9,c('year','month','day'),sep = '-') %>% 
  mutate(year=str_squish(year),month=str_squish(month),day=str_squish(day))
total=total %>% separate(col = V2,c('current_flat','max_flat'),sep = '/') %>% 
  mutate(current_flat=str_squish(current_flat),max_flat=str_squish(max_flat))

temp = tempdir()
data.table::fwrite(total, file = glue::glue("{temp}/total_Bina_Az.csv"))
df = fread(glue::glue("{temp}/total_Bina_Az.csv"),encoding = 'UTF-8')

rm(ll,problems,total,temp)
purrr::map_df(df,~class(.)) %>% as.data.frame() %>% .[,1:14]

df$V8  %>% as.numeric()->df$V8

df=df %>% mutate_if(is.character,as.factor)

h2o.init()


h2o_data<-as.h2o(df)


h2o_data<-as.h2o(df %>% select(-V8)) %>% .[-1,]


price<-'V6'
aml<-h2o.automl(y=price,
                training_frame = h2o_data, nfolds = 5,seed=3,max_models = 5)


aml@leader

aml@leaderboard %>% as_tibble() %>% head(.,20)


aml@leaderboard %>% as_tibble() %>% select(model_id) %>% .[,1] %>% .[1,] %>% 
  str_split(.,'_',simplify = TRUE) %>% .[,1:1]->leader



predictions = h2o.predict(aml@leader,h2o_data) %>% as_tibble()  %>% 
  mutate(actual = df$V6, error = abs(predict-actual))
predictions



h2o.varimp_plot(aml@leader,num_of_features = 20)

h2o.partialPlot(aml@leader,h2o_data,cols = 'V3')


aml@leaderboard %>% as.tibble() %>% slice(2) %>% pull(model_id) %>%  h2o.getModel() %>% 
  h2o.saveModel(path = 'new')
