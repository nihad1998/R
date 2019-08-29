install.packages(data.table,highcharter,h2o,tibble)
library(tidyquant)
library(data.table)
library(highcharter)
library(h2o)
library(tibble)
setwd('/home/nihad/Desktop/DATA_Analtyics/Session3')
df = fread('900_houses.csv')

glimpse(df)


df %>% rownames_to_column() %>% 
  filter(!V7 %in% c("mülkiyyətçi","vasitəçi (agent)")) %>% as_tibble()->problems

df= df[-c(659,695,744),]

df$V3 = df$V3 %>% gsub(pattern = "[A-z]|²",replacement = '') %>% str_squish()

df$V6 = df$V6 %>% gsub(pattern = "AZN| ",replacement = '') 

df[df=='']<-NA

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
total= total %>% separate(col = V9,c('year','month','day'),sep = '-') 
total=total %>% separate(col = V2,c('current_flat','max_flat'),sep = '/')
write.csv2(total, file = "total_Bina_Az.csv")
df = fread('total_Bina_Az.csv')
df=df[,-1]
df=df[,-13]
#df$V3 %>% unique()
recovery_of_V3=df$V3
recovery_of_V6=df$V6 
recovery_of_V8=df$V8
df =  df %>% mutate_if(is.character,as.factor) %>% mutate_if(is.integer,as.factor)
str(df)
df$V6=recovery_of_V6
df$V8=recovery_of_V8
df$V3=recovery_of_V3
str(df)

df=df[-c(9)]


h2o.init()


h2o_data<-as.h2o(df)


h2o_data<-h2o.splitFrame(h2o_data,ratios = c(0.7,0.15),seed=1)


train<-h2o_data[[1]]


test<-h2o_data[[2]]


validation<-h2o_data[[3]]


price<-'V6'
aml<-h2o.automl(y=price,
                training_frame = train, nfolds = 0, #blending_frame = test,
                validation_frame = validation, 
                leaderboard_frame = test,seed=3,max_runtime_secs = 120,exclude_algos = c("StackedEnsemble"))#,
#max_models = 2))


aml@leader

aml@leaderboard %>% as_tibble() %>% head(.,20)


aml@leaderboard %>% as.tibble() %>% select(model_id) %>% .[,1] %>% .[1,] %>% 
  str_split(.,'_',simplify = TRUE) %>% .[,1:1]->leader


h2o.predict(aml@leader,test) %>% as.tibble() %>% .$predict %>% table()

test %>% as.data.frame() %>% .$y %>% table()



h2o.predict(aml@leader,test) %>% as.tibble() ->predictions
predictions


predictions %>% mutate(new_outcome = if_else('yes' >=0.1, 'yes','no')) %>% 
  mutate(actual=test %>% as.data.frame() %>% .$y)->predictions

predictions %>%  .$new_outcome %>% table()

predictions %>% count(actual,new_outcome)

library(caret)

table(predictions$predict,predictions$actual)
confusionMatrix(predictions$predict,predictions$actual)
table(as.factor(predictions$new_outcome),predictions$actual)


h2o.varimp_plot(aml@leader,num_of_features = 20)

h2o.partialPlot(aml@leader,test,cols = 'V3')


aml@leaderboard %>% as.tibble() %>% slice(2) %>% pull(model_id) %>%  h2o.getModel() %>% 
  h2o.saveModel(path = 'newww')
