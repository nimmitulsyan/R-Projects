library (tidymodels)
library(tidyr)
library(dplyr)
library(visdat)
library(car)

getwd()
setwd("C:/Data/Project Data")

h_train=read.csv('housing_train.csv')
h_test=read.csv('housing_test.csv')

glimpse(h_train)
vis_dat(h_train)

table(h_train$Suburb)
table (h_train$Address) #drop it
table (h_train$Rooms)
table(h_train$Type)
table(h_train$Method) 

sort(table(h_train$SellerG), decreasing=T)[1:3] 
table(h_train$Distance)
table(h_train$Postcode)
table(h_train$YearBuilt)
table(h_train$CouncilArea)

postcode_substr= function(x){
  x=as.character(x)
  x=substr(x, 1,2)
  return(x)
}

dp_pipe=recipe(Price~., data = h_train) %>% 
  update_role(Address,  new_role = "drop_vars") %>% 
  step_mutate_at(Postcode, fn=postcode_substr) %>% 
  update_role(Suburb, Type, CouncilArea, SellerG, Method, Postcode, new_role="to_dummies") %>% 
  step_rm(has_role("drop_vars")) %>% 
  step_unknown(has_role("to_dummies"), new_level = "__missing__") %>% 
  step_other(has_role("to_dummies"), threshold = 0.02, other="__other__") %>% 
  step_dummy(has_role("to_dummies")) %>% 
  step_impute_median(all_numeric(), -all_outcomes())

dp_pipe=prep(dp_pipe)

train=bake(dp_pipe, new_data=NULL)
test=bake(dp_pipe, new_data = h_test)

set.seed(2)
s=sample(1:nrow(train), 0.8*nrow(train))
t1=train[s,]
t2=train[-s,]

vis_dat(train)

fit=lm(Price~.-Type_X__other__-Postcode_X__other__, data = t1)
sort(vif(fit), decreasing=T)[1:3]

summary(fit)

fit=stats::step(fit)

formula(fit)

fit=lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + 
         BuildingArea + YearBuilt + Suburb_Richmond + Suburb_X__other__ + 
         Type_t + Type_u + Method_S + Method_SP +  
         SellerG_Biggin + SellerG_hockingstuart + SellerG_Jellis + 
         SellerG_Marshall + SellerG_X__other__ + Postcode_X31 + 
         Postcode_X32 + CouncilArea_Banyule + CouncilArea_Bayside + 
         CouncilArea_Boroondara + CouncilArea_Brimbank + CouncilArea_Darebin + 
         CouncilArea_Glen.Eira + CouncilArea_Hobsons.Bay + CouncilArea_Manningham + 
         CouncilArea_Maribyrnong + CouncilArea_Melbourne + CouncilArea_Moonee.Valley + 
         CouncilArea_Moreland + CouncilArea_Port.Phillip + CouncilArea_Stonnington + 
         CouncilArea_Yarra + CouncilArea_X__other__, data=t1)

summary(fit)

t2.pred=predict(fit, newdata = t2)
errors=t2$Price-t2.pred

rmse=errors**2 %>% mean() %>% sqrt()
rmse

fit.final=lm(Price~., data= train)
sort(vif(fit), decreasing = T)[1:3]

fit.final=stats::step(fit.final)
formula(fit.final)

fit.final=lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + 
               BuildingArea + YearBuilt + Suburb_Richmond + Suburb_X__other__ + 
               Type_t + Type_u + Method_S + Method_SP + Method_VB  + 
               SellerG_Biggin + SellerG_hockingstuart + SellerG_Jellis + 
               SellerG_Marshall + SellerG_Ray + Postcode_X31 + 
               Postcode_X32 + CouncilArea_Banyule + CouncilArea_Bayside + 
               CouncilArea_Boroondara + CouncilArea_Brimbank + CouncilArea_Darebin + 
               CouncilArea_Glen.Eira + CouncilArea_Hobsons.Bay + CouncilArea_Manningham + 
               CouncilArea_Maribyrnong + CouncilArea_Melbourne + CouncilArea_Moonee.Valley + 
               CouncilArea_Moreland + CouncilArea_Port.Phillip + CouncilArea_Stonnington + 
               CouncilArea_Yarra + CouncilArea_X__other__, data=train)

summary(fit.final)

test.pred=predict(fit.final, newdata=test)
write.csv(test.pred, "Nimmi_Tulsyan_P1_part2.csv", row.names = F)
