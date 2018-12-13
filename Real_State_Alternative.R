#Data acquation step

setwd('E:/documents/R Course/Real Estate')   #setting derictory path

house_ing = read.csv('housing_train.csv', stringsAsFactors = FALSE)  # Uploading the data set on R 

library(dplyr)

glimpse(house_ing) #getting the structure of the data frame 

#######################################################################
#Data Prepration begins

#Imputing the NA values with the mean of their respective column
house_ing[is.na(house_ing$Landsize), "Landsize"] <- mean(house_ing$Landsize,na.rm = T)
round(tapply(house_ing$Price,house_ing$Postcode,mean), 0)

#again having a look on the data
glimpse(house_ing)

####### Creating the dummy variable for the categorical columns #######
house_ing=house_ing %>%
  mutate(Type_h=as.numeric(Type=="h"),
         Type_t=as.numeric(Type=="t")
  ) %>%
  select(-Type)

house_ing = house_ing %>% 
  mutate(c_area_1=as.numeric(CouncilArea %in% c("Banyule","Hobsons Bay","Kingston","Moonee Valley","Melbourne")),
         c_area_2=as.numeric(CouncilArea %in% c("Bayside","Boroondara")),
         c_area_3=as.numeric(CouncilArea %in% c("Monash","Port Phillip","Glen Eira")),
         c_area_4=as.numeric(CouncilArea %in% c("Stonnington","Whitehorse")),
         c_area_5=as.numeric(CouncilArea %in% c("Manningham","Yarra")),
         c_area_6=as.numeric(CouncilArea %in% c("Moreland","Maribyrnong")),
         c_area_7=as.numeric(CouncilArea %in% c("Hume")),
         c_area_8=as.numeric(CouncilArea %in% c("Darebin")),
         c_area_9=as.numeric(CouncilArea %in% c("Brimbank"))
  ) %>%
  select(-CouncilArea)

house_ing=house_ing %>% 
  select(-Address)

house_ing = house_ing %>%
  mutate(PC_1 = as.numeric(Postcode %in% c("3021","3047","3008","3000")),
         PC_2 = as.numeric(Postcode %in% c("3043","3060","3006","3073","3020","3046","3083","3019","3087")),
         PC_3 = as.numeric(Postcode %in% c( "3081","3055","3183","3044","3012","3034","3033","3025","3018","3042","3011","3031")),
         PC_4 = as.numeric(Postcode %in% c( "3085","3058","3072","3182","3066","3051","3071","3032" )),
         PC_5 = as.numeric(Postcode %in% c("3163","3003","3184","3167","3189","3166","3015","3141","3057","3056","3013")),
         PC_6 = as.numeric(Postcode %in% c("3165","3148","3162","3084","3181","3121","3040","3068")),
         PC_7 = as.numeric(Postcode %in% c("3122","3053","3065","3041","3185","3067","3079","3078","3039","3107","3105","3128","3204")),
         PC_8 = as.numeric(Postcode %in% c("3205","3016","3207","3125","3108","3161","3052")),
         PC_9 = as.numeric(Postcode %in% c( "3070","3002","3147" )),
         PC_10 = as.numeric(Postcode %in% c("3145","3054","3188" )),
         PC_11 = as.numeric(Postcode %in% c("3123","3127","3187","3143")),
         PC_12 = as.numeric(Postcode %in% c("3102","3124")),
         PC_13 = as.numeric(Postcode %in% c("3146","3142","3206","3104","3101")),
         PC_14 = as.numeric(Postcode %in% c("3144","3186")),
         PC_15 = as.numeric(Postcode %in% c("3103","3126"))
         ) %>%
  select(-Postcode)
house_ing=house_ing %>%
  mutate(Meth_1=as.numeric(Method=="PI"),
         Meth_2=as.numeric(Method=="S"),
         Meth_3=as.numeric(Method=="SP"),
         Meth_4=as.numeric(Method=="VB")
         ) %>%
  select(-Method)

trail = round(tapply(house_ing$Price,house_ing$Suburb,mean), 0)
sort(trail)

house_ing = house_ing %>% 
  mutate(surb_1 = as.numeric(Suburb %in% c("Brooklyn","Albion")),
         surb_2 = as.numeric(Suburb %in% c("Kealba","Jacana","Sunshine West","Docklands","Ripponlea","Melbourne")),
         surb_3 = as.numeric(Suburb %in% c("Gowanbrae","Glenroy","Southbank","Keilor Park","Reservoir","Fawkner","Sunshine North","Kingsbury","Heidelberg West","Hadfield","Braybrook","Maidstone","Watsonia","South Kingsville","Footscray")),
         surb_4 = as.numeric(Suburb %in% c("Airport West","Sunshine","Brunswick West","Balaclava","Pascoe Vale","Heidelberg Heights","Avondale Heights","Kingsville","Altona","Keilor East","Altona North","Maribyrnong","Flemington","West Footscray")),
         surb_5 = as.numeric(Suburb %in% c("Yallambie","Bellfield","Kensington","Oak Park","Coburg North","Carnegie","Preston","Coburg","St Kilda","Viewbank","Strathmore Heights","Niddrie","Collingwood","North Melbourne","Thornbury","Essendon North","Heidelberg","Williamstown North" )),
         surb_6 = as.numeric(Suburb %in% c("West Melbourne","Elwood","Oakleigh South","Moorabbin","Oakleigh","Spotswood","Caulfield","South Yarra","Hughesdale","Ascot Vale","Brunswick East","Seddon","Brunswick","Cremorne","Glen Huntly","Rosanna","Clifton Hill","Yarraville")),
         surb_7 = as.numeric(Suburb %in% c("Bentleigh East","Newport","Windsor","Essendon West","Ormond","Murrumbeena","Essendon","Hampton East","Chadstone","Gardenvale","Caulfield East","Caulfield South","Ivanhoe","Ashwood","Richmond")),
         surb_8 = as.numeric(Suburb %in% c("Prahran  Fairfield","Hawthorn","Carlton","Fitzroy","Abbotsford","Fitzroy North","Moonee Ponds","Templestowe Lower","Bulleen","Box Hill","Elsternwick")),
         surb_9 = as.numeric(Suburb %in% c("South Melbourne","Port Melbourne","Burwood","Alphington","Burnley","Doncaster","Aberfeldie","Williamstown","Strathmore","Caulfield North","Bentleigh","Parkville","Kooyong")),
         surb_10 = as.numeric(Suburb %in% c("Northcote","East Melbourne","Carlton North")),
         surb_11= as.numeric(Suburb %in% c("Travancore","Seaholme","Malvern East","Ivanhoe East","Surrey Hills")),
         surb_12= as.numeric(Suburb %in% c("Hawthorn East","Brighton East","Armadale","Ashburton")),
         surb_13= as.numeric(Suburb %in% c("Hampton","Kew East","Camberwell")),
         surb_14= as.numeric(Suburb %in% c("Glen Iris","Toorak","Albert Park","Balwyn North","Kew","Middle Park")),
         surb_15= as.numeric(Suburb %in% c("Mont Albert","Malvern","Brighton","Eaglemont")),
         surb_16= as.numeric(Suburb %in% c("Princes Hill","Balwyn","Canterbury"))
        ) %>% 
  select(-Suburb)
###############################################################################
######## Dividing the data set into traning and testing #########
set.seed(1)
s=sample(1:nrow(house_ing),0.75*nrow(house_ing))
houseing_train=house_ing[s,] # Trainset: 75% of rows
houseing_test=house_ing[-s,] # Testset: 25% of rows

###Fitting the data for our dependent variable####
# dependent variable is the one on which we do our prediction where as independent variables are the variables that becomes the deciding factor in the prediction of the dependent variable##

fit=lm(Price ~ ., data=houseing_train)## . stands for "Include ALL VARIABLES "
summary(fit)

house_ing = house_ing %>% 
  select(- surb_14)

houseing_train = houseing_train %>% 
  select(- surb_14)


library(car)
tes_t=vif(fit)# checking vif
sort(tes_t,decreasing = T)

fit = lm(Price ~. - c_area_1, data = house_ing)

fit=lm(Price ~ . -c_area_1 -PC_3, data=houseing_train)
summary(fit)

fit=lm(Price ~ . -PC_3 - c_area_1 -Meth_2, data=houseing_train)
summary(fit)
tes_t=vif(fit)
sort(tes_t,decreasing = T)

fit=lm(Price ~ . -PC_3 - c_area_1 -Meth_2 -surb_3, data=houseing_train)
tes_t=vif(fit)
sort(tes_t,decreasing = T)
summary(fit)

fit=lm(Price ~ . -PC_3 - c_area_1 -Meth_2 -surb_3 - PC_11, data=houseing_train)
tes_t=vif(fit)
sort(tes_t,decreasing = T)

fit=lm(Price ~ . -PC_3 - c_area_1 -Meth_2 -surb_3 - PC_11 - Rooms, data=houseing_train)
tes_t=vif(fit)
sort(tes_t,decreasing = T)


###############################################################3

# Checking performance using test set
ir_predict=predict(fit,newdata=houseing_test)

# Lets put Actual, Predicted and Errors (of testset) in a dataframe
d=data.frame(Actual=houseing_test$Price, Predicted=ir_predict, Error=houseing_test$Price-ir_predict)
View(d)

## Plot Actual vs. Predicted values 
plot(d$Actual,d$Predicted) # Looks okay
# Ideally, all your points should be close to a 45 degree diagonal line. 
# So, if the Actual is 5, your predicted should be reasonably close to 5. 
# If the Actual is 30, your predicted should also be reasonably close to 30.



# # Mean Absolute Percentage Error (MAPE)
MAPE_test = mean(abs(d$Error/d$Actual),na.rm = T)*100
MAPE_test # Lower the better

# Root Mean Square Error (RMSE)
RMSE_test=sqrt(mean(d$Error^2))
RMSE_test # Lower the better


############################################################################################



# prediction for the houseing_test dataset
house_ingt = read.csv('housing_test.csv', stringsAsFactors = FALSE)
house_ingt = house_ingt %>%
  select(-YearBuilt,-BuildingArea,-SellerG)
#house_ingt = filter(house_ingt,!is.na(house_ingt$Bedroom2) & !is.na(house_ingt$Bathroom))# rows who don't have any any re data 
house_ingt[is.na(house_ingt$Landsize), "Landsize"] <- mean(house_ingt$Landsize,na.rm = T)
house_ingt[is.na(house_ingt$Bedroom2), "Bedroom2"] <- mean(house_ingt$Bedroom2,na.rm = T)
house_ingt[is.na(house_ingt$Bathroom), "Bathroom"] <- mean(house_ingt$Bathroom,na.rm = T)
house_ingt[is.na(house_ingt$Car), "Car"] <- mean(house_ing$Car,na.rm = T)
#removing out lier in the Landsize
boxplot(house_ingt$Landsize)$out
house_ingt[house_ingt$Landsize > 20000 , "Landsize"] <- mean(house_ingt$Landsize)
boxplot(house_ingt$Landsize)$out

data = house_ingt

glimpse(house_ingt)

house_ingt =house_ingt  %>%
  mutate(Type_h=as.numeric(Type=="h"),
         Type_t=as.numeric(Type=="t")
  ) %>%
  select(-Type)

house_ingt = house_ingt %>% 
  mutate(c_area_1=as.numeric(CouncilArea %in% c("Banyule","Hobsons Bay","Kingston","Moonee Valley","Melbourne")),
         c_area_2=as.numeric(CouncilArea %in% c("Bayside","Boroondara")),
         c_area_3=as.numeric(CouncilArea %in% c("Monash","Port Phillip","Glen Eira")),
         c_area_4=as.numeric(CouncilArea %in% c("Stonnington","Whitehorse")),
         c_area_5=as.numeric(CouncilArea %in% c("Manningham","Yarra")),
         c_area_6=as.numeric(CouncilArea %in% c("Moreland","Maribyrnong")),
         c_area_7=as.numeric(CouncilArea %in% c("Hume")),
         c_area_8=as.numeric(CouncilArea %in% c("Darebin")),
         c_area_9=as.numeric(CouncilArea %in% c("Brimbank"))
  ) %>%
  select(-CouncilArea)

house_ingt = house_ingt %>% 
  select(-Address)

house_ingt = house_ingt %>%
  mutate(PC_1 = as.numeric(Postcode %in% c("3021","3047","3008","3000")),
         PC_2 = as.numeric(Postcode %in% c("3043","3060","3006","3073","3020","3046","3083","3019","3087")),
         PC_3 = as.numeric(Postcode %in% c( "3081","3055","3183","3044","3012","3034","3033","3025","3018","3042","3011","3031")),
         PC_4 = as.numeric(Postcode %in% c( "3085","3058","3072","3182","3066","3051","3071","3032" )),
         PC_5 = as.numeric(Postcode %in% c("3163","3003","3184","3167","3189","3166","3015","3141","3057","3056","3013")),
         PC_6 = as.numeric(Postcode %in% c("3165","3148","3162","3084","3181","3121","3040","3068")),
         PC_7 = as.numeric(Postcode %in% c("3122","3053","3065","3041","3185","3067","3079","3078","3039","3107","3105","3128","3204")),
         PC_8 = as.numeric(Postcode %in% c("3205","3016","3207","3125","3108","3161","3052")),
         PC_9 = as.numeric(Postcode %in% c( "3070","3002","3147" )),
         PC_10 = as.numeric(Postcode %in% c("3145","3054","3188" )),
         PC_11 = as.numeric(Postcode %in% c("3123","3127","3187","3143")),
         PC_12 = as.numeric(Postcode %in% c("3102","3124")),
         PC_13 = as.numeric(Postcode %in% c("3146","3142","3206","3104","3101")),
         PC_14 = as.numeric(Postcode %in% c("3144","3186")),
         PC_15 = as.numeric(Postcode %in% c("3103","3126"))
  ) %>%
  select(-Postcode)
house_ingt = house_ingt %>%
  mutate(Meth_1=as.numeric(Method=="PI"),
         Meth_2=as.numeric(Method=="S"),
         Meth_3=as.numeric(Method=="SP"),
         Meth_4=as.numeric(Method=="VB")
  ) %>%
  select(-Method)



house_ingt = house_ingt %>% 
  mutate(surb_1 = as.numeric(Suburb %in% c("Brooklyn","Albion")),
         surb_2 = as.numeric(Suburb %in% c("Kealba","Jacana","Sunshine West","Docklands","Ripponlea","Melbourne")),
         surb_3 = as.numeric(Suburb %in% c("Gowanbrae","Glenroy","Southbank","Keilor Park","Reservoir","Fawkner","Sunshine North","Kingsbury","Heidelberg West","Hadfield","Braybrook","Maidstone","Watsonia","South Kingsville","Footscray")),
         surb_4 = as.numeric(Suburb %in% c("Airport West","Sunshine","Brunswick West","Balaclava","Pascoe Vale","Heidelberg Heights","Avondale Heights","Kingsville","Altona","Keilor East","Altona North","Maribyrnong","Flemington","West Footscray")),
         surb_5 = as.numeric(Suburb %in% c("Yallambie","Bellfield","Kensington","Oak Park","Coburg North","Carnegie","Preston","Coburg","St Kilda","Viewbank","Strathmore Heights","Niddrie","Collingwood","North Melbourne","Thornbury","Essendon North","Heidelberg","Williamstown North" )),
         surb_6 = as.numeric(Suburb %in% c("West Melbourne","Elwood","Oakleigh South","Moorabbin","Oakleigh","Spotswood","Caulfield","South Yarra","Hughesdale","Ascot Vale","Brunswick East","Seddon","Brunswick","Cremorne","Glen Huntly","Rosanna","Clifton Hill","Yarraville")),
         surb_7 = as.numeric(Suburb %in% c("Bentleigh East","Newport","Windsor","Essendon West","Ormond","Murrumbeena","Essendon","Hampton East","Chadstone","Gardenvale","Caulfield East","Caulfield South","Ivanhoe","Ashwood","Richmond")),
         surb_8 = as.numeric(Suburb %in% c("Prahran  Fairfield","Hawthorn","Carlton","Fitzroy","Abbotsford","Fitzroy North","Moonee Ponds","Templestowe Lower","Bulleen","Box Hill","Elsternwick")),
         surb_9 = as.numeric(Suburb %in% c("South Melbourne","Port Melbourne","Burwood","Alphington","Burnley","Doncaster","Aberfeldie","Williamstown","Strathmore","Caulfield North","Bentleigh","Parkville","Kooyong")),
         surb_10 = as.numeric(Suburb %in% c("Northcote","East Melbourne","Carlton North")),
         surb_11= as.numeric(Suburb %in% c("Travancore","Seaholme","Malvern East","Ivanhoe East","Surrey Hills")),
         surb_12= as.numeric(Suburb %in% c("Hawthorn East","Brighton East","Armadale","Ashburton")),
         surb_13= as.numeric(Suburb %in% c("Hampton","Kew East","Camberwell")),
         surb_14= as.numeric(Suburb %in% c("Glen Iris","Toorak","Albert Park","Balwyn North","Kew","Middle Park")),
         surb_15= as.numeric(Suburb %in% c("Mont Albert","Malvern","Brighton","Eaglemont"))
  ) %>% 
  select(-Suburb)

mn = mean(d$Predicted)

ir_predict = predict(fit,newdata=house_ingt) 

data_set = data.frame(data, predicted = ir_predict)
View(data_set)

data_set = data_set %>% 
  mutate(predicted = ifelse(is.na(predicted)== T,mn, predicted ))

