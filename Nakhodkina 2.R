Находкина Владлена Васильевна - для дневных данных за лето 2019 года, постройте регрессионную зависимость скорости сокотечения (переменная Flux) для деревьев с возрастом выше III


data = read.csv("data.csv")
summary(data)

data_filtered_1 = data[data$doy > 243 & data$doy < 335,]
data_filtered_2 = data_filtered_1[data_filtered_1$hour > 19 | data_filtered_1$hour < 7,]
data_filtered_3 = data_filtered_2[data_filtered_2$age_group_index == "IV" | data_filtered_2$age_group_index == "V",]
data_filtered_4 = data_filtered_3[data_filtered_3$in_site_antrop_load == "High",]
data_filtered_4=data_filtered_4[ ,-c("id", "Species","age_group_index","time", "antrop_load", 
                                     "in_site_antrop_load")]
library(dplyr)

data_filtered_4=select(data_filtered_4,-c("id", "Species","age_group_index","time", "antrop_load", 
                                          "in_site_antrop_load") )


install.packages("ggcorrplot")
corr = cor(data_filtered_4,use = "na.or.complete")^2
##corr=corr[corr>0.49]
#install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot(corr,
           type = "lower",
           insig = "blank",
           lab = TRUE,
           digits = 3
)
ggcorrplot(corr,
           tl.cex=4
)
flux_corr=corr[ ,"Flux"]
flux_corr=flux_corr[flux_corr>0.1]
#cor.test(data$flux, data$t1)

formula8 = Flux ~ u+rh
formula9 = Flux ~ u+rh + u:rh

model8 = lm(data=data_filtered_4, formula8)
model9 = lm(data=data_filtered_4, formula9)

summary(model8)
summary(model9)