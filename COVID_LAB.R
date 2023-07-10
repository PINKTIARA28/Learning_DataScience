file<-"D:\\SEMESTER 2\\Data Mining and visualization\\LAB\\COVID.csv"
ti<-read.csv(file)
head(ti)

summary(ti)

ti1<-ti
dim(ti1)
sum(is.na(ti1))
#hasilnya banyak missing value

#cari dimana missinf valnya
colSums(is.na(ti1))
ti2<-na.omit(ti1)
dim(ti2)
#banyak data yg didrop, jadi liat corr dl, yg mana paling berhubungan

ti3<-ti2
ti4<-ti2

#ubah ke numeric biar bisa di cor
str(ti3)
ti3$presence_of_water_violation<-as.numeric(factor(as.matrix(ti3$presence_of_water_violation)))
ti3$numdate<-as.numeric(as.Date(ti3$date))
ti3$numdatestay<-as.numeric(as.POSIXct(ti3$date_stay_at_home_effective))
ti3$numdateannoun<-as.numeric(as.POSIXct(ti3$date_stay_at_home_announced))
ti3$fipsnum<-as.numeric(ti3$fips)
ti3$county<-as.numeric(factor(as.matrix(ti3$county)))
ti3$state<-as.numeric(factor(as.matrix(ti3$state)))
ti3$CALL<-as.numeric(factor(as.matrix(ti3$CALL)))
ti3$station_name<-as.numeric(factor(as.matrix(ti3$station_name)))
ti3$precip_flag<-as.numeric(factor(as.matrix(ti3$precip_flag)))
ti3$stay_at_home_announced<-as.numeric(factor(as.matrix(ti3$stay_at_home_announced)))
ti3$stay_at_home_effective<-as.numeric(factor(as.matrix(ti3$stay_at_home_effective)))



ti5<-ti3
#cek nomor kolom yg mau didrop
non_num<-unlist(lapply(ti5,function(x)!is.numeric(x)))
print(non_num)
non_numname<-names(ti5)[non_num]
print(non_numname)
col_num<-which(colnames(ti5)=="date")
col_num<-which(colnames(ti5)=="fips")
col_num<-which(colnames(ti5)=="date_stay_at_home_announced")
col_num<-which(colnames(ti5)=="date_stay_at_home_effective")

print(col_num)
ti5<-ti5[-1]
ti5<-ti5[-3]
ti5<-ti5[-224]
ti5<-ti5[-224]


dim(ti5)


coor<-cor(ti5)

heatmap(coor)



#install.packages("skimr")
library(skimr)
skim(ti5)


boxplot(ti5$num_deaths)

remove_outliers <- function(ti5, multiplier = 1.5) {
  ti5_outliers_removed <- ti5
  
  for (col in names(ti5)) {
    data <- ti5[[col]]
    Q1 <- quantile(data, probs = 0.25)
    Q3 <- quantile(data, probs = 0.75)
    IQR <- Q3 - Q1
    
    upper_limit <- Q3 + (multiplier * IQR)
    lower_limit <- Q1 - (multiplier * IQR)
    
    outliers <- data > upper_limit | data < lower_limit
    ti5_outliers_removed[[col]][outliers] <- NA
  }
  
  ti5_outliers_removed
}

ti5_outliers_removed <- remove_outliers(ti5, multiplier = 1.5)

library(ggplot2)
ggplot(ti5, aes(x = ti5$stay_at_home_effective, y = ti2$state)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(x = "", y = "state", title = "stay at home effective in state") +
  theme_bw()
ggplot(ti5, aes(x = ti5$stay_at_home_effective, y = ti2$stay_at_home_announced)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(x = "", y = "effective", title = "Keefektivan stay at home ketika stay at home diumumkan") +
  theme_bw()


library(dplyr)

ti2_sorted <- ti5 %>%
  arrange(desc(cases))

ggplot(ti2_sorted) +
  geom_bar(aes(x = cases, y = ti2$state, fill = "Cases"), stat = "identity", position = "stack") +
  geom_bar(aes(x = deaths, y = ti2$state, fill = "Deaths"), stat = "identity", position = "stack") +
  labs(x = "Count", y = "State", title = "Cases and Deaths by State") +
  scale_fill_manual(values = c("Cases" = "skyblue", "Deaths" = "pink")) +
  theme_bw()

total_cases_texas <- sum(ti2$cases[ti2$state== "Texas"])

print(total_cases_texas)
total_death_florida <- sum(ti2$deaths[ti2$state== "Florida"])
print(total_death_florida)

ggplot(ti2, aes(x = ti2$cases, y = ti2$state)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(x = "", y = "state", title = "Banyak case di setiap state") +
  theme_bw()

write.csv(ti2, file = "covidbaru.csv", row.names = FALSE)
write.csv(ti5, file = "covidbaru2.csv", row.names = FALSE)


library(corrplot)
ti6 <- data.frame(ti5$deaths,ti5$stay_at_home_announced,ti5$stay_at_home_effective,ti5$cases)
co<-cor(ti6)
corrplot(co,method = 'square')



total_cases_texas <- sum(ti5$stay_at_home_announced[ti2$state== "Florida"])
total_cases_texas

