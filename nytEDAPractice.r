##################################################################################
#                                                                                #
#  Copyright (c) 2014 Yao Nien, Yang, paulyang0125@gmail.com                     #  
#  Licensed under the Apache License, Version 2.0 (the "License"); you may not   #
#  use this file except in compliance with the License. You may obtain a copy    #
#  of the License at http://www.apache.org/licenses/LICENSE-2.0. Unless required #
#  by applicable law or agreed to in writing, software distributed under the     #
#  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS  #
#  OF ANY KIND, either express or implied. See the License for the specific      #
#  language governing permissions and limitations under the License.             # 
#                                                                                #
##################################################################################


data1 = read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))
head(data1)
data1$agecat = cut(data1$Age, c(-Inf, 0, 18, 24, 34, 44, 54, 64, Inf)) # 用c()這個command去create一個從負的-Inf aka Not a Number (NaN), 18 一直到64 及正的NaN 其是就是infinite 代表一直延續的數的vector , 然後在用cut()將data1中的Age這個factor以之前產生的interval bin去做分類
summary(data1) # 使用summary function去知道資料基本狀態 你可以看到不同年齡group的各自的總數
install.packages("doBy") 
library("doBy") #安裝doBy這個package - 
siterange = function(x){c(length(x),min(x),mean(x),max(x))}
summaryBy(Age~agecat, data = data1, FUN=siterange)
summaryBy(Gender+Signed_In+Impressions+Clicks~agecat, data=data1)

##################

install.packages("ggplot2") # 安裝ggplot2套件
library("ggplot2")


################### Impression vs. agecat

ggplot(data1, aes(x=Impressions, fill=agecat)) + geom_histogram(binwidth=1)

ggplot(data1, aes(x=agecat, y=Impressions, fill=agecat)) + geom_boxplot()

################## CTR status vs. agecat 

data1$hasimp = cut(data1$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks~hasimp, data=data1, FUN=siterange) 

ggplot(subset(data1,Impressions>0),aes(x=Clicks/Impressions,colour=agecat)) + geom_density()

ggplot(subset(data1,Clicks>0),aes(x=Clicks/Impressions,colour=agecat)) + geom_density()

ggplot(subset(data1,Clicks>0),aes(x=agecat,y=Clicks,colour=agecat)) + geom_boxplot()

ggplot(subset(data1,Clicks>0),aes(x=Clicks, colour=agecat)) + geom_density()

#####################create一個scode欄位 去分類不同使用族群 (有click,只有impression 或 0)

data1$scode[data1$Impressions == 0] = 'NoImps' 
data1$scode[data1$Impressions > 0] = 'Imps'
data1$scode[data1$Clicks > 0] = 'Clicks'

data1$scode = factor(data1$scode)

clen = function(x){c(length(x))}
etable = summaryBy(Impressions~scode+Gender+agecat, data=data1, FUN=clen)

View(etable)

######################

data1$teens = cut(data1$Age, c(0,18))
summaryBy(Clicks~teens+Gender, data = data1, FUN = siterange)


