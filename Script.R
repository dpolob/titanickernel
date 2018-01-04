#RandomForest
library(dplyr)
library(ggplot2)
library(ggthemes)

getsetwd("C:/Users/Diego/3D Objects")

train=read.csv('train.csv',stringsAsFactors = FALSE,header = TRUE)
test=read.csv('test.csv',stringsAsFactors = FALSE,header = TRUE)

full=bind_rows(train,test)

#Empiezo por Embarque, la cual no debiera dar mucha información...
full%>%group_by(Embarked)%>%summarise(embarco=n())
#Veo que hay dos NA, los voy a cambiar a S que es lo mas frecuente y no creo que su valor influya
#Ademas lo cambio a factors
full$Embarked[full$Embarked=='']='S'
full$Embarked=as.factor(full$Embarked)

#Me meto con fare. Supongo que a mayor nivel adquisitivo mas se habran librado no? o había menos botes en su cubiertas
outcomes=full[1:891,]%>%group_by(Fare,Survived)%>%summarise(num_personas=n())
#Hay una pasada de fares, voy a dividirla en 4 categorias: luxury >300 200>high>300 100>medium>200 50>low>100 y 
#0>extralow>50
full$FareLevels=ifelse(full$Fare>=0 & full$Fare<50,'Extralow',ifelse(full$Fare>=50 & full$Fare<100,'Low',
                ifelse(full$Fare>=100 & full$Fare<200,'Medium',ifelse(full$Fare>=200 & full$Fare<300,'High','Luxury'))))
#hagamos el analisis

outcome=full[1:891,] %>% group_by(FareLevels,Survived) %>% summarise(num_pasajeros=n())
ggplot(outcome,aes(x=FareLevels, y=num_pasajeros,fill = Survived))+
        geom_bar(stat='identity',position='fill', colour='black')+
        labs(y='Pasajeros', x='Fare',title='Outcomes')+
        ggthemes::theme_few()
#Lo que imaginabamos, el que tiene mas pasta se libro, quiza estaba en cubiertas superiores y tuvo mas tiempo
#Lo voy a poner como factors
full$FareLevels=as.factor(full$FareLevels)

#Analizar tickets, por lo que veo me voy a quedar con los numeros y si es compuesto
full$TicketCompuesto=ifelse(grepl(pattern=" ",x=full$Ticket ),"Yes","No")
full$NumeroDeTicket=sapply(full$Ticket, function(x) strsplit(x, split=" ")[[1]][2])
full$NumeroDeTicket=ifelse(is.na(full$NumeroDeTicket),full$Ticket,full$NumeroDeTicket)

#Voy a ver si los tickets compuestos hacen algo o no
outcome=full[1:801,]%>%group_by(TicketCompuesto,Survived)%>%summarise(num_pasajeros=n())
ggplot(outcome, aes(x=TicketCompuesto, y=num_pasajeros,fill=Survived))+geom_bar(stat="identity",position="fill")+ggthemes::theme_few()
#no me dice nada pero parece que el compuesto libra mas.. no se porque.

#Queda alguno que no es numerico
full$NumeroDeTicket=ifelse(grepl(x=full$NumeroDeTicket,pattern="[0-9]"),full$NumeroDeTicket,"0")
#convertir a numero
full$NumeroDeTicket=as.numeric(full$NumeroDeTicket)

#Ver como esta el sexo
full$Sex=as.factor(full$Sex)
outcome=full[1:891,]%>%group_by(Sex,Survived)%>%summarise(num_supervivientes=n())
ggplot(outcome, aes(x=Sex,y=num_supervivientes,fill=Survived))+
    geom_bar(stat='identity',position='fill', colour='Black')+ggthemes::theme_few()
#las mujeres libran mas, ya lo esperaba
#Ahora la clase, nada que reseñar
full$Pclass=as.factor(full$Pclass)



full$RangeAge=ifelse(full$Age<5,'Kid',ifelse(full$Age<18, "Teenager",
                                             ifelse(full$Age<45,"Adult",
                                                    ifelse(full$Age<90,"elder",
                                                           is.na()))))
full[1:891,]%>%group_by(Survived)%>%summarise(media=mean(Age,na.rm=TRUE))


#histograma de distribucion de edades
agemen=full%>%filter(Sex=='male' & !is.na(Age))
agewomen=full%>%filter(Sex=='female' & !is.na(Age))
data=data.frame(Age=agemen$Age,Gender='Man')
data2=data.frame(Age=agewomen$Age,Gender='Woman')
data=rbind(data,data2)
ggplot(data, aes(Age,fill=Gender))+geom_histogram(alpha=0.6)+ggthemes::theme_fivethirtyeight()
#muy similar

#bonita grafica
outcome=full[1:891,]%>%group_by(RangeAge,Sex,Survived)%>%summarise(num_pasajeros=n())
ggplot(outcome, aes(x=RangeAge, y=num_pasajeros,fill=Survived))+geom_bar(stat='identity',position='fill')+ facet_wrap(~Sex)+ggthemes::theme_economist_white()
#siendo male o eres kid o la palmas, con mujeres es distinto, por lo que que eso de mujeres y niños primero
#todavia hay:
sum(is.na(full$Age))
#sin saber la edad, siendo un factor tan clave habría que predecirla de alguna manera
#Voy a reducir a apellido o FamilyName
full$Surname=sapply(full$Name, function(x) strsplit(x,split=",")[[1]][1])
#Otra variable si son familia o alone (para ello si SbSp o Parch es diferente de 0 => hay relaciones)
full$IsFamily=ifelse(full$SibSp>0 | full$Parch>0,"Yes","No")
#Otra variable tratamiento
data=sapply(full$Name, function(x) strsplit(x, split=", ")[[1]][2])
full$Treatment=sapply(data, function(x) strsplit(x, split="[.]")[[1]][1])
levels(factor(full$Treatment))
nombresmasculinos=c("Capt","Col","Don","Dr","Jonkheer","Major","Master","Mr","Rev","Sir")
nombresfemeninos=c("Dona","Mlle","Mme","Mrs","Ms","the Countess")
full$Treatment=ifelse(grepl(paste(nombresmasculinos,collapse="|"),full$Treatment),"Mr",ifelse(grepl(paste(nombresfemeninos,collapse="|"), full$Treatment),"Mrs","Miss"))
full$Treatment=as.factor(full$Treatment)
#Tenemos que solucionar el tema de la edad. Para ello tomare la clase y el tratamiento y su edad media
edadesmedias=full%>%group_by(Pclass,Treatment)%>%summarise(edadamedia=mean(Age,na.rm=TRUE))
#Quito el ultimo que es NaN
edadesmedias=edadesmedias[-9,]
#y tendre que poner algo por defecto
mediadeedaddesenoras=mean(full$Age[which(full$Treatment=="Mrs")],na.rm=TRUE)


full$Age[which(is.na(full$Age))]=ifelse(full$Pclass==1 & full$Treatment=="Miss",edadesmedias$edadamedia[1],
                                 ifelse(full$Pclass==1 & full$Treatment=="Mr",edadesmedias$edadamedia[2],
                                ifelse(full$Pclass==1 & full$Treatment=="Mrs",edadesmedias$edadamedia[3],
                                ifelse(full$Pclass==2 & full$Treatment=="Miss",edadesmedias$edadamedia[4],
                                ifelse(full$Pclass==2 & full$Treatment=="Mr",edadesmedias$edadamedia[5],
                                ifelse(full$Pclass==2 & full$Treatment=="Mrs",edadesmedias$edadamedia[6],
                                ifelse(full$Pclass==3 & full$Treatment=="Miss",edadesmedias$edadamedia[7],
                                ifelse(full$Pclass==3 & full$Treatment=="Mr",edadesmedias$edadamedia[8],
                                mediadeedaddesenoras))))))))

#Vuelvo a aplicar el RangeAge 
full$RangeAge=ifelse(full$Age<5,'Kid',ifelse(full$Age<18, "Teenager",
                                             ifelse(full$Age<45,"Adult",
                                                    ifelse(full$Age<90,"elder",
                                                           is.na()))))
#y no debiera haber mas, se comprueba
sum(is.na(full$Age))

#y ya no se me ocurre mas!! asi que a aplicar el randomforest







