library("openfda")
library("ggplot2")
library(nlme)
library(ggwordcloud)
library(ggpubr)
# Queries generally have the following format
## Not run: 
#fda_query(endpoint) %>%
#  fda_filter(field, value) %>%
#  fda_count(field) OR fda_search() %>%
#  fda_exec()

## End(Not run)

my_query = fda_query("/drug/event.json")

## Question 1 Are different adverse events reported in different countries? 

# ad types: death, disabling, hospitalizaion, lifethreatening
# congenitalanomali is less frequent happened and is not included in further analysis

ad.congenitalanomali = my_query %>%  fda_filter("seriousnesscongenitalanomali", "1")%>%
           fda_count("occurcountry") %>%
           fda_exec()
ad.congenitalanomali=data.frame(ad.death, type="congenitalanomali")


ad.death = my_query %>%  fda_filter("seriousnessdeath", "1")%>%
       fda_count("occurcountry") %>%
       fda_exec()
ad.death=data.frame(ad.death, type="death")


ad.disabling = my_query %>%  fda_filter("seriousnessdisabling", "1")%>%
  fda_count("occurcountry") %>%
  fda_exec()
ad.disabling=data.frame(ad.disabling, type="disabling")



ad.hospitalization = my_query %>%  fda_filter("seriousnesshospitalization", "1")%>%
  fda_count("occurcountry") %>%
  fda_exec()
ad.hospitalization=data.frame(ad.hospitalization, type="hospitalization")

ad.lifethreatening = my_query %>%  fda_filter("seriousnesslifethreatening", "1")%>%
  fda_count("occurcountry") %>%
  fda_exec()
ad.lifethreatening=data.frame(ad.lifethreatening, type="lifethreatening")


par(mfrow=c(2,2))
hist(log10(ad.death$count), xlab="log10 (Adverse Event)", prob = TRUE, main="Distribution of Death Event Among Countries")
lines(density(log10(ad.death$count)), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")


hist(log10(ad.disabling$count), xlab="log10 (Adverse Event)", prob = TRUE, main="Distribution of Disabling Event Among Countries")
lines(density(log10(ad.disabling$count)), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")


hist(log10(ad.hospitalization$count), xlab="log10 (Adverse Event)", prob = TRUE, main="Distribution of Hospitalization Event Among Countries")
lines(density(log10(ad.hospitalization$count)), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

hist(log10(ad.lifethreatening$count), xlab="log10 (Adverse Event)", prob = TRUE, main="Distribution of Lifethreatening Event Among Countries")
lines(density(log10(ad.lifethreatening$count)), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")


set.seed(1)
p.death=ggplot(ad.death, aes(label=term, size=count))+
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 100) +
  theme_minimal()

p.dis=ggplot(ad.disabling, aes(label=term, size=count))+
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 100) +
  theme_minimal()

p.lt=ggplot(ad.lifethreatening, aes(label=term, size=count))+
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 100) +
  theme_minimal()

p.hosp=ggplot(ad.hospitalization, aes(label=term, size=count))+
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 100) +
  theme_minimal()


figure=ggarrange(p.death, p.dis, p.lt, p.hosp, ncol=2, nrow=2, labels=c("Death","Disabling", "Lifethreatening","Hospitalization"))
figure
#### US, GB, JP, CA, FR, DE alway report most ADs   
#### No further analysis


### Question 2. What are the different adverse events associated with different disease areas? 
ad.indication=my_query %>% fda_count("patient.drug.drugindication.exact") %>% fda_exec()

## take the top 3 (RHEUMATOID ARTHRITIS, MULTIPLE SCLEROSIS, and HYPERTENSION)

type.in.indication=matrix(0, ncol=4, nrow=3)
ad.names=c("seriousnessdeath","seriousnessdisabling",  "seriousnesshospitalization","seriousnesslifethreatening" )
for (i in 1:3){
    disease=ad.indication$term[1+i]; disease=gsub(" ", "+", disease)
    for (j in 1:4){
      ad=ad.names[j]
      tmp=my_query %>% 
        fda_filter("patient.drug.drugindication.exact", disease)%>%
        fda_count(ad) %>% 
        fda_exec()
        type.in.indication[i,j]=unlist(tmp, use.name=F)[2]
    }
}

colnames(type.in.indication)=ad.names
rownames(type.in.indication)=c("RHEUMATOID ARTHRITIS", "MULTIPLE SCLEROSIS", "HYPERTENSION")
type.in.indication.percentage=round(type.in.indication/rowSums(type.in.indication)*100,2)


#                     seriousnessdeath seriousnessdisabling seriousnesshospitalization seriousnesslifethreatening
#RHEUMATOID ARTHRITIS            17.78                 6.56                      68.82                       6.84
#MULTIPLE SCLEROSIS              11.21                 3.49                      82.62                       2.68
#HYPERTENSION                    12.09                 6.15                      70.86                      10.91
### hospitalization is the most freqent adverse event 

### Question 3	What drugs tend to be taken together? 


tmp.ra=my_query %>% 
  fda_filter("patient.drug.drugindication.exact", "RHEUMATOID+ARTHRITIS")%>%
  fda_count("patient.drug.drugindication.exact") %>% 
  fda_exec()



tmp.mc=my_query %>% 
  fda_filter("patient.drug.drugindication.exact", "MULTIPLE+SCLEROSIS")%>%
  fda_count("patient.drug.drugindication.exact") %>% 
  fda_exec()


tmp.ht=my_query %>% 
  fda_filter("patient.drug.drugindication.exact", "HYPERTENSION")%>%
  fda_count("patient.drug.drugindication.exact") %>% 
  fda_exec()


#### pain and depression drugs tend to be taken together


