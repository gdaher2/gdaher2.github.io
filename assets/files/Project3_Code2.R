attorneys <- read.csv("~/College/2023 DataFest/DataFest 2023 Data For Distribution/data/attorneys.csv")
questionposts <- read.csv("~/College/2023 DataFest/DataFest 2023 Data For Distribution/data/questionposts.csv")


ymd_hms()
temp<-questionposts[order(QuestionUno),]
