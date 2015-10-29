# Recode Educ sub-departments as Education
## If Descr is sub-department of education, code as education. Otherwise, original Descr
attach(data)
data$CoEduc = ifelse((Descr=="Teaching & Learning" | 
                          Descr=="Educational & Psychological" |
                          Descr=="Student Affairs" |
                          Descr=="Ldrshp Cnsl Adlt Career&Hi Ed" |
                          Descr=="Fla Ctr for Instructional Tech"),
"Education","Not Education")
detach(data)

# Subset by getting only the Education cases
Educ.data<-subset(data, CoEduc=="Education")

Educ.total.appts = as.integer(length(Educ.data$ID))

# Summary stats
## How many by FTE
Educ.FTE <- ddply(Educ.data, .(FTE),
                 summarise,
                 n=length(ID),
                 Proportion=(length(ID)/Educ.total.appts)
)
## How many by FTE x Degree Level
Educ.FTExDeg <- ddply(Educ.data, .(FTE,Degree.Level.Coded),
                      summarise,
                      n=length(ID),
                      Proportion=(length(ID)/Educ.total.appts)
)
