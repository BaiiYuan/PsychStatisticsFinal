#### DATA LOAD
df <- read.csv("pre-processed_data.csv")
df <- na.omit(df)
#df$Gender = as.integer(df$Gender == "female")
df1 <- subset(df, select = c("Musical.instruments",
                             #"Gender", "Education", "Only.child",
                             "Smoking", "Alcohol", "Keeping.promises",
                             "Reliability", "Cheating.in.school", "Lying", "Punctuality",
                             "Getting.angry", "Charity"))
df1 <- df1[(df1$Smoking) != 2,]
df1$Smoking = as.integer(df1$Smoking >= 2)
df1$Alcohol = as.integer(df1$Alcohol >= 1)
#### DATA LOAD


#### CORR MATRIX
cor(df1)
#### CORR MATRIX


#### ANOVA
library(jmv)
anova(df1, dep="Musical.instruments", factors="Smoking", descStats = T, homo = T, effectSize = "eta")
stats::anova(lm(Musical.instruments~Smoking, data = df1))

anova(df1, dep="Musical.instruments", factors="Alcohol", descStats = T, homo = T, effectSize = "eta")
stats::anova(lm(Musical.instruments~Alcohol, data = df1))

anova(df1, dep="Musical.instruments", factors="Keeping.promises", descStats = T, homo = T, effectSize = "eta")
stats::anova(lm(Musical.instruments~Keeping.promises, data = df1))

anova(df1, dep="Musical.instruments", factors="Reliability", descStats = T, homo = T, effectSize = "eta")
stats::anova(lm(Musical.instruments~Reliability, data = df1))

anova(df1, dep="Musical.instruments", factors="Cheating.in.school", descStats = T, homo = T, effectSize = "eta")
stats::anova(lm(Musical.instruments~Cheating.in.school, data = df1))

anova(df1, dep="Musical.instruments", factors="Lying", descStats = T, homo = T, effectSize = "eta")
stats::anova(lm(Musical.instruments~Lying, data = df1))

anova(df1, dep="Musical.instruments", factors="Punctuality", descStats = T, homo = T, effectSize = "eta")
stats::anova(lm(Musical.instruments~Punctuality, data = df1))

anova(df1, dep="Musical.instruments", factors="Getting.angry", descStats = T, homo = T, effectSize = "eta")
stats::anova(lm(Musical.instruments~Getting.angry, data = df1))

anova(df1, dep="Musical.instruments", factors="Charity", descStats = T, homo = T, effectSize = "eta")
stats::anova(lm(Musical.instruments~Charity, data = df1))
#### ANOVA


#summary(df1)


#install.packages("polycor")
library("polycor")
library("vcd")


#test
#data("Arthritis")
#tab <- xtabs(~Improved + Treatment, data = Arthritis)
#summary(assocstats(tab))




# Smoke
# df_smoke <- subset(df, select = c("Musical.instruments", "Smoking"))
# df_smoke <- df_smoke[(df_smoke$Smoking) != 2,]
# df_smoke$Smoking = as.integer(df_smoke$Smoking >= 2)
#polychoric(df_smoke[,c(1,2)])


polychor(df1$Musical.instruments, df1$Smoking)
chisq.test(df1$Musical.instruments, df1$Smoking, correct = F)

# Alcohol
# df_alc <- subset(df, select = c("Musical.instruments", "Alcohol"))
# df_alc$Alcohol = as.integer(df_alc$Alcohol >= 1)
#polychoric(df_alc[,c(1,2)])
polychor(df1$Musical.instruments, df1$Alcohol)
chisq.test(df1$Musical.instruments, df1$Alcohol, correct = F)



# Keeping promises
cor.test(rank(df1$Musical.instruments), rank(df1$Keeping.promises), method = 'pearson')
# Reliability
cor.test(rank(df1$Musical.instruments), rank(df1$Reliability), method = 'pearson')
# Cheating in school
cor.test(rank(df1$Musical.instruments), rank(df1$Cheating.in.school), method = 'pearson')
# Lying
cor.test(rank(df1$Musical.instruments), rank(df1$Lying), method = 'pearson')
# Punctuality
cor.test(rank(df1$Musical.instruments), rank(df1$Punctuality), method = 'pearson')
# Getting.angry
cor.test(rank(df1$Musical.instruments), rank(df1$Getting.angry), method = 'pearson')
# Charity
cor.test(rank(df1$Musical.instruments), rank(df1$Charity), method = 'pearson')

model1 <- lm(Musical.instruments ~ Keeping.promises + Reliability + Cheating.in.school
             + Lying + Punctuality + Getting.angry + Charity, df1)
summary(model1)




# not used

df2 <- subset(df, select = c("Musical.instruments", "Finances", "Branded.clothing","Entertainment.spending", 
                             "Spending.on.looks", "Spending.on.gadgets")) #1010
df2 <- na.omit(df2) #998


df2$Musical.instruments <- rank(df2$Musical.instruments)
df2$Finances <- rank(df2$Finances)
df2$Branded.clothing <- rank(df2$Branded.clothing)
df2$Entertainment.spending <- rank(df2$Entertainment.spending)
df2$Spending.on.looks <- rank(df2$Spending.on.looks)
df2$Spending.on.gadgets <- rank(df2$Spending.on.gadgets)

cor.test(df2$Musical.instruments, df2$Finances, method = 'pearson')
cor.test(df2$Musical.instruments, df2$Branded.clothing, method = 'pearson')
cor.test(df2$Musical.instruments, df2$Entertainment.spending, method = 'pearson')
cor.test(df2$Musical.instruments, df2$Spending.on.looks, method = 'pearson')
cor.test(df2$Musical.instruments, df2$Spending.on.gadgets, method = 'pearson')



