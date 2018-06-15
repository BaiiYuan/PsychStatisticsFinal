df <- read.csv("responses.csv")
df$Gender = as.integer(df$Gender == "female")
#df$Education = as.factor(df$Education)
#as.numeric
#df$Education[df$Education == "secondary school"] = 2

mode = "Smoking"
df1 <- subset(df, select = c("Music", "Gender", "Education", "Only.child",
                             "Smoking", "Alcohol", "Keeping.promises",
                             "Reliability", "Cheating.in.school", "Lying", "Punctuality",
                             "Getting.angry", "Charity"))
df2 <- subset(df, select = c("Music", "Finances", "Branded.clothing", "Entertainment.spending",
                             "Spending.on.looks", "Spending.on.gadgets"))





a = cor.test(df1$Music, df1$Keeping.promises, method = 'pearson')

