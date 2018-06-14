df <- read.csv("responses.csv")
mode = "Smoking"
df1 <- subset(df, select = c("Music", "Smoking", "Alcohol", "Keeping.promises",
                             "Reliability", "Cheating.in.school", "Lying", "Punctuality",
                             "Getting.angry", "Charity"))
df2 <- subset(df, select = c("Music", "Finances", "Branded.clothing", "Entertainment.spending",
                             "Spending.on.looks", "Spending.on.gadgets"))

if (mode == "Smoking") {
  
}
