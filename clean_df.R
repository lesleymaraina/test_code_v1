library(tidyverse)
library(data.table)

#Read in file
args <- commandArgs(trailingOnly = TRUE)
file <- args[1]

#Parse input datafile
df <- fread(file)

df2 <- as.data.frame(t(df))
#colnames(df2) <- df$sampleID
#df2 <- df2 %>% select(-c(varSum))
print(head(df2))

df3 <- df2 %>% mutate_if(is.character, str_trim)
print(head(df3))

df3$one <- rowSums(df3 == "1")
df3$two <- rowSums(df3 == "2")

df3$sumvar <- df3$one + df3$two
sum <- ncol(df2)
df3$ratio <- df3$sumvar/sum

df4 <- df3 %>% filter(ratio <= 0.4)
df5 <- as.data.frame(t(df4))
df5$sampleID <- rownames(df5)

df5 <- df5 %>% select(-c(sampleID))

write.table(df5,row.names=FALSE, col.names = F, quote=FALSE, sep = '\t', file='input_df_1.tsv')

