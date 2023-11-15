library(FactoMineR)
library(pheatmap)
library(RcppML)
library(ggplot2)
library(cowplot)
library(umap)
library(data.table)
library(tidyverse)

#Read in output from parse_dataframe.R
args <- commandArgs(trailingOnly = TRUE)
file <- args[1]

#Parse input datafile
df <- fread(file)
df_rare_c2 <- fread(file)
df_rare_c2 <- as.data.frame(df_rare_c2)
df2 <- as.data.frame(t(df))

df3 <- df2[ , grepl( "V" , names( df2) ) ]
df3 <- df3 %>% mutate_if(is.character, as.integer)
df_rare_c2 <- as.data.frame(df3)

df_rare_c2 <- df_rare_c2 %>% select(where(~ any(. != 0)))
df_rare_c2[is.na(df_rare_c2)] <- 0

#Run NMF
model <- RcppML::nmf(df_rare_c2, 2, verbose = F)

nmf_df <- as.data.frame(model$h)
colnames(nmf_df) <- colnames(df_rare_c2) 

#Create heatmap
save_pheatmap_pdf <- function(x, filename, width=7, height=7) {
   stopifnot(!missing(x))
   stopifnot(!missing(filename))
   pdf(filename, width=width, height=height)
   grid::grid.newpage()
   grid::grid.draw(x$gtable)
   dev.off()
}

df2 <- nmf_df
df3 <- as.data.frame(t(df2))
df3 <- df3 %>% rename(Component_1 = V1, 
                     Component_2 = V2)
df4 <- df3 
rownames(df4) <- seq.int(nrow(df4))

#Save heatmap
xx <- pheatmap(df4,
         main = "CNV",
         fontsize = 8,
                   show_row_dend=FALSE)
save_pheatmap_pdf(xx, "all_cnv_rare_coding.pdf")





