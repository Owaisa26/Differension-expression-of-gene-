
install.packages("limma")
 library(limma)
library(tibble)

#we need to remove target column 
data_2<-data[,-c(1)]
#transpose data 
data_t<-data.frame(t(data_2))
# Create design matrix

target<-as.factor(final_dataset$Target)
design <- model.matrix(~0+ target)


# Fit linear model
fit<-lmFit(data_t, design)


# Calculate empirical Bayes statistics
fit <- eBayes(fit)


# Create contrast matrix for differential expression analysis
contr.matrix <- makeContrasts(Group1_vs_Group2 = "Group1 - Group2", levels=design)

# Perform differential expression analysis
results <- decideTests(fit, method="global", adjust.method="BH", p=0.05, lfc=1, contrast=contr.matrix)

# Get the differentially expressed genes
DE_genes <- rownames(results)[results == "TRUE"]
