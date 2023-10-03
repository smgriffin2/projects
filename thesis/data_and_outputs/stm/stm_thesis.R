## Thesis, GPT, Climate Mitigation Appeals, Partisan Messenging
## Kaiping Chen
## November 2, 2022
## Dataset we used: Canvas -> Module -> Week 9 -> "arboretum.csv"

# Step 1: Install the Structural Topic Model Package
install.packages("stm")
library("stm")
install.packages("tm")
library("tm")
install.packages("Rtsne")
library("Rtsne")
install.packages("geometry")
library("geometry")
install.packages("rsvd")
library("rsdv")




# set up your working directory 
# remember to adjust this code line to set the working directory to the place where you stored HW3 dataset and this script
setwd("~/Documents/lsc/thesis/testing/stm")

# load data
data <- read.csv("stm_data.csv") 
colnames(data) #Description, Post_Type, Post.Created.Time, etc.

# subset data based on policy varuable: tree, tax, fuel
tree_data = data[data$policy == "tree",]
tax_data = data[data$policy == "tax",]
fuel_data = data[data$policy == "fuel",]

# ::::::: TREES :::::::
# Step 2: Preparation for Text Analysis
processed <- textProcessor(tree_data$text, metadata = tree_data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta


# Step 3: Run the function stm to generate the most prevalent 20 topics
tree_InstaPrevFit <- stm(documents = out$documents, vocab = out$vocab,
                    K = 10,  max.em.its = 200,
                    data = out$meta, init.type = "Spectral", seed=6221433)

# plot the keywords for the 10 most prevalent topics
plot(tree_InstaPrevFit, type = "summary", xlim = c(0, 0.5), labeltype = "frex", n = 6) #n=6 means I want to plot 6 keywords under each topic

# Inspect the keywords for each topic (FREX is the most useful ones to look at)
labelTopics(tree_InstaPrevFit)

# For instance let us look at the facebook posts under topic 1
thoughts1 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 1)
thoughts1
thoughts2 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 2)
thoughts3 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 3)
thoughts4 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 4)
thoughts5 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 5)
thoughts6 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 6)
thoughts7 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 7)
thoughts8 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 8)
thoughts9 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 9)
thoughts10 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 10)

out$meta$partisan <- as.factor(out$meta$partisan)
levels(out$meta$partisan) # this function tells us what categories the variable Post_Type has: Photo, Album_Video
# note: the first category you see when you run this function (i.e., Album_Video) will be the value you put under cov.value2 in the plot function

prep <- estimateEffect(1:10 ~ partisan, tree_InstaPrevFit, meta = out$meta, uncertainty = "Global")
summary(prep) # from the coefficient, you can see for each topic, which type of posts is more or less likely to talk about a specific topic

labels = c("EnvUtil", "EconGain", "CommonSenseSolution", "CommunalEffort", "NonGovStrat", "RuralRestorEcon", "IndivAction", "SocMentHealth", "FiscalResp", "10")
typeof(labels)

plot(prep, covariate = "partisan",
     model = tree_InstaPrevFit, method = "difference", cov.value1 = "rep",
     cov.value2 = "dem",
     xlab = "dem   ...   rep",
     labeltype = "custom",
     custom.labels = labels,
     verbose.labels = F,
     cex = 2,
     main = "Effect of Partisan Cue on GPT output frames (planting trees)", xlim = c(-0.2, 0.2))
