## Thesis, GPT, Climate Mitigation Appeals, Partisan Messenging
## Kaiping Chen
## November 2, 2022
## Dataset we used: Canvas -> Module -> Week 9 -> "arboretum.csv"

# Step 1: Install the Structural Topic Model Package
install.packages("stm")
install.packages("tm")
install.packages("Rtsne")
install.packages("geometry")
install.packages("rsvd")
install.packages("ggplot2")
library("ggplot2")
library("stm")
library("rsvd")
library("geometry")
library("Rtsne")
library("tm")





# set up your working directory 
# remember to adjust this code line to set the working directory to the place where you stored HW3 dataset and this script
setwd("~/Documents/lsc/thesis/testing/stm")

# load data
data <- read.csv("para_data.csv") 
colnames(data) #Description, Post_Type, Post.Created.Time, etc.

# subset data based on policy variable: tree, tax, fuel
tree_data = data[data$policy == "tree",]

# ::::::: TREES :::::::
# Step 2: Preparation for Text Analysis
processed <- textProcessor(tree_data$processed_text, metadata = tree_data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta


# Step 3: Run the function stm to generate the most prevalent 20 topics
tree_InstaPrevFit <- stm(documents = out$documents, vocab = out$vocab,
                    K = 8,  max.em.its = 200,
                    data = out$meta, init.type = "Spectral", seed=6221433)

# plot the keywords for the 10 most prevalent topics
plot(tree_InstaPrevFit, type = "summary", xlim = c(0, 0.5), main = "Tree Policy: Top Topics", n = 6) #n=6 means I want to plot 6 keywords under each topic

# Inspect the keywords for each topic (FREX is the most useful ones to look at)
labelTopics(tree_InstaPrevFit)

# For instance let us look at the posts under topic 1 - 10
tthoughts1 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 1)
tthoughts1
tthoughts2 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 2)
tthoughts3 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 3)
tthoughts4 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 4)
tthoughts5 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 5)
tthoughts6 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 6)
tthoughts7 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 7)
tthoughts8 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 8)
tthoughts9 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 9)
tthoughts10 <- findThoughts(tree_InstaPrevFit, texts = tree_data$text, n = 10, topics = 10)

out$meta$partisan <- as.factor(out$meta$partisan)
levels(out$meta$partisan) # this function tells us what categories the variable Post_Type has: Photo, Album_Video
# note: the first category you see when you run this function (i.e., Album_Video) will be the value you put under cov.value2 in the plot function

prep <- estimateEffect(1:8 ~ partisan, tree_InstaPrevFit, meta = out$meta, uncertainty = "Global")
summary(prep) # from the coefficient, you can see for each topic, which type of posts is more or less likely to talk about a specific topic

prep




labels = c("Common Sense", "Future Generations", "Cost Effective", "Funding Strategy", "Science/Technical", "Environmental Benefit", "Economic Benefit", "Threat")
typeof(labels)

plot(prep, covariate = "partisan",
     model = tree_InstaPrevFit, method = "difference", cov.value1 = "rep",
     cov.value2 = "dem",
     xlab = "dem   ...   rep",
     labeltype = "custom",
     custom.labels = labels,
     verbose.labels = F,
     cex = 2,
     main = "Effect of Partisan Cue on GPT3 output frames (planting trees)", xlim = c(-0.2, 0.2))


## Put labels in a vector
labels = c("Compromise", "Future Generations", "Low Cost", "Funding Strategy", "Threat", "Environmental Benefit", "Economic Benefit", "Call to Action", "Science/Technology", "Climate Optimism")
## Include here your own labels, you probably have more than four

#################################
# pulling probabilities, visualizing
#################################
## Extract theta from the stm-model
df <- data.frame(labels)
proportion <- as.data.frame(colSums(tree_InstaPrevFit$theta/nrow(tree_InstaPrevFit$theta)))
df <- cbind(df, proportion)
colnames(df) <- c("Labels", "Probability")

## Sort the dataframe
df <- df[order(-proportion), ] 
df$Labels <- factor(df$Labels, levels = rev(df$Labels))
df$Probability <- as.numeric(df$Probability)
df$Probability <- round(df$Probability, 3)

## Plot graph
tree_prob_plot <- ggplot(df, aes(x = Labels, y = Probability, fill=labels)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#808080",
                             "#808080",
                             "#808080",
                             "#808080",
                             "#808080",
                             "#808080",
                             "#808080",
                             "#808080")) +
  scale_y_continuous(breaks = c(0, 0.15), limits = c(0, 0.25), expand = c(0, 0)) + #change breaks and limits as you need
  coord_flip() + 
  geom_text(aes(label = scales::percent(Probability)), #Scale in percent
            hjust = -0.25, size = 3.5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE) + 
  theme(panel.border = element_blank()) + ggtitle("Tree Policy Topics")

