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

# subset data based on policy varuable: tree, tax, or fuel
tax_data = data[data$policy == "tax",]

# ::::::: TREES :::::::
# Step 2: Preparation for Text Analysis
processed <- textProcessor(tax_data$processed_text, metadata = tax_data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta


# Step 3: Run the function stm to generate the most prevalent 20 topics
tax_InstaPrevFit <- stm(documents = out$documents, vocab = out$vocab,
                         K = 10,  max.em.its = 200,
                         data = out$meta, init.type = "Spectral", seed=6221433)

# plot the keywords for the 10 most prevalent topics
plot(tax_InstaPrevFit, type = "summary", xlim = c(0, 0.5), labeltype = "frex", main = "Tax: Top Topics", n = 6,) #n=6 means I want to plot 6 keywords under each topic


# Inspect the keywords for each topic (FREX is the most useful ones to look at)
labelTopics(tax_InstaPrevFit)

# Examine the top 10 most associated entries per topic

z<-tree_data$text[-processed$docs.removed,]
length(z)

thoughts1 <- findThoughts(tax_InstaPrevFit, texts = tax_data$text, n = 10, topics = 1)
thoughts1
thoughts2 <- findThoughts(tax_InstaPrevFit, texts = tax_data$text, n = 10, topics = 2)
thoughts3 <- findThoughts(tax_InstaPrevFit, texts = tax_data$text, n = 10, topics = 3)
thoughts4 <- findThoughts(tax_InstaPrevFit, texts = tax_data$text, n = 10, topics = 4)
thoughts5 <- findThoughts(tax_InstaPrevFit, texts = tax_data$text, n = 10, topics = 5)
thoughts6 <- findThoughts(tax_InstaPrevFit, texts = tax_data$text, n = 10, topics = 6)
thoughts7 <- findThoughts(tax_InstaPrevFit, texts = tax_data$text, n = 10, topics = 7)
thoughts8 <- findThoughts(tax_InstaPrevFit, texts = tax_data$text, n = 10, topics = 8)
thoughts9 <- findThoughts(tax_InstaPrevFit, texts = tax_data$text, n = 10, topics = 9)
thoughts10 <- findThoughts(tax_InstaPrevFit, texts = tax_data$text, n = 10, topics = 10)


out$meta$partisan <- as.factor(out$meta$partisan)
levels(out$meta$partisan) # this function tells us what categories the variable Post_Type has: Photo, Album_Video
# note: the first category you see when you run this function (i.e., Album_Video) will be the value you put under cov.value2 in the plot function

prep <- estimateEffect(1:10 ~ partisan, tax_InstaPrevFit, meta = out$meta, uncertainty = "Global")
summary(prep) # from the coefficient, you can see for each topic, which type of posts is more or less likely to talk about a specific topic

labels = c("Health and Air Quality", "Business Incentives", "FutureGenerations", "Science/Technology", "Protect Econ/Future", "Energy Independance", "Job Creation", "Threat", "Fiscal Responsability", "Technology Development")
typeof(labels)

plot(prep, covariate = "partisan",
     model = tax_InstaPrevFit, method = "difference", cov.value1 = "rep",
     cov.value2 = "dem",
     xlab = "dem   ...   rep",
     labeltype = "custom",
     custom.labels = labels,
     verbose.labels = F,
     cex = 2,
     main = "Effect of Partisan Cue on GPT output frames (tax credits)", xlim = c(-0.2, 0.2))

#################################
# pulling probabilities, visualizing
#################################
## Extract theta from the stm-model
df <- data.frame(labels)
proportion <- as.data.frame(colSums(tax_InstaPrevFit$theta/nrow(tax_InstaPrevFit$theta)))
df <- cbind(df, proportion)
colnames(df) <- c("Labels", "Probability")

## Sort the dataframe
df <- df[order(-proportion), ] 
df$Labels <- factor(df$Labels, levels = rev(df$Labels))
df$Probability <- as.numeric(df$Probability)
df$Probability <- round(df$Probability, 3)

## Plot graph
tax_prob_plot <- ggplot(df, aes(x = Labels, y = Probability, fill=labels)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#808080",
                             "blue",
                             "red",
                             "blue",
                             "#808080",
                             "blue",
                             "red",
                             "blue",
                             "#808080",
                             "red")) +
  scale_y_continuous(breaks = c(0, 0.15), limits = c(0, 0.25), expand = c(0, 0)) + #change breaks and limits as you need
  coord_flip() + 
  geom_text(aes(label = scales::percent(Probability)), #Scale in percent
            hjust = -0.25, size = 3.5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE) + 
  theme(panel.border = element_blank()) + ggtitle("Tax Policy Topics")



