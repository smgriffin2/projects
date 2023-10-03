## LSC660, Demonstration of How to Implement Structural Topic Model on Social Media Posts
## Kaiping Chen
## November 2, 2022
## Dataset we used: Canvas -> Module -> Week 9 -> "arboretum.csv"

# Step 1: Install the Structural Topic Model Package
install.packages("stm")
library("stm")
install.packages("tm")
library("tm")

# set up your working directory 
# remember to adjust this code line to set the working directory to the place where you stored HW3 dataset and this script
setwd("~/OneDrive - UW-Madison/teaching/LSC660/week 9/")

# import the dataset (make sure your csv file is exactly the name like below)
data <- read.csv("clean_data.csv") #173 observations, 22 columns
colnames(data) #Description, Post_Type, Post.Created.Time, etc.
 
# Step 2: Preparation for Text Analysis
processed <- textProcessor(data$text, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta


# Step 3: Run the function stm to generate the most prevalent 20 topics
InstaPrevFit <- stm(documents = out$documents, vocab = out$vocab,
                       K = 10,  max.em.its = 200,
                       data = out$meta, init.type = "Spectral", seed=6221433)

# plot the keywords for the 10 most prevalent topics
plot(InstaPrevFit, type = "summary", xlim = c(0, 0.5), labeltype = "frex", n = 6) #n=6 means I want to plot 6 keywords under each topic

## Interpret the meaning of each topic by looking at its keywords (FREX word) and the example posts that belong to each topic

# Inspect the keywords for each topic (FREX is the most useful ones to look at)
labelTopics(InstaPrevFit)

# Examine facebook posts that are highly associated with each topic. 
  # In the findThoughts function, n=2 means you want to examine 3 posts under this topic, if you want to examine more, you can change the value of n
  # In the findThoughts function, topics=6 means we now look at the example posts under topic 6. If you want to examine other topics, just change this number

# For instance let us look at the facebook posts under topic 1
thoughts1 <- findThoughts(InstaPrevFit, texts = data$text, n = 3, topics = 1)$docs[[1]]
thoughts1

# For instance let us look at the facebook posts under topic 2
thoughts2 <- findThoughts(InstaPrevFit, texts = data$Description, n = 3, topics = 2)$docs[[1]]
thoughts2

  # You can repeat the above process to interpret all of the ten topics

# Step 4: Estimate the relationship between topic and meta data. Here we care about how topics differ between posts that use Photo format vs. posts that use album or video format. 
  #So we use the column Post_Type as meta data

out$meta$Post_Type <- as.factor(out$meta$Post_Type)
levels(out$meta$Post_Type) # this function tells us what categories the variable Post_Type has: Photo, Album_Video
  # note: the first category you see when you run this function (i.e., Album_Video) will be the value you put under cov.value2 in the plot function

prep <- estimateEffect(1:10 ~ Post_Type, InstaPrevFit, meta = out$meta, uncertainty = "Global")
summary(prep) # from the coefficient, you can see for each topic, which type of posts is more or less likely to talk about a specific topic

plot(prep, covariate = "Post_Type",
        model = InstaPrevFit, method = "difference", cov.value1 = "Photo",
        cov.value2 = "Album_Video",
        xlab = "album/video   ...   photo",
        main = "Effect of Post Type (Album/Video vs. Photo) on Arboretum's Instagram Content", xlim = c(-0.3, 0.3))

