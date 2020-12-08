library(dplyr)
library(tidyr)

# PART 1 AND 3 AND 4
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt",header=TRUE)
x_test <- read.table("UCI HAR Dataset/test/X_test.txt",header=TRUE)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt",header=TRUE)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",header=TRUE)
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt",header=TRUE)
y_train_edited <- y_train %>%
    unlist(y_train) %>%
    gsub(pattern = 1, replacement = "WALKING")%>%
    gsub(pattern = 2, replacement = "WALKING_UPSTAIRS") %>%
    gsub(pattern = 3, replacement = "WALKING_DOWNSTAIRS") %>%
    gsub(pattern = 4, replacement = "SITTING") %>%
    gsub(pattern = 5, replacement = "STANDING") %>%
    gsub(pattern = 6, replacement = "LAYING")
train_df <- cbind(subject_train,y_train_edited,x_train)
names(train_df)[1:2] <- c("Identifier","Activity")
y_test_edited <- y_test %>%
    unlist(y_train) %>%
    gsub(pattern = 1, replacement = "WALKING")%>%
    gsub(pattern = 2, replacement = "WALKING_UPSTAIRS") %>%
    gsub(pattern = 3, replacement = "WALKING_DOWNSTAIRS") %>%
    gsub(pattern = 4, replacement = "SITTING") %>%
    gsub(pattern = 5, replacement = "STANDING") %>%
    gsub(pattern = 6, replacement = "LAYING")
test_df <- cbind(subject_test,y_test_edited,x_test)
names(test_df)[1:2] <- c("Identifier","Activity")
combined_df <- rbind(train_df,test_df)
features <- read.table("UCI HAR Dataset/features.txt")
colnames(combined_df)[3:ncol(combined_df)] <- features[,2]

# PART 2
combined_df2 <- combined_df[,c(1,2,grep(pattern = "std\\(\\)|mean\\(\\)", names(combined_df)))]

# PART 5
summary_table <- combined_df2 %>%
    group_by(Identifier, Activity) %>%
    summarise_all(mean)