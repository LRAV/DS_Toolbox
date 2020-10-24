### Getting and Cleaning Data Course Project ###
# Prepare tidy data that can be used for later analysis.

library(tidyverse)

### Preprocess training info with functions from tidyverse and dplyr

# load activity and features data
activity_labels <- read.delim("./data/UCI HAR Dataset/activity_labels.txt", 
                              sep = " ", 
                              header = F, 
                              col.names = c("act_num", "activity"))
features <- read.delim("./data/UCI HAR Dataset/features.txt", 
                       sep = " ", 
                       header = F, 
                       col.names = c("feat_num", "feature"))

# get positions for mean and std
mu = grep("mean()", features[, 2], fixed = T) 
sd = grep("std()", features[, 2])
mu_names = grep("mean()", features[, 2], fixed = T, value = T) 
sd_names = grep("std()", features[, 2], value = T)
# prepare vectors to filter by position. Keep var names
var_pos = c(mu, sd)
var_names = c(mu_names, sd_names)

# load full X_train and apply filter
dat_tr <- read.table("./data/UCI HAR Dataset/train/X_train.txt", sep = "")
dat_tr <- dat_tr %>% select(all_of(var_pos))
colnames(dat_tr) <- var_names
# load activities
act_tr <- read.delim("./data/UCI HAR Dataset/train/Y_train.txt", 
                     sep = " ", 
                     header = F, 
                     col.names = c("act_num"))
# change activity number for activity name
act_tr <- act_tr %>% 
  dplyr::left_join(activity_labels) %>% 
  select(activity)
# load persons
sbj_tr <- read.delim("./data/UCI HAR Dataset/train/subject_train.txt", 
                     sep = " ", 
                     header = F, 
                     col.names = c("subject"))
# join training data
x_tr <- cbind(dat_tr, act_tr, sbj_tr)
rm(dat_tr, act_tr, sbj_tr)

### Preprocess test info with functions from tidyverse and dplyr

# load full test data
dat_te <- read.table("./data/UCI HAR Dataset/test/X_test.txt", sep = "")
dat_te <- dat_te %>% select(all_of(var_pos))
colnames(dat_te) <- var_names
# load activities
act_te <- read.delim("./data/UCI HAR Dataset/test/Y_test.txt", 
                     sep = " ", 
                     header = F, 
                     col.names = c("act_num"))
# change activity number for activity name
act_te <- act_te %>% 
  dplyr::left_join(activity_labels) %>% 
  select(activity)
# load persons
sbj_te <- read.delim("./data/UCI HAR Dataset/test/subject_test.txt", 
                     sep = " ", 
                     header = F, 
                     col.names = c("subject"))
# join test data
x_te <- cbind(dat_te, act_te, sbj_te)
rm(dat_te, act_te, sbj_te)
# join all data
full_data = rbind(x_tr, x_te)
rm(x_te, x_tr)
# save dataset
write.csv(full_data, "train_test_dataset.csv")


# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Use of purr for mapping functions
require(reshape2)

df_melt <- melt(full_data, id = c("activity", "subject"))
mean_data <- df_melt %>%  dcast(activity + subject ~ variable, mean)  
# save dataset
write.csv(mean_data, "train_test_mean_dataset.csv")

