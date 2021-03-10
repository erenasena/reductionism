## Libraries 
library(pwr) # for power analysis 
library(qdapRegex) # to extract emails
library(stringdist) # to match emails with names

### Author data

## Load Scopus author data 
scopus <- list.files(pattern = "*.csv")
data <- lapply(scopus, read.csv)
class(data)
length(data)

## Extract unique authors 

# Extract the authors 
authors <- character()
for(i in 1:length(data)){
  authors <- append(x = authors, values = data[[i]]$Authors)
}
authors <- unlist(strsplit(authors, ","))

# Index the unique authors 
unique_authors <- unique(authors)

# Check N and proportion of unique authors
print(length(unique_authors))
length(unique_authors)/length(authors)

## Extract unique emails

# Extract the emails 
emails <- character()
for(i in 1:length(data)){
  emails <- append(x = emails, 
                   values = rm_email(text.var = data[[i]]$Correspondence.Address, extract = TRUE), 
                   length(emails))
}

# Check N and proportion of missing values 
length(which(is.na(emails) == TRUE))
length(which(is.na(emails) == TRUE))/length(emails)

# Remove the missing values 
emails <- emails[-which(is.na(emails) == TRUE)]

# Index the unique emails
unique_emails <- unique(unlist(emails))
any(duplicated(x = unique_emails))

#df <- data.frame("E-mails" = df)
#write.csv(x = df, file = "Author E-mails.csv")

# Check for N and proportion of unique emails
N <- length(unique_emails)
print(N)
print(N/length(emails))
print(length(unique_emails)/length(unique_authors))

### Power analysis
p <- 0.10 # expected response rate 
N <- N*p

# Chi square hypothesis 1 
null <- c(0.05, rep(0.95/7, 7))
alt <- c(0.025, rep(0.975/7))
effect_size <- ES.w1(null, alt)
pwr.chisq.test(w = effect_size, N = N, df = 7, sig.level = 0.05)

# Chi square hypothesis 2 
null2 <- rep(1/8, 8)
alt2 <- c(rep(0.6/4, 4), rep(0.4/4, 4))
effect_size2 <- ES.w1(null2, alt2)
pwr.chisq.test(w = effect_size2, N = N, df = 7, sig.level = 0.05)
