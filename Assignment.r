# Import needed library
library(datasets)
library(dplyr)
library(tidyr)
library(zoo)

# Necessary helper function
#-------------------------------
# Function convert TB to GB (1 TB = 1024 GB)
convertToGB <- function(number) {
  if(grepl('G', number)) {
    return(as.double(gsub(" GB","",number)))
  }
  return(as.double(gsub(" TB","",number)) * 1024)
}
# Function convert GHz to MHz (1 GHz = 1000 MHz)
convertToMHz <- function(number) {
  if(grepl('M', number)) {
    return(as.double(gsub(" MHz","",number)))
  }
  return(as.double(gsub(" GHz","",number)) * 1000)
}
# Function convert KB to MB (1 KB = 1/1024 MB)
convertToMB <- function(number) {
  if(grepl('M', number)) {
    return(as.double(gsub(" M","",number)))
  }
  return(as.double(gsub(" K","",number)) / 1024)
}
# Function to convert range of price to the average
average_price <- function(price) {
  if(grepl('-', price)) {
    number <- strsplit(price, "-")[[1]]
    return((as.double(number[1]) + as.double(number[2])) / 2)
  }
  return (price)
}
#-------------------------------

# Data from file Intel_CPUs.csv
#-------------------------------
# Read the data from file Intel_CPUs.csv, change the blank data and N/A to NA 
IntelCPU = read.csv("Intel_CPUs.csv", na.strings = c("", "N/A"))
# Choose the data columns which will be used
dataCPU = IntelCPU[,c("Product_Collection", "Vertical_Segment", "Processor_Number", "Status", "Launch_Date", "Lithography",
                      "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "Cache", "TDP", "Max_Memory_Size",
                      "Max_Memory_Bandwidth", "Graphics_Base_Frequency", "Graphics_Max_Dynamic_Frequency", "Instruction_Set", "Recommended_Customer_Price")]
# Print summary of CPU data
print(summary(dataCPU))
# For each field, count number of NA data
print(apply(is.na(dataCPU), 2, sum))

# For each field, count rate of NA data
print(apply(is.na(dataCPU), 2, mean))

# There are so many missing data, so we will remove all data which have "NA" that we cannot predicted and data that have less than 10% miss
dataCPU <- dataCPU[complete.cases(dataCPU$TDP),]
dataCPU <- dataCPU[complete.cases(dataCPU$Processor_Base_Frequency),]
dataCPU <- dataCPU[complete.cases(dataCPU$Launch_Date),]
dataCPU <- dataCPU[complete.cases(dataCPU$Lithography),]
dataCPU <- dataCPU[complete.cases(dataCPU$Cache),]
dataCPU <- dataCPU[complete.cases(dataCPU$Processor_Number),]
dataCPU <- dataCPU[complete.cases(dataCPU$Instruction_Set),]

dataCPU$Lithography <- gsub("nm", "", dataCPU$Lithography)
dataCPU$Lithography <- as.integer(dataCPU$Lithography)
colnames(dataCPU)[6] = "Lithography(nm)"

# Convert "Launch_Date" to year
dataCPU$Launch_Date <- substr(dataCPU$Launch_Date, nchar(dataCPU$Launch_Date) - 1, nchar(dataCPU$Launch_Date))
dataCPU$Launch_Date <- as.integer(dataCPU$Launch_Date)
dataCPU$Launch_Date = 2000 + dataCPU$Launch_Date

# Change the frequency from GHz to MHz
dataCPU$Processor_Base_Frequency <- sapply(dataCPU$Processor_Base_Frequency, convertToMHz)
colnames(dataCPU)[9] = "Processor_Base_Frequency(MHz)"

# Split "Cache" to 2 column "Cache_Size" and "Type_Of_Cache"
dataCPU <- separate(dataCPU, Cache, into = c("Cache_Size(MB)","Type_Of_Cache"), sep="B")
dataCPU$`Cache_Size(MB)` <- sapply(dataCPU$`Cache_Size(MB)`, convertToMB)
dataCPU$Type_Of_Cache <- ifelse(dataCPU$Type_Of_Cache == "", "Original", sub(" ","",dataCPU$Type_Of_Cache))

dataCPU$TDP <- gsub("W", "", dataCPU$TDP)
dataCPU$TDP <- as.double(dataCPU$TDP)
colnames(dataCPU)[12] = "TDP(Watts)"

# Change the memory size from TB to GB
dataCPU$Max_Memory_Size <- sapply(dataCPU$Max_Memory_Size, convertToGB)
colnames(dataCPU)[13] = "Max_Memory_Size(GB)"

# Convert data of "Max_Memory_Bandwidth" from string to number
dataCPU$Max_Memory_Bandwidth <- gsub("GB/s", "", dataCPU$Max_Memory_Bandwidth)
dataCPU$Max_Memory_Bandwidth <- as.double(dataCPU$Max_Memory_Bandwidth)
colnames(dataCPU)[14] = "Max_Memory_Bandwidth(GB/s)"

# Change unit of "Graphics_Base_Frequency" to MHz, convert from string to number
dataCPU$Graphics_Base_Frequency <- sapply(dataCPU$Graphics_Base_Frequency, convertToMHz)
colnames(dataCPU)[15] = "Graphics_Base_Frequency(MHz)"

# Change unit of "Graphics_Max_Dynamic_Frequency" to MHz, convert from string to number
dataCPU$Graphics_Max_Dynamic_Frequency <- sapply(dataCPU$Graphics_Max_Dynamic_Frequency, convertToMHz)
colnames(dataCPU)[16] = "Graphics_Max_Dynamic_Frequency(MHz)"

dataCPU$Instruction_Set <- ifelse(dataCPU$Instruction_Set == "32-bit", "32-bit", "64-bit")

dataCPU$Recommended_Customer_Price <- gsub("\\$", "", dataCPU$Recommended_Customer_Price) 
dataCPU$Recommended_Customer_Price <- gsub(",", "", dataCPU$Recommended_Customer_Price)
dataCPU$Recommended_Customer_Price <- sapply(dataCPU$Recommended_Customer_Price, average_price) 
dataCPU$Recommended_Customer_Price <- as.double(dataCPU$Recommended_Customer_Price)
colnames(dataCPU)[18] = "Recommended_Customer_Price(USD)"

# Fill NA in "nb_of_Threads" with 2 times "nb_of_Cores" because usually, 1 core has 2 threads
dataCPU$nb_of_Threads <- ifelse(is.na(dataCPU$nb_of_Threads), dataCPU$nb_of_Cores * 2, dataCPU$nb_of_Threads)

# Fill NA in "Max_Memory_Size(GB)" with median
dataCPU$`Max_Memory_Size(GB)`[is.na(dataCPU$`Max_Memory_Size(GB)`)] <- median(dataCPU$`Max_Memory_Size(GB)`, na.rm = TRUE)

# Fill NA in "Max_Memory_Bandwidth(GB/s)" with median
dataCPU$`Max_Memory_Bandwidth(GB/s)`[is.na(dataCPU$`Max_Memory_Bandwidth(GB/s)`)] <- median(dataCPU$`Max_Memory_Bandwidth(GB/s)`, na.rm = TRUE)

# Fill NA in "Graphics_Base_Frequency(MHz)" with median
dataCPU$`Graphics_Base_Frequency(MHz)`[is.na(dataCPU$`Graphics_Base_Frequency(MHz)`)] <- median(dataCPU$`Graphics_Base_Frequency(MHz)`, na.rm = TRUE)

# Fill NA in "Graphics_Max_Dynamic_Frequency(MHz)" with median
dataCPU$`Graphics_Max_Dynamic_Frequency(MHz)`[is.na(dataCPU$`Graphics_Max_Dynamic_Frequency(MHz)`)] <- median(dataCPU$`Graphics_Max_Dynamic_Frequency(MHz)`, na.rm = TRUE)

# Fill NA in "Recommended_Customer_Price(USD)" according to the Product_Collection, then Vertical_Segment
dataCPU <- dataCPU %>% group_by(Product_Collection) %>% fill(`Recommended_Customer_Price(USD)`, .direction = "updown")
dataCPU <- dataCPU %>% group_by(Vertical_Segment) %>% fill(`Recommended_Customer_Price(USD)`, .direction = "updown")

# Check to make sure that there is no other NA data
print(apply(is.na(dataCPU), 2, sum))

# Create "dataNumeric" from dataCPU which only have numeric data
dataNumeric <- subset(dataCPU, select = -c(Product_Collection, Vertical_Segment, Processor_Number, Status, Type_Of_Cache, Instruction_Set))

average <- apply(dataNumeric, 2, mean)
variance <- apply(dataNumeric, 2, var)
std_deviation <- apply(dataNumeric, 2, sd)
min_value <- apply(dataNumeric, 2, min)
q1 <- apply(dataNumeric, 2, quantile, probs = 0.25)
med <- apply(dataNumeric, 2, median)
q3 <- apply(dataNumeric, 2, quantile, probs = 0.75)
max_value <- apply(dataNumeric, 2, max)

data.frame(average, variance, std_deviation, min_value, q1, med, q3, max_value)

print(length(dataCPU$Product_Collection))
table(dataCPU$Product_Collection)

table(dataCPU$Vertical_Segment)

print(length(dataCPU$Processor_Number))
print(length(unique(dataCPU$Processor_Number)))

table(dataCPU$Status)

table(dataCPU$Type_Of_Cache)

table(dataCPU$Instruction_Set)
#-------------------------------
