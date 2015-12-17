###### SCRIPT FOR CLEANING UP THE DATASET #####

### LIST DATASET ####
# list all data sources in this domain
list.Data.In.Domain <- ls.socrata("https://data.cityofnewyork.us/resource/xx67-kt59.json")

### DOWNLOAD DATASET ####
# The following lines go get the full dataset, most of which we don't need, so we will clean it up right after
token <- "pRe7y3yLD1ixibQCEOG77lPom"
restaurant.inspection <- read.socrata("https://data.cityofnewyork.us/resource/xx67-kt59.json")
nrow(restaurant.inspection)
colnames(restaurant.inspection)

#### Delete rows without grades ####
df <- subset(restaurant.inspection, grade!= "N/A")
nrow(df)

#### Keep only certain columns ####
keep <- c("camis", "dba", "boro", "building", "street", "zipcode", "cuisine_description", "inspection_date", "score", "grade", "violation_code", "critical_flag")
df.select <- df[keep]
colnames(df.select)
num_row <- nrow(df.select)
num_row

#### Counting violation code 4M & 4L, 4K #####
value <- NULL
camis_list <- hash()
for( i in 1 : num_row)
{
  row <- df.select[i,]
  row
  camis <- row['camis']
  inspection_date <- row['inspection_date']
  violation_code <- row['violation_code']
  
  if(has.key(camis, camis_list))
  {
    value <- values(camis_list, keys=camis)
    if(value[1] == inspection_date)
    {
      if(!is.na(violation_code))
      {
        
        if((violation_code == "04K") || (violation_code == "04L"))
        {
          value[3] <- TRUE
        }
        if(violation_code == "04M")
        {
          value[4] <- TRUE
        }
      }
      else
      {
        # print(row)
      }
    }
  }
  else
  {
    value <- list(inspection_date = inspection_date, rodent = FALSE, roach = FALSE)
    if(!is.na(violation_code))
    {
      if((violation_code == "04K") || (violation_code == "04L"))
      {
        value['rodent'] <- TRUE
      }
      if(violation_code == "04M")
      {
        value['roach'] <- TRUE
      }
    }
    else
    {
      # print(row)
    }
    .set(camis_list, keys = camis, values = value)
  }
}

camis_list

##### Take out duplicate camis key #####
data.restaurant <- df.select[!duplicated(df.select$camis),]
nrow(data.restaurant)
colnames(data.restaurant)

#### Take out inspection date & violation code ####
keep <- c("camis", "dba", "boro", "building", "street", "zipcode", "cuisine_description", "score", "grade")
data.restaurant <- data.restaurant[keep]


#### Put new rodent & roaches column into dataset ####
num_row <- nrow(data.restaurant)
rodent_list <- c()
roach_list <- c()

for( i in 1 : num_row)
{
  row <- data.restaurant[i,]
  row
  camis <- row[['camis']]

  value <- values(camis_list, keys= camis)
  rodent_value <- value[[2]]
  roach_value <- value[[3]]
  
  rodent_list <- c(rodent_list, rodent_value)
  roach_list <- c(roach_list, roach_value)
}

data.restaurant <- cbind(data.restaurant, rodent_list)
data.restaurant <- cbind(data.restaurant, roach_list)

###### Put demographic into the data frame ######
demographics_list <- hash() #key: zipcode
num_row_demographics <- nrow(Demographics)

# create numeric list for median_income
median_list <- as.numeric(as.character(Demographics$Median_Income))
Demographics <- cbind(Demographics, median_list)

for (i in 1 : num_row_demographics)
{
  row <- Demographics[i,]
  Zipcode <- row["Zipcode"]
  Population_total <- row[["Total"]]
  White <- row[["White"]]
  Black <- row[["Black"]]
  Asian <- row[["Asian"]]
  Multi <- row[["Multi"]]
  Other <- row[["Other"]]
  Mean_income <- row[["Mean_Income"]]
  Median_income <- median_list[i]
  value <- list(Population_total = Population_total, White = White, Black = Black, Asian = Asian, Multi = Multi, Other = Other, Mean_income = Mean_income, Median_income = Median_income)
  .set(demographics_list, keys = Zipcode, values = value)
}  

###### Combine demographics with data.restaurant by zipcode ####
total.list <- numeric(num_row)
white.list <- numeric(num_row)
black.list <- numeric(num_row)
asian.list <- numeric(num_row)
multi.list <- numeric(num_row)
other.list <- numeric(num_row)
mean_income.list <- numeric(num_row)
median_income.list <- numeric(num_row)

for (i in 1: num_row)
{
  zip <- data.restaurant[i,][["zipcode"]]
  if (has.key((zip), demographics_list))
  {
    value <- values(demographics_list, keys = zip)
    total.list[i] <- value[[1]]
    white.list[i] <- value[[2]]
    black.list[i] <- value[[3]]
    asian.list[i] <- value[[4]]
    multi.list[i] <- value[[5]]
    other.list[i] <- value[[6]]
    mean_income.list[i] <- value[[7]]
    median_income.list[i] <- value[[8]]
  }
  else
  {
    total.list[i] <- NA
    white.list[i] <- NA
    black.list[i] <- NA
    asian.list[i] <- NA
    multi.list[i] <- NA
    other.list[i] <- NA
    mean_income.list[i] <- NA
    median_income.list[i] <- NA
  }  
}

data.restaurant.final <- data.restaurant
data.restaurant.final <- cbind(data.restaurant.final, total.list)
data.restaurant.final <- cbind(data.restaurant.final, white.list)
data.restaurant.final <- cbind(data.restaurant.final, black.list)
data.restaurant.final <- cbind(data.restaurant.final, asian.list)
data.restaurant.final <- cbind(data.restaurant.final, multi.list)
data.restaurant.final <- cbind(data.restaurant.final, other.list)
data.restaurant.final <- cbind(data.restaurant.final, mean_income.list)
data.restaurant.final <- cbind(data.restaurant.final, median_income.list)

#####  Group cuisine based on cuisine description ####
num_row_cuisine <- nrow(data.restaurant.final)

cuisine_group <- c()
for(i in 1:num_row_cuisine)
{
    row <- data.restaurant.final[i,]
    if(row['cuisine_description'] == "Thai" || row['cuisine_description'] == "Asian" || row['cuisine_description'] == "Japanese"
       || row['cuisine_description'] == "Vietnamese/Cambodian/Malaysia" || row['cuisine_description'] == "Korean")
    {
      cuisine_group <- c(cuisine_group, "Asian")
    }
    else if(row['cuisine_description'] == "Chinese/Japanese" || row['cuisine_description'] == "Chinese") 
    {
      cuisine_group <- c(cuisine_group, "Chinese")
    }
    else if(row['cuisine_description'] == "Barbecue" || row['cuisine_description'] == "America" || row['cuisine_description'] == "Steak"
            || row['cuisine_description'] == "Hamburgers")
    {
      cuisine_group <- c(cuisine_group, "American")
    }
    else if(row['cuisine_description'] == "Pizza")
    {
      cuisine_group <- c(cuisine_group, "Pizza")
    }
    else if(row['cuisine_description'] == "Italian" || row['cuisine_description'] == "Pizza/Italian")
    {
      cuisine_group <- c(cuisine_group, "Italian")
    }
    else if(row['cuisine_description'] == "Latin (Cuban, Dominican, Puerto Rican, South & Central American)" || 
            row['cuisine_description'] == "Tapas" || row['cuisine_description'] == "Spanish" ||
            row['cuisine_description'] == "Mexican" || row['cuisine_description'] == "Caribbean")
    {
      cuisine_group <- c(cuisine_group, "Latin/Spanish")
    }
    else if(row['cuisine_description'] == "Sandwiches" || row['cuisine_description'] == "Soups & Sandwiches"
            || row['cuisine_description'] == "Delicatessen" || row['cuisine_description'] == "Sandwiches/Salads/Mixed Buffet")
    {
      cuisine_group <- c(cuisine_group, "Sandwiches")
    }
    else
    {
      cuisine_group <- c(cuisine_group, NA)
    }
}


cuisine_group_factor <- as.factor(cuisine_group) # change from character to factor
data.restaurant.final.cuisine <- cbind(data.restaurant.final, cuisine_group_factor)
data.restaurant.final.cuisine <- subset(data.restaurant.final.cuisine, !is.na(cuisine_group_factor)) # drop the rows that is NA

##### Save the final dataframe #####
restaurant.clean.2 <- data.restaurant.final.cuisine
View(restaurant.clean)

####### Save data frame into .RData so that we don't have to run the cleaning process again ####
save(restaurant.clean.2,file="restaurantCleanWithAddress.Rda")
save(restaurant.inspection,file="restaurantRaw.Rda")

##### Convert the raw data file to csv so we can clean it with Python as well ####
write.csv(restaurant.inspection, file = "restaurantRaw.csv")
write.csv(restaurant.clean, file = "restaurantClean.csv")

