####################################################################################
### Exercise 3 
####################################################################################

### a) import csv file

# Import the Pima dataset from dropbox link using the below two commands in lines 9-10. Does it work? What is the difference between these two commands?
  
read.csv(file = url("https://www.dropbox.com/s/4s4rhf6abda6gw2/Pima_diabetes.csv?dl=1"))
dat <- read.csv(file = url("https://www.dropbox.com/s/4s4rhf6abda6gw2/Pima_diabetes.csv?dl=1"))

# Now download the file Pima_diabetes.csv from Moodle, save it on your laptop, and import the data into R.
#never forget "" while naming
read.csv(file = "C:/Users/Juan Carlos/Desktop/HPI/Semestre III/Stadistics with R/repository SWR/Pima_diabetes.csv")
Pima_diabetes_file <- read.csv(file = "C:/Users/Juan Carlos/Desktop/HPI/Semestre III/Stadistics with R/repository SWR/Pima_diabetes.csv")
View(Pima_diabetes_file)
save(Pima_diabetes_file, file = "C:/Users/Juan Carlos/Desktop/HPI/Semestre III/Stadistics with R/repository SWR/Pima_diabetes.csv")
