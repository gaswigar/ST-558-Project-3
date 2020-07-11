# https://github.com/UrbanInstitute/education-data-package-r
# https://educationdata.urban.org/documentation/colleges.html#ipeds_institutional-characteristics


devtools::install_github('UrbanInstitute/education-data-package-r')
library(educationdata)
data <- get_education_data(level = "college-university",
                           source = "ipeds",
                           topic = "institutional-characteristics",
                           filters = list(year = 1990))