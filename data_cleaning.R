## Department of Education
## School Improvement Grants 2010
```{r}
library('dplyr')
```


```{r}
grants <- read.csv("userssharedsdfschoolimprovement2010grants.csv", 
                   header=TRUE, sep=",",na.strings = c("", "NA"))
```

```{r}
grants <- select(grants, -Location)
grants <- na.omit(grants)

grants <- rename(grants, "SchoolName" = "School.Name", 
                         "District" = "District.Name", 
                 "Award" = "X2010.11.Award.Amount", 
                 "Model" = "Model.Selected")

grants$Award <- as.numeric(grants$Award)

grants
```


```{r}
write.csv(grants, file = "GrantData.csv")
```