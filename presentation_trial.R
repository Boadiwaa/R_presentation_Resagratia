#Warming Up

#Useful shortcuts
#ctrl+shift+f10 -> restart r session
#ctrl + enter key -> run this line/selected block of code
#ctrl+shift+C -> comment out 
#ctrl+shift+M -> %>%
#Ctrl+Shift+E -> automatic indenting
#ctrl+L -> Clear console
#Alt + - %>% <- 


getwd()
setwd()

c(2500,3000,430,580) -> b
length(b)
names(b) <-   c("First", "Second", "Third", "Fourth")
b

top_income <- b[c("First", "Second")]
top_income

table(b) #find frequency of elements

#charToRaw() - converts character data to raw data
#rawToChar()


#Agenda for the day
#library(reticulate) used to code in python in rstudio
 
library(carData)

dataset<-Salaries

str(dataset)
summary(dataset)
glimpse(dataset) #must import dplyr first
head(dataset)
tail(dataset)
colnames(dataset)



#Average salary per rank
#Gender distribution
#Is there a difference in salaries amongst men and women?

## ---- a 


hist(dataset$salary)
quantile(dataset$salary)
class(dataset$discipline)
table(dataset$discipline)

#if dataset$discipline were of class character, we could recode this way:
# dataset$discipline[dataset$discipline == "A"]  <- "Theoretical Department"
# dataset$discipline[dataset$discipline == "B"]  <- "Applied Department"

library(tidyverse)

dataset<- dataset %>% 
  mutate(
    discipline = fct_recode(discipline, 
      "Theoretical Department" = "A", 
      "Applied Department" = "B"),
    rank = fct_recode(rank, 
                      "Assistant Professor" = "AsstProf", 
                      "Associate Professor" = "AssocProf",
                      "Professor" = "Prof")
          )

dataset <- dataset %>% rename_with(toupper)
dataset <- dataset %>% rename("YEARS SINCE PhD" = YRS.SINCE.PHD)

library(scales)
currency_fmt <- function(x) label_number_si(accuracy=0.1,prefix="$")(x)

library(gtsummary)
library(gt)

dataset %>% 
  tbl_summary(label = list(
            YRS.SERVICE ~ "YEARS OF SERVICE",
            SALARY ~ "SALARY (in dollars)"),
            digits= starts_with("SALARY") ~ currency_fmt )%>%  
bold_labels() %>% 
  modify_caption(
    "**Resagratia R Session**") %>% #show_header_names()
  as_gt() %>% tab_style(locations = cells_body(
    columns = everything(),
    rows=c(4,7,12)),
    # rows=variable=="RANK"),
    style=list(cell_fill(color="firebrick"),
    cell_text(color="white"))
    ) %>% tab_header(subtitle= "Data Digest Episode 8",
                     title="Table 1: General Overview of the Salaries Dataset")
library(ggthemes)
dataset %>% ggplot(aes(x=`YEARS SINCE PhD`, y=SALARY, color=SEX))+ 
  geom_point()+geom_smooth(method=lm, se=F)+
 theme_wsj() +  scale_color_manual(values=c("#070604","#883A19"))+
  labs(title="Salary trends per number of years post-PhD",
       subtitle="Dataset obtained from \"Salaries\" in the carData package")+
  scale_y_continuous(labels = scales::dollar_format())+
  theme(legend.position = "bottom")
  
dataset %>% group_by(RANK) %>%
  summarize("Median Salary" = median(SALARY)) %>%
  mutate("Median Salary" = label_number_si(accuracy = 0.1, prefix = "$")(`Median Salary`)) %>% 
  gt() %>% 
  tab_source_note(source_note = "Data Source: carData package") %>% 
  cols_label(
    RANK = md("**RANK**"),
    `Median Salary`= md("**Median Salary**")
  )
 
  
install.packages("fortunes") #opposite of this is remove.packages()
library(fortunes)
fortune(108)