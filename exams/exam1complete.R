covid_data <- read.csv("cleaned_covid_data.csv")
library(tidyverse)
A_states <- covid_data %>%
  filter(grepl("^A", Province_State))
ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point(color = "purple") +
  facet_wrap(~Province_State, scales = "free") +
  labs(title = "Deaths Over Time for states starting with 'A'",
       x = "Date",
       y = "Number of Deaths")
A_states$Last_Update <- as.Date(A_states$Last_Update)
state_max_fatality_rate <- covid_data %>%
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

ggplot(state_max_fatality_rate, aes(x = reorder(Province_State, Maximum_Fatality_Ratio), y = Maximum_Fatality_Ratio)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Maximum case fatality ratio by state", x = "state", y = "Maximum Fatality ration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))