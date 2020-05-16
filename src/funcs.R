library(data.table)

# for individual states
state_reopening <- function(state_name, reopen_date="2022-12-31") {
  state <- by_state %>% select(date, all_of(state_name)) 
  
  ggplot(state, aes_(x = as.name(names(state)[1]), y = as.name(names(state)[2]))) +
    geom_bar(stat= "identity") +
    scale_x_date(date_breaks = "1 week") +
    ylab("Number of Cases") +
    xlab("Date") +
    scale_y_continuous(labels = comma) +
    geom_vline(xintercept = as.Date(reopen_date), linetype="solid",
               color = "red", size=1) +
    theme_light() +
    labs(title = "COVID-19 Confirmed Cases (Cumulative)", subtitle = state_name) +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  
  ggplot(state, aes_(x = as.name(names(state)[1]), y = as.name(names(state)[2]))) + 
    geom_line() +
    scale_x_date(date_breaks = "1 weeks") +
    ylab("Number of Cases (logarithmic scale)") +
    xlab("Week") +
    scale_y_continuous(trans = "log2") +
    geom_vline(xintercept = as.Date(reopen_date), linetype="dotted",
               color = "red", size=1) +
      theme_bw() +
      theme(axis.text.x=element_text(angle=60, hjust=1))
  
}


# ggplot(states, aes(x = date, y = value)) + 
#   geom_line(aes(color = State)) + 
#   scale_x_date(date_breaks = "1 weeks") +
#   ylab("Number of Cases (logarithmic axis)") + 
#   xlab("Week") +
#   scale_y_continuous(trans = "log2") +
#   geom_vline(xintercept = as.Date("2020-05-26"), linetype="dotted", 
#              color = "blue", size=1.5) +
#   theme_bw() + 
#   theme(axis.text.x=element_text(angle=60, hjust=1))
# 
