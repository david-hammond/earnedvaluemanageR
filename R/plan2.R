#' Get linearly scaled variable based on optimal bins
#'
#' Finds outliers and then bands between 0 and 1 on optimal bins of
#' non-outlier data
#' @importFrom plan ganttAddTask
#' @importFrom lubridate today now
#' @importFrom zoo na.approx
#'
#' @export
#'

# https://www.projectengineer.net/the-earned-value-formulas/

newPlan <- R6::R6Class("newPlan",
                        lock_objects = FALSE,
                        public = list(
                          gantt = NULL,
                          planned_value = NULL,
                          earned_value = NULL,
                            initialize = function(phase,
                                                  activity,
                                                  start,
                                                  due,
                                                  progress,
                                                  planned_cost,
                                                  project_value,
                                                  cost_to_date,
                                                  date = today()){
                            self$date = date

                            df = data.frame(phase, activity, start, due, progress, planned_cost) %>%
                              mutate(planned_value = project_value*planned_cost/sum(planned_cost))
                            self$gantt = get_gantt(df)
                            self$planned_value = get_planned_value(df)
                            self$budget_at_completion = project_value
                            self$earned_value = sum(df$progress*df$planned_value)
                            self$actual_cost = cost_to_date
                            self$schedule_variance = self$earned_value - self$planned_value$planned_value[self$planned_value$due == today()]
                            self$cost_variance = self$earned_value-self$actual_cost
                            self$cost_performance_index = self$earned_value/self$actual_cost
                            self$estimate_at_completion = project_value/self$cost_performance_index
                            self$estimate_to_complete = self$estimate_at_completion - cost_to_date
                            self$variance_at_completion = self$budget_at_completion - self$estimate_at_completion
                            self$to_complete_performance_index =  (project_value - self$earned_value)/(project_value - cost_to_date)
                            self$project_complete = self$earned_value/project_value
                            self$schedule_pc = self$schedule_variance/project_value
                            self$allocated_budget = sum(planned_cost)
                            },
                          viewGantt = function(){
                            g = self$gantt
                            font <- ifelse(is.na(g[["start"]]), 2, 1)
                            plan::plot(g, ylabel=list(font=font),
                                       event.time=today(), event.label="Today")
                            par(lend="square") # default is round
                            legend("topright", pch=22, pt.cex=2, pt.bg=gray(c(0.3, 0.9)),
                                   border="black", xpd=NA,
                                   legend=c("Completed", "Not Yet Done"), bg="white")
                          },
                          viewEV = function(){
                            pv = data.frame(date = self$planned_value$due, type = 'PV', value = self$planned_value$planned_value)
                            ev = data.frame(date = self$date,
                                                           type = 'EV',
                                                           value = self$earned_value)
                            ac = data.frame(date = self$date,
                                                           type = 'AC',
                                                           value = self$actual_cost)
                            plot(pv$date, pv$value, type = 's',
                                 yaxt = 'n', xlab = "",
                                 ylab = "", main = "Burndown Chart")
                            points(ev$date, ev$value, col = 'red')
                            points(ac$date, ac$value, col = 'blue')
                          }
                        )

)


get_planned_value = function(df, project_value){
  df = df %>% select(due, planned_value) %>%
    group_by(due) %>%
    summarise(planned_value = sum(planned_value)) %>%
    ungroup() %>%
    mutate(planned_value = cumsum(planned_value))
  df = df %>% rbind(data.frame(due = min(df$due) - days(1), planned_value = 0))
  df = data.frame(due = seq.Date(min(df$due), max(df$due), "days")) %>%
    left_join(df) %>%
    mutate(planned_value = na.approx(planned_value))
  return(df)
}

get_gantt = function(y){
  #require(plan)
  g <- new("gantt")
  y = y %>% arrange((start))
  y = split(y, factor(y$phase, unique(y$phase), ordered = T))
  for (i in y){
    g <- ganttAddTask(g, as.character(i$phase[1]))
    for(j in 1:nrow(i)){
      g <- ganttAddTask(g, i$activity[j], as.character(i$start[j]),  as.character(i$due[j]),
                        done = 100*i$progress[j])
    }

  }
  font <- ifelse(is.na(g[["start"]]), 2, 1)
  return(g)
}
