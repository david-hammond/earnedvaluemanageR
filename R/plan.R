#' Get linearly scaled variable based on optimal bins
#'
#' Finds outliers and then bands between 0 and 1 on optimal bins of
#' non-outlier data
#' @importFrom plan ganttAddTask
#' @importFrom lubridate today now
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
                            initialize = function(df){
                            self$gantt = get_gantt(df)
                            self$planned_value = df %>% select(due, planned_cost) %>%
                              mutate(planned_costs = cumsum(planned_cost))
                            tmp = data.frame(date = today()) %>%
                                mutate(budget_at_completion = sum(df$planned_cost),
                                       planned_value = sum(df$planned_cost[df$due <= now()]),
                                       earned_value = sum(df$progress*df$planned_cost),
                                       actual_cost = df$actual_cost[1],
                                       schedule_variance = earned_value - planned_value,
                                       schedule_performance_index = earned_value/planned_value,
                                       cost_variance = earned_value-actual_cost,
                                       cost_performance_index = earned_value/actual_cost,
                                       estimate_at_completion = budget_at_completion/cost_performance_index,
                                       estimate_to_complete = estimate_at_completion - actual_cost,
                                       variance_at_completion = budget_at_completion - estimate_at_completion,
                                       to_complete_performance_index_fixed_budget =  (budget_at_completion - earned_value)/(budget_at_completion - actual_cost),
                                       to_complete_performance_index_flexible_budget = (budget_at_completion - earned_value)/(estimate_at_completion - actual_cost))
                            self$earned_value = tmp
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
                            pv = data.frame(date = self$planned_value$due, type = 'PC', value = self$planned_value$planned_costs)
                            ev = data.frame(date = self$earned_value$date,
                                                           type = 'EV',
                                                           value = self$earned_value$earned_value)
                            ac = data.frame(date = self$earned_value$date,
                                                           type = 'AC',
                                                           value = self$earned_value$actual_cost)
                            plot(pv$date, pv$value, type = 's',
                                 yaxt = 'n', xlab = "",
                                 ylab = "", main = "Burndown Chart")
                            lines(ev$date, ev$value, col = 'red')
                            lines(ac$date, ac$value, col = 'blue')
                          }
                        )

)


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


