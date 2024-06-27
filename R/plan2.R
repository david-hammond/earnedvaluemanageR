#' Get linearly scaled variable based on optimal bins
#'
#' Finds outliers and then bands between 0 and 1 on optimal bins of
#' non-outlier data
#' @importFrom plan ganttAddTask
#' @importFrom lubridate today now
#' @importFrom zoo na.approx
#' @importFrom ggplot2 ggplot geom_line
#' @importFrom scales dollar percent
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
                            self$earned_value = data.frame(date = date,
                                                           budget_at_completion = project_value,
                                                           planned_value = self$planned_value$planned_value[which(abs(self$planned_value$due - today()) == min(abs(self$planned_value$due - today()) ))],
                                                           earned_value = sum(df$progress*df$planned_value),
                                                           actual_cost = cost_to_date)
                            self$earned_value = self$earned_value %>%
                              mutate(schedule_variance = earned_value - planned_value,
                                     cost_variance = earned_value-actual_cost,
                                     cost_performance_index = earned_value/actual_cost,
                                     estimate_at_completion = project_value/cost_performance_index,
                                     estimate_to_complete = estimate_at_completion - cost_to_date,
                                     variance_at_completion = budget_at_completion - estimate_at_completion,
                                     to_complete_performance_index =  (project_value - earned_value)/(project_value - cost_to_date),
                                     project_complete = earned_value/project_value,
                                     schedule_complete = schedule_variance/project_value,
                                     allocated_budget = sum(planned_cost))
                            self$earned_value[-1] = apply(self$earned_value[-1], 1, round, digits = 2)
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
                          }

                        )

)


get_planned_value = function(df, project_value){
  start_at_zero = data.frame(due = min(df$start), planned_value = 0)
  df = df %>% select(due, planned_value) %>%
    group_by(due) %>%
    summarise(planned_value = sum(planned_value)) %>%
    ungroup() %>%
    mutate(planned_value = cumsum(planned_value))
  df = start_at_zero %>% rbind(df)
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


#' Get linearly scaled variable based on optimal bins
#'
#' Finds outliers and then bands between 0 and 1 on optimal bins of
#' non-outlier data
#' @importFrom plan ganttAddTask
#' @importFrom lubridate today now
#' @importFrom zoo na.approx
#' @importFrom ggplot2 ggplot geom_line
#' @importFrom scales dollar percent
#'
#' @export
#'
setMethod(f="plot",
          signature=signature("newPlan"),
          definition=function (x)
          {
            if (!inherits(x, "newPlan")) stop("method is only for gantt objects")
            pv = data.frame(date = x$planned_value$due, type = 'Planned Value', value = x$planned_value$planned_value)
            ev = rbind(pv[1,] %>% mutate(type = "Earned Value"), data.frame(date = x$date,
                                                                            type = 'Earned Value',
                                                                            value = x$earned_value$earned_value))
            ac = rbind(pv[1,] %>% mutate(type = "Actual Cost"), data.frame(date = x$date,
                                                                           type = 'Actual Cost',
                                                                           value = x$earned_value$actual_cost))
            ev = pv %>% rbind(ev) %>% rbind(ac)
            p = ggplot(ev, aes(date, value, colour = type)) + geom_line() +
              scale_y_continuous(labels = scales::dollar, name = "Total Value",
                                 sec.axis = sec_axis(~ . / max(pv$value),
                                                     labels = scales::label_percent())) +
              labs(colour = "", title = "Earned Value Chart", x = "") + theme_bw() +
              theme(legend.position = c(0.8, 0.2)) +
              geom_vline(xintercept = as.numeric(today()), linetype = "dashed", color = "cornflowerblue") +
              annotate("text", x = today() + days(1), y = max(ev$value),
                       label = "Today", hjust = 0, vjust = 1.5, color = "cornflowerblue")
            return(p)
          })
