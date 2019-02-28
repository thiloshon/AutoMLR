library(data.table)
library(OpenML)
options(scipen = 999)

data01 <- read.csv("./data/OpenMLRunEvaluationsSet01.csv")

data01 <- as.data.table(data01)

setnames(data01, "function.", "fun")

wideDataFormula = run_id + task_id + flow_name + setup_id + flow_id + data_name + upload_time ~ fun

data01 = data.table::dcast(
    data = data01,
    formula = wideDataFormula,
    value.var = c("value"),
    fun.aggregate = mean
)

tasks <- listOMLTasks(limit = NULL)

data01 <-
    merge(x = data01,
          y = tasks,
          by.x = "task_id",
          by.y = "task.id")

colMeans(is.na(data01))

data01$flow_name_fixed <-
    gsub("\\s*\\([^\\)]+\\)", "", data01$flow_name)
data01$flow_name_fixed <-
    as.factor(data01$flow_name_fixed)

d <- data01[!is.na(data01$build_memory),]

d <- aggregate(d$build_memory, list(d$flow_name_fixed), mean)






requiredRows <-
    data.frame(
        !is.na(data01$build_cpu_time),
        !is.na(data01$build_memory),
        !is.na(data01$ram_hours),
        !is.na(data01$run_cpu_time),
        !is.na(data01$usercpu_time_millis),
        !is.na(data01$usercpu_time_millis_testing),
        !is.na(data01$usercpu_time_millis_training)
    )

requiredRows$useRecord <- apply(requiredRows, 1, function(r) any(r %in% c(TRUE)))

f <- data01[requiredRows$useRecord, ]

#rm(requiredRows)




f <- aggregate(f[, c(10, 11, 23, 29, 32,33,34)], list(f$flow_name_fixed), mean)
# "build_cpu_time"               "build_memory"                 "ram_hours"
# [5] "run_cpu_time"                 "usercpu_time_millis"          "usercpu_time_millis_testing"  "usercpu_time_millis_training"

f <- f[order(-f$build_memory),]
f
