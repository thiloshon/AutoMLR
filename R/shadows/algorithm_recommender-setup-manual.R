library(data.table)
library(OpenML)
options(scipen = 999)

# Data set one

data01 <- read.csv("./data/OpenMLRunEvaluations.csv")

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

build_memory01 <- data01[!is.na(data01$build_memory),]
build_cpu_time01 <- data01[!is.na(data01$build_cpu_time),]
ram_hours01 <- data01[!is.na(data01$ram_hours),]
run_cpu_time01 <- data01[!is.na(data01$run_cpu_time),]
usercpu_time_millis01 <- data01[!is.na(data01$usercpu_time_millis),]
usercpu_time_millis_testing01 <- data01[!is.na(data01$usercpu_time_millis_testing),]
usercpu_time_millis_training01 <- data01[!is.na(data01$usercpu_time_millis_training),]



# Data set two

data01 <- read.csv("./data/OpenMLRunEvaluationsSet02.csv")

data01 <- as.data.table(data01)

setnames(data01, "function.", "fun")

wideDataFormula = run_id + task_id + flow_name + setup_id + flow_id + data_name + upload_time ~ fun

data01 = data.table::dcast(
    data = data01,
    formula = wideDataFormula,
    value.var = c("value"),
    fun.aggregate = mean
)

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

build_memory02 <- data01[!is.na(data01$build_memory),]
build_cpu_time02 <- data01[!is.na(data01$build_cpu_time),]
ram_hours02 <- data01[!is.na(data01$ram_hours),]
run_cpu_time02 <- data01[!is.na(data01$run_cpu_time),]
usercpu_time_millis02 <- data01[!is.na(data01$usercpu_time_millis),]
usercpu_time_millis_testing02 <- data01[!is.na(data01$usercpu_time_millis_testing),]
usercpu_time_millis_training02 <- data01[!is.na(data01$usercpu_time_millis_training),]


build_memory <- rbind.fill(build_memory01, build_memory02)
build_cpu_time <- rbind.fill(build_cpu_time01, build_cpu_time02)
ram_hours <- rbind.fill(ram_hours01, ram_hours02)
run_cpu_time <- rbind.fill(run_cpu_time01, run_cpu_time02)
usercpu_time_millis <- rbind.fill(usercpu_time_millis01, usercpu_time_millis02)
usercpu_time_millis_testing <- rbind.fill(usercpu_time_millis_testing01, usercpu_time_millis_testing02)
usercpu_time_millis_training <- rbind.fill(usercpu_time_millis_training01, usercpu_time_millis_training02)



build_memory.rank <- aggregate(build_memory$build_memory, list(build_memory$flow_name_fixed), mean)
build_cpu_time.rank <- aggregate(build_cpu_time$build_cpu_time, list(build_cpu_time$flow_name_fixed), mean)
ram_hours.rank <- aggregate(ram_hours$ram_hours, list(ram_hours$flow_name_fixed), mean)
run_cpu_time.rank <- aggregate(run_cpu_time$run_cpu_time, list(run_cpu_time$flow_name_fixed), mean)
usercpu_time_millis.rank <- aggregate(usercpu_time_millis$usercpu_time_millis, list(usercpu_time_millis$flow_name_fixed), mean)
usercpu_time_millis_testing.rank <- aggregate(usercpu_time_millis_testing$usercpu_time_millis_testing, list(usercpu_time_millis_testing$flow_name_fixed), mean)
usercpu_time_millis_training.rank <- aggregate(usercpu_time_millis_training$usercpu_time_millis_training, list(usercpu_time_millis_training$flow_name_fixed), mean)


build_memory.rank <- build_memory.rank[order(-build_memory.rank$x),]
build_cpu_time.rank <- build_cpu_time.rank[order(-build_cpu_time.rank$x),]
ram_hours.rank <- ram_hours.rank[order(-ram_hours.rank$x),]
run_cpu_time.rank <- run_cpu_time.rank[order(-run_cpu_time.rank$x),]
usercpu_time_millis.rank <- usercpu_time_millis.rank[order(-usercpu_time_millis.rank$x),]
usercpu_time_millis_testing.rank <- usercpu_time_millis_testing.rank[order(-usercpu_time_millis_testing.rank$x),]
usercpu_time_millis_training.rank <- usercpu_time_millis_training.rank[order(-usercpu_time_millis_training.rank$x),]



head(build_memory.rank)
head(build_cpu_time.rank)
head(ram_hours.rank)
head(run_cpu_time.rank)
head(usercpu_time_millis.rank)
head(usercpu_time_millis_testing.rank)
head(usercpu_time_millis_training.rank)

usercpu_time_millis.rank$score <- usercpu_time_millis.rank$x / max(usercpu_time_millis.rank$x)
build_memory.rank$score <- build_memory.rank$x / max(build_memory.rank$x)



mlr <- usercpu_time_millis.rank[grep("mlr", usercpu_time_millis.rank$Group.1, ignore.case = T, fixed = T), ]
mlr$score <- mlr$x / max(mlr$x)

usercpu_time_millis.rank[grep("ada", usercpu_time_millis.rank$Group.1, ignore.case = T, fixed = T), ]
mlr[grep("bgp", mlr$Group.1, ignore.case = T, fixed = T), ]

# build_memory.rank <- data.frame(learner = build_memory.rank$Group.1, build_memory = build_memory.rank$x)
#
# build_cpu_time.rank <- aggregate(build_cpu_time$build_cpu_time, list(build_cpu_time$flow_name_fixed), mean)
# build_cpu_time.rank <- data.frame(learner = build_cpu_time$Group.1, build_cpu_time = build_cpu_time$x)





# deprecated


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


