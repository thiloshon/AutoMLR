library(OpenML)
library(data.table)
tasks = listOMLTasks()
run.results = listOMLRunEvaluations(task.id = ids)


#in loop get 10000 records with 10000 offsets of run data for each 100 task ids in task data
# until no data is returned and append to dataframe for all task ids


ids <- c(tasks$task.id[1:16034])

run.results1 = listOMLRunEvaluations(task.id = tasks$task.id[1:100], limit = 10000, offset = 10000)

max.nominal.att.distinct.values

