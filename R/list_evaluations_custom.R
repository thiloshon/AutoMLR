library(plyr)
library(jsonlite)
library(purrr)
library(tidyverse)
library(data.table)
library(arules)

# run.evaluations = data.frame()

# Download Data
for (task in tasks$task.id[622:16034]) {
    # remove index

    print(paste("Task:", task, ", Index:", match(task, tasks$task.id)))

    run.offset <<- 0
    while (TRUE) {
        print(paste("Offset:", run.offset))

        dest.file = paste("E:/OpenMLData/", task, "-", run.offset, ".json", sep = "")

        #ERROR HANDLING
        possibleError <- tryCatch(
            download.file(
                paste(
                    "https://www.openml.org/api/v1/json/evaluation/list/task/",
                    task,
                    "/limit/10000/offset/",
                    run.offset,
                    sep = ""

                ),
                destfile = dest.file
            ),
            error = function(e)
                e
        )

        if (inherits(possibleError, "error")) {
            print("!!!!!!!!!!!!!!!!!!! Breaking !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
            break
        }

        a <- readLines(dest.file)
        b <- paste(paste(a, collapse = ""), "}")
        #ERROR HANDLING
        run.results.set.n <- tryCatch(
            fromJSON(b)$evaluations$evaluation,
            error = function(e)
                e
        )

        if (inherits(run.results.set.n, "error")) {
            print("!!!!!!!!!!!!!!!!!!! Bad String !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
            run.offset <<- run.offset + 10000
            next
        }

        print(paste(
            "Returned Data:",
            nrow(run.results.set.n),
            nrow(run.evaluations)
        ))

        # run.evaluations <- rbind.fill(run.evaluations, run.results.set.n)
        run.offset <<- run.offset + 10000
    }
}

# Create dataframe from JSON files
nm <- list.files(path = "E:/OpenMLData/")
OpenMLRunEvaluations <- do.call(rbind.fill,
        lapply(nm, function(x) {

            print(paste("Reading:" , x))

            run.results.set.n <- tryCatch(
                fromJSON(paste(
                    paste(readLines(paste(
                        "E:/OpenMLData/", x, sep = ""
                    )), collapse = ""), "}"
                ))$evaluations$evaluation,
                error = function(e)
                    e
            )

            if (inherits(run.results.set.n, "error")) {
                print("!!!!!!!!!!!!!!!!!!! Bad String !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

                return(data.frame())
            }

            run.results.set.n
        })
)




evals <- as.data.table(OpenMLRunEvaluations)
# convert long format to wide format
setnames(evals, "function", "fun")
form = run_id + task_id + setup_id + flow_id + flow_name + data_name + upload_time ~ fun
evals = data.table::dcast(
    data = evals,
    formula = form,
    value.var = c("value", "array_data"),
    fun = sum
)

# drop "all NA" columns
evals = as.data.frame(evals)[, vlapply(evals, function(x)
    ! all(is.na(x)))]
# drop all array columns that are NULL
#drop.array = vlapply(evals[,grepl("array_data[_]", colnames(evals))], function(x) all(vlapply(x, is.null)))
#drop.array = names(drop.array)[drop.array]
#evals = evals[, colnames(evals)%nin%drop.array]

# unfortunately column names are f***ed up now. Some tedious work is neccessary
# to achive our naming conventions
colnames(evals) = stri_replace_all_fixed(colnames(evals), "value_", "")
arr.ind = stri_detect_fixed(colnames(evals), "array_data_")
colnames(evals)[arr.ind] = paste0(stri_replace_all_fixed(colnames(evals)[arr.ind], "array_data_", ""),
                                  "_array")
if (!show.array.measures) {
    evals = evals[,!arr.ind]
}

# convert types (by default all is character)
#evals = as.data.frame(lapply(evals, type.convert, numerals = "no.loss", as.is = TRUE))

# finally convert _ to . in col names
names(evals) = convertNamesOMLToR(names(evals))























# --------------- Deprecated Temp

tryCatch({
    if (nrow(run.results.set.n) > 0) {

    } else{
        print("!!!!!!!!!!!!!!!!!!! Breaking2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        break
    }

}, error = function(e) {
    print("!!!!!!!!!!!!!!!!!!! Breaking !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
})
