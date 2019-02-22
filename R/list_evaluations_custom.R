library(plyr)
library(jsonlite)

# run.evaluations = data.frame()



for (task in tasks$task.id[500:16034]) {
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





tryCatch({
    if (nrow(run.results.set.n) > 0) {

    } else{
        print("!!!!!!!!!!!!!!!!!!! Breaking2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        break
    }

}, error = function(e) {
    print("!!!!!!!!!!!!!!!!!!! Breaking !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
})
