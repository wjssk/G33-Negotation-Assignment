source("aana_utils.R")
library(gtools)

repo_path <- "../"
final_result_dir <- 'final_run/all_traces/'
domains <- mixedsort(dir(final_result_dir))

# create a table with one run in each row
data <-
  data.frame(matrix(sapply(domains,
                           FUN = function (domain) { 
                             rbind(domain, paste(final_result_dir, domain, "/",
                                                 dir(paste(final_result_dir, domain, sep='')), sep=''))
                           }), byrow=TRUE, ncol=2))
colnames(data) <- c("domain", "file_path")

# Load the data to memory
jsons <- sapply(data$file_path, function(path) { 
  read_results_json(path)
})

ns <- seq(nrow(data))

# agent 1 column
data$against <- as.factor(sapply(ns, function(x) {
  agent_name(sapply(jsons[,x]$settings$SAOPSettings$participants$TeamInfo$parties, 
                    function(x) {x[["party"]][["partyref"]]})[2])
}))

# utility space reversed?
data$reversed <- as.factor(sapply(data$file_path, function(x) {
  length(strsplit(x, "profileB_profileA")[[1]]) == 2
}))

# 'failed?' column
data$failed <- sapply(ns, function(x) {is.null(jsons[,x]$actions$Accept)})

# our utility
data$util_ours <- sapply(ns, function(x) {
  na.omit(jsons[,x]$actions$Accept$utilities[[1]])
})
data$util_ours[which(data$failed)] <- NA
data$util_ours <- as.numeric(data$util_ours)

# opponent utility
data$util_opp <- sapply(ns, function(x) {
  unlist(na.omit(jsons[,x]$actions$Accept$utilities[2]))
})
data$util_opp[which(data$failed)] <- NA
data$util_opp <- as.numeric(data$util_opp)

all_agents <- sort(unique(data$against))