
knitr::opts_chunk$set(echo = FALSE)
library(grid)
library(gridExtra)
library(scales)
library(reticulate)

run_agents <- function (repo_path, venv_dir, results_dir, results_name, agent1, agent2, profile1, profile2) {
  
  use_virtualenv(paste(repo_path, venv_dir, sep=''))
  
  file.copy("run_from_r.py", paste(repo_path, "run_from_r.py", sep=''))
  
  py_eval("0")
  
  py$results_dir <- paste(getwd(), results_dir, sep='/')
  py$results_name <- results_name
  py$agent1 <- agent1
  py$agent2 <- agent2
  py$profile1 <- paste(repo_path, profile1, sep='')
  py$profile2 <- paste(repo_path, profile2, sep='')
  
  py_run_file(paste(repo_path, "run_from_r.py", sep=''), convert = FALSE)
}

read_results <- function (repo_path, results_name) {
  
  read_results_json <- function (results_dir, results) {
    cat(file=stderr(), sprintf("\nReading the results for %s\n", results))
    json_path <- paste(results_dir, results, '/session_results_trace.json', sep='')
    jsonlite::fromJSON(json_path)
  }
  
  json <- read_results_json(repo_path, results_name)
  
  agents <- sapply(json$settings$SAOPSettings$participants$TeamInfo$parties, 
                   function(x) {x[["party"]][["partyref"]]})
  
  connections <- json$connections
  
  if (!is.null(json$error)) {
    stop(sprintf("Results contain an error: %s\n", json$error$message))
  }
  
  duration <- json$progress$ProgressTime$duration
  
  data <- na.omit(json$actions$Offer$utilities)
  colnames(data) <- c("Utility1", "Utility2")
  
  data$OfferedBy <- na.omit(factor(json$actions$Offer$actor,
                                   levels=connections, labels=c("Agent1", "Agent2")))
  
  accepts <- factor(json$actions$Accept$actor,
                    levels=connections, labels=c("Agent1", "Agent2"))
  
  accepted_bids <- which(!is.na(accepts))
  if (is.null(json$actions$Accept)) {
    stop("The run failed. No bid was accepted.")
  }
  if (accepted_bids != c(length(data$Utility1) + 1)) {
    stop(sprintf("Expected accepted bid at %d, got %s\n", length(data) + 1, accepted_bids))
  }
  
  return (list(data, connections, agents, duration, json))
}

profile_utilities <- function (util_json) {
  issueUtilities <- sapply(util_json$LinearAdditiveUtilitySpace$issueUtilities,
                           function(x) {unlist(x$DiscreteValueSetUtilities$valueUtilities)})
  # all combinations
  combos <- expand.grid(issueUtilities)
  weights <- unlist(util_json$LinearAdditiveUtilitySpace$issueWeights)
  
  apply(combos, MARGIN=1, FUN=function(x) {sum(x * t(weights))})
}

read_utilities_json <- function (repo_path, results_name, results_json) {
  json <- results_json
  
  connections <- json$connections
  
  util_filename <- function(x) {
    path <- substr(x, 6, 999)
    if (substr(path, 1, 1) == '/') {
      return (path)
    } else {
      return (paste(repo_path, path, sep=''))
    }
  }
  
  util1_file <- util_filename(json$partyprofiles[[connections[1]]]$profile)
  util2_file <- util_filename(json$partyprofiles[[connections[2]]]$profile)
  
  util1_json <- jsonlite::fromJSON(util1_file)
  util2_json <- jsonlite::fromJSON(util2_file)
  
  return (list(util1_json, util2_json))
}

# NAs at start = NA, the rest is cumulative minimum ignoring NAs
cummin_na <- function(x) {
  first_num = which(!is.na(x))[1]
  if (is.na(first_num)) {
    return (x)
  }
  rest <- x[seq(first_num, length(x))]
  rest[is.na(rest)] <- max(na.omit(x))
  c(rep(NA, first_num - 1), cummin(rest))
}
cummax_na <- function(x) {
  first_num = which(!is.na(x))[1]
  if (is.na(first_num)) {
    return (x)
  }
  rest <- x[seq(first_num, length(x))]
  rest[is.na(rest)] <- min(na.omit(x))
  c(rep(NA, first_num - 1), cummax(rest))
}

paretos <- function(x) {
  x <- apply(x, MARGIN=2, FUN=function(z) {sort_by(z, -x[,1], x[,2])})
  na.omit(x[x[,2] == cummax_na(x[,2]),])
}

agent_name <- function(x) {
  sp <- strsplit(x, "\\.")[[1]]
  sp[length(sp)]
}

plot_run_results <- function (result_data) {
  
  data <- result_data[[1]]
  connections <- result_data[[2]]
  agents <- sapply(result_data[[3]], agent_name)
  duration <- result_data[[4]]
  json <- result_data[[5]]
  
  cat("\nAgents in the negotiation: \n\\begin{itemize}\n")
  for (agent in agents) {
    cat(sprintf("    \\item\\verb|%s| \n", agent))
  }
  cat("\\end{itemize}\n")
  
  accepted_bid <- which(!is.na(data$AcceptedBy))[1]
  
  agent_names = split
  
  colors <- c("steelblue1", "lightsalmon1", "royalblue3", "tomato3")
  
  consensus <- length(data$Utility1)
  
  offered1 <- data$Utility1
  offered1[(data$OfferedBy == "Agent2")] <- NA
  offered2 <- data$Utility2
  offered2[(data$OfferedBy == "Agent1")] <- NA
  received1 <- data$Utility1
  received1[(data$OfferedBy == "Agent1")] <- NA
  received2 <- data$Utility2
  received2[(data$OfferedBy == "Agent2")] <- NA
  
  #layout(matrix(c(1,4,2,3), 2, 2, byrow = TRUE))
  
  plot(offered1, pch=18, cex=0.6, col=alpha(colors[1], 0.5),
       ylim=c(0, 1), xlab="Bid number", ylab="Bid utility", main="Bids exchanged between agents")
  points(offered2, pch=18, cex=0.6, col=alpha(colors[2], 0.5))
  points(received1, pch=18, cex=0.5, col=alpha(colors[3], 0.5))
  points(received2, pch=18, cex=0.5, col=alpha(colors[4], 0.5))
  abline(1, 0, lty='dashed', col='gray')
  legend(x ="bottomleft", bg="white", inset=0.05, legend = c("   Bid offered:",
                                                             sprintf("1 - %s", agents[1]),
                                                             sprintf("2 - %s", agents[2]),
                                                             "   Bid received:",
                                                             sprintf("1 - %s", agents[1]),
                                                             sprintf("2 - %s", agents[2])),
         lty=c("blank", "solid")[c(1, 2, 2, 1, 2, 2)],
         pch=c(NA, 18)[c(1, 2, 2, 2, 1, 1)],
         col=colors[c(NA, 1, 2, NA, 3, 4)],
         lwd=3, cex=0.7)
  
  plot(cummin_na(offered1), type="s", col=colors[1], lwd=1.5,
       ylim=c(0, 1), xlab="Bid number", ylab="Utility",
       main="Best/worst bid exchanged")
  get#  abline(data$Utility1[consensus], 0, lty='dashed', col=colors[1], lwd=1.5)
  #  abline(data$Utility2[consensus], 0, lty='dashed', col=colors[2], lwd=1.5)
  points(cummin_na(offered2), type="s", col=colors[2], lwd=1.5)
  points(cummax_na(received1), type="s", col=colors[3], lwd=1.5)
  points(cummax_na(received2), type="s", col=colors[4], lwd=1.5)
  points(offered1[consensus], x=consensus, pch=18, cex=2, col=colors[3])
  points(received1[consensus], x=consensus, pch=18, cex=2, col=colors[3])
  points(offered2[consensus], x=consensus, pch=18, cex=2, col=colors[4])
  points(received2[consensus], x=consensus, pch=18, cex=2, col=colors[4])
  legend(x ="bottomright", bg="white", inset=0.05, legend = c("   Worst bid offered:",
                                                              sprintf("1 - %s", agents[1]),
                                                              sprintf("2 - %s", agents[2]),
                                                              "   Best bid received:",
                                                              sprintf("1 - %s", agents[1]),
                                                              sprintf("2 - %s", agents[2]),
                                                              "   Accepted bid:",
                                                              sprintf("1 - %s", agents[1]),
                                                              sprintf("2 - %s", agents[2])),
         lty=c("blank", "solid")[c(1, 2, 2, 1, 2, 2, 1, 1, 1)],
         pch=c(NA, 18)[c(1, 2, 2, 2, 1, 1, 1, 2, 2)],
         col=colors[c(NA, 1, 2, NA, 3, 4, NA, 3, 4)],
         lwd=3, cex=0.7)
  
  utils_jsons <- read_utilities_json(repo_path, results_name, json)
  all_utils <- sapply(utils_jsons, profile_utilities)
  
  plot(all_utils, pch=16, cex=0.2, col='gray60', asp=1, main="Utility space",
       xlim=c(0,1), ylim=c(0,1), xlab=sprintf("Utility 1 - %s", agents[1]),
       ylab=sprintf("Utility 2 - %s", agents[2]))
  
  
  points(paretos(all_utils), type='l', col='gray70', lwd=1.5)
  points(paretos(all_utils), pch=18, cex=0.5, col='black')
  
  points(paretos(cbind(offered1, received2)), type='s', col=colors[1], lwd=1.5)
  points(paretos(cbind(received1, offered2)), type='s', col=colors[2], lwd=1.5)
  
  points(offered1, received2, pch=18, cex=0.7, col=colors[3])
  points(received1, offered2, pch=18, cex=0.7, col=colors[4])
  
  points(offered1[consensus], received2[consensus], pch=18, cex=2, col=colors[3])
  points(received1[consensus], offered2[consensus], pch=18, cex=2, col=colors[4])
  
  legend(x ="bottomleft", bg="white", inset=0.05, legend = c("   Bid offered by:",
                                                             sprintf("1 - %s", agents[1]),
                                                             sprintf("2 - %s", agents[2]),
                                                             "Not offered",
                                                             "   Pareto frontier:",
                                                             sprintf("1 - %s", agents[1]),
                                                             sprintf("2 - %s", agents[2]),
                                                             "Utility space"),
         lty=c("blank", "solid")[c(1, 1, 1, 1, 1, 2, 2, 2)],
         pch=c(NA, 18, 16)[c(1, 2, 2, 3, 1, 1, 1, 1)],
         col=c(colors, "gray70")[c(NA, 3, 4, 5, NA, 1, 2, 5)],
         lwd=3, cex=0.7)
  
  concessions1 <- cummin_na(offered1)[c(-consensus, -consensus+1)] - cummin_na(offered1)[c(-1, -2)]
  concessions2 <- cummin_na(offered2)[c(-consensus, -consensus+1)] - cummin_na(offered2)[c(-1, -2)]
  plot(filter(concessions1, rep(1/1000, 1000)), col=colors[1],
       main="Concession rate", xlab=sprintf("Bid number"),
       ylab=sprintf("Average decrease in offered utility per round"))
  points(filter(concessions2, rep(1/1000, 1000)), type='l', pch=16, col=colors[2])
  
  cat(file=stderr(), "\nAll graphs plotted for these results")
}
