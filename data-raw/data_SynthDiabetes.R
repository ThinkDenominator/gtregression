if (utils::packageVersion("mlbench") < "2.1.11") {
  stop("mlbench version 2.1-11 or later is required.", call. = FALSE)
}

utils::data("SynthDiabetes2", package = "mlbench", envir = environment())
data_SynthDiabetes <- SynthDiabetes2

save(data_SynthDiabetes, file = "data/data_SynthDiabetes.rda", compress = "xz")
