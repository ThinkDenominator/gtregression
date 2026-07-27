# Endometrial cancer data for demonstrating separation in logistic regression.
# Source: brglm2::endometrial, originally analyzed by Heinze and Schemper (2002).

data("endometrial", package = "brglm2")

data_endometrial <- endometrial

save(data_endometrial, file = "data/data_endometrial.rda", compress = "xz")
