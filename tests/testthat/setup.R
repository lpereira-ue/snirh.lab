# Use the parameter table bundled with the package, so tests run offline and
# do not write to the user cache
options(snirh.lab.parameters = system.file("extdata", "parameters.csv", package = "snirh.lab"))
