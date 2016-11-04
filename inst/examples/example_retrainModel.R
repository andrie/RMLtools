library(MicrosoftML)
frm <- as.formula(isCase ~ age + parity + education + spontaneous + induced)
model <- neuralNet(frm,
                     data = infert,
                     transforms = list(isCase = case == 1),
                     numHiddenNodes = 3, 
                     verbose = 1, 
                     reportProgress = 0,
                     numIterations = 10
)

tf <- tempfile(fileext = ".nn")
dummy <- reconstructNetDefinition(model, tf)
cat(dummy)




model2 <- neuralNet(frm,
                      transforms = list(isCase = case == 1),
                      data = infert, 
                      netDefinition = readNetDefinition(tf),
                      numIterations = 10,
                      verbose = 1
)


