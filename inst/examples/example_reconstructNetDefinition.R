library(MicrosoftML)
neuralNet(isCase ~ age + parity + education + spontaneous + induced,
                     transforms = list(isCase = case == 1),
                     data = infert,
                     numHiddenNodes = 3, 
                     verbose = 1, 
                     reportProgress = 0,
            numIterations = 20
)
model <- neuralNet(isCase ~ age + parity + education + spontaneous + induced,
                    transforms = list(isCase = case == 1),
                    data = infert,
                    numHiddenNodes = 3, 
                    verbose = 1, 
                    reportProgress = 0,
                    numIterations = 10
)
reconstructNetDefinition(model)
cat(reconstructNetDefinition(model))
tf <- tempfile(fileext = ".nn")

reconstructNetDefinition(model, filename = tf)
cat(
  readNetDefinition(tf)
)


neuralNet(isCase ~ age + parity + education + spontaneous + induced,
            transforms = list(isCase = case == 1),
            data = infert, 
            netDefinition = readNetDefinition(tf),
            numIterations = 10
            )
