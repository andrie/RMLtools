netDefinition <- "
const { T = true; F = false; }
input pixels [3, 50, 50];

hidden conv1 [48, 24, 24] rlinear from pixels convolve {
InputShape  = [3, 50, 50];
KernelShape = [3,  5,  5];
Stride      = [1,  2,  2];
LowerPad    = [0, 1, 1];
Sharing     = [T, T, T];
MapCount    = 48;
}

hidden rnorm1 [48, 11, 11] from conv1 response norm {
InputShape  = [48, 24, 24];
KernelShape = [1,   4,  4];
Stride      = [1,   2,  2];
LowerPad    = [0, 0, 0];
Alpha       = 0.0001;
Beta        = 0.75;
}

hidden pool1 [48, 9, 9] from rnorm1 max pool {
InputShape  = [48, 11, 11];
KernelShape = [1, 3, 3];
Stride      = [1, 1, 1];
}

hidden hid1 [256] rlinear from pool1 all;
hidden hid2 [256] rlinear from hid1 all;
output Class [6] from hid2 all;
"

# devtools::install_github("andrie/mxNeuralNetExtra")

library(mxNeuralNetExtra)
library(magrittr)
nn <- mxlayer_input(shape = c(3, 50, 50), name ="pixels") %>% 
  mxlayer_conv(
    kernelshape = c(3, 5, 5), 
    stride      = c(1, 2, 2), 
    sharing = c(1, 1, 1),
    mapcount = 64,
    name = "conv1"
  ) %>% 
  mxlayer_norm(
    kernelshape = c(1, 4, 4), 
    stride      = c(1, 2, 2), 
    name = "rnorm1",
    alpha = 0.0001,
    beta  = 0.75
  ) %>% 
  mxlayer_pool(
    kernelshape = c(1, 3, 3), 
    stride      = c(1, 1, 1), 
    name = "pool1"
  ) %>% 
  mxlayer_full(
    nodes      = 256, 
    name       = "hid1",
    activation = "rlinear"
  ) %>% 
  mxlayer_full(
    nodes      = 256, 
    name       = "hid2",
    activation = "rlinear"
  ) %>% 
  mxlayer_output(
    nodes = 6, 
    name  = "Class"
  )

print(nn)
