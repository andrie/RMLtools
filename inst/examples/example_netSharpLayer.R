
# Use the layer functions to generate individual layer specifications

nnlayer_input(c(13, 13))
nnlayer_input(c(3, 7, 7), name = "pixels")

# Convolution layers automatically compute the output size and padding

nnlayer_conv(NULL, c(2, 2), 
           inputshape = c(13, 13), 
           name = "conv1", 
           inputname = "pixels"
)

nnlayer_conv(NULL, 
           c(2, 2), 
           inputshape = c(13, 13), 
           name = "conv1", 
           inputname = "pixels", 
           stride = c(2, 2)
)

nnlayer_conv(NULL, 
           c(1, 2, 2), 
           inputshape = c(3, 13, 13), 
           name = "conv1", 
           inputname = "pixels", 
           stride = c(1, 2, 2)
)

nnlayer_pool(NULL, 
           c(1, 2, 2), 
           inputshape = c(3, 13, 13), 
           name = "conv1", 
           inputname = "pixels", 
           stride = c(1, 2, 2)
)



# Specify the number of nodes in a fully connected layer

nnlayer_full(NULL, nodes = 100, name = "h3", inputname = "conv")

# Output layer

nnlayer_output(NULL, 6, name = "class", inputname = "h3")


# using magrittr pipes to connect layers ----------------------------------

require(magrittr)

nnlayer_input(c(3, 50, 50), name = "pixels") %>% 
  nnlayer_conv(
    kernelshape = c(1, 5, 5),
    name = "conv1", 
    stride = c(1, 2, 3)
  )

nnlayer_input(c(3, 50, 50), name = "pixels") %>% 
  nnlayer_conv(
    kernelshape = c(1, 5, 5),
    name = "conv1", 
    stride = c(1, 2, 3)
  ) %>% 
  nnlayer_pool(
    kernelshape = c(1, 5, 5),
    name = "conv1", 
    stride = c(1, 2, 3)
  )

nnlayer_norm(NULL, inputshape = c(3, 11, 5), kernelshape = c(1,5,5), name = "rnorm1", inputname = "conv")

nnlayer_input(c(3, 50, 50), name = "pixels") %>% 
  nnlayer_conv(
    kernelshape = c(1, 5, 5),
    name = "conv1", 
    stride = c(1, 2, 3)
  ) %>% 
  nnlayer_norm(
    kernelshape = c(1, 5, 5),
    name = "norm1", 
    stride = c(1, 2, 3),
    alpha = 0.0001,
    beta = 0.75
  )


nnlayer_input(c(3, 50, 50), name = "pixels") %>% 
  nnlayer_conv(
    kernelshape = c(3, 5, 5), 
    name = "conv1", 
    stride = c(1, 2, 2),
    mapcount = 48
  ) %>% 
  nnlayer_conv(
    kernelshape = c(1, 4, 4), 
    stride = c(1, 2, 2),
    name = "conv2"
  ) %>% 
  nnlayer_full(nodes = 100, name = "hid1") %>% 
  nnlayer_full(nodes = 30, name = "hid2") %>% 
  nnlayer_output(nodes = 6, name = "class")




