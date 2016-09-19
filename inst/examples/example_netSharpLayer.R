
# Use the layer functions to generate individual layer specifications

layer_input(c(13, 13))
layer_input(c(3, 7, 7), name = "pixels")

# Convolution layers automatically compute the output size and padding

layer_conv(NULL, c(2, 2), 
           inputshape = c(13, 13), 
           name = "conv1", 
           inputname = "pixels"
)

layer_conv(NULL, 
           c(2, 2), 
           inputshape = c(13, 13), 
           name = "conv1", 
           inputname = "pixels", 
           stride = c(2, 2)
)

layer_conv(NULL, 
           c(1, 2, 2), 
           inputshape = c(3, 13, 13), 
           name = "conv1", 
           inputname = "pixels", 
           stride = c(1, 2, 2)
)

layer_pool(NULL, 
           c(1, 2, 2), 
           inputshape = c(3, 13, 13), 
           name = "conv1", 
           inputname = "pixels", 
           stride = c(1, 2, 2)
)



# Specify the number of nodes in a fully connected layer

layer_full(NULL, nodes = 100, name = "h3", inputname = "conv")

# Output layer

layer_output(NULL, 6, name = "class", inputname = "h3")


# using magrittr pipes to connect layers ----------------------------------

require(magrittr)

layer_input(c(3, 50, 50), name = "pixels") %>% 
  layer_conv(
    kernelshape = c(1, 5, 5),
    name = "conv1", 
    stride = c(1, 2, 3)
  )


layer_input(c(3, 50, 50), name = "pixels") %>% 
  layer_conv(
    kernelshape = c(3, 5, 5), 
    name = "conv1", 
    stride = c(1, 2, 2),
    mapcount = 48
  ) %>% 
  layer_conv(
    kernelshape = c(1, 4, 4), 
    stride = c(1, 2, 2),
    name = "conv2"
  ) %>% 
  layer_full(nodes = 100, name = "hid1") %>% 
  layer_full(nodes = 30, name = "hid2") %>% 
  layer_output(nodes = 6, name = "class")




