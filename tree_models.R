# The code here was informed by:
# https://stackoverflow.com/questions/41112046/using-purrrmap-to-iterate-linear-model-over-columns-in-data-frame
# https://r4ds.had.co.nz/many-models.html#making-tidy-data-with-broom
# https://stackoverflow.com/questions/39228502/double-nesting-in-the-tidyverse

# Load libraries
if (!require(pacman)) install.packages("pacman")
# Broom package has some functions to help make linear model output cleaner (see
# the tidy_models column later)
pacman::p_load(broom)
pacman::p_load(modelr)
pacman::p_load(magrittr)
pacman::p_load(tidyverse)
pacman::p_load(stringr)

# Load data
dat <- read.csv("sitetotals_v.csv")
# lowercase variable names
# I do this because I hate typing all caps. This makes all your variable names
# lowercase.
dat <- dat %>% rename_all(~str_to_lower(.))

# Let's get the data into long format for beetle counts
dat_long <- dat %>% 
   gather(beetle, count, acmaeodera.pulchella:t_spectralia.gracilipes) %>%
   # I just learned about the starts_with() function. You can use it with
   # select() to more generally select columns that start with a certain string
   select(beetle, count, site, starts_with("ba_"), starts_with("c_"))
# Maybe we just nest from here? Let's nest first by beetles
nested_dat <- dat_long %>% 
   group_by(beetle) %>% 
   nest(.key = tree_data)
# Let's check out the nesting.
nested_dat$tree_data[[1]]

# The tree data is now nested as a dataframe within the "tree_data" column of
# the nested_dat dataframe. Now, let's double nest the actual tree variables
# within each beetle measurement. This is a fairly complicated set of commands
# using map. I'll try to explain here
# We're going pipe our data to mutate and mutate the existing tree_data column,
# which currently is a list holding dataframes of the tree data tied to each
# beetle. Because we're working with the tree_data column, which is technically
# a list we can use the map() function. Map takes each item in a list and 
# applies the same function to each item independently. We start by telling map
# that the list we want to work with is tree_data (because we piped our the 
# dataframe containing tree_data to the mutate and map function, it knows to
# find tree_data in our dataframe). Then we tell it to take each item in the
# list, represented in the abstract by .x and apply some function to each item.
# The ~ is one way to write "take the item .x and apply the following functions
# to it".
# Then we pipe the .x list object to the gather function. So at this point, the
# .x item is one of our tree variables dataframes associated with a single
# species of beetle. We take that data frame and convert it from wide to long
# with gather. This creates one variable which identifies the tree variables
# (tree_vars) and another variable that identifies the corresponding measurement
# for that variable (tree_values). The argument after tree_values tells gather
# which columns to convert into this format. Then, we group by each type of tree
# variable in tree_vars. After that we pipe the grouped, long data to the nest
# function. So now our tree_data column, which was a list of data frames is
# still a list of data frames. But, inside each of those dataframes are an
# additional set of nested data frames. We have two levels of nesting, at the
# beetle level first then the tree level.
nested_dat <- nested_dat %>% 
   mutate(tree_data = map(tree_data, ~.x %>% 
                             gather(tree_vars, tree_values, ba_abies:c_ulmus) %>% 
                             group_by(tree_vars) %>% 
                             nest(.key = tree_vars_values)))
# First level - Beetle
nested_dat
# Second level - Tree variables for each beetle
nested_dat %>% unnest
# Thir level - actual values observed for each tree variable and each beetle
nested_dat %>% unnest %>% unnest


# Now, we should be able to apply models at the third level of the nested data,
# summarize them at the second level
# This function is really gross, but I'm not sure there's a better way to do it.
# This code will work like the above code where we created our second level of
# nesting. Because we want to retain the structure of our nesting, we have to
# start by mutating the existing tree_data column, taking the data frames in
# that list and using map to apply the same function to each item and then using
# mutate to create a new column in our second level nested dataframes. We'll
# call this variable "models" and it will eventually become a list of linear
# model objects. So we're into our second level of nesting, which recall is a
# list of data frames of our tree variables. We'll take these data frames one at
# a time and perform a linear regression with beetle counts for that particular
# species as the response and the values for our given tree variable (each tree
# variable has its own data frame, which is what we're mapping on). We then pipe
# the models column to another mutate command to create a "tidy_models" column,
# which will make it easier to see our output. We use map with our list of model
# output and use the tidy() function which just converts model output into a
# form compatible with dataframes.
# Of course, we can't model without checking our assumptions. We can do this
# by capturing the residuals for each observation based on our models, which is
# what the "resid" variable we use mutate to create does. The function map2() is
# used when you want to perform some operation that requires two separate lists
# with equal numbers of objects. So map2 moves along the objects in each list
# (can be represented as .x and .y) and applies some function to both or using
# both. In this case, our .x objects are the tree variable data frames we used
# in our regression and our .y objects are the models created with that data.
# The add_residuals() function uses those two objects to determine the residuals
# for each observation in .x.
# NOTE: If you need to change the type of model you're using, find the lm()
# function in the code chunk below and change it to a GLM or whatever you need
# it to be.
nested_models <- nested_dat %>% 
   mutate(tree_data = 
             map(tree_data, ~.x %>% 
                    mutate(models = map(tree_vars_values,
                                        ~lm(count ~ tree_values, 
                                            data = .x))) %>% 
                    mutate(tidy_models = map(models, tidy),
                           resids = map2(tree_vars_values,
                                         models,
                                         add_residuals))))

# Now we can write some code to look at our results and check our assumptions.

# Summarise different models
# We can pull out the results for specific models if we want (change the code in
# the filter function to look at other stuff)
nested_models %>% 
   unnest() %>% 
   filter(beetle == "agrilus.planipennis" & tree_vars == "ba_acer") %$% 
   tidy_models %>% 
   extract2(1)

# Or we can look at everything
nested_models %>% 
   unnest %>% 
   unnest(tidy_models) %>% 
   View()

# Plot of our p-values to help identify where there might be patterns Because we
# have so many results, it's best to filter our data before plotting. Use filter
# with specific names like you normally would or use the str_detect() function
# with a genus or just the first letter (really any string you want all
# potential matches for). 
nested_models %>% 
   unnest %>% 
   unnest(tidy_models) %>% 
   filter(str_detect(beetle, "acmaeodera")) %>% 
   ggplot(aes(x = tree_vars, y = p.value)) +
      geom_point() +
      geom_hline(yintercept = 0.05, color = "blue", linetype = "dashed") +
      facet_grid(~ beetle) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90))

# Plot the residuals to check our assumptions
# You'll probably have to do this one or a few at a time by filtering first by
# a specific beetle or two and then using the second filter to select the tree
# variables you're interested in.
nested_models %>% 
   unnest %>% 
   unnest(resids) %>% 
   filter(str_detect(beetle, "planipennis")) %>% 
   filter(str_detect(tree_vars, "abies")) %>% 
   ggplot(aes(x = site, y = resid)) +
   geom_point() +
   facet_wrap(beetle ~ tree_vars) +
   theme_classic() +
   theme(axis.text.x = element_text(angle = 90))

