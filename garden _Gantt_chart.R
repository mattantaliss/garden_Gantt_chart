# Garden Gantt Chart
#
# Instructions:
# Edit the 'crops' data in the User-defined data section, then run the script
# to generate and save the chart. The chart is saved in the working directory.

### User-defined data ###
# Define crops with start and end times for sowing indoors, sowing outdoors, and
# harvesting.
# The format of each row is Crop name (character), Activity (should be one of
# "Sow indoors", "Sow outdoors", or "Harvest"), Start.month (numeric),
# End.month (numeric).
# Start.month and End.month are numerical month numbers. For example, planting
# between mid-May and end-June would be encoded as 5.5, 7, and harvesting
# between December and mid-February is encoded as 12, 2.5.
crops <- setNames(data.frame(
  rbind(
    c("Corn", "Sow indoors", 5, 5.5),
    c("Corn", "Sow outdoors", 5.5, 6.5),
    c("Corn", "Harvest", 8, 9),
    c("Cucumber", "Sow indoors", 5, 5.5),
    c("Cucumber", "Sow outdoors", 5.5, 6.5),
    c("Cucumber", "Harvest", 7.5, 9),
    c("Pumpkin", "Sow indoors", 5.5, 6),
    c("Pumpkin", "Sow outdoors", 6, 7),
    c("Pumpkin", "Harvest", 8.5, 10.5),
    c("Lettuce", "Sow indoors", 3, 4.5),
    c("Lettuce", "Sow outdoors", 4, 9),
    c("Lettuce", "Harvest", 5.5, 11.5),
    c("Onion", "Sow indoors", 2, 4.5),
    c("Onion", "Sow outdoors", 4, 6.5),
    c("Onion", "Harvest", 5.5, 10),
    c("Carrot", "Sow outdoors", 4.5, 7),
    c("Carrot", "Harvest", 7.5, 12),
    c("Tomato", "Sow indoors", 3.5, 5.5),
    c("Tomato", "Sow outdoors", 5.5, 7),
    c("Tomato", "Harvest", 6, 10),
    c("Potato", "Sow outdoors", 4.5, 6.5),
    c("Potato", "Harvest", 8, 10.5),
    c("Garlic", "Sow outdoors", 10.5, 13),
    c("Garlic", "Harvest", 6.5, 10),
    c("Brussels Sprout", "Sow indoors", 3.5, 4.5),
    c("Brussels Sprout", "Sow outdoors", 4, 5.5),
    c("Brussels Sprout", "Harvest", 11, 3.5),
    c("Zucchini", "Sow indoors", 5, 5.5),
    c("Zucchini", "Sow outdoors", 5.5, 7),
    c("Zucchini", "Harvest", 7, 10),
    c("Leek", "Sow indoors", 2.5, 5),
    c("Leek", "Sow outdoors", 4.5, 6),
    c("Leek", "Harvest", 9, 3),
    c("Pea", "Sow indoors", 3.5, 4.5),
    c("Pea", "Sow outdoors", 4, 7),
    c("Pea", "Harvest", 6, 10),
    c("Beet", "Sow outdoors", 5, 7),
    c("Beet", "Harvest", 7.5, 11)),
  stringsAsFactors = FALSE),
  c("Crop", "Activity", "Start.month", "End.month"))

# Set the crops to include in the chart (as is, everything is included).
# This allows for having 'crops' be a sort of master database of times which is
# then filtered for only the crops of current interest here. To have only a
# subset of crops, change the next line to something like
# current_crops <- c("Corn", "Pea", "Beet")
current_crops <- unique(crops$Crop)
crops <- crops %>%
  filter(Crop %in% current_crops)

### Data processing ###
# Edits are only needed in this section if the script behavior needs to be
# modified (e.g., change max.month to 25 for a two-year cycle)

# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Set the start and end months for the chart range.
# Times that go into the next year (e.g., harvesting between December and March)
# will be wrapped around based on these values.
min.month = 1  # Start of January
max.month = 13 # End of December

# Define a function to implement periodic boundaries for this chart
# x - the value(s) to convert
# min.val - the minimum value of the chart (i.e., min.month)
# max.val - the maximum value of the chart (i.e., max.month)
pb_trans <- function(x, min.val, max.val) {
  # Calculate modified values
  y <- x %% max.val + min.val

  # If the value is within the range, return it, otherwise return the max of
  # the modified value and max.val
  ifelse(min.val <= x & x <= max.val,
         x,
         ifelse(max.val < y, max.val, y))
}

# Convert the user-defined data frame columns to correct types
crops$Crop <- as.factor(crops$Crop)
# Activity is set as a reverse-ordered factor. This will make it so
# "Sow indoors" points are above "Sow outdoors" points which are above "Harvest"
# points in the chart, but the legend needs another reverse to put the
# activities in the same top-to-bottom order.
crops$Activity <- factor(crops$Activity,
                         levels = c("Harvest", "Sow outdoors", "Sow indoors"),
                         ordered = TRUE)
crops$Start.month <- as.numeric(crops$Start.month)
crops$End.month <- as.numeric(crops$End.month)

# Adjust end month for use in pb_trans (to wrap around the end of the chart
# range if needed)
crops <- crops %>%
  mutate(End.month = ifelse(End.month < Start.month,
                            End.month + max.month - min.month,
                            End.month))

# The bars of the chart will be faked by plotting sequences of square points.
# This sets the number of points per unit:
points_per_unit <- 25

# Create sequences of times (with periodic boundaries) given start.month,
# end.month, min.month, and max.month
times <- unlist(apply(crops[, 3:4], 1, function(x) {
  pb_trans(seq(x[1], x[2],
               length.out = round(points_per_unit * (x[2] - x[1]) + 1)),
           min.month,
           max.month)
}))

# Expand the data frame to have one row for each time point
crops <-
  cbind(crops[rep(row.names(crops),
                  round(points_per_unit *
                          (crops$End.month - crops$Start.month) + 1)), ],
        Time = times)

# Order crop names by activity (Sow, Harvest), then by start month, end month,
# and name. Activity is a reverse-ordered factor (for producing a top-to-bottom
# sorted display in the plot), so it is sorted descending. It is also sorted
# only by the first three characters; this groups "Sow indoors" and
# "Sow outdoors" together as "Sow". Everything else is sorted ascending to
# correctly put them in order (by start month, then end month and crop name).
# The Crop levels are then the unique crops (ordered by first encounter)
# reversed to give the desired top-to-bottom sorting in the plot.
# The result here gives crops listed from top to bottom by first sow dates
# (indoors or outdoors) in the chart.
crops <- crops %>%
  arrange(desc(substr(Activity, 1, 3)), Start.month, End.month, Crop) %>%
  mutate(Crop = factor(Crop, rev(unique(Crop)), ordered = TRUE)) %>%
  select(-Start.month, -End.month)

# Create the cyclic Gantt chart
# Crops will be on the vertical (y-)axis and time on the horizontal (x-)axis,
# but to make use of 'position_dodge' to keep the points (fake bars) from
# overlapping the chart needs to be created with crops on the horizontal axis
# and then flipped at the end.
ggplot(crops, aes(x = Crop, y = Time, colour = Activity)) +
  # Point size gets smaller as more crops are included. The numerator was
  # determined by what looked best for a few different charts.
  geom_point(size = round(25 / nlevels(crops$Crop)), shape = 15,
             position = position_dodge(width = 0.5)) +
  # Remove the x- and y-axis labels
  xlab(NULL) +
  ylab(NULL) +
  # Set the chart range based on min.month and max.month, and label the ticks
  # with abbreviated month names.
  scale_y_continuous(limits = c(min.month, max.month),
                     breaks = min.month:max.month,
                     labels =
                       rep_len(month.abb, max.month)[min.month:max.month]) +
  # Reverse the order of the activities to give the desired top-to-bottom
  # ordering, and set a fixed size for the colored point.
  guides(colour = guide_legend(reverse = TRUE, override.aes = list(size = 5))) +
  # Flip the plot to put crops on the vertical axis and time on the horizontal
  # axis
  coord_flip() +
  # Use the bw theme
  theme_bw() +
  # Set major grid lines to be dashed black lines and remove the boxes
  # surrounding the colored point in the legend
  theme(panel.grid.major = element_line(colour = "black", linetype = "dashed"),
        legend.key = element_blank())

# Save the chart as a PDF in the working directory. The width and height here
# are a good size for printing on 8.5"x11" paper; adjust as needed.
ggsave("garden_Gantt_chart.pdf", width = 7.5, height = 10, units = "in")
