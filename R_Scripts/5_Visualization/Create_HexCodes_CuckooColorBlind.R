### Hex codes for cuckoo colors and colorblind friendly colors
cuckoo_palette <- c("#B75F4A","#656131","#4C2C13","#CD9F6E","#B9C6BB")

# Black-Billed Cuckoo
# eyering_red1 <- "#B75F4A"
# eyering_red2 <- "#CC3F1F"
# primaries_brown1 <- "#6A5424"
# primaries_brown2 <- "#4C2C13"
# head_cinnamon <- "#CD9F6E"
# chest_cream1 <- "#F0E4CA"
# chest_cream2 <- "#F6EFDE"
# chest_cream3 <- "#E3DAD1"
# foilage_green1 <- "#B9C6BB"
# foliage_green2 <-"#656131"
# sunshine_yellow <- "#F6DD62"

# Colorblind friendly hex codes
# Resources: https://davidmathlogic.com/colorblind/#%23648FFF-%23785EF0-%23DC267F-%23FE6100-%23FFB000 
# col1_8p <- "#332288"
# col2_8p <- "#117733"
# col3_8p <- "#44AA99"
# col4_8p <- "#88CCEE"
# col5_8p <- "#DDCC77"
# col6_8p <- "#CC6677"
# col7_8p <- "#AA4499"
# col8_8p <- "#882255"
palette_8 <- c("#332288","#117733","#44AA99","#88CCEE","#DDCC77","#CC6677","#AA4499","#882255")


# Palette for 5 colors
# col1_5p <- "#648FFF"
# col2_5p <- "#785EF0"
# col3_5p <- "#DC267F"
# col4_5p <- "#FE6100"
# col5_5p <- "#FFB000"
#palette_5 <- c("#648FFF","#785EF0","#DC267F","#FE6100","#FFB000")
palette_5 <- c("#88CCEE","#785EF0","#DC267F","#FE6100","#FFB000")


# Detection palette
dp_all <- colorRampPalette(c("#863300","#EA5900","#FFCBAB"))
d_palette <- dp_all(10)
# Landscape palette
lp_all <- colorRampPalette(c("#531534","#882255","#EFBFD7"))
l_palette <- lp_all(10)
# Core area palette
cp_all <- colorRampPalette(c("#115475","#88CCEE","#CCE9F8"))
c_palette <- cp_all(10)
# Point palette
pp_all <- colorRampPalette(c("#0C5424","#169A42","#C0F6D2"))
p_palette <- pp_all(10)


# Could add on YBCU if wanted


### Old
# col1_8p <- "#009E73"
# col2_8p <- "#E69F00"
# col3_8p <- "#56B4E9"
# col4_8p <- "#CC79A7"
# col5_8p <- "#D55E00"
# col6_8p <- "#0072B2"
# col7_8p <- "#F0E442"
# col8_8p <- "#000000"

# col1_8p <- "#009E73"
# col2_8p <- "#FFB000"
# col3_8p <- "#56B4E9"
# col4_8p <- "#CC79A7"
# col5_8p <- "#D55E00"
# col6_8p <- "#0072B2"
# col7_8p <- "#F0E442"
# col8_8p <- "#000000"

# Testing
# categories <- rep(c("Category1", "Category2", "Category3", "Category4", "Category5", "Category6", "Category7", "Category8"), each = 30)
# values <- rnorm(240, mean = rep(1:8, each = 30), sd = 1) # 30 samples per category, different means
# 
# simulated_data <- data.frame(
#   Category = categories,
#   Value = values
# )
# ggplot(simulated_data, aes(x = Category, y = Value, fill = Category)) +
#   geom_boxplot() +
#   scale_fill_manual(values = c("#332288","#117733","#44AA99","#88CCEE","#DDCC77","#CC6677","#AA4499","#882255")) +
#   theme_minimal()