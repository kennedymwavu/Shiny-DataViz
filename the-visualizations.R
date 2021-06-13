library(readxl)
library(tidyverse)
library(reshape2)
library(magrittr)

# Read in the sample data:
all_data <- df <-  read_xlsx(
  path = "data for assignment.xlsx", 
  sheet = 1, col_names = TRUE
)

# take a peak at the data:
glimpse(df)
head(df)

# Convert to tidy data ie. each column is a variable and each row is an observation:
df %<>% t() %>% as.data.frame()

head(df)
str(df)

# The first observation in each column should be the column name:
colnames(df) <- df[1, ]

# The rownames should be a column in the data so we add it as the first column:
df <- df[-1, ]
df %<>% mutate(Year = rownames(df), .before = 1)

# All the data should be numeric but `Year` should be a factor:
df %<>% sapply(., as.numeric) %>% as_tibble() %>% 
  mutate(Year = factor(Year))

df_f <- df
# Visualizations:

# --------------------------------------------------------------------------------
# Gross direct premium:
# --------------------------------------------------------------------------------
# Subset required data:
gdp <- df %>% select(Year, `Gross Direct Premium`) %>% 
  mutate(`Gross Direct Premium` = `Gross Direct Premium` / 1e6)
gdp_f <- gdp %>% 
  mutate(`Gross Direct Premium` = `Gross Direct Premium` %>% round(digits = 1))

# windows()
viz1 <- gdp %>% 
  ggplot(mapping = aes(x = Year, y = `Gross Direct Premium`))  + 
  geom_col(width = 0.5, fill = '#425e72') +
  ylab(expression(italic("Amount in Ksh(Billions)"))) + 
  xlab(expression(italic("Year"))) + 
  # Underline the ggtitle:
  ggtitle(expression(underline("Gross Direct Premium"))) + 
  coord_cartesian(ylim = c(40, 47)) + 
  geom_text(mapping = aes(label = `Gross Direct Premium` %>% round(digits = 1)), 
            vjust = -0.2, fontface = "bold") + 
  # refine the aesthetics of the graph:
  theme(
    aspect.ratio = 0.65,
    axis.ticks.x = element_blank(), 
    plot.title = element_text(hjust = 0.5), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line()
  )
# viz1

# --------------------------------------------------------------------------------
# incurred claims vs net earned premium
# --------------------------------------------------------------------------------
# Subset the required data:
claims_prem <- df %>% dplyr::select(Year, `Incurred Claims`, `Net Earned Premium Income`) %>% 
  transmute(Year = Year, 
            `Incurred Claims` = `Incurred Claims` / 1e6, 
            `Net Earned Premium Income` = `Net Earned Premium Income` / 1e6)
claims_prem_f <- claims_prem %>% 
  mutate(
    `Incurred Claims` = `Incurred Claims` %>% round(digits = 1), 
    `Net Earned Premium Income` = `Net Earned Premium Income` %>% round(digits = 1)
  )

# It would be easier to work with this data in long format:
claims_prem %<>% pivot_longer(cols = c(`Incurred Claims`, `Net Earned Premium Income`))
claims_prem

# visualization:
# windows()
viz2 <- claims_prem %>% 
  ggplot(mapping = aes(x = Year, y = value, fill = name)) + 
  geom_col(position = "dodge", width = 0.6) + 
  xlab(expression(italic("Year"))) + ylab(expression(italic("Amount in Ksh (Billions)"))) + 
  ggtitle(expression(underline("Incurred Claims vs Net Earned Premium"))) + 
  geom_text(mapping = aes(label = value %>% round(digits = 1), group = name), 
            vjust = 5, hjust = 0.5, position = position_dodge(width = 0.5),  
            fontface = "bold") + 
  theme(
    aspect.ratio = 0.65, 
    plot.title = element_text(hjust = 0.5), 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.ticks.x = element_blank(), 
    legend.title = element_blank(), 
    legend.position = "bottom", 
    legend.text = element_text(face = "bold")
  )
# viz2

# ------------------------------------------------------------------
# Loss ratios
# ------------------------------------------------------------------
loss_ratios <- df %>% 
  dplyr::select(Year, `Incurred Claims`, `Net Earned Premium Income`) %>% 
  transmute(
    Year = Year, 
    `Loss Ratios` = `Incurred Claims` / `Net Earned Premium Income`
  )
loss_ratios_f <- loss_ratios %>% 
  mutate(`Loss Ratios` = {`Loss Ratios` * 100} %>% round(digits = 1) %>% paste0("%"))


# Visualization of the loss ratios:

# point labels:
labels <- {loss_ratios$`Loss Ratios` * 100} %>% round(digits = 1) %>% paste0("%")

# windows()
viz3 <- loss_ratios %>% 
  ggplot(mapping = aes(x = Year, y = `Loss Ratios`, group = 1)) + 
  geom_line(size = 1.5, color = "#0099f9") + 
  geom_point(size = 5, color = "#0099f9", shape = 19) + 
  geom_text(mapping = aes(label = labels), vjust = -0.8, hjust = 0.9) + 
  coord_cartesian(ylim = c(0.5, 0.8)) + 
  ggtitle(expression(bold(underline("Loss Ratios")))) + 
  ylab("%") + xlab(expression(italic("Year"))) + 
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) + 
  theme(
    aspect.ratio = 0.65, 
    plot.title = element_text(hjust = 0.5), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.line = element_line(), 
    axis.text.y = element_text()
  )
# viz3

# -------------------------------------------------------------------
# Combined Ratios
# -------------------------------------------------------------------
comb_ratios <- df %>% 
  transmute(
    Year = Year, 
    `Combined Ratio` = Combined / `Net Earned Premium Income`, 
    `Investment Income Ratio` = `Investment Income` / `Net Earned Premium Income`, 
    `Operating Ratio` = `Combined Ratio` - `Investment Income Ratio`
  )

# format the table to display:
add_percent <- function(x) {x * 100} %>% round(digits = 1) %>% paste0("%")
comb_ratios_f <- comb_ratios %>% 
  mutate(
    `Combined Ratio` = `Combined Ratio` %>% add_percent(), 
    `Investment Income Ratio` = `Investment Income Ratio` %>% add_percent(), 
    `Operating Ratio` = `Operating Ratio` %>% add_percent()
  )


# Visualizations:
# windows()
viz4 <- comb_ratios %>% melt(id = "Year") %>% 
  ggplot(mapping = aes(x = Year, y = value, color = variable, group = variable)) + 
  geom_line(size = 1.5) + 
  geom_point(size = 3, shape = 19) + 
  geom_text(mapping = aes(label = {value * 100} %>% round(digits = 1) %>% paste0("%")), 
            vjust = -0.6, size = 4, colour = "black") + 
  ggtitle(expression(bold(underline("Performance Ratios")))) + 
  ylab("%") + xlab(expression(italic("Year"))) + 
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) + 
  theme(
    aspect.ratio = 0.65, 
    legend.position = "bottom", 
    legend.title = element_blank(), 
    plot.title = element_text(hjust = 0.5), 
    axis.ticks.x = element_blank(), 
    panel.background = element_blank(), 
    axis.line = element_line(), 
    legend.text = element_text(face = "bold")
  )
# viz4

# ------------------------------------------------------------------------
# Commission vs Expense Ratio
# ------------------------------------------------------------------------
com_exp <- df %>% 
  transmute(
    Year = Year, 
    `Net Commission Ratio` = `Net Commissions` / `Net Earned Premium Income`, 
    `Management Expense Ratio` = `Expense of Management` / `Net Earned Premium Income`
  )
com_exp_f <- com_exp %>% 
  mutate(
    `Net Commission Ratio` = `Net Commission Ratio` %>% add_percent(), 
    `Management Expense Ratio` = `Management Expense Ratio` %>% add_percent()
  )

# windows()
viz5 <- com_exp %>% melt(id = "Year") %>% 
  ggplot(mapping = aes(x = Year, y = value, group = variable, color = variable)) + 
  geom_line(size = 1.5) + 
  geom_point(mapping = aes(shape = variable, fill = variable), size = 3) + 
  geom_text(mapping = aes(label = {value * 100} %>% round(digits = 1) %>% paste0("%")), 
            vjust = -0.6, color = "black") + 
  scale_shape_manual(values = c(19, 22)) + 
  ggtitle(expression(bold(underline("Net Commission Ratio vs Management Expense Ratio")))) + 
  xlab(expression(italic("Year"))) + ylab("%") + 
  coord_cartesian(ylim = c(0, 0.5)) + 
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) + 
  theme(
    aspect.ratio = 0.7, 
    legend.position = "bottom", 
    legend.title = element_blank(), 
    plot.title = element_text(hjust = 0.5), 
    axis.ticks.x = element_blank(), 
    panel.grid.major = element_blank(), 
    legend.text = element_text(face = "bold")
  )
# viz5

# -----------------------------------------------------------------------
# Underwriting Profit Ratio
# -----------------------------------------------------------------------
profit_ratio <- df %>% 
  select(`Underwriting Profit(Loss)`, `Net Earned Premium Income`) %>% {. / 1e6} %>%  
  mutate(
    `Profit Ratio` = `Underwriting Profit(Loss)` / `Net Earned Premium Income`, 
    Year = df$Year
  ) %>% relocate(Year) %>% select(-`Net Earned Premium Income`)
profit_ratio_f <- profit_ratio %>% 
  mutate(
    `Underwriting Profit(Loss)` = `Underwriting Profit(Loss)` %>% round(digits = 1), 
    `Profit Ratio` = `Profit Ratio` %>% add_percent()
  )


# windows()
coeff <- 40
viz6 <- profit_ratio %>% 
  ggplot(mapping = aes(x = Year)) + 
  geom_col(mapping = aes(y = `Underwriting Profit(Loss)`, fill = "#ff7c7c"),
           width = 0.7) + 
  geom_text(mapping = aes(y = -3, 
                          label = `Underwriting Profit(Loss)` %>% round(digits = 1)), 
            vjust = -10, color = "black", size = 4.5, fontface = "bold") + 
  geom_line(mapping = aes(y = `Profit Ratio` * coeff, group = 1, col = "#607c3c"), #color = linecol, 
            size = 1.3) + 
  geom_point(mapping = aes(y = `Profit Ratio` * coeff), color = "#607c3c", size = 3) + 
  geom_text(mapping = aes(y = `Profit Ratio` * coeff, 
                          label = {`Profit Ratio` * 100} %>% 
                            round(digits = 1) %>% paste0("%")), 
            vjust = -1.3, hjust = 0.2, color = "#607c3c", fontface = "bold") + 
  scale_y_continuous(
    name = expression(italic("Amount Ksh (Billions)")), 
    sec.axis = sec_axis(~.*coeff, name = "%", labels = function(x) x / 20 * 1.5)
  ) + 
  scale_color_identity(
    name = "", 
    labels = c("Profit Ratio"), 
    guide = "legend") + 
  scale_fill_identity(
    name = "", 
    labels = c("Underwriting Profit(Loss)"), 
    guide = "legend") + 
  coord_cartesian(ylim = c(-8, 0))  +
  ggtitle(expression(bold(underline("Underwriting Profit(Loss)")))) + 
  labs(color = '', fill = "") +
  theme(
    aspect.ratio = 0.9, 
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(), 
    legend.position = "bottom", 
    legend.text = element_text(face = "bold")
  )
# viz6



