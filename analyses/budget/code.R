library(dplyr)
library(gsheet)
library(databrew)
library(ggplot2)
url <- 'https://docs.google.com/spreadsheets/d/1Rf12eBUL4-NnwLtJTm8n0v3_cfWN8_b7nNdkR37IzCA/edit#gid=1700791765'

df <- gsheet2tbl(url)
out <- df[,c(1,8:10)]
databrew::prettify(out, nrows = nrow(out))

# # Get the narrower confidence bounds
# out$lwrn <- (out$`USD (LWR)` + out$USD) / 2
# out$uprn <- (out$`USD (UPR)` + out$USD) / 2


# Example plot

# grand total
gt <- out[nrow(out), ((ncol(out)-2):ncol(out))]
names(gt) <- c('lwr', 'pt', 'upr')
# Get monthly
monthly <- gt / 8

# Make dataframe
ts <- tibble(date = seq(as.Date('2021-05-15'), as.Date('2022-12-15'), by = 'month'))
ts$n <- 1:nrow(ts)
ts$lwr <- ts$n * monthly$lwr
ts$pt <- ts$n * monthly$pt
ts$upr <- ts$n * monthly$upr

options(scipen = '999')
ts <- ts %>% filter(date <= '2021-12-31')

# Adjust for narrowwer bounds
ts$lwr <- (ts$lwr + ts$pt) / 2
ts$upr <- (ts$upr + ts$pt) / 2

# Example of actual expenditure
real <- c(10679.72, 16614.72)
real <- cumsum(real)
real <- c(real, rep(NA, 8-length(real)))
ts$real <- real  #c(20000, 24100, 38000, 66000, 69000, 78000, NA, NA) * 1.3

# ts$real <- cumsum(rep(15599.72, nrow(ts))) - 4920
ggplot(data = ts) +
  geom_ribbon(aes(x = date,
                  ymin = lwr,
                  ymax = upr),
              fill = 'darkorange',
              alpha = 0.6) +
  theme_bw() +
  geom_line(aes(x = date,
                y = pt)) +
  geom_line(aes(x = date,
                y = real),
            color = 'red') +
  geom_point(aes(x = date,
                 y = real),
             color = 'red') +
  labs(x = 'Month', y = 'Cumulative USD',
       title = 'Actual vs projected expenditures',
       subtitle = 'May 1 2021 onwards') +
  scale_y_continuous(breaks = seq(0, max(ts$upr), by = 20000)) +
  scale_x_date(name = 'Month',
               breaks = sort(unique(ts$date)),
               labels = format(sort(unique(ts$date)), '%m'))

pd <- tibble(label = c('Lower (extreme)',
                       'Lower (realistic)',
                       'Point estimate',
                       'Upper (realistic)',
                       'Upper (extreme)'),
             val = c(54640,
                     111600,
                     168560,
                     232920,
                     297280))
pd$label <- factor(pd$label, levels = pd$label)

ggplot(data = pd,
       aes(x = label,
           y = val)) +
  geom_point(size = 5) +
  theme_bw() +
  labs(x = 'Scenario',
       y = 'USD') +
  ylim(0, 1.1*max(pd$val)) 

pd$monthly <- pd$val / 8
topper <- 168560
lwr <- 111600
upr <- 232920
pd$months <- topper / pd$monthly
pd$date <- as.Date('2021-05-01') + (30.25 * pd$months)

pd$months_lwr <- lwr / pd$monthly
pd$date_lwr <- as.Date('2021-05-01') + (30.25 * pd$months_lwr)

pd$months_upr <- upr / pd$monthly
pd$date_upr <- as.Date('2021-05-01') + (30.25 * pd$months_upr)

lng <- pd %>%
  dplyr::select(label, date_lwr, date_upr, date) %>%
  tidyr::gather(key, value, date_lwr:date) %>%
  mutate(key = ifelse(key == 'date', 'Budget of 168,560',
                      ifelse(key == 'date_lwr', 'Budget of 111,600',
                             ifelse(key == 'date_upr', 'Budget of 232,920', NA))))

ggplot(data = lng,
       aes(x = label,
           y = value, 
           color = key)) +
  geom_point() +
  geom_line(aes(group = key)) +
  scale_color_manual(name = '', values = c('red', 'darkorange', 'blue')) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = 'Burn rate scenario',
       y = 'Date of budget depletion')
