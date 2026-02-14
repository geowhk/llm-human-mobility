library(tidyverse)

files <- list.files(
    path = "raw_flow(2024.11)",
    full.names = TRUE
)
length(files)


flow_all <- files |> 
  map_dfr(~ read_csv(.x, 
    locale = locale(encoding = "CP949"),
    col_types = cols(.default = col_character())
))

test_flow <- flow_all |> 
  select(요일, 도착시간, `출발 시군구 코드`, `도착 시군구 코드`, `이동인구(합)`) |> 
  rename(
    days = 요일,
    arrival_hour = 도착시간,
    orig = `출발 시군구 코드`,
    dest = `도착 시군구 코드`,
    flow = `이동인구(합)`
  ) |> 
  filter(str_starts(orig, "11")) |> 
  filter(str_starts(dest, "11")) |>
  filter(orig != dest) |> 
  mutate(flow = ifelse(flow == "*", "1.5", flow)) |>
  mutate(
    arrival_hour = as.numeric(arrival_hour),
    flow = as.numeric(flow),
) |> 
  filter(days %in% c("월", "화", "수", "목", "금")) |> 
  group_by(orig, dest, arrival_hour) |> 
  summarize(
    flow = mean(flow)
  )

glimpse(test_flow)
summary(test_flow)
hist(test_flow$flow)


write.csv(test_flow, "test_flow.csv")
