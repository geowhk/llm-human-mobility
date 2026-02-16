library(tidyverse)
library(sf)
library(tmap)

##### GU data preprocess #####
files_gu <- list.files(
    path = "data/raw_data/raw_flow(gu)",
    full.names = TRUE
)
length(files_gu) # 24(hours)


flow_gu_raw <- files_gu |> 
  map_dfr(~ read_csv(.x, 
    locale = locale(encoding = "CP949"),
    col_types = cols(.default = col_character())
))

flow_gu <- flow_gu_raw |> 
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
    flow = mean(flow),
    .groups = "drop"
  )

glimpse(flow_gu)
summary(gflow_gu)
hist(flow_gu$flow)


write.csv(flow_gu, "data/processed/flow_gu.csv", row.names = FALSE)


##### DONG data preprocess #####
read_dong_flow <- function(time) {
  file_path <- paste0(
    "data/raw_data/raw_flow(dong)/생활이동_행정동_2025.11_",
    sprintf("%02d", time),
    "시.csv"
  )

  read_csv(
    file_path,
    locale = locale(encoding = "CP949"),
    col_types = cols(.default = col_character())
  ) |>
    select(요일, 도착시간, `출발 행정동 코드`, `도착 행정동 코드`, `이동인구(합)`) |>
    rename(
      days = 요일,
      arrival_hour = 도착시간,
      orig = `출발 행정동 코드`,
      dest = `도착 행정동 코드`,
      flow = `이동인구(합)`
    ) |>
    filter(str_starts(orig, "11")) |>
    filter(str_starts(dest, "11")) |>
    filter(orig != dest) |>
    mutate(flow = ifelse(flow == "*", "1.5", flow)) |>
    mutate(
      arrival_hour = as.numeric(arrival_hour),
      flow = as.numeric(flow)
    ) |>
    filter(days %in% c("월", "화", "수", "목", "금")) |>
    group_by(orig, dest, arrival_hour) |>
    summarize(
      flow = mean(flow),
      .groups = "drop"
    )
}

flow_dong <- lapply(0:23, read_dong_flow) |> 
  bind_rows()

length(unique(flow_dong$orig))
length(unique(flow_dong$dest))

write.csv(flow_dong, "data/processed/flow_dong.csv", row.names = FALSE)


##### shapefile #####

flow_gu <- read_csv("data/processed/flow_gu.csv") |> 
  mutate(
    orig = as.character(orig),
    dest = as.character(dest)
  )
flow_dong <- read_csv("data/processed/flow_dong.csv") |> 
  mutate(
    orig = as.character(orig),
    dest = as.character(dest)
  )

shp_gu <- st_read("data/shapefile/gu/SEOUL_GU_2019_2Q_GEN_0020.shp") |> 
  mutate(SGG1_CD = as.character(SGG1_CD))
qtm(shp_gu)

shp_dong <- st_read("data/shapefile/dong/SEOUL_EMD_2019_2Q_GEN_0020.shp")
qtm(shp_dong)

# check GU code
unique(shp_gu$SGG1_CD) == unique(flow_gu$orig) # all TRUE
unique(shp_gu$SGG1_CD) == sort(unique(flow_gu$dest)) # all TRUE

# complete GU ground truth flow
gu_codes <- sort(unique(shp_gu$SGG1_CD))

gu_centroids <- shp_gu |>
  st_transform(5179) |>
  st_centroid()

gu_xy <- tibble(
  gu_code = gu_centroids$SGG1_CD,
  x = st_coordinates(gu_centroids)[, 1],
  y = st_coordinates(gu_centroids)[, 2]
)

gu_distance <- crossing(
  orig = gu_codes,
  dest = gu_codes
) |>
  filter(orig != dest) |>
  left_join(
    gu_xy |>
      rename(orig = gu_code, orig_x = x, orig_y = y),
    by = "orig"
  ) |>
  left_join(
    gu_xy |>
      rename(dest = gu_code, dest_x = x, dest_y = y),
    by = "dest"
  ) |>
  mutate(dist_km = (sqrt((orig_x - dest_x)^2 + (orig_y - dest_y)^2)) / 1000) |>
  select(orig, dest, dist_km)

gt_flow_gu <- flow_gu |> 
  left_join(gu_distance, by = c("orig", "dest"))

write_csv(gt_flow_gu, "data/processed/gt_flow_gu.csv")

# check DONG code
length(unique(flow_dong$orig)) # 424
length(unique(shp_dong$EMD_ID)) # 424

unique(shp_dong$EMD_ID) == unique(flow_dong$orig) # all TRUE
unique(shp_dong$EMD_ID) == sort(unique(flow_dong$dest)) # all TRUE

# complete DONG ground truth flow
dong_codes <- sort(unique(shp_dong$EMD_ID))

full_grid <- crossing(
  orig = dong_codes,
  dest = dong_codes,
  arrival_hour = 0:23
) |>
  filter(orig != dest)

flow_dong_full <- full_grid |> 
  left_join(flow_dong, by = c("orig", "dest", "arrival_hour")) |>
  mutate(flow = ifelse(is.na(flow), 0, flow))

dong_centroids <- shp_dong |>
  st_transform(5179) |>
  st_centroid()

dong_xy <- tibble(
  dong_code = dong_centroids$EMD_ID,
  x = st_coordinates(dong_centroids)[, 1],
  y = st_coordinates(dong_centroids)[, 2]
)

dong_distance <- crossing(
  orig = dong_codes,
  dest = dong_codes
) |>
  filter(orig != dest) |>
  left_join(
    dong_xy |>
      rename(orig = dong_code, orig_x = x, orig_y = y),
    by = "orig"
  ) |>
  left_join(
    dong_xy |>
      rename(dest = dong_code, dest_x = x, dest_y = y),
    by = "dest"
  ) |>
  mutate(dist_km = (sqrt((orig_x - dest_x)^2 + (orig_y - dest_y)^2)) / 1000) |>
  select(orig, dest, dist_km)

gt_flow_dong <- flow_dong |> 
  left_join(dong_distance, by = c("orig", "dest")) |> 
  arrange(orig, dest)

write_csv(gt_flow_dong, "data/processed/gt_flow_dong.csv")
