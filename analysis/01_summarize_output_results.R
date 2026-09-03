library(tidyverse)
library(jsonlite)
library(fs)
library(arrow)

result_root <- Sys.getenv("MOBILITY_RESULTS_ROOT", unset = "aws_results")

# =========================
# 1. helper functions
# =========================

# 가장 최근 폴더 1개 찾기
latest_dir <- function(root, pattern) {
  dirs <- dir_ls(root, regexp = pattern, type = "directory")
  if (length(dirs) == 0) {
    stop("No directory matched pattern: ", pattern)
  }
  dirs <- sort(dirs)
  dirs[length(dirs)]
}

# metrics.json의 특정 block(gu / dong / all) 읽기
read_metrics_block <- function(path, block_name, model, family, scale) {
  x <- fromJSON(path, simplifyVector = TRUE)
  b <- x[[block_name]]

  tibble(
    model = model,
    family = family,
    scale = scale,
    n_eval = b$n_eval,
    mae = b$accuracy$mae,
    rmse = b$accuracy$rmse,
    smape = b$accuracy$smape,
    cpc = b$patterns$cpc,
    delta_beta = b$patterns$delta_beta,
    rho_origin = b$patterns$rho_origin,
    rho_destination = b$patterns$rho_destination,
    delta_gini = b$patterns$delta_gini
  )
}

# baseline subfolder별 metrics.json 읽기
read_baseline_metrics <- function(run_dir, scale) {
  baseline_names <- c("hourly_mean", "od_marginal_product", "loglinear_gravity")

  bind_rows(lapply(baseline_names, function(bn) {
    path <- file.path(run_dir, bn, "metrics.json")
    read_metrics_block(
      path = path,
      block_name = scale,
      model = bn,
      family = "baseline",
      scale = scale
    )
  }))
}

# =========================
# 2. locate latest result folders
# =========================

root <- result_root

gu_original_rq12_dir  <- latest_dir(root, "gu_original_rq12")
gu_geometry_rq12_dir  <- latest_dir(root, "gu_geometry_rq12")
gu_baseline_dir       <- latest_dir(root, "gu_rq12_baselines")

dong_original_dir     <- file.path(root, "original_dong")
dong_geometry_dir     <- file.path(root, "geometry_dong")
dong_baseline_dir     <- latest_dir(root, "dong_rq12_baselines")

# 확인용 출력
gu_original_rq12_dir
gu_geometry_rq12_dir
gu_baseline_dir
dong_original_dir
dong_geometry_dir
dong_baseline_dir

# =========================
# 3. read LLM RQ12 metrics
# =========================

rq12_llm_gu_original <- read_metrics_block(
  path = file.path(gu_original_rq12_dir, "rq12", "metrics.json"),
  block_name = "gu",
  model = "original",
  family = "llm",
  scale = "gu"
)

rq12_llm_gu_geometry <- read_metrics_block(
  path = file.path(gu_geometry_rq12_dir, "rq12", "metrics.json"),
  block_name = "gu",
  model = "geometry",
  family = "llm",
  scale = "gu"
)

rq12_llm_dong_original <- read_metrics_block(
  path = file.path(dong_original_dir, "dong_metrics_merged.json"),
  block_name = "dong",
  model = "original",
  family = "llm",
  scale = "dong"
)

rq12_llm_dong_geometry <- read_metrics_block(
  path = file.path(dong_geometry_dir, "dong_metrics_merged.json"),
  block_name = "dong",
  model = "geometry",
  family = "llm",
  scale = "dong"
)

# =========================
# 4. read baseline metrics
# =========================

rq12_baseline_gu <- read_baseline_metrics(gu_baseline_dir, "gu")
rq12_baseline_dong <- read_baseline_metrics(dong_baseline_dir, "dong")

# =========================
# 5. combine into one comparison table
# =========================

rq12_compare_all <- bind_rows(
  rq12_llm_gu_original,
  rq12_llm_gu_geometry,
  rq12_llm_dong_original,
  rq12_llm_dong_geometry,
  rq12_baseline_gu,
  rq12_baseline_dong
) |>
  mutate(
    model = factor(
      model,
      levels = c(
        "hourly_mean",
        "od_marginal_product",
        "loglinear_gravity",
        "geometry",
        "original"
      )
    ),
    scale = factor(scale, levels = c("gu", "dong"))
  ) |>
  arrange(scale, model)

rq12_compare_all

rq12_table_wide <- rq12_compare_all |>
  select(scale, model, mae, rmse, smape, cpc, delta_beta, rho_origin, rho_destination, delta_gini) |>
  arrange(scale, model)

rq12_table_wide

rq12_table_long <- rq12_compare_all |>
  pivot_longer(
    cols = c(mae, rmse, smape, cpc, delta_beta, rho_origin, rho_destination, delta_gini),
    names_to = "metric",
    values_to = "value"
  )

rq12_table_long

# =======================
# 논문 시각화
# =======================

##### 공통 전처리

library(tidyverse)
library(scales)
library(fs)

dir_create("outputs/figures")
dir_create("outputs/tables")

metric_meta_kr <- tibble(
  metric = c(
    "mae", "rmse", "smape",
    "cpc", "delta_beta", "rho_origin", "rho_destination", "delta_gini"
  ),
  metric_label = c(
    "MAE ↓", "RMSE ↓", "sMAPE ↓",
    "CPC ↑", "Δβ ↓", "ρ_origin ↑", "ρ_destination ↑", "ΔG ↓"
  ),
  metric_group = c(
    "개별 이동량 오차", "개별 이동량 오차", "개별 이동량 오차",
    "공간 패턴 재현", "공간 패턴 재현", "공간 패턴 재현", "공간 패턴 재현", "공간 패턴 재현"
  ),
  direction = c(
    "lower_better", "lower_better", "lower_better",
    "higher_better", "lower_better",
    "higher_better", "higher_better",
    "lower_better"
  )
)

scale_meta_kr <- tibble(
  scale = factor(c("gu", "dong"), levels = c("gu", "dong")),
  scale_label = c("구", "동")
)



rq12_long_kr <- rq12_table_long |>
  left_join(metric_meta_kr, by = "metric") |>
  left_join(scale_meta_kr, by = "scale") |>
  mutate(
    metric_label = factor(
      metric_label,
      levels = c(
        "MAE ↓", "RMSE ↓", "sMAPE ↓",
        "CPC ↑", "Δβ ↓", "ρ_origin ↑", "ρ_destination ↑", "ΔG ↓"
      )
    ),
    metric_group = factor(
      metric_group,
      levels = c("개별 이동량 오차", "공간 패턴 재현")
    ),
    scale_label = factor(scale_label, levels = c("구", "동"))
  )

# =========================
# Table 4-1
# =========================
write_csv(
  rq12_table_wide,
  "outputs/tables/table4_1_rq12_metrics.csv"
)


# =========================
# Figure 4-1
# LLM original vs geometry
# direction-adjusted relative difference
# =========================

fig41_data <- rq12_long_kr |>
  filter(family == "llm") |>
  filter(model %in% c("original", "geometry")) |>
  select(scale, scale_label, metric, metric_label, metric_group, direction, model, value) |>
  pivot_wider(
    names_from = model,
    values_from = value
  ) |>
  mutate(
    denom = (abs(original) + abs(geometry)) / 2,
    relative_advantage_original = case_when(
      denom == 0 ~ NA_real_,
      direction == "lower_better"  ~ (geometry - original) / denom * 100,
      direction == "higher_better" ~ (original - geometry) / denom * 100
    ),
    label = case_when(
      is.na(relative_advantage_original) ~ "",
      abs(relative_advantage_original) < 0.05 ~ "0.0%",
      relative_advantage_original > 0 ~ paste0("+", round(relative_advantage_original, 1), "%"),
      TRUE ~ paste0(round(relative_advantage_original, 1), "%")
    )
  )

p41 <- ggplot(
  fig41_data,
  aes(
    x = scale_label,
    y = metric_label,
    fill = relative_advantage_original
  )
) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = label), size = 3.5) +
  facet_grid(metric_group ~ ., scales = "free_y", space = "free_y") +
  scale_fill_gradient2(
    low = "#3B6FB6",
    mid = "white",
    high = "#B64040",
    midpoint = 0,
    name = "지명 기반 우위\n(방향 보정, %)"
  ) +
  labs(
    x = "공간 단위",
    y = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(size = 11),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

print(p41)

ggsave(
  filename = "outputs/figures/supplementary_prompt_relative_difference_kr.png",
  plot = p41,
  width = 7.5,
  height = 5.8,
  dpi = 300
)

# =========================
# Figure 4-2(tentative)
# Model rank heatmap
# =========================

model_meta_kr <- tibble(
  model = factor(
    c("original", "geometry", "hourly_mean", "od_marginal_product", "loglinear_gravity"),
    levels = c("original", "geometry", "hourly_mean", "od_marginal_product", "loglinear_gravity")
  ),
  model_label = c(
    "지명 기반 LLM",
    "기하 기반 LLM",
    "시간대 평균 기준 모형",
    "주변부 구조 기반 모형",
    "로그-선형 중력형 모형"
  )
)

rq12_rank_data <- rq12_table_long |>
  left_join(metric_meta_kr, by = "metric") |>
  left_join(model_meta_kr, by = "model") |>
  left_join(scale_meta_kr, by = "scale") |>
  group_by(scale, metric) |>
  mutate(
    rank_value = case_when(
      direction == "lower_better"  ~ min_rank(value),
      direction == "higher_better" ~ min_rank(desc(value))
    )
  ) |>
  ungroup() |>
  mutate(
    metric_label = factor(
      metric_label,
      levels = c(
        "MAE ↓", "RMSE ↓", "sMAPE ↓",
        "CPC ↑", "Δβ ↓", "ρ_origin ↑", "ρ_destination ↑", "ΔG ↓"
      )
    ),
    # ggplot의 y축은 첫 level이 아래쪽에 오므로, 원하는 표시 순서의 역순으로 설정
    model_label = factor(
      model_label,
      levels = rev(c(
        "지명 기반 LLM",
        "기하 기반 LLM",
        "시간대 평균 기준 모형",
        "주변부 구조 기반 모형",
        "로그-선형 중력형 모형"
      ))
    ),
    scale_label = factor(scale_label, levels = c("구", "동")),
    rank_label = paste0("#", rank_value),
    value_label = case_when(
      metric %in% c("mae", "rmse") ~ sprintf("%.2f", value),
      TRUE ~ sprintf("%.3f", value)
    ),
    label = paste0(rank_label, "\n", value_label)
  )

p42 <- ggplot(
  rq12_rank_data,
  aes(
    x = metric_label,
    y = model_label,
    fill = rank_value
  )
) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = label), size = 3.0, lineheight = 0.9) +
  facet_wrap(~ scale_label, ncol = 1) +
  scale_fill_gradient(
    low = "#2C7BB6",
    high = "#FEE090",
    trans = "reverse",
    breaks = 1:5,
    name = "순위\n(1 = 최고)"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(size = 11),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

print(p42)

ggsave(
  filename = "outputs/figures/supplementary_model_rank_heatmap_kr.png",
  plot = p42,
  width = 9.5,
  height = 7,
  dpi = 300
)
