################
##### RQ3 #####
################

library(arrow)
library(tidyverse)
library(jsonlite)
library(fs)

# =========================
# 0. output directory
# =========================

dir_create("outputs/figures")
dir_create("outputs/tables")

result_root <- Sys.getenv("MOBILITY_RESULTS_ROOT", unset = "aws_results")

# =========================
# 1. paths
# =========================

gu_original_rq3_dir <- file.path(result_root, "gu_original_rq3")
gu_geometry_rq3_dir <- file.path(result_root, "gu_geometry_rq3")

# =========================
# 2. read files
# =========================

probe_orig <- read_parquet(
  file.path(gu_original_rq3_dir, "rq3", "rq3_distance_probe.parquet")
)

probe_geom <- read_parquet(
  file.path(gu_geometry_rq3_dir, "rq3", "rq3_distance_probe.parquet")
)

align_orig <- read_parquet(
  file.path(gu_original_rq3_dir, "rq3", "rq3_role_alignment.parquet")
)

align_geom <- read_parquet(
  file.path(gu_geometry_rq3_dir, "rq3", "rq3_role_alignment.parquet")
)

summary_orig <- fromJSON(
  file.path(gu_original_rq3_dir, "rq3", "rq3_role_alignment_summary.json")
)

summary_geom <- fromJSON(
  file.path(gu_geometry_rq3_dir, "rq3", "rq3_role_alignment_summary.json")
)

# =========================
# 3. common labels
# =========================

variant_levels <- c("original", "geometry")

variant_labels <- c(
  original = "Name condition",
  geometry = "Coordinate condition"
)

# =========================
# 4. tidy tables
# =========================

# 4.1 Distance probe
rq3_probe <- bind_rows(
  probe_orig |> mutate(variant = "original"),
  probe_geom |> mutate(variant = "geometry")
) |>
  filter(metric_name == "accuracy") |>
  rename(probe_accuracy = metric_value) |>
  mutate(
    variant = factor(variant, levels = variant_levels),
    variant_label = recode(as.character(variant), !!!variant_labels),
    variant_label = factor(variant_label, levels = c("Name condition", "Coordinate condition"))
  )

# 4.2 Role alignment / association
rq3_align <- bind_rows(
  align_orig |> mutate(variant = "original"),
  align_geom |> mutate(variant = "geometry")
) |>
  mutate(
    variant = factor(variant, levels = variant_levels),
    variant_label = recode(as.character(variant), !!!variant_labels),
    variant_label = factor(variant_label, levels = c("Name condition", "Coordinate condition"))
  )


# =========================
# Figure 4-1
# 층별 거리 정보 탐지 정확도
# =========================

p_distance_probe <- ggplot(
  rq3_probe,
  aes(x = layer, y = probe_accuracy, color = variant_label)
) +
  geom_hline(
    yintercept = 0.2,
    linetype = "dashed",
    linewidth = 0.6,
    color = "grey30"
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_x_continuous(
    breaks = seq(0, 31, by = 5),
    minor_breaks = NULL
  ) +
  labs(
    x = "Layer",
    y = "Distance-bin probe accuracy",
    color = "Input condition"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(p_distance_probe)

ggsave(
  filename = "outputs/figures/figure_4_1_distance_probe.png",
  plot = p_distance_probe,
  width = 7,
  height = 4.5,
  dpi = 300
)


# =========================
# Figure 4-2
# 층별 출발지–도착지 역할 분리
# =========================
p_role_sep <- ggplot(
  rq3_align,
  aes(x = layer, y = role_sep_median_cos, color = variant_label)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_x_continuous(
    breaks = seq(0, 31, by = 5),
    minor_breaks = NULL
  ) +
  labs(
    x = "Layer",
    y = "Median cosine similarity",
    color = "Input condition"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(p_role_sep)

ggsave(
  filename = "outputs/figures/figure_4_2_role_conditioning.png",
  plot = p_role_sep,
  width = 7,
  height = 4.5,
  dpi = 300
)

# =========================
# Figure 4-3
# 표현 크기와 노드 수준 흐름의 연관성
# =========================

rq3_flow <- rq3_align |>
  pivot_longer(
    cols = c(role_assoc_out_spearman, role_assoc_in_spearman),
    names_to = "role",
    values_to = "correlation"
  ) |>
  mutate(
    role_label = case_when(
      role == "role_assoc_out_spearman" ~ "Origin role",
      role == "role_assoc_in_spearman" ~ "Destination role"
    ),
    role_label = factor(role_label, levels = c("Origin role", "Destination role"))
  )

p_flow <- ggplot(
  rq3_flow,
  aes(
    x = layer,
    y = correlation,
    color = variant_label,
    linetype = role_label,
    group = interaction(variant_label, role_label)
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  scale_x_continuous(
    breaks = seq(0, 31, by = 5),
    minor_breaks = NULL
  ) +
  labs(
    x = "Layer",
    y = "Spearman correlation",
    color = "Input condition",
    linetype = "Role"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(p_flow)

ggsave(
  filename = "outputs/figures/figure_4_3_flow_association.png",
  plot = p_flow,
  width = 7.5,
  height = 4.8,
  dpi = 300
)

# =========================
# Figure 4-4
# 표현 공간과 지리 공간의 정렬
# =========================
rq3_geo <- rq3_align |>
  pivot_longer(
    cols = c(align_out_spearman, align_in_spearman),
    names_to = "role",
    values_to = "alignment"
  ) |>
  mutate(
    role_label = case_when(
      role == "align_out_spearman" ~ "Origin role",
      role == "align_in_spearman" ~ "Destination role"
    ),
    role_label = factor(role_label, levels = c("Origin role", "Destination role"))
  )

p_geo <- ggplot(
  rq3_geo,
  aes(
    x = layer,
    y = alignment,
    color = variant_label,
    linetype = role_label,
    group = interaction(variant_label, role_label)
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  scale_x_continuous(
    breaks = seq(0, 31, by = 5),
    minor_breaks = NULL
  ) +
  labs(
    x = "Layer",
    y = "Spearman correlation",
    color = "Input condition",
    linetype = "Role"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(p_geo)

ggsave(
  filename = "outputs/figures/figure_4_4_geographic_alignment.png",
  plot = p_geo,
  width = 7.5,
  height = 4.8,
  dpi = 300
)

# =========================
# Supplementary table
# Best-layer summary used to inspect the layer-wise figures
# =========================

get_value_at_layer <- function(df, layer_value, col) {
  df |>
    filter(layer == layer_value) |>
    pull({{ col }}) |>
    as.numeric()
}

rq3_best_table <- bind_rows(
  tibble(
    variant = "original",
    variant_label = "Name condition",
    criterion = c(
      "Distance-bin probe accuracy",
      "Role separation",
      "Origin-flow association",
      "Destination-flow association",
      "Origin geographic alignment",
      "Destination geographic alignment"
    ),
    direction = c(
      "maximum",
      "minimum",
      "maximum",
      "maximum",
      "maximum",
      "maximum"
    ),
    best_layer = c(
      rq3_probe |> filter(variant == "original") |> slice_max(probe_accuracy, n = 1, with_ties = FALSE) |> pull(layer),
      summary_orig$best_layer_by_role_sep,
      summary_orig$best_layer_by_role_assoc_out,
      summary_orig$best_layer_by_role_assoc_in,
      summary_orig$best_layer_by_align_out,
      summary_orig$best_layer_by_align_in
    ),
    value = c(
      rq3_probe |> filter(variant == "original") |> slice_max(probe_accuracy, n = 1, with_ties = FALSE) |> pull(probe_accuracy),
      align_orig |> filter(layer == summary_orig$best_layer_by_role_sep) |> pull(role_sep_median_cos),
      align_orig |> filter(layer == summary_orig$best_layer_by_role_assoc_out) |> pull(role_assoc_out_spearman),
      align_orig |> filter(layer == summary_orig$best_layer_by_role_assoc_in) |> pull(role_assoc_in_spearman),
      align_orig |> filter(layer == summary_orig$best_layer_by_align_out) |> pull(align_out_spearman),
      align_orig |> filter(layer == summary_orig$best_layer_by_align_in) |> pull(align_in_spearman)
    )
  ),
  tibble(
    variant = "geometry",
    variant_label = "Coordinate condition",
    criterion = c(
      "Distance-bin probe accuracy",
      "Role separation",
      "Origin-flow association",
      "Destination-flow association",
      "Origin geographic alignment",
      "Destination geographic alignment"
    ),
    direction = c(
      "maximum",
      "minimum",
      "maximum",
      "maximum",
      "maximum",
      "maximum"
    ),
    best_layer = c(
      rq3_probe |> filter(variant == "geometry") |> slice_max(probe_accuracy, n = 1, with_ties = FALSE) |> pull(layer),
      summary_geom$best_layer_by_role_sep,
      summary_geom$best_layer_by_role_assoc_out,
      summary_geom$best_layer_by_role_assoc_in,
      summary_geom$best_layer_by_align_out,
      summary_geom$best_layer_by_align_in
    ),
    value = c(
      rq3_probe |> filter(variant == "geometry") |> slice_max(probe_accuracy, n = 1, with_ties = FALSE) |> pull(probe_accuracy),
      align_geom |> filter(layer == summary_geom$best_layer_by_role_sep) |> pull(role_sep_median_cos),
      align_geom |> filter(layer == summary_geom$best_layer_by_role_assoc_out) |> pull(role_assoc_out_spearman),
      align_geom |> filter(layer == summary_geom$best_layer_by_role_assoc_in) |> pull(role_assoc_in_spearman),
      align_geom |> filter(layer == summary_geom$best_layer_by_align_out) |> pull(align_out_spearman),
      align_geom |> filter(layer == summary_geom$best_layer_by_align_in) |> pull(align_in_spearman)
    )
  )
) |>
  mutate(
    value = round(value, 3)
  ) |>
  select(
    input_condition = variant_label,
    criterion,
    direction,
    best_layer,
    value
  )

rq3_best_table

write_csv(
  rq3_best_table,
  "outputs/tables/supplementary_rq3_best_layers.csv"
)
