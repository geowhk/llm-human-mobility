# Data inputs

No raw or processed mobility records are included in this repository.

## Source and study subset

The thesis used Seoul Living Mobility data from the Seoul Open Data Plaza. The analytical sample was constructed from November 2025 weekday observations and restricted to trips whose origins and destinations were both within Seoul. Within-zone movements were excluded.

- District scale: 25 districts, 600 directed OD pairs, 14,400 hourly observations.
- Neighborhood scale: 424 administrative neighborhoods, 179,352 directed OD pairs, 4,304,448 hourly observations.
- Values masked as three or fewer travelers in the source data were replaced with 1.5 as an operational preprocessing value.
- Unrecorded neighborhood-level origin-destination-hour combinations were represented as zero for the evaluation matrix. These zeros should not be interpreted as directly observed zero movements.

Source page: <https://data.seoul.go.kr/dataVisual/seoul/seoulLivingMigration.do>

## Expected processed files

The experiment configurations expect:

```text
data/processed/gt_flow_gu.parquet
data/processed/gt_flow_dong.parquet
data/external/code_to_name_gu.parquet
data/external/code_to_name_dong.parquet
```

The two flow tables require the following fields:

| Field | Meaning |
|---|---|
| `orig` | Origin administrative-area code |
| `dest` | Destination administrative-area code |
| `arrival_hour` | Arrival hour from 0 to 23 |
| `flow` | Constructed weekday-average mobility volume |
| `dist_km` | Centroid-to-centroid distance in kilometers |
| `orig_lat`, `orig_lon` | Origin centroid coordinates |
| `dest_lat`, `dest_lon` | Destination centroid coordinates |

The name-mapping tables require `code` and either `name` or `prompt_name`.

## Availability boundary

This repository deliberately excludes the source downloads, boundary files, processed OD tables, split assignments, and row-level predictions. Users must obtain source materials independently and review the provider's current terms before reproducing the preprocessing or analysis.
