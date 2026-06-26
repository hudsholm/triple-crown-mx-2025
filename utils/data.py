import pandas as pd
import streamlit as st

NULLABLE_INT_COLS = ["lap", "place", "prev_place", "next_place"]
INT_COLS = ["year", "round", "moto", "finish_position", "points"]
FLOAT_COLS = [
    "lap_time", "behind_time", "place_change", "frac_completed",
    "frac_remaining", "z_score", "pct_off_best", "cumulative_time",
    "gap_ahead", "gap_behind",
]
CAT_COLS = ["race_id", "track", "manufacturer", "rider_state", "rider_country", "traffic_state"]


@st.cache_data
def load_data() -> pd.DataFrame:
    df = pd.read_csv(
        "tcmx_24-25_master_dataset.csv",
        dtype={"number": str, "class_label": str},
    )
    df["date"] = pd.to_datetime(df["date"])
    for col in INT_COLS:
        df[col] = df[col].astype("int64")
    for col in NULLABLE_INT_COLS:
        df[col] = pd.array(df[col], dtype="Int64")
    for col in FLOAT_COLS:
        df[col] = df[col].astype(float)
    for col in CAT_COLS:
        df[col] = df[col].astype("category")
    # Ensure consistent class ordering
    df["class_label"] = pd.Categorical(df["class_label"], categories=["450", "250", "WMX"], ordered=False)
    return df


@st.cache_data
def load_weather() -> pd.DataFrame:
    w = pd.read_csv("weather_data.csv")
    w["date"] = pd.to_datetime(w["date"])
    return w
