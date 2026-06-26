import streamlit as st
import pandas as pd
import numpy as np
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(
    page_title="Triple Crown MX · Quartile Places",
    page_icon="🏍️",
    layout="wide",
    initial_sidebar_state="expanded",
)

df = load_data()

st.title("10. Place by Moto Quartile")
st.markdown("Position snapshots at 0%, 25%, 50%, 75%, and 100% of each moto — per rider and season averages")

@st.cache_data
def build_quartile_df(_df):
    base = _df.dropna(subset=["lap", "place"]).copy()
    base["lap"]   = base["lap"].astype(float)
    base["place"] = base["place"].astype(float)
    base["year"]  = base["year"].astype(str)
    base["round"] = base["round"].astype(str)
    base["moto"]  = base["moto"].astype(str)

    max_lap = (
        base.groupby("race_id", observed=True)["lap"]
        .max().rename("max_lap").reset_index()
    )
    race_laps = max_lap.copy()
    race_laps["lap_start"] = 1.0
    race_laps["lap_25"] = (race_laps["max_lap"] * 0.25 + 0.5).astype(int).astype(float)
    race_laps["lap_50"] = (race_laps["max_lap"] * 0.50 + 0.5).astype(int).astype(float)
    race_laps["lap_75"] = (race_laps["max_lap"] * 0.75 + 0.5).astype(int).astype(float)
    base = base.merge(
        race_laps[["race_id", "lap_start", "lap_25", "lap_50", "lap_75"]],
        on="race_id", how="left"
    )

    def extract_quartile(b, lap_col, out_col):
        return (
            b[b["lap"] == b[lap_col]]
            .groupby(["race_id", "name"], observed=True)["place"]
            .first().reset_index(name=out_col)
        )

    q = extract_quartile(base, "lap_start", "start")
    for col, name in [("lap_25", "q25"), ("lap_50", "q50"), ("lap_75", "q75")]:
        q = q.merge(extract_quartile(base, col, name), on=["race_id", "name"], how="outer")

    q = q.merge(
        _df.drop_duplicates(subset=["race_id", "name"])[[
            "race_id", "name", "finish_position",
            "class_label", "year", "round", "moto", "track"
        ]],
        on=["race_id", "name"], how="left"
    )
    q["year"]  = q["year"].astype(str)
    q["round"] = q["round"].astype(str)
    q["moto"]  = q["moto"].astype(str)
    return q

quartile_df = build_quartile_df(df)

all_years   = sorted(quartile_df["year"].unique())
all_classes = ["450", "250", "WMX"]

# ═══════════════════════════════════════════════════════════════════════════════
# 10a. Per-rider moto-by-moto view
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Per-rider quartile positions")

c1, c2 = st.columns(2)
with c1:
    yr_r  = st.selectbox("Year",  all_years,   index=len(all_years) - 1, key="q_yr_r")
with c2:
    cls_r = st.selectbox("Class", all_classes, key="q_cls_r")

eligible = sorted(
    quartile_df[
        (quartile_df["year"] == yr_r) &
        (quartile_df["class_label"] == cls_r)
    ]["name"].dropna().unique()
)
rider_r = st.selectbox("Rider", eligible if eligible else ["— no data —"], key="q_rdr_r")

if st.button("Update — rider view", type="primary", key="q_btn_r") and eligible:
    subset = quartile_df[
        (quartile_df["year"] == yr_r) &
        (quartile_df["class_label"] == cls_r) &
        (quartile_df["name"] == rider_r)
    ].copy().sort_values(["round", "moto"], key=lambda x: x.astype(int))

    if subset.empty:
        st.warning("No data for selected filters.")
    else:
        result = subset[["round", "moto", "track", "start", "q25", "q50", "q75", "finish_position"]].copy()
        for col in ["start", "q25", "q50", "q75", "finish_position"]:
            result[col] = result[col].astype("Int64")
        result = result.rename(columns={
            "round": "Round", "moto": "Moto", "track": "Track",
            "start": "Start", "q25": "25%", "q50": "50%",
            "q75": "75%", "finish_position": "Finish",
        }).reset_index(drop=True)
        result.index += 1
        st.markdown(f"**{rider_r} — {cls_r} | {yr_r}**")
        st.dataframe(result, use_container_width=False)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 10b. Season average quartile table
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Season average quartile positions")

@st.cache_data
def build_season_quartiles(_q):
    sq = (
        _q.groupby(["name", "class_label", "year"], observed=True)
        .agg(
            motos=("race_id", "count"),
            avg_start=("start", "mean"),
            avg_q25=("q25", "mean"),
            avg_q50=("q50", "mean"),
            avg_q75=("q75", "mean"),
            avg_finish=("finish_position", "mean"),
        ).reset_index()
    )
    for col in ["avg_start", "avg_q25", "avg_q50", "avg_q75", "avg_finish"]:
        sq[col] = sq[col].round(1)
    sq["year"] = sq["year"].astype(str)
    return sq

season_q = build_season_quartiles(quartile_df)

SORT_OPTIONS = [
    ("Avg Finish",  "avg_finish", True),
    ("Avg Start",   "avg_start",  True),
    ("Avg 25%",     "avg_q25",    True),
    ("Avg 50%",     "avg_q50",    True),
    ("Avg 75%",     "avg_q75",    True),
    ("Total Motos", "motos",      False),
]

c1, c2, c3, c4 = st.columns(4)
with c1:
    yr_s   = st.selectbox("Year",    all_years,   index=len(all_years) - 1, key="q_yr_s")
with c2:
    cls_s  = st.selectbox("Class",   all_classes, key="q_cls_s")
with c3:
    top_s  = st.slider("Top N", 5, 50, 20, 5, key="q_n_s")
with c4:
    sort_s = st.selectbox("Sort by", SORT_OPTIONS, format_func=lambda x: x[0], key="q_sort_s")

if st.button("Update — season averages", type="primary", key="q_btn_s"):
    subset = season_q[
        (season_q["year"] == yr_s) &
        (season_q["class_label"] == cls_s)
    ]
    if subset.empty:
        st.warning("No data for selected filters.")
    else:
        col, asc = sort_s[1], sort_s[2]
        result = (
            subset.sort_values(col, ascending=asc, na_position="last")
            .head(top_s).reset_index(drop=True)
            .rename(columns={
                "name": "Rider", "motos": "Motos",
                "avg_start": "Avg Start", "avg_q25": "Avg 25%",
                "avg_q50": "Avg 50%", "avg_q75": "Avg 75%",
                "avg_finish": "Avg Finish",
            })
        )
        result.index += 1
        st.dataframe(
            result[["Rider", "Motos", "Avg Start", "Avg 25%", "Avg 50%", "Avg 75%", "Avg Finish"]],
            use_container_width=False,
        )

st.divider()
st.markdown("""
**Notes**
- "Start" is position at the end of lap 1, not gate position — lap 1 finish is the closest proxy available.
- Quartile checkpoints (25%, 50%, 75%) are calculated per moto based on that moto's total lap count, rounded to the nearest whole lap. A 12-lap moto's 25% mark is lap 3; an 18-lap moto's is lap 5.
- Riders who DNF'd before reaching a checkpoint show `<NA>` for that column.
- Season averages exclude `<NA>` values, so the denominator differs column-by-column for riders with DNFs.
- DNF-prone riders can look artificially better at later checkpoints — the bad late-race laps that triggered the DNF aren't in the data.
""")
