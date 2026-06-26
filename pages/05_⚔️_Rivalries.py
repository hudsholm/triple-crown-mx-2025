import streamlit as st
import pandas as pd
import numpy as np
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(
    page_title="Triple Crown MX · Rivalries",
    page_icon="🏍️",
    layout="wide",
    initial_sidebar_state="expanded",
)

df = load_data()

st.title("5. Rider Rivalries")
st.markdown("On-track proximity between consecutively-placed riders.")

@st.cache_data
def build_rivalry_base(_df):
    base = _df.dropna(subset=["lap", "place", "behind_time"]).copy()
    base["lap"]         = base["lap"].astype(float)
    base["place"]       = base["place"].astype(float)
    base["behind_time"] = base["behind_time"].astype(float)
    base["moto"]        = base["moto"].astype(str)
    base["year"]        = base["year"].astype(str)
    base["track"]       = base["track"].astype(str)
    base = base.sort_values(["race_id", "lap", "place"])

    ahead_lookup = (
        base[["race_id", "lap", "place", "name", "behind_time"]]
        .copy()
        .rename(columns={
            "name": "rider_ahead",
            "behind_time": "behind_time_ahead",
            "place": "place_ahead_lookup",
        })
    )

    base["place_ahead"] = base["place"] - 1
    laps = base.merge(
        ahead_lookup,
        left_on=["race_id", "lap", "place_ahead"],
        right_on=["race_id", "lap", "place_ahead_lookup"],
        how="inner"
    ).drop(columns=["place_ahead", "place_ahead_lookup"])

    laps["gap_to_ahead"] = laps["behind_time"] - laps["behind_time_ahead"]

    # Normalise pair alphabetically
    laps["rider_1"] = np.where(laps["name"] < laps["rider_ahead"], laps["name"], laps["rider_ahead"])
    laps["rider_2"] = np.where(laps["name"] < laps["rider_ahead"], laps["rider_ahead"], laps["name"])

    laps["within_1s"] = laps["gap_to_ahead"] < 1
    laps["within_2s"] = laps["gap_to_ahead"] < 2
    laps["within_5s"] = laps["gap_to_ahead"] < 5

    # Max consecutive streak within 2s per moto
    laps = laps.sort_values(["race_id", "rider_1", "rider_2", "lap"])

    def calc_max_streak(group):
        within = group["within_2s"].astype(int).values
        max_streak = streak = 0
        for v in within:
            if v:
                streak += 1
                max_streak = max(max_streak, streak)
            else:
                streak = 0
        return max_streak

    streak_df = (
        laps.groupby(["race_id", "rider_1", "rider_2"], observed=True)
        .apply(calc_max_streak, include_groups=False)
        .reset_index(name="streak")
    )
    max_streak = (
        streak_df.merge(
            base[["race_id", "class_label", "year", "track"]].drop_duplicates(),
            on="race_id", how="left"
        )
        .groupby(["rider_1", "rider_2", "class_label", "year", "track"], observed=True)
        ["streak"].max().reset_index(name="max_streak")
    )

    rivalry_agg = (
        laps.groupby(["rider_1", "rider_2", "class_label", "year", "track"], observed=True)
        .agg(
            laps_within_1s=("within_1s", "sum"),
            laps_within_2s=("within_2s", "sum"),
            laps_within_5s=("within_5s", "sum"),
        ).reset_index()
    )
    rivalry_agg = rivalry_agg.merge(
        max_streak, on=["rider_1", "rider_2", "class_label", "year", "track"], how="left"
    )
    rivalry_agg["max_streak"]      = rivalry_agg["max_streak"].fillna(0).astype(int)
    rivalry_agg["laps_within_1s"]  = rivalry_agg["laps_within_1s"].astype(int)
    rivalry_agg["laps_within_2s"]  = rivalry_agg["laps_within_2s"].astype(int)
    rivalry_agg["laps_within_5s"]  = rivalry_agg["laps_within_5s"].astype(int)
    return rivalry_agg

with st.spinner("Building rivalry data…"):
    rivalry_agg = build_rivalry_base(df)

all_years   = sorted(rivalry_agg["year"].astype(str).unique())
all_classes = ["450", "250", "WMX"]
all_tracks  = sorted(rivalry_agg["track"].astype(str).unique())

# ── Filters ────────────────────────────────────────────────────────────────────
col1, col2, col3 = st.columns(3)
with col1:
    yr_sel  = st.multiselect("Year",  all_years,   all_years)
with col2:
    cls_sel = st.multiselect("Class", all_classes, all_classes)
with col3:
    trk_sel = st.multiselect("Track", all_tracks,  all_tracks)

col4, col5 = st.columns(2)
with col4:
    sort_col = st.selectbox("Sort by", [
        ("Laps within 1s",         "laps_within_1s"),
        ("Laps within 2s",         "laps_within_2s"),
        ("Laps within 5s",         "laps_within_5s"),
        ("Max Streak (within 2s)", "max_streak"),
    ], format_func=lambda x: x[0], index=1)
with col5:
    top_n = st.slider("Top N", min_value=5, max_value=50, value=20, step=5)

if st.button("Update", type="primary"):
    filtered = rivalry_agg[
        rivalry_agg["year"].astype(str).isin(yr_sel) &
        rivalry_agg["class_label"].isin(cls_sel) &
        rivalry_agg["track"].astype(str).isin(trk_sel)
    ]

    filtered_agg = (
        filtered.groupby(["rider_1", "rider_2", "class_label"], observed=True)
        .agg(
            laps_within_1s=("laps_within_1s", "sum"),
            laps_within_2s=("laps_within_2s", "sum"),
            laps_within_5s=("laps_within_5s", "sum"),
            max_streak=("max_streak", "max"),
        ).reset_index()
    )
    filtered_agg = filtered_agg[filtered_agg["laps_within_5s"] >= 10].copy()

    if filtered_agg.empty:
        st.warning("No rivalries found for selected filters.")
    else:
        sort_key = sort_col[1]
        result = (
            filtered_agg
            .sort_values(sort_key, ascending=False)
            .head(top_n)
            .reset_index(drop=True)
            .rename(columns={
                "rider_1": "Rider 1", "rider_2": "Rider 2", "class_label": "Class",
                "laps_within_1s": "Laps < 1s", "laps_within_2s": "Laps < 2s",
                "laps_within_5s": "Laps < 5s", "max_streak": "Max Streak (< 2s)",
            })
        )
        result.index += 1
        st.dataframe(
            result[["Rider 1", "Rider 2", "Class", "Laps < 1s", "Laps < 2s", "Laps < 5s", "Max Streak (< 2s)"]],
            use_container_width=False,
        )

st.divider()
st.markdown("""
**Notes**
- "Rivalry" here means on-track proximity, not off-track narrative. Pairs top the list because they were consistently fast and near each other — not necessarily because of any specific battle.
- Gaps are computed only between consecutively-placed riders. A third rider sandwiched between two rivals means those laps don't count toward the pair's totals.
- Pairs are normalised alphabetically — A vs B and B vs A are the same pair regardless of who led.
- "Max streak" is the longest consecutive run of laps within 2s in a single moto, taken as the max across all matching motos.
- Pairs must have ≥10 laps within 5s across the filter to appear, filtering out one-off close encounters.
- WMX has shorter motos and smaller fields — absolute lap counts are lower than 450/250 by structure.
""")
