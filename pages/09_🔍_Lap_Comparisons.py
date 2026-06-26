import streamlit as st
import pandas as pd
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(
    page_title="Triple Crown MX · Lap Comparisons",
    page_icon="🏍️",
    layout="wide",
    initial_sidebar_state="expanded",
)

df = load_data()

st.title("9. Lap Comparisons to Race Leader")
st.markdown("Per-lap time table with difference to the previous lap and to the leader's lap time")

all_years   = sorted(df["year"].astype(str).unique())
all_classes = ["450", "250", "WMX"]
all_rounds  = sorted(df["round"].astype(str).unique(), key=lambda x: int(x))
all_motos   = sorted(df["moto"].astype(str).unique(), key=lambda x: int(x))

# ── Race filters — rider list cascades from these ──────────────────────────────
c1, c2, c3, c4 = st.columns(4)
with c1:
    yr_sel  = st.selectbox("Year",  all_years,   index=len(all_years) - 1)
with c2:
    cls_sel = st.selectbox("Class", all_classes)
with c3:
    rnd_sel = st.selectbox("Round", all_rounds)
with c4:
    mto_sel = st.selectbox("Moto",  all_motos)

# Rider dropdown narrows to whoever raced in the selected moto
race_subset = df[
    (df["year"].astype(str)  == yr_sel) &
    (df["class_label"]       == cls_sel) &
    (df["round"].astype(str) == rnd_sel) &
    (df["moto"].astype(str)  == mto_sel)
]
eligible_riders = sorted(race_subset["name"].dropna().unique())
rider_sel = st.selectbox(
    "Rider",
    eligible_riders if eligible_riders else ["— no data —"],
)

if st.button("Update", type="primary") and eligible_riders:
    with st.spinner("Loading lap data..."):
        subset = race_subset.copy()

        # Leader lap times (place == 1)
        leader_laps = (
            subset[subset["place"] == 1]
            .dropna(subset=["lap", "lap_time"])
            [["lap", "lap_time"]]
            .rename(columns={"lap_time": "leader_lap_time"})
            .drop_duplicates(subset=["lap"])
        )
        leader_laps["lap"] = leader_laps["lap"].astype(float)

        # Selected rider laps
        rider_laps = (
            subset[subset["name"] == rider_sel]
            .dropna(subset=["lap", "lap_time"])
            [["lap", "lap_time", "place"]]
            .sort_values("lap")
            .copy()
        )
        rider_laps["lap"] = rider_laps["lap"].astype(float)

        if rider_laps.empty:
            st.warning(f"No lap time data for {rider_sel}.")
        else:
            rider_laps["diff_to_last"]  = rider_laps["lap_time"].diff()
            rider_laps = rider_laps.merge(leader_laps, on="lap", how="left")
            rider_laps["diff_to_leader"] = rider_laps["lap_time"] - rider_laps["leader_lap_time"]

            track = subset["track"].iloc[0]
            fp_series = subset[subset["name"] == rider_sel]["finish_position"].dropna()
            finish_pos = int(fp_series.iloc[0]) if not fp_series.empty else "N/A"

            st.markdown(
                f"**{rider_sel} — P{finish_pos} | {cls_sel} | {track} | "
                f"Round {rnd_sel} Moto {mto_sel}**"
            )

            result = rider_laps[["lap", "place", "lap_time", "diff_to_last", "diff_to_leader"]].copy()
            result["lap"]            = result["lap"].astype(int)
            result["place"]          = result["place"].astype("Int64")
            result["lap_time"]       = result["lap_time"].round(3)
            result["diff_to_last"]   = result["diff_to_last"].round(3)
            result["diff_to_leader"] = result["diff_to_leader"].round(3)
            result = result.rename(columns={
                "lap":             "Lap",
                "place":           "Place",
                "lap_time":        "Time (s)",
                "diff_to_last":    "Diff to Last Lap",
                "diff_to_leader":  "Diff to Leader Lap",
            }).reset_index(drop=True)

            # Colour-code the diff columns: green = faster, red = slower
            def colour_diff(val):
                if pd.isna(val):
                    return ""
                return "color: #1aad1a" if val < 0 else ("color: #cc3333" if val > 0 else "")

            styled = (
                result.style
                .map(colour_diff, subset=["Diff to Last Lap", "Diff to Leader Lap"])
                .format({
                    "Diff to Last Lap":   lambda v: f"{v:+.3f}" if pd.notna(v) else "—",
                    "Diff to Leader Lap": lambda v: f"{v:+.3f}" if pd.notna(v) else "—",
                    "Time (s)":           "{:.3f}",
                })
            )
            st.dataframe(styled, use_container_width=False, hide_index=True)

st.divider()
st.markdown("""
**Notes**
- "Diff to Last Lap" is this lap minus the previous lap for the same rider — negative means the rider went faster.
- "Diff to Leader Lap" is this rider's lap time minus the leader's lap time on the same lap number — negative means this rider was faster than the leader on that lap.
- The leader is whoever held 1st place on each lap. If the lead changed hands mid-moto, the reference time changes accordingly.
- Lap 1 diff to last lap is always blank (no prior lap to compare).
""")
