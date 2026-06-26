import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
from scipy import stats
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(
    page_title="Triple Crown MX · Fatigue",
    page_icon="🏍️",
    layout="wide",
    initial_sidebar_state="expanded",
)

df = load_data()

st.title("4. Fatigue Effects")
st.markdown("How does pace degrade through a moto, and from moto 1 to moto 2?")

# ── Pre-build fatigue base ─────────────────────────────────────────────────────
@st.cache_data
def build_fatigue_base(_df):
    base = _df.dropna(subset=["lap", "lap_time"]).copy()
    base["lap"] = base["lap"].astype(float)

    max_lap = (
        base.groupby("race_id", observed=True)["lap"]
        .max().rename("max_lap").reset_index()
    )
    base = base.merge(max_lap, on="race_id", how="left")
    base["threshold"] = (base["max_lap"] * 0.75).apply(np.floor)

    laps_comp = (
        base.groupby(["race_id", "name"], observed=True)["lap"]
        .max().rename("laps_completed").reset_index()
    )
    base = base.merge(laps_comp, on=["race_id", "name"], how="left")
    base = base[base["laps_completed"] >= base["threshold"]].copy()
    base = base[base["lap"] > 1].copy()

    personal_best = (
        base.groupby(["race_id", "name"], observed=True)["lap_time"]
        .min().rename("personal_best").reset_index()
    )
    base = base.merge(personal_best, on=["race_id", "name"], how="left")
    base["delta_pct"] = (
        (base["lap_time"] - base["personal_best"]) / base["personal_best"] * 100
    )
    base["pct_completed"] = base["lap"] / base["max_lap"] * 100

    bin_edges  = list(range(0, 101, 10))
    bin_labels = [str(b + 10) for b in range(0, 100, 10)]
    base["pct_bin"] = pd.cut(
        base["pct_completed"], bins=bin_edges,
        labels=bin_labels, include_lowest=True
    )
    return base

@st.cache_data
def build_moto_comp_base(_df):
    base = _df.dropna(subset=["lap", "lap_time"]).copy()
    base["lap"]  = base["lap"].astype(float)
    base["moto"] = base["moto"].astype(str)

    max_lap = (
        base.groupby("race_id", observed=True)["lap"]
        .max().rename("max_lap").reset_index()
    )
    base = base.merge(max_lap, on="race_id", how="left")
    base["threshold"] = (base["max_lap"] * 0.75).apply(np.floor)

    laps_comp = (
        base.groupby(["race_id", "name"], observed=True)["lap"]
        .max().rename("laps_completed").reset_index()
    )
    base = base.merge(laps_comp, on=["race_id", "name"], how="left")
    base = base[base["laps_completed"] >= base["threshold"]].copy()

    avg_lap = (
        base[base["lap"] > 1]
        .groupby(["race_id", "name", "class_label", "year", "round", "moto"], observed=True)
        ["lap_time"].mean().reset_index(name="avg_lap_time")
    )
    avg_lap["round"] = avg_lap["round"].astype(str)
    avg_lap["moto"]  = avg_lap["moto"].astype(str)
    avg_lap["year"]  = avg_lap["year"].astype(str)

    # WMX round 2 moto remapping
    wmx_r2 = (avg_lap["class_label"] == "WMX") & (avg_lap["round"] == "2")
    avg_lap = avg_lap[~(wmx_r2 & (avg_lap["moto"] == "1"))].copy()
    avg_lap.loc[wmx_r2 & (avg_lap["moto"] == "2"), "moto"] = "1"
    avg_lap.loc[wmx_r2 & (avg_lap["moto"] == "3"), "moto"] = "2"
    avg_lap = avg_lap[avg_lap["moto"].isin(["1", "2"])].copy()

    both = (
        avg_lap.groupby(["name", "class_label", "year", "round"], observed=True)["moto"]
        .nunique().reset_index(name="motos_completed")
    )
    both = both[both["motos_completed"] == 2]
    avg_lap = avg_lap.merge(
        both[["name", "class_label", "year", "round"]],
        on=["name", "class_label", "year", "round"], how="inner"
    )

    pivot = avg_lap.pivot_table(
        index=["name", "class_label", "year", "round"],
        columns="moto", values="avg_lap_time", observed=True
    ).reset_index()
    pivot.columns.name = None
    pivot = pivot.rename(columns={"1": "moto1_avg", "2": "moto2_avg"})
    pivot = pivot.dropna(subset=["moto1_avg", "moto2_avg"])
    pivot["diff"] = pivot["moto2_avg"] - pivot["moto1_avg"]
    pivot["slower_in_moto2"] = pivot["diff"] > 0
    return pivot

fatigue_base = build_fatigue_base(df)
moto_pivot   = build_moto_comp_base(df)

all_years   = sorted(fatigue_base["year"].astype(str).unique())
all_classes = ["450", "250", "WMX"]
all_tracks  = sorted(fatigue_base["track"].astype(str).unique())
all_motos   = sorted(fatigue_base["moto"].astype(str).unique())

def make_label(years, classes, tracks, motos, rider):
    y = ", ".join(sorted(years))
    c = ", ".join(sorted(classes))
    t = "All Tracks" if set(tracks) == set(all_tracks) else (
        ", ".join(sorted(tracks)) if len(tracks) <= 2 else f"{len(tracks)} Tracks"
    )
    m = "All Motos" if set(motos) == set(all_motos) else "Moto " + ", ".join(sorted(motos))
    return f"{c} | {y} | {t} | {m} | {rider}"

# ═══════════════════════════════════════════════════════════════════════════════
# 4a. Pace arc — within-moto fatigue
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Within-moto pace arc")

col_l, col_r = st.columns(2)
with col_l:
    st.markdown("**Series A**")
    yr_a   = st.multiselect("Year (A)",  all_years,   all_years,   key="fat_yr_a")
    cls_a  = st.multiselect("Class (A)", all_classes, all_classes, key="fat_cls_a")
    trk_a  = st.multiselect("Track (A)", all_tracks,  all_tracks,  key="fat_trk_a")
    mto_a  = st.multiselect("Moto (A)",  all_motos,   all_motos,   key="fat_mto_a")
    elig_a = sorted(fatigue_base[
        fatigue_base["year"].astype(str).isin(yr_a) &
        fatigue_base["class_label"].isin(cls_a) &
        fatigue_base["track"].astype(str).isin(trk_a) &
        fatigue_base["moto"].astype(str).isin(mto_a)
    ]["name"].astype(str).unique())
    rdr_a = st.selectbox("Rider (A)", elig_a, key="fat_rdr_a") if elig_a else None

with col_r:
    st.markdown("**Series B**")
    yr_b   = st.multiselect("Year (B)",  all_years,   all_years,   key="fat_yr_b")
    cls_b  = st.multiselect("Class (B)", all_classes, all_classes, key="fat_cls_b")
    trk_b  = st.multiselect("Track (B)", all_tracks,  all_tracks,  key="fat_trk_b")
    mto_b  = st.multiselect("Moto (B)",  all_motos,   all_motos,   key="fat_mto_b")
    elig_b = sorted(fatigue_base[
        fatigue_base["year"].astype(str).isin(yr_b) &
        fatigue_base["class_label"].isin(cls_b) &
        fatigue_base["track"].astype(str).isin(trk_b) &
        fatigue_base["moto"].astype(str).isin(mto_b)
    ]["name"].astype(str).unique())
    rdr_b = st.selectbox("Rider (B)", elig_b, key="fat_rdr_b") if elig_b else None

if st.button("Update — pace arc", type="primary"):
    def filter_fat(yr, cls, trk, mto, rdr):
        if not rdr:
            return fatigue_base.iloc[0:0]
        return fatigue_base[
            fatigue_base["year"].astype(str).isin(yr) &
            fatigue_base["class_label"].isin(cls) &
            fatigue_base["track"].astype(str).isin(trk) &
            fatigue_base["moto"].astype(str).isin(mto) &
            (fatigue_base["name"].astype(str) == rdr)
        ]

    def build_line(subset, color, label):
        if subset.empty:
            return []
        avg = (
            subset.groupby("pct_bin", observed=True)["delta_pct"]
            .agg(avg_delta="mean", n="count").reset_index()
        )
        avg["pct_num"] = avg["pct_bin"].astype(float)
        avg = avg.sort_values("pct_num")
        slope, intercept, r, _, _ = stats.linregress(avg["pct_num"], avg["avg_delta"])
        trend_y = slope * avg["pct_num"] + intercept
        traces = [
            go.Scatter(
                x=avg["pct_num"], y=avg["avg_delta"], mode="lines+markers",
                name=label, line=dict(color=color, width=2), marker=dict(size=6),
                customdata=avg["n"],
                hovertemplate=f"<b>{label}</b><br>Race %: %{{x}}<br>Delta: +%{{y:.2f}}%<br>N: %{{customdata}}<extra></extra>",
            ),
            go.Scatter(
                x=avg["pct_num"], y=trend_y, mode="lines",
                name=f"Trend {label}: {slope:+.3f}%/10% (R²={r**2:.3f})",
                line=dict(color=color, width=2, dash="dash"), hoverinfo="skip",
            ),
        ]
        return traces, avg, trend_y

    sub_a = filter_fat(yr_a, cls_a, trk_a, mto_a, rdr_a)
    sub_b = filter_fat(yr_b, cls_b, trk_b, mto_b, rdr_b)
    lbl_a = make_label(yr_a, cls_a, trk_a, mto_a, rdr_a or "—")
    lbl_b = make_label(yr_b, cls_b, trk_b, mto_b, rdr_b or "—")

    fig = go.Figure()
    all_y, all_x = [], set()
    for sub, color, lbl in [(sub_a, "#E8641A", lbl_a), (sub_b, "#1A7FE8", lbl_b)]:
        result = build_line(sub, color, lbl)
        if not result:
            continue
        traces, avg, trend_y = result
        for t in traces:
            fig.add_trace(t)
        all_y += list(avg["avg_delta"]) + list(trend_y)
        all_x.update(avg["pct_num"].tolist())

    if all_y:
        padding = (max(all_y) - min(all_y)) * 0.15 or 0.5
        sorted_x = sorted(all_x)
        fig.update_layout(
            title="Fatigue Effect — Lap Time Delta from Personal Best",
            xaxis=dict(
                title="% of Race Completed",
                tickmode="array", tickvals=sorted_x,
                ticktext=[f"{int(v)}%" for v in sorted_x],
            ),
            yaxis=dict(
                title="Lap Time Delta from Personal Best (%)",
                range=[max(0, min(all_y) - padding), max(all_y) + padding + 1.5],
                ticksuffix="%",
            ),
            height=550, margin=dict(l=60, r=60, t=60, b=100),
            legend=dict(orientation="h", yanchor="bottom", y=-0.4, xanchor="left", x=0),
            hovermode="x unified",
        )
        st.plotly_chart(fig, use_container_width=True)
    else:
        st.warning("No data for selected filters.")

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 4b. Moto 1 → Moto 2 comparison
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Moto 1 → Moto 2 average lap time change")

summary = (
    moto_pivot.groupby(["class_label", "year"], observed=True)
    .agg(
        mean_diff=("diff", "mean"),
        median_diff=("diff", "median"),
        pct_slower=("slower_in_moto2", "mean"),
        n=("diff", "count"),
    ).reset_index()
)
summary["mean_diff"]   = summary["mean_diff"].round(2)
summary["median_diff"] = summary["median_diff"].round(2)
summary["pct_slower"]  = (summary["pct_slower"] * 100).round(1).astype(str) + "%"
summary = summary.rename(columns={
    "class_label": "Class", "year": "Year",
    "mean_diff": "Mean Diff (s)", "median_diff": "Median Diff (s)",
    "pct_slower": "% Slower in Moto 2", "n": "N",
}).sort_values(["Class", "Year"]).reset_index(drop=True)

st.caption("Positive = slower in Moto 2, Negative = faster. Riders must complete both motos and ≥75% of laps in each.")
st.dataframe(summary, hide_index=True, use_container_width=False)

overall = (
    moto_pivot.groupby("class_label", observed=True)
    .agg(
        mean_diff=("diff", "mean"),
        median_diff=("diff", "median"),
        pct_slower=("slower_in_moto2", "mean"),
        n=("diff", "count"),
    ).reset_index()
)
overall["mean_diff"]   = overall["mean_diff"].round(2)
overall["median_diff"] = overall["median_diff"].round(2)
overall["pct_slower"]  = (overall["pct_slower"] * 100).round(1).astype(str) + "%"
overall = overall.rename(columns={
    "class_label": "Class",
    "mean_diff": "Mean Diff (s)", "median_diff": "Median Diff (s)",
    "pct_slower": "% Slower in Moto 2", "n": "N",
}).sort_values("Class").reset_index(drop=True)

st.markdown("**Overall — 2024 + 2025 combined**")
st.dataframe(overall, hide_index=True, use_container_width=False)

st.divider()
st.markdown("""
**Notes**
- Y-axis is % a lap is slower than that rider's personal best in the same moto (not season).
- Riders must complete ≥75% of the moto's laps to be included; lap 1 is excluded (noisy start).
- Binned by % of race completed rather than lap number to avoid survivorship bias across different track lengths.
- Steeper trend slope = more degradation per 10% of race. R² shows how well the linear fit describes the pattern.
- WMX Round 2 ran three motos; motos 2 and 3 (same-day pair) are remapped to moto 1 and 2 for the comparison table.
""")
