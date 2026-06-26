import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(
    page_title="Triple Crown MX · Points Race",
    page_icon="🏍️",
    layout="wide",
    initial_sidebar_state="expanded",
)

df = load_data()

st.title("6. Points Race")
st.markdown("Cumulative championship points across rounds for the top 10 riders per class.")

@st.cache_data
def build_points_race(_df):
    base = (
        _df.drop_duplicates(subset=["race_id", "name", "class_label"])
        .dropna(subset=["finish_position"])
        .copy()
    )
    base["year"]  = base["year"].astype(str)
    base["round"] = base["round"].astype(int)
    base["moto"]  = base["moto"].astype(str)

    per_round = (
        base.groupby(["name", "class_label", "year", "round"], observed=True)
        .agg(round_points=("points", "sum"))
        .reset_index()
    )
    per_round = per_round.sort_values(["name", "class_label", "year", "round"])
    per_round["cumulative_points"] = (
        per_round.groupby(["name", "class_label", "year"], observed=True)
        ["round_points"].cumsum()
    )

    final = (
        per_round.groupby(["name", "class_label", "year"], observed=True)
        ["cumulative_points"].max().reset_index(name="final_points")
        .sort_values(["class_label", "year", "final_points"], ascending=[True, True, False])
    )
    top10 = (
        final.groupby(["class_label", "year"], observed=True)
        .head(10)[["name", "class_label", "year"]]
    )
    points_race = per_round.merge(top10, on=["name", "class_label", "year"], how="inner")

    all_rounds = sorted(points_race["round"].unique())
    rider_idx  = points_race[["name", "class_label", "year"]].drop_duplicates()
    full_grid  = rider_idx.merge(pd.DataFrame({"round": all_rounds}), how="cross")

    filled = full_grid.merge(
        points_race[["name", "class_label", "year", "round", "cumulative_points", "round_points"]],
        on=["name", "class_label", "year", "round"], how="left"
    ).sort_values(["name", "class_label", "year", "round"])

    filled["cumulative_points"] = (
        filled.groupby(["name", "class_label", "year"], observed=False)
        ["cumulative_points"].ffill().fillna(0)
    )
    filled["round_points"] = filled["round_points"].fillna(0).astype(int)
    return filled, all_rounds

points_race, all_rounds = build_points_race(df)

all_years   = sorted(points_race["year"].astype(str).unique())
all_classes = ["450", "250", "WMX"]

# ── Filters ────────────────────────────────────────────────────────────────────
col1, col2 = st.columns(2)
with col1:
    year_sel  = st.selectbox("Year",  all_years,  index=len(all_years) - 1)
with col2:
    class_sel = st.selectbox("Class", all_classes)

if st.button("Update", type="primary"):
    subset = points_race[
        (points_race["year"] == year_sel) &
        (points_race["class_label"] == class_sel)
    ]

    if subset.empty:
        st.warning("No data for selected filters.")
    else:
        final = (
            subset.groupby("name")["cumulative_points"].max()
            .reset_index().sort_values("cumulative_points", ascending=False)
        )
        rider_order = final["name"].tolist()

        fig = go.Figure()
        for rider in rider_order:
            rd = subset[subset["name"] == rider].sort_values("round")
            final_pts = int(rd["cumulative_points"].max())
            fig.add_trace(go.Scatter(
                x=rd["round"], y=rd["cumulative_points"],
                mode="lines+markers",
                name=f"{rider} ({final_pts} pts)",
                line=dict(width=2), marker=dict(size=7),
                customdata=rd["round_points"],
                hovertemplate=(
                    f"<b>{rider}</b><br>"
                    "Round: %{x}<br>"
                    "Points: %{y}<br>"
                    "+%{customdata} this round<extra></extra>"
                ),
            ))

        fig.update_layout(
            title=f"Points Race — {class_sel} | {year_sel}",
            xaxis=dict(title="Round", tickmode="linear", dtick=1,
                       range=[0.8, len(all_rounds) + 0.2]),
            yaxis=dict(title="Cumulative Points"),
            height=550, margin=dict(l=60, r=60, t=60, b=60),
            legend=dict(title="Rider (Final Points)", traceorder="normal"),
            hovermode="x unified",
        )
        st.plotly_chart(fig, use_container_width=True)

st.divider()
st.markdown("""
**Notes**
- Only the top 10 riders by final cumulative points per class per year are shown.
- WMX is split into West and East series; cumulative points are merged into one standing for simplicity.
- Points are awarded per moto (not per round), then summed to the round level for this chart.
""")
