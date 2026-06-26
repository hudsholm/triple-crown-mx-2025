import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from scipy.stats import gaussian_kde
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(
    page_title="Triple Crown MX · Transitions",
    page_icon="🏍️",
    layout="wide",
    initial_sidebar_state="expanded",
)

df = load_data()

BUCKETS = ["1-3", "4-6", "7-10", "11-15", "16+"]
CLASS_COLORS = {"450": "#1A7FE8", "250": "#E8641A", "WMX": "#1AE87F"}

def assign_bucket(pos):
    if pd.isna(pos): return None
    pos = float(pos)
    if pos <= 3:  return "1-3"
    if pos <= 6:  return "4-6"
    if pos <= 10: return "7-10"
    if pos <= 15: return "11-15"
    return "16+"

def hex_to_rgba(hex_color, opacity=0.08):
    hex_color = hex_color.lstrip("#")
    r, g, b = int(hex_color[0:2], 16), int(hex_color[2:4], 16), int(hex_color[4:6], 16)
    return f"rgba({r},{g},{b},{opacity})"

def make_filter_label(years, classes, tracks, all_tracks, rider=None):
    y = ", ".join(sorted(years))
    c = ", ".join(sorted(classes))
    if set(tracks) == set(all_tracks):
        t = "All Tracks"
    elif len(tracks) <= 2:
        t = ", ".join(sorted(tracks))
    else:
        t = f"{len(tracks)} Tracks"
    return f"{c} | {y} | {t}" + (f" | {rider}" if rider else "")

# ── Pre-build transition base (shared across sub-sections) ────────────────────
@st.cache_data
def build_transitions(_df):
    lap_data = (
        _df.dropna(subset=["lap", "place"])
        .sort_values(["race_id", "name", "lap"])
        .copy()
    )
    lap_data["place"] = lap_data["place"].astype(float)
    lap_data["bucket"] = lap_data["place"].apply(assign_bucket)
    lap_data["next_bucket"] = lap_data.groupby(["race_id", "name"], observed=True)["bucket"].shift(-1)
    lap_data["next_lap"] = lap_data.groupby(["race_id", "name"], observed=True)["lap"].shift(-1)
    lap_data["next_place"] = lap_data.groupby(["race_id", "name"], observed=True)["place"].shift(-1)

    transitions = lap_data[
        lap_data["next_bucket"].notna() &
        lap_data["bucket"].notna() &
        (lap_data["next_lap"] == lap_data["lap"] + 1)
    ].copy()
    transitions["made_pass"] = transitions["next_place"] < transitions["place"]
    return transitions

@st.cache_data
def build_sf_transitions(_df):
    lap1 = (
        _df[_df["lap"] == 1]
        .dropna(subset=["place"])
        [["race_id", "name", "class_label", "year", "track", "place"]]
        .rename(columns={"place": "lap1_place"})
        .copy()
    )
    finish = (
        _df.drop_duplicates(subset=["race_id", "name"])
        .dropna(subset=["finish_position"])
        [["race_id", "name", "finish_position"]]
        .copy()
    )
    sf = lap1.merge(finish, on=["race_id", "name"], how="inner")
    sf["start_bucket"] = sf["lap1_place"].astype(float).apply(assign_bucket)
    sf["finish_bucket"] = sf["finish_position"].astype(float).apply(assign_bucket)
    sf["lap1_place"] = sf["lap1_place"].astype(float)
    return sf.dropna(subset=["start_bucket", "finish_bucket"])

transitions = build_transitions(df)
sf_transitions = build_sf_transitions(df)

all_years   = sorted(df["year"].astype(str).unique())
all_classes = ["450", "250", "WMX"]
all_tracks  = sorted(df["track"].astype(str).unique())
all_riders  = sorted(df["name"].astype(str).unique())

st.title("3. Transition Matrices")
st.markdown("""
- Passing probabilities
- Start-finish conversion
- Pass rates by lap
- Gap distributions
""")

# ═══════════════════════════════════════════════════════════════════════════════
# 3a. Lap-to-lap transition matrix
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Lap-to-lap position transitions")

def build_heatmap(subset, label, row_col, col_col):
    matrix = (
        subset.groupby([row_col, col_col], observed=True)
        .size()
        .reset_index(name="count")
    )
    if matrix.empty:
        return go.Heatmap(z=[[0]*5]*5, x=BUCKETS, y=BUCKETS,
                          colorscale="Oranges", zmin=0, zmax=1, showscale=False)
    matrix["prob"] = (
        matrix["count"] /
        matrix.groupby(row_col, observed=True)["count"].transform("sum")
    )
    pivot = (
        matrix.pivot(index=row_col, columns=col_col, values="prob")
        .reindex(index=BUCKETS, columns=BUCKETS)
        .fillna(0)
    )
    text_labels = pivot.map(lambda v: f"{v:.0%}" if v > 0 else "")
    return go.Heatmap(
        z=pivot.values, x=BUCKETS, y=BUCKETS,
        text=text_labels.values, texttemplate="%{text}",
        textfont=dict(size=11), colorscale="Oranges", zmin=0, zmax=1,
        showscale=(label == "B"),
        colorbar=dict(title="Prob", tickformat=".0%", x=1.02) if label == "B" else None,
        hovertemplate=f"From: %{{y}}<br>To: %{{x}}<br>Prob: %{{z:.1%}}<extra></extra>",
    )

col_l, col_r = st.columns(2)
with col_l:
    st.markdown("**Matrix A**")
    yr_a  = st.multiselect("Year (A)",    all_years,   all_years,  key="tm_yr_a")
    cls_a = st.multiselect("Class (A)",   all_classes, all_classes, key="tm_cls_a")
    trk_a = st.multiselect("Track (A)",   all_tracks,  all_tracks, key="tm_trk_a")
    eligible_a = sorted(transitions[
        transitions["year"].astype(str).isin(yr_a) &
        transitions["class_label"].isin(cls_a) &
        transitions["track"].astype(str).isin(trk_a)
    ]["name"].astype(str).unique())
    rdr_a = st.selectbox("Rider (A)", eligible_a, key="tm_rdr_a") if eligible_a else None

with col_r:
    st.markdown("**Matrix B**")
    yr_b  = st.multiselect("Year (B)",    all_years,   all_years,  key="tm_yr_b")
    cls_b = st.multiselect("Class (B)",   all_classes, all_classes, key="tm_cls_b")
    trk_b = st.multiselect("Track (B)",   all_tracks,  all_tracks, key="tm_trk_b")
    eligible_b = sorted(transitions[
        transitions["year"].astype(str).isin(yr_b) &
        transitions["class_label"].isin(cls_b) &
        transitions["track"].astype(str).isin(trk_b)
    ]["name"].astype(str).unique())
    rdr_b = st.selectbox("Rider (B)", eligible_b, key="tm_rdr_b") if eligible_b else None

if st.button("Update — lap-to-lap matrix", type="primary"):
    def filter_t(yr, cls, trk, rdr):
        return transitions[
            transitions["year"].astype(str).isin(yr) &
            transitions["class_label"].isin(cls) &
            transitions["track"].astype(str).isin(trk) &
            (transitions["name"].astype(str) == rdr)
        ] if rdr else transitions.iloc[0:0]

    sub_a = filter_t(yr_a, cls_a, trk_a, rdr_a)
    sub_b = filter_t(yr_b, cls_b, trk_b, rdr_b)
    lbl_a = make_filter_label(yr_a, cls_a, trk_a, all_tracks, rdr_a)
    lbl_b = make_filter_label(yr_b, cls_b, trk_b, all_tracks, rdr_b)

    fig = make_subplots(rows=1, cols=2, subplot_titles=[lbl_a, lbl_b], horizontal_spacing=0.15)
    fig.add_trace(build_heatmap(sub_a, "A", "bucket", "next_bucket"), row=1, col=1)
    fig.add_trace(build_heatmap(sub_b, "B", "bucket", "next_bucket"), row=1, col=2)
    for c in [1, 2]:
        fig.update_xaxes(title_text="Position (Lap N+1)", row=1, col=c)
        fig.update_yaxes(title_text="Position (Lap N)",   row=1, col=c)
    fig.update_layout(height=500, margin=dict(l=60, r=80, t=80, b=60))
    st.plotly_chart(fig, use_container_width=True)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 3b. Start-finish transition matrix
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Start → finish position conversion")

col_l2, col_r2 = st.columns(2)
with col_l2:
    st.markdown("**Matrix A**")
    sf_yr_a  = st.multiselect("Year (A)",  all_years,   all_years,   key="sf_yr_a")
    sf_cls_a = st.multiselect("Class (A)", all_classes, all_classes, key="sf_cls_a")
    sf_trk_a = st.multiselect("Track (A)", all_tracks,  all_tracks,  key="sf_trk_a")
    sf_eligible_a = sorted(sf_transitions[
        sf_transitions["year"].astype(str).isin(sf_yr_a) &
        sf_transitions["class_label"].isin(sf_cls_a) &
        sf_transitions["track"].astype(str).isin(sf_trk_a)
    ]["name"].astype(str).unique())
    sf_rdr_a = st.selectbox("Rider (A)", sf_eligible_a, key="sf_rdr_a") if sf_eligible_a else None

with col_r2:
    st.markdown("**Matrix B**")
    sf_yr_b  = st.multiselect("Year (B)",  all_years,   all_years,   key="sf_yr_b")
    sf_cls_b = st.multiselect("Class (B)", all_classes, all_classes, key="sf_cls_b")
    sf_trk_b = st.multiselect("Track (B)", all_tracks,  all_tracks,  key="sf_trk_b")
    sf_eligible_b = sorted(sf_transitions[
        sf_transitions["year"].astype(str).isin(sf_yr_b) &
        sf_transitions["class_label"].isin(sf_cls_b) &
        sf_transitions["track"].astype(str).isin(sf_trk_b)
    ]["name"].astype(str).unique())
    sf_rdr_b = st.selectbox("Rider (B)", sf_eligible_b, key="sf_rdr_b") if sf_eligible_b else None

if st.button("Update — start/finish matrix", type="primary"):
    def filter_sf(yr, cls, trk, rdr):
        return sf_transitions[
            sf_transitions["year"].astype(str).isin(yr) &
            sf_transitions["class_label"].isin(cls) &
            sf_transitions["track"].astype(str).isin(trk) &
            (sf_transitions["name"].astype(str) == rdr)
        ] if rdr else sf_transitions.iloc[0:0]

    sub_a = filter_sf(sf_yr_a, sf_cls_a, sf_trk_a, sf_rdr_a)
    sub_b = filter_sf(sf_yr_b, sf_cls_b, sf_trk_b, sf_rdr_b)
    lbl_a = make_filter_label(sf_yr_a, sf_cls_a, sf_trk_a, all_tracks, sf_rdr_a)
    lbl_b = make_filter_label(sf_yr_b, sf_cls_b, sf_trk_b, all_tracks, sf_rdr_b)

    fig = make_subplots(rows=1, cols=2, subplot_titles=[lbl_a, lbl_b], horizontal_spacing=0.15)
    fig.add_trace(build_heatmap(sub_a, "A", "start_bucket", "finish_bucket"), row=1, col=1)
    fig.add_trace(build_heatmap(sub_b, "B", "start_bucket", "finish_bucket"), row=1, col=2)
    for c in [1, 2]:
        fig.update_xaxes(title_text="Finish Position", row=1, col=c)
        fig.update_yaxes(title_text="Start Position",  row=1, col=c)
    fig.update_layout(height=500, margin=dict(l=60, r=80, t=80, b=60))
    st.plotly_chart(fig, use_container_width=True)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 3c. Pass rate by lap number
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Pass rate by lap number")

col_l3, col_r3 = st.columns(2)
with col_l3:
    st.markdown("**Series A**")
    pr_yr_a  = st.multiselect("Year (A)",  all_years,   all_years,   key="pr_yr_a")
    pr_cls_a = st.multiselect("Class (A)", all_classes, all_classes, key="pr_cls_a")
    pr_trk_a = st.multiselect("Track (A)", all_tracks,  all_tracks,  key="pr_trk_a")
with col_r3:
    st.markdown("**Series B**")
    pr_yr_b  = st.multiselect("Year (B)",  all_years,   all_years,   key="pr_yr_b")
    pr_cls_b = st.multiselect("Class (B)", all_classes, all_classes, key="pr_cls_b")
    pr_trk_b = st.multiselect("Track (B)", all_tracks,  all_tracks,  key="pr_trk_b")

if st.button("Update — pass rate", type="primary"):
    def filter_pass(yr, cls, trk):
        return transitions[
            transitions["year"].astype(str).isin(yr) &
            transitions["class_label"].isin(cls) &
            transitions["track"].astype(str).isin(trk) &
            transitions["next_place"].notna()
        ]

    def compute_pass_rate(subset):
        if subset.empty:
            return pd.DataFrame(columns=["lap", "pass_rate", "riders"])
        g = (
            subset.groupby("lap", observed=True)
            .agg(riders=("name", "count"), passers=("made_pass", "sum"))
            .reset_index()
        )
        g["pass_rate"] = (g["passers"] / g["riders"] * 100).round(1)
        g["lap"] = g["lap"].astype(float)
        return g.sort_values("lap")

    sub_a = filter_pass(pr_yr_a, pr_cls_a, pr_trk_a)
    sub_b = filter_pass(pr_yr_b, pr_cls_b, pr_trk_b)
    lbl_a = make_filter_label(pr_yr_a, pr_cls_a, pr_trk_a, all_tracks)
    lbl_b = make_filter_label(pr_yr_b, pr_cls_b, pr_trk_b, all_tracks)

    fig = go.Figure()
    for sub, color, lbl in [(sub_a, "#E8641A", lbl_a), (sub_b, "#1A7FE8", lbl_b)]:
        pr = compute_pass_rate(sub)
        if pr.empty:
            continue
        x, y = pr["lap"].values, pr["pass_rate"].values
        fig.add_trace(go.Scatter(
            x=x, y=y, mode="lines+markers", name=lbl,
            line=dict(color=color, width=2), marker=dict(size=6),
            customdata=pr["riders"].values,
            hovertemplate="<b>" + lbl + "</b><br>Lap: %{x}<br>Pass rate: %{y}%<br>Rider-laps: %{customdata}<extra></extra>",
        ))
        if len(x) >= 2:
            coeffs  = np.polyfit(x, y, 1)
            trend_y = np.polyval(coeffs, x)
            ss_res  = np.sum((y - trend_y) ** 2)
            ss_tot  = np.sum((y - np.mean(y)) ** 2)
            r2      = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0
            sign    = "↑" if coeffs[0] >= 0 else "↓"
            fig.add_trace(go.Scatter(
                x=x, y=trend_y, mode="lines",
                name=f"{lbl} trend {sign} (R²={r2:.2f})",
                line=dict(color=color, width=1.5, dash="dash"),
                hoverinfo="skip",
            ))

    fig.update_layout(
        title="% of riders who made a pass — by lap number",
        xaxis=dict(title="Lap Number", tickmode="linear", dtick=1),
        yaxis=dict(title="% of riders who made a pass", ticksuffix="%"),
        height=500, margin=dict(l=60, r=60, t=60, b=100),
        legend=dict(orientation="h", yanchor="bottom", y=-0.45, xanchor="left", x=0),
        hovermode="x unified",
    )
    st.plotly_chart(fig, use_container_width=True)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 3d. Gap to leader (1st → 2nd) KDE
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Gap to leader — 2nd place distribution")

gap_base = (
    df.dropna(subset=["lap", "behind_time", "place"])
    .copy()
)
gap_base["place"] = gap_base["place"].astype(float)
gap_base = gap_base[gap_base["place"] == 2].copy()
gap_years = sorted(df["year"].astype(str).unique())

gap_year = st.selectbox("Year", gap_years, index=len(gap_years) - 1, key="gap_year")

if st.button("Update — gap distribution", type="primary"):
    fig = go.Figure()
    all_vals = []

    for cls, color in CLASS_COLORS.items():
        vals = gap_base[
            (gap_base["year"].astype(str) == gap_year) &
            (gap_base["class_label"] == cls)
        ]["behind_time"].dropna().values

        if len(vals) < 2:
            continue
        all_vals.extend(vals)
        p95    = np.percentile(vals, 95)
        kde    = gaussian_kde(vals, bw_method=0.3)
        x_rng  = np.linspace(0, p95, 500)
        density = kde(x_rng)
        median  = np.median(vals)
        med_d   = kde(np.array([median]))[0]

        fig.add_trace(go.Scatter(
            x=x_rng, y=density, mode="lines",
            name=f"{cls} (n={len(vals)}, median={median:.2f}s)",
            line=dict(color=color, width=2),
            fill="tozeroy", fillcolor=hex_to_rgba(color, 0.08),
            hovertemplate=f"<b>{cls}</b><br>Gap: %{{x:.2f}}s<br>Density: %{{y:.4f}}<extra></extra>",
        ))
        fig.add_trace(go.Scatter(
            x=[median, median], y=[0, med_d], mode="lines",
            line=dict(color=color, width=1.5, dash="dot"),
            showlegend=False, hoverinfo="skip",
        ))

    if all_vals:
        p95_global = np.percentile(all_vals, 95)
        fig.update_layout(
            title=f"Gap to Leader (1st→2nd) — All Classes | {gap_year} | capped at 95th pct",
            xaxis=dict(title="Gap to leader (seconds)", range=[0, p95_global]),
            yaxis=dict(title="Density", showticklabels=False),
            height=500, margin=dict(l=60, r=60, t=60, b=60),
            legend=dict(title="Class"), hovermode="x unified",
        )
        st.plotly_chart(fig, use_container_width=True)
    else:
        st.warning("No data for selected year.")

st.divider()
st.markdown("""
**Notes**
- The lap-to-lap matrix shows the probability a rider in position bucket X is in bucket Y on the next lap.
- The start-finish matrix uses lap 1 position as the start and official finish position as the end.
- Pass rate counts the fraction of riders who improved their place from lap N to N+1.
- Gap-to-leader uses the 2nd-place rider's `behind_time` each lap, capped at the 95th percentile to trim outliers.
""")
