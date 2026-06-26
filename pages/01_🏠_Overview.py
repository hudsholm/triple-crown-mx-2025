import streamlit as st
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(
    page_title="Triple Crown MX · Overview",
    page_icon="🏍️",
    layout="wide",
    initial_sidebar_state="expanded",
)

df = load_data()

st.title("🏍️ Triple Crown Motocross — 2024 & 2025")
st.markdown(
    "Lap time and race data for the **CMRC / MXTOUR** Canadian motocross series. "
    "Data sourced from [resultsmx.com](https://resultsmx.com/cmrc-archive-2024/) (2024) "
    "and [cmrc.tracksideresults.com](https://cmrc.tracksideresults.com/) (2025)."
)

st.divider()

# ── Top-line metrics ───────────────────────────────────────────────────────────
col1, col2, col3, col4, col5 = st.columns(5)
col1.metric("Lap rows", f"{len(df):,}")
col2.metric("Unique riders", df["name"].nunique())
col3.metric("Classes", "450 · 250 · WMX")
col4.metric("Tracks", df["track"].nunique())
col5.metric("Seasons", f"{df['year'].min()} – {df['year'].max()}")

st.divider()

# ── Per-class breakdown ────────────────────────────────────────────────────────
st.subheader("Dataset at a glance")

for cls in ["450", "250", "WMX"]:
    sub = df[df["class_label"] == cls]
    riders = sub["name"].nunique()
    laps   = len(sub)
    races  = sub["race_id"].nunique()
    c1, c2, c3, c4 = st.columns([1, 2, 2, 2])
    c1.markdown(f"**{cls}**")
    c2.markdown(f"{riders} unique riders")
    c3.markdown(f"{races} motos")
    c4.markdown(f"{laps:,} lap rows")

st.divider()

# ── Rounds per year ────────────────────────────────────────────────────────────
st.subheader("Rounds per year")
rounds = (
    df.drop_duplicates(subset=["year", "round"])
    .groupby("year")["round"]
    .max()
    .reset_index()
    .rename(columns={"year": "Season", "round": "Rounds"})
)
st.dataframe(rounds, hide_index=True, use_container_width=False)

st.divider()

# ── Track list ─────────────────────────────────────────────────────────────────
st.subheader("Tracks")
tracks = (
    df.drop_duplicates(subset=["track"])
    [["track", "track_location"]]
    .sort_values("track")
    .rename(columns={"track": "Track", "track_location": "Location"})
    .reset_index(drop=True)
)
st.dataframe(tracks, hide_index=True, use_container_width=False)
