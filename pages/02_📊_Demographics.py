import streamlit as st
import pandas as pd
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(
    page_title="Triple Crown MX · Demographics",
    page_icon="🏍️",
    layout="wide",
    initial_sidebar_state="expanded",
)

df = load_data()

# ── Lookup maps ────────────────────────────────────────────────────────────────
PROV_ABBREV_TO_NAME = {
    "AB": "Alberta", "BC": "British Columbia", "MB": "Manitoba",
    "NB": "New Brunswick", "NL": "Newfoundland and Labrador",
    "NT": "Northwest Territories", "NS": "Nova Scotia", "NU": "Nunavut",
    "ON": "Ontario", "PE": "Prince Edward Island", "QC": "Quebec",
    "SK": "Saskatchewan", "YT": "Yukon",
}
US_ABBREV_TO_NAME = {
    "AL": "Alabama", "AK": "Alaska", "AZ": "Arizona", "AR": "Arkansas",
    "CA": "California", "CO": "Colorado", "CT": "Connecticut", "DE": "Delaware",
    "FL": "Florida", "GA": "Georgia", "HI": "Hawaii", "ID": "Idaho",
    "IL": "Illinois", "IN": "Indiana", "IA": "Iowa", "KS": "Kansas",
    "KY": "Kentucky", "LA": "Louisiana", "ME": "Maine", "MD": "Maryland",
    "MA": "Massachusetts", "MI": "Michigan", "MN": "Minnesota",
    "MS": "Mississippi", "MO": "Missouri", "MT": "Montana", "NE": "Nebraska",
    "NV": "Nevada", "NH": "New Hampshire", "NJ": "New Jersey",
    "NM": "New Mexico", "NY": "New York", "NC": "North Carolina",
    "ND": "North Dakota", "OH": "Ohio", "OK": "Oklahoma", "OR": "Oregon",
    "PA": "Pennsylvania", "RI": "Rhode Island", "SC": "South Carolina",
    "SD": "South Dakota", "TN": "Tennessee", "TX": "Texas", "UT": "Utah",
    "VT": "Vermont", "VA": "Virginia", "WA": "Washington",
    "WV": "West Virginia", "WI": "Wisconsin", "WY": "Wyoming",
    "DC": "District of Columbia",
}
ABBREV_TO_NAME = {**PROV_ABBREV_TO_NAME, **US_ABBREV_TO_NAME}
NA_STATES = set(ABBREV_TO_NAME.keys())

st.title("2. Demographic Analysis")
st.markdown("""
- Breakdown by province/state
- Rider retention
- Manufacturer success
""")

# ── Riders geo base ────────────────────────────────────────────────────────────
riders_geo = (
    df.drop_duplicates(subset=["name", "class_label"])
    [["name", "class_label", "rider_state", "rider_country"]]
    .copy()
)
riders_geo["rider_state"] = riders_geo["rider_state"].astype(str)
riders_geo["rider_country"] = riders_geo["rider_country"].astype(str)
class_totals = riders_geo.groupby("class_label", observed=True)["name"].nunique()

# ── Geographic breakdown ───────────────────────────────────────────────────────
st.subheader("Riders by province / state")
tabs = st.tabs(["450", "250", "WMX"])

for tab, cls in zip(tabs, ["450", "250", "WMX"]):
    with tab:
        cls_riders = riders_geo[riders_geo["class_label"] == cls]
        total = int(class_totals[cls])

        na_riders   = cls_riders[cls_riders["rider_state"].isin(NA_STATES)]
        intl_riders = cls_riders[~cls_riders["rider_state"].isin(NA_STATES)]

        state_counts = (
            na_riders.groupby("rider_state", observed=True)
            .size()
            .reset_index(name="Riders")
            .sort_values("Riders", ascending=False)
        )
        state_counts["Pct"] = (state_counts["Riders"] / total * 100).round(1).astype(str) + "%"
        state_counts["State / Province"] = state_counts["rider_state"].map(ABBREV_TO_NAME)
        state_counts = state_counts[["State / Province", "Riders", "Pct"]].reset_index(drop=True)

        st.markdown(f"**{cls} class — {total} unique riders**")
        st.dataframe(state_counts.head(15), hide_index=True, use_container_width=False)

        if not intl_riders.empty:
            intl_summary = (
                intl_riders.groupby("rider_country", observed=True)
                .size()
                .reset_index(name="Riders")
                .sort_values("Riders", ascending=False)
                .rename(columns={"rider_country": "Country"})
            )
            if not intl_summary.empty:
                st.markdown("**Riders outside North America:**")
                st.dataframe(intl_summary, hide_index=True, use_container_width=False)
            else:
                st.caption("All riders are based in North America.")
        else:
            st.caption("All riders are based in North America.")

st.divider()

# ── Motos entered ──────────────────────────────────────────────────────────────
st.subheader("Top 25 by motos entered — all classes")

rider_motos = (
    df.drop_duplicates(subset=["race_id", "name"])
    .groupby("name", observed=True)
    .agg(
        motos_entered=("race_id", "count"),
        total_points=("points", "sum"),
    )
    .reset_index()
    .sort_values(["motos_entered", "total_points"], ascending=[False, False])
)
rider_motos["total_points"] = rider_motos["total_points"].astype(int)
top25 = (
    rider_motos.head(25)
    .reset_index(drop=True)
    .rename(columns={"name": "Rider", "motos_entered": "Motos Entered", "total_points": "Total Points"})
)
top25.index += 1
st.dataframe(top25, use_container_width=False)

st.divider()

# ── Rider retention ────────────────────────────────────────────────────────────
st.subheader("Rider retention: 2024 → 2025")

retention_rows = []
for cls in ["450", "250", "WMX"]:
    riders_by_year = df.drop_duplicates(subset=["name", "class_label", "year"])
    y2024 = set(riders_by_year[(riders_by_year["year"] == 2024) & (riders_by_year["class_label"] == cls)]["name"])
    y2025 = set(riders_by_year[(riders_by_year["year"] == 2025) & (riders_by_year["class_label"] == cls)]["name"])
    returned = y2024 & y2025
    pct = len(returned) / len(y2024) * 100 if y2024 else 0
    retention_rows.append({
        "Class": cls,
        "2024 riders": len(y2024),
        "2025 riders": len(y2025),
        "Returned in 2025": len(returned),
        "Retention %": f"{pct:.1f}%",
        "New in 2025": len(y2025 - y2024),
        "Didn't return": len(y2024 - y2025),
    })

st.dataframe(pd.DataFrame(retention_rows), hide_index=True, use_container_width=False)

st.divider()

# ── Manufacturer breakdown ─────────────────────────────────────────────────────
st.subheader("Manufacturer breakdown (2025)")

mfr_base_2025 = (
    df[(df["year"] == 2025)]
    .drop_duplicates(subset=["name", "class_label"])
    .dropna(subset=["manufacturer"])
    .copy()
)
mfr_base_2025["manufacturer"] = mfr_base_2025["manufacturer"].astype(str)

mfr_riders = (
    mfr_base_2025.groupby(["class_label", "manufacturer"], observed=True)
    .agg(riders=("name", "nunique"))
    .reset_index()
)
mfr_riders = mfr_riders[mfr_riders["riders"] >= 3].copy()

points_2025 = (
    df[df["year"] == 2025]
    .drop_duplicates(subset=["race_id", "name", "class_label"])
    .groupby(["name", "class_label"], observed=True)
    .agg(total_points=("points", "sum"))
    .reset_index()
    .sort_values(["class_label", "total_points"], ascending=[True, False])
)
mfr_lookup = (
    df[(df["year"] == 2025)]
    .drop_duplicates(subset=["name", "class_label"])
    [["name", "class_label", "manufacturer"]]
    .dropna(subset=["manufacturer"])
    .copy()
)
mfr_lookup["manufacturer"] = mfr_lookup["manufacturer"].astype(str)
top10_mfr = (
    points_2025.groupby("class_label", observed=True)
    .head(10)
    .merge(mfr_lookup, on=["name", "class_label"], how="left")
)

mfr_tabs = st.tabs(["450", "250", "WMX"])
for tab, cls in zip(mfr_tabs, ["450", "250", "WMX"]):
    with tab:
        col_a, col_b = st.columns(2)

        with col_a:
            st.markdown("**Riders by manufacturer (≥3 riders)**")
            mfr_sub = (
                mfr_riders[mfr_riders["class_label"] == cls]
                .drop(columns="class_label")
                .sort_values("riders", ascending=False)
                .reset_index(drop=True)
                .rename(columns={"manufacturer": "Manufacturer", "riders": "Riders"})
            )
            st.dataframe(mfr_sub, hide_index=True, use_container_width=False)

        with col_b:
            st.markdown("**Manufacturer share of top 10 (2025 points)**")
            top10_sub = top10_mfr[top10_mfr["class_label"] == cls]
            mfr_counts = (
                top10_sub.groupby("manufacturer", observed=True)
                .size()
                .reset_index(name="Riders in Top 10")
            )
            mfr_counts["% of Top 10"] = (mfr_counts["Riders in Top 10"] / len(top10_sub) * 100).round(1)
            mfr_counts = (
                mfr_counts.sort_values("Riders in Top 10", ascending=False)
                .reset_index(drop=True)
                .rename(columns={"manufacturer": "Manufacturer"})
            )
            st.dataframe(mfr_counts, hide_index=True, use_container_width=False)
