import streamlit as st
import pandas as pd
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(page_title="Triple Crown MX · Prize Money", page_icon="🏍️", layout="wide")

df = load_data()

st.title("14. Prize Money")
st.markdown("Simulated prize money per rider per season using the 2026 MXTOUR payout structure")

_img_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), "2026_MXTOUR_Payout.png")
if os.path.exists(_img_path):
    col_img, _ = st.columns([1, 2])
    with col_img:
        st.image(_img_path, caption="2026 MXTOUR Payout Structure — $16,675 per race")

st.divider()

PAYOUT_450 = {1:1200,2:1000,3:950,4:850,5:700,6:600,7:500,8:400,9:300,10:250,11:200,12:200,13:150,14:150,15:150}
PAYOUT_250 = {1:900,2:700,3:600,4:500,5:400,6:300,7:250,8:200,9:200,10:175,11:150,12:150,13:150,14:125,15:100}
PAYOUT_WMX = {1:900,2:700,3:600,4:500,5:400,6:300,7:250,8:200,9:175,10:150}
PAYOUT_MAP = {"450":PAYOUT_450,"250":PAYOUT_250,"WMX":PAYOUT_WMX}

TCMX_POINTS = {1:25,2:22,3:20,4:18,5:16,6:15,7:14,8:13,9:12,10:11,
               11:10,12:9,13:8,14:7,15:6,16:5,17:4,18:3,19:2,20:1}

@st.cache_data
def build_prize_data(_df):
    base = (_df.drop_duplicates(subset=["race_id","name"])
            .dropna(subset=["finish_position"]).copy())
    base["year"]            = base["year"].astype(str)
    base["round"]           = base["round"].astype(int)
    base["moto"]            = base["moto"].astype(int)
    base["finish_position"] = base["finish_position"].astype(int)
    base["moto_points"]     = base["finish_position"].map(TCMX_POINTS).fillna(0)

    round_pts = (
        base.groupby(["name","class_label","year","round"],observed=True)
        .agg(
            combined_points=("moto_points","sum"),
            last_moto_pos=("finish_position", lambda x: x.loc[
                x.index[base.loc[x.index,"moto"]==base.loc[x.index,"moto"].max()]
            ].iloc[0] if len(x)>0 else 999)
        ).reset_index()
    )

    round_pts["overall_place"] = (
        round_pts.groupby(["class_label","year","round"],observed=True)
        .apply(lambda g: g.sort_values(["combined_points","last_moto_pos"],ascending=[False,True])
               .assign(overall_place=range(1,len(g)+1))["overall_place"], include_groups=False)
        .reset_index(level=[0,1,2],drop=True)
    )

    def get_payout(row):
        return PAYOUT_MAP.get(row["class_label"],{}).get(int(row["overall_place"]),0)
    round_pts["prize_money"] = round_pts.apply(get_payout, axis=1)

    season = (
        round_pts.groupby(["name","class_label","year"],observed=True)
        .agg(rounds_entered=("round","count"), total_prize=("prize_money","sum"))
        .reset_index()
    )
    season["year"] = season["year"].astype(str)
    return season, round_pts

season_prize, round_pts = build_prize_data(df)

all_years   = sorted(season_prize["year"].unique())
all_classes = ["450","250","WMX"]

c1,c2 = st.columns(2)
with c1: yr_pr  = st.selectbox("Year",  all_years,   index=len(all_years)-1)
with c2: cls_pr = st.selectbox("Class", all_classes)

if st.button("Update", type="primary"):
    subset = (
        season_prize[
            (season_prize["year"]==yr_pr) &
            (season_prize["class_label"]==cls_pr) &
            (season_prize["total_prize"]>0)
        ].sort_values("total_prize",ascending=False).reset_index(drop=True)
    )
    if subset.empty:
        st.warning("No data for selected filters.")
    else:
        total = round_pts[
            (round_pts["year"]==yr_pr) &
            (round_pts["class_label"]==cls_pr)
        ]["prize_money"].sum()

        result = subset.rename(columns={"name":"Rider","rounds_entered":"Rounds","total_prize":"Total Prize ($)"}).copy()
        result["Total Prize ($)"] = result["Total Prize ($)"].apply(lambda x: f"${x:,.0f}")
        result.index += 1
        st.dataframe(result[["Rider","Rounds","Total Prize ($)"]], use_container_width=False)
        st.metric("Total class payout", f"${total:,.0f}")

st.divider()
st.markdown("""
**Notes**
- Simulates earnings using the 2026 MXTOUR payout structure applied to 2024 and 2025 results.
- Payout is determined by **overall round place** (combined two-moto points), not moto-by-moto — this matches how TCMX actually awards cheques.
- Tiebreak: better last-moto finish wins. Matches the official TCMX standings tiebreak rule.
- Manufacturer contracts, performance bonuses, and sponsorships are not included.
- Even for the best riders, series prize money alone does not cover entry fees, bike maintenance, transportation, and accommodation. The payout structure is designed more as a points incentive than a living wage.
""")
