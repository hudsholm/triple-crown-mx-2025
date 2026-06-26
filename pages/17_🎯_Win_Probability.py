import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score, brier_score_loss
from sklearn.calibration import calibration_curve, CalibratedClassifierCV
from sklearn.preprocessing import StandardScaler
from itertools import combinations
import xgboost as xgb
import warnings
import sys, os
warnings.filterwarnings("ignore")
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(page_title="Triple Crown MX · Win Probability", page_icon="🏍️", layout="wide")

df = load_data()

st.title("17. Win Probability Model")
st.markdown("Five model ablation (Logistic Regression variants + XGBoost) trained on 2024, tested on 2025, plus Bradley-Terry strength rankings.")

FEATURES_FULL    = ["place","behind_time","frac_remaining","place_x_frac","behind_x_frac"]
FEATURES_NO_GAP  = ["place","frac_remaining","place_x_frac"]

# ═══════════════════════════════════════════════════════════════════════════════
# Build model base + fit all models (cached)
# ═══════════════════════════════════════════════════════════════════════════════
@st.cache_data
def build_model_data(_df):
    base = _df.dropna(subset=["lap","lap_time","place","finish_position","race_id"]).copy()
    base["lap"]             = base["lap"].astype(float)
    base["place"]           = base["place"].astype(float)
    base["behind_time"]     = base["behind_time"].fillna(0).astype(float)
    base["finish_position"] = base["finish_position"].astype(float)
    base["year"]            = base["year"].astype(str)
    base["won_moto"]        = (base["finish_position"] == 1).astype(int)

    max_lap = base.groupby("race_id",observed=True)["lap"].max().rename("max_lap").reset_index()
    base = base.merge(max_lap, on="race_id", how="left")
    base["frac_remaining"] = (base["max_lap"] - base["lap"]) / base["max_lap"]
    base["place_x_frac"]   = base["place"]       * base["frac_remaining"]
    base["behind_x_frac"]  = base["behind_time"] * base["frac_remaining"]
    return base

@st.cache_data
def fit_all_models(_base):
    all_feats = FEATURES_FULL + ["won_moto"]
    train = _base[_base["year"]=="2024"].dropna(subset=all_feats).copy()
    test  = _base[_base["year"]=="2025"].dropna(subset=all_feats).copy()

    y_train, y_test = train["won_moto"].values, test["won_moto"].values

    scaler = StandardScaler()
    Xt = scaler.fit_transform(train[FEATURES_FULL].values)
    Xs = scaler.transform(test[FEATURES_FULL].values)

    scaler3 = StandardScaler()
    Xt3 = scaler3.fit_transform(train[FEATURES_NO_GAP].values)
    Xs3 = scaler3.transform(test[FEATURES_NO_GAP].values)

    results  = {}
    prob_cols = {}

    def run_lr(name, X_tr, X_te, y_tr, y_te, **kwargs):
        m = LogisticRegression(max_iter=1000, random_state=42, **kwargs)
        m.fit(X_tr, y_tr)
        p_tr = m.predict_proba(X_tr)[:,1]
        p_te = m.predict_proba(X_te)[:,1]
        results[name] = dict(
            train_auc=round(roc_auc_score(y_tr,p_tr),4),
            test_auc=round(roc_auc_score(y_te,p_te),4),
            train_brier=round(brier_score_loss(y_tr,p_tr),4),
            test_brier=round(brier_score_loss(y_te,p_te),4),
        )
        fp,mp = calibration_curve(y_te,p_te,n_bins=10,strategy="quantile")
        coef = pd.DataFrame({"Feature":FEATURES_FULL if X_tr.shape[1]==5 else FEATURES_NO_GAP,
                             "Coefficient":m.coef_[0].round(4),
                             "Odds Ratio":np.exp(m.coef_[0]).round(4)}).sort_values("Coefficient",ascending=False).reset_index(drop=True)
        coef.index+=1
        results[name]["cal_frac_pos"] = fp.tolist()
        results[name]["cal_mean_pred"] = mp.tolist()
        results[name]["coef"] = coef
        return p_tr, p_te

    # Model 1 — LR balanced
    p1tr,p1te = run_lr("Model 1 — LR balanced", Xt,Xs,y_train,y_test, class_weight="balanced")
    # Model 2 — LR no weighting
    p2tr,p2te = run_lr("Model 2 — LR no weighting", Xt,Xs,y_train,y_test)
    # Model 3 — LR no gap features
    p3tr,p3te = run_lr("Model 3 — LR no gap features", Xt3,Xs3,y_train,y_test)
    # Model 4 — Platt scaling
    m4 = CalibratedClassifierCV(LogisticRegression(max_iter=1000,random_state=42),method="sigmoid",cv=5)
    m4.fit(Xt,y_train)
    p4tr=m4.predict_proba(Xt)[:,1]; p4te=m4.predict_proba(Xs)[:,1]
    fp4,mp4=calibration_curve(y_test,p4te,n_bins=10,strategy="quantile")
    results["Model 4 — LR + Platt scaling"]=dict(
        train_auc=round(roc_auc_score(y_train,p4tr),4),test_auc=round(roc_auc_score(y_test,p4te),4),
        train_brier=round(brier_score_loss(y_train,p4tr),4),test_brier=round(brier_score_loss(y_test,p4te),4),
        cal_frac_pos=fp4.tolist(),cal_mean_pred=mp4.tolist(),coef=None)
    # Model 5 — XGBoost
    m5=xgb.XGBClassifier(n_estimators=300,max_depth=4,learning_rate=0.05,
        subsample=0.8,colsample_bytree=0.8,eval_metric="auc",random_state=42,verbosity=0)
    m5.fit(Xt,y_train,eval_set=[(Xt,y_train),(Xs,y_test)],verbose=False)
    p5tr=m5.predict_proba(Xt)[:,1]; p5te=m5.predict_proba(Xs)[:,1]
    fp5,mp5=calibration_curve(y_test,p5te,n_bins=10,strategy="quantile")
    lc=m5.evals_result()
    imp=pd.DataFrame({"Feature":FEATURES_FULL,"Importance":m5.feature_importances_.round(4)}).sort_values("Importance",ascending=False).reset_index(drop=True)
    imp.index+=1
    results["Model 5 — XGBoost"]=dict(
        train_auc=round(roc_auc_score(y_train,p5tr),4),test_auc=round(roc_auc_score(y_test,p5te),4),
        train_brier=round(brier_score_loss(y_train,p5tr),4),test_brier=round(brier_score_loss(y_test,p5te),4),
        cal_frac_pos=fp5.tolist(),cal_mean_pred=mp5.tolist(),
        lc_train=lc["validation_0"]["auc"],lc_test=lc["validation_1"]["auc"],
        importance=imp,coef=None)

    # Store win_prob columns per model on test rows
    prob_cols = {
        "Model 1 — LR balanced":     (train["race_id"].values,train["name"].values,train["lap"].values,p1tr,
                                        test["race_id"].values,test["name"].values,test["lap"].values,p1te),
        "Model 2 — LR no weighting":  (train["race_id"].values,train["name"].values,train["lap"].values,p2tr,
                                        test["race_id"].values,test["name"].values,test["lap"].values,p2te),
        "Model 3 — LR no gap features":(train["race_id"].values,train["name"].values,train["lap"].values,p3tr,
                                        test["race_id"].values,test["name"].values,test["lap"].values,p3te),
        "Model 4 — LR + Platt scaling":(train["race_id"].values,train["name"].values,train["lap"].values,p4tr,
                                        test["race_id"].values,test["name"].values,test["lap"].values,p4te),
        "Model 5 — XGBoost":          (train["race_id"].values,train["name"].values,train["lap"].values,p5tr,
                                        test["race_id"].values,test["name"].values,test["lap"].values,p5te),
    }
    return results, prob_cols

@st.cache_data
def build_bt_rankings(_df):
    base = _df.dropna(subset=["lap","place","race_id"]).copy()
    base["lap"]   = base["lap"].astype(float)
    base["place"] = base["place"].astype(float)
    base["year"]  = base["year"].astype(str)

    def build_pairwise(data):
        rows=[]
        for (_,__), grp in data.groupby(["race_id","lap"],observed=True):
            riders=grp[["name","place"]].dropna().values.tolist()
            if len(riders)<2: continue
            for (n1,p1),(n2,p2) in combinations(riders,2):
                if p1<p2: rows.append({"winner":n1,"loser":n2})
                elif p2<p1: rows.append({"winner":n2,"loser":n1})
        return pd.DataFrame(rows)

    def fit_bt(pairs,n_iter=500,tol=1e-8):
        riders=sorted(set(pairs["winner"])|set(pairs["loser"]))
        n=len(riders); idx={r:i for i,r in enumerate(riders)}
        wins=np.zeros(n); cc=np.zeros((n,n))
        for _,row in pairs.iterrows():
            i,j=idx[row["winner"]],idx[row["loser"]]
            wins[i]+=1; cc[i,j]+=1; cc[j,i]+=1
        s=np.ones(n)
        for _ in range(n_iter):
            old=s.copy()
            for i in range(n):
                d=sum(cc[i,j]/(s[i]+s[j]) for j in range(n) if i!=j and cc[i,j]>0)
                if d>0: s[i]=wins[i]/d
            s=s/s.sum()*n
            if np.max(np.abs(s-old))<tol: break
        return {r:s[idx[r]] for r in riders}

    bt={}
    for yr in ["2024","2025"]:
        pairs=build_pairwise(base[base["year"]==yr])
        bt[yr]=fit_bt(pairs)
    return bt, base

with st.spinner("Fitting models… (first load only)"):
    model_base  = build_model_data(df)
    results, prob_cols = fit_all_models(model_base)
    bt_strengths, bt_base = build_bt_rankings(df)

# ═══════════════════════════════════════════════════════════════════════════════
# 17a. Model comparison table
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Model comparison")

cmp_rows=[]
for name,r in results.items():
    cmp_rows.append({"Model":name,"Train AUC":r["train_auc"],"Test AUC":r["test_auc"],
                     "Train Brier":r["train_brier"],"Test Brier":r["test_brier"]})
st.dataframe(pd.DataFrame(cmp_rows), hide_index=True, use_container_width=False)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 17b. Per-model detail: calibration + coefficients/importance
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Model detail")

model_names = list(results.keys())
sel_model   = st.selectbox("Select model", model_names, index=1)
r           = results[sel_model]

col_a, col_b = st.columns(2)

with col_a:
    st.markdown("**Calibration plot (test set 2025)**")
    fig_cal=go.Figure()
    fig_cal.add_trace(go.Scatter(x=r["cal_mean_pred"],y=r["cal_frac_pos"],mode="lines+markers",
        name=sel_model,line=dict(color="#1A7FE8",width=2),marker=dict(size=8),
        hovertemplate="Predicted: %{x:.2f}<br>Actual: %{y:.2f}<extra></extra>"))
    fig_cal.add_trace(go.Scatter(x=[0,1],y=[0,1],mode="lines",name="Perfect",
        line=dict(color="grey",width=1,dash="dash")))
    fig_cal.update_layout(xaxis=dict(title="Mean Predicted Prob",range=[0,1]),
        yaxis=dict(title="Fraction of Positives",range=[0,1]),
        height=380,margin=dict(l=60,r=40,t=30,b=60),legend=dict(title=""))
    st.plotly_chart(fig_cal,use_container_width=True)

with col_b:
    if r.get("coef") is not None:
        st.markdown("**Feature coefficients**")
        st.dataframe(r["coef"], use_container_width=False)
    elif sel_model=="Model 5 — XGBoost" and r.get("importance") is not None:
        st.markdown("**Feature importances**")
        st.dataframe(r["importance"], use_container_width=False)
        st.markdown("**Learning curve**")
        lc_tr,lc_te=r["lc_train"],r["lc_test"]
        best_t=lc_te.index(max(lc_te))+1
        fig_lc=go.Figure()
        fig_lc.add_trace(go.Scatter(x=list(range(1,len(lc_tr)+1)),y=lc_tr,mode="lines",name="Train AUC",line=dict(color="#1A7FE8",width=2)))
        fig_lc.add_trace(go.Scatter(x=list(range(1,len(lc_te)+1)),y=lc_te,mode="lines",name="Test AUC",line=dict(color="#E8641A",width=2)))
        fig_lc.add_vline(x=best_t,line_dash="dash",line_color="grey",
            annotation_text=f"Best: {max(lc_te):.4f} (tree {best_t})",annotation_position="top right")
        fig_lc.update_layout(xaxis=dict(title="Trees"),yaxis=dict(title="AUC",range=[0.95,1.0]),
            height=300,margin=dict(l=60,r=40,t=30,b=60),legend=dict(title=""))
        st.plotly_chart(fig_lc,use_container_width=True)
    else:
        st.markdown("**Note:** Platt scaling (cv=5) fits 5 separate base models — no single coefficient table.")

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 17c. Interactive win-probability viewer
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Win probability viewer")

viewer_base = model_base.copy()
viewer_base["round"] = viewer_base["round"].astype(str)
viewer_base["moto"]  = viewer_base["moto"].astype(str)
viewer_base["year"]  = viewer_base["year"].astype(str)

all_years   = sorted(viewer_base["year"].unique())
all_classes = ["450","250","WMX"]
all_rounds  = sorted(viewer_base["round"].unique(), key=int)
all_motos   = sorted(viewer_base["moto"].unique(),  key=int)

c1,c2,c3,c4,c5 = st.columns(5)
with c1: vyr   = st.selectbox("Year",  all_years,   index=len(all_years)-1, key="wp_yr")
with c2: vcls  = st.selectbox("Class", all_classes, key="wp_cls")
with c3: vrnd  = st.selectbox("Round", all_rounds,  key="wp_rnd")
with c4: vmto  = st.selectbox("Moto",  all_motos,   key="wp_mto")
with c5: vmod  = st.selectbox("Model", model_names, index=1, key="wp_mod")

mask = ((viewer_base["year"]==vyr)&(viewer_base["class_label"]==vcls)&
        (viewer_base["round"]==vrnd)&(viewer_base["moto"]==vmto))
elig = sorted(viewer_base[mask]["name"].dropna().unique())
vrdr = st.selectbox("Rider", elig if elig else ["— no data —"], key="wp_rdr")

if st.button("Update — win probability", type="primary", key="wp_btn") and elig:
    # Attach win_prob for chosen model
    pcols = prob_cols[vmod]
    tr_idx=pd.DataFrame({"race_id":pcols[0],"name":pcols[1],"lap":pcols[2],"win_prob":pcols[3]})
    te_idx=pd.DataFrame({"race_id":pcols[4],"name":pcols[5],"lap":pcols[6],"win_prob":pcols[7]})
    wp_map=pd.concat([tr_idx,te_idx])

    moto_data = viewer_base[mask].merge(wp_map,on=["race_id","name","lap"],how="left")
    rider_data = moto_data[moto_data["name"]==vrdr].sort_values("lap")

    if rider_data.empty:
        st.warning(f"No data for {vrdr}.")
    else:
        fig=go.Figure()
        for other in moto_data["name"].unique():
            if other==vrdr: continue
            od=moto_data[moto_data["name"]==other].sort_values("lap")
            if od["win_prob"].isna().all(): continue
            fig.add_trace(go.Scatter(x=od["lap"],y=od["win_prob"],mode="lines",
                line=dict(color="lightgrey",width=1),showlegend=False,hoverinfo="skip"))

        fig.add_trace(go.Scatter(x=rider_data["lap"],y=rider_data["win_prob"],mode="lines+markers",
            name=vrdr,line=dict(color="#1A7FE8",width=3),marker=dict(size=7),
            customdata=rider_data[["place","behind_time","frac_remaining"]].values,
            hovertemplate=(f"<b>{vrdr}</b><br>Lap: %{{x}}<br>Win prob: %{{y:.1%}}<br>"
                           "Place: %{customdata[0]:.0f}<br>Gap to leader: %{customdata[1]:.2f}s<br>"
                           "Race remaining: %{customdata[2]:.1%}<extra></extra>")))

        af=int(rider_data["finish_position"].iloc[0])
        sfx="st" if af==1 else "nd" if af==2 else "rd" if af==3 else "th"
        fig.add_annotation(x=rider_data["lap"].max(),
            y=rider_data["win_prob"].dropna().iloc[-1] if rider_data["win_prob"].notna().any() else 0,
            text=f"  Finished {af}{sfx}",showarrow=False,font=dict(color="#1A7FE8",size=11),xanchor="left")

        track=rider_data["track"].iloc[0]
        fig.update_layout(title=f"Win Prob — {vrdr} | {vcls} | {vyr} | Rd {vrnd} | Moto {vmto} | {track} | {vmod}",
            xaxis=dict(title="Lap",dtick=1),
            yaxis=dict(title="Win Probability",range=[0,1],tickformat=".0%"),
            height=500,margin=dict(l=60,r=60,t=60,b=60),
            legend=dict(title=""),hovermode="x unified")
        st.plotly_chart(fig,use_container_width=True)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 17d. Bradley-Terry strength rankings
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Bradley-Terry strength rankings")
st.caption("Pairwise comparisons at every lap of every race. Strength > 1.0 = stronger than average. Within-class rankings only — cross-class comparisons are not meaningful.")

c1,c2 = st.columns(2)
with c1: bt_yr  = st.selectbox("Year",  ["2024","2025"], index=1, key="bt_yr")
with c2: bt_cls = st.selectbox("Class", ["450","250","WMX"], key="bt_cls")

if st.button("Update — Bradley-Terry", type="primary", key="bt_btn"):
    strengths = bt_strengths[bt_yr]
    class_riders = set(bt_base[(bt_base["year"]==bt_yr)&(bt_base["class_label"]==bt_cls)]["name"].unique())

    win_rate = (bt_base[(bt_base["year"]==bt_yr)&(bt_base["class_label"]==bt_cls)]
                .drop_duplicates(subset=["race_id","name"])
                .groupby("name",observed=True)
                .agg(motos=("race_id","count"),wins=("finish_position",lambda x:(x==1).sum()))
                .reset_index())
    win_rate["win_rate"] = (win_rate["wins"]/win_rate["motos"]).round(3)

    subset = (pd.DataFrame([{"name":r,"bt_strength":round(s,4)} for r,s in strengths.items() if r in class_riders])
              .sort_values("bt_strength",ascending=False)
              .merge(win_rate,on="name",how="left")
              .head(30).reset_index(drop=True))
    subset.index+=1

    fig=go.Figure(go.Bar(y=subset["name"],x=subset["bt_strength"],orientation="h",
        marker=dict(color=subset["bt_strength"],colorscale="Blues",showscale=False),
        customdata=subset[["win_rate","wins","motos"]].values,
        hovertemplate="<b>%{y}</b><br>BT Strength: %{x:.4f}<br>Win Rate: %{customdata[0]:.1%}<br>Wins: %{customdata[1]:.0f} / %{customdata[2]:.0f}<extra></extra>"))
    fig.update_layout(title=f"Bradley-Terry Rankings — {bt_cls} | {bt_yr}",
        xaxis=dict(title="BT Strength"),yaxis=dict(title="",categoryorder="total ascending"),
        height=max(400,len(subset)*22),margin=dict(l=180,r=60,t=60,b=60),hovermode="closest")
    st.plotly_chart(fig,use_container_width=True)

st.divider()
st.markdown("""
**Model notes**
- Train: 2024 data. Test: 2025 data. Features: `place`, `behind_time`, `frac_remaining` + interaction terms.
- **Model 1** uses `class_weight="balanced"` — inflates win_prob, hurts Brier/calibration.
- **Model 2** removes class weighting — probabilities track the true ~5–8% win rate. Best calibration baseline.
- **Model 3** drops `behind_time` and its interaction — tests whether gap-to-leader adds real signal beyond place.
- **Model 4** adds Platt scaling (cv=5 sigmoid) on top of Model 2's setup.
- **Model 5** is XGBoost with no scale_pos_weight reweighting. Learning curve shows train/test AUC divergence.
- **Bradley-Terry** is a descriptive ranking, not a predictive model — no AUC/Brier applies. Cross-class strength comparisons are not meaningful since riders never race each other across classes.
""")
