(:raw-claims
 ((:claim
   "自動遮断装置がある場合、C種接地工事の接地抵抗値は500Ω以下に緩和される"
   :kind number :candidates ("cand-A"))
  (:claim
   "自動遮断装置がある場合、C種接地工事の接地抵抗値は100Ω以下に緩和される"
   :kind number :candidates ("cand-B")))
 :annotated
 ((:claim
   "自動遮断装置がある場合、C種接地工事の接地抵抗値は500Ω以下に緩和される"
   :kind number :candidates ("cand-A") :verdict confirmed :evidence
   "/home/madblack-21/Cowork/Notes/node/20250518222320-接地抵抗測定.org:125 — KB該当箇所(20250518222320-接地抵抗測定.org:125)が主張と完全一致し、電技解釈上もC種接地工事は0.5秒以内遮断装置設置時に500Ω以下へ緩和される既知の規定と整合する。")
  (:claim
   "自動遮断装置がある場合、C種接地工事の接地抵抗値は100Ω以下に緩和される"
   :kind number :candidates ("cand-B") :verdict refuted :evidence
   "/home/madblack-21/Cowork/Notes/node/20250518222320-接地抵抗測定.org:125 — ローカルKBの証拠(20250518222320-接地抵抗測定.org:125)では自動遮断装置施設時のC種接地抵抗値は500Ω以下であり、主張の100Ωと矛盾する。"))
 :fallback nil)
