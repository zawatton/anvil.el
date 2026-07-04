(:raw-claims
 ((:claim
   "B種接地抵抗は公式R=150/Iを適用して1線地絡電流で除算し算出される。"
   :kind code :candidates ("cand-A"))
  (:claim "係数150をそのまま接地抵抗の上限値として扱うべきである。"
	  :kind code :candidates ("cand-B"))
  (:claim "前記公式を適用した場合のB種接地抵抗の最大値は15Ωである。"
	  :kind number :candidates ("cand-A"))
  (:claim "接地抵抗の最大値は150Ωである。" :kind number :candidates
	  ("cand-B"))
  (:claim
   "係数150を適用した場合の割り算の最終結果について再計算確認が必要である。"
   :kind fact :candidates ("cand-C")))
 :annotated
 ((:claim
   "B種接地抵抗は公式R=150/Iを適用して1線地絡電流で除算し算出される。"
   :kind code :candidates ("cand-A") :verdict confirmed :evidence
   "/home/madblack-21/Cowork/Notes/node/20250321220701-無人航空機操縦士.org:428 — 電技解釈第17条により遮断時間の区分は1秒以内→600/I、1秒超2秒以内→300/I、それ以外(2秒超を含む)→150/Iであり、本問の「3秒以内」は2秒を超えるため「それ以外」の区分に該当し、R=150/I=150/10=15Ωの適用は妥当。")
  (:claim "係数150をそのまま接地抵抗の上限値として扱うべきである。"
	  :kind code :candidates ("cand-B") :verdict refuted :evidence
	  "/home/madblack-21/Cowork/Notes/node/20250816213143-chatgptプロンプトについて.org:276 — 係数150は算定式の分子に過ぎず、B種接地抵抗の最大値は「係数÷1線地絡電流」(150/10=15Ω)であって、係数150をそのまま抵抗値とするのは誤り。")
  (:claim "前記公式を適用した場合のB種接地抵抗の最大値は15Ωである。"
	  :kind number :candidates ("cand-A") :verdict confirmed
	  :evidence
	  "/home/madblack-21/Cowork/Notes/node/20251116075705-64g_地絡過電圧(ovgr).org:33 — 電技解釈第17条により、1秒以内なら600/I、1秒超2秒以内なら300/Iだが、3秒以内はどちらにも該当せず一般式150/Iが適用されるため、150/10A=15Ωで計算は正しい(ローカルKB証拠は無関係だが規格の確実な知識で裏付けられる)。")
  (:claim "接地抵抗の最大値は150Ωである。" :kind number :candidates
	  ("cand-B") :verdict refuted :evidence
	  "/home/madblack-21/Cowork/Notes/node/20250518222320-接地抵抗測定.org:111 — B種接地抵抗の一般式はR=150/Ig[Ω]であり、Ig=10Aのとき最大値は150÷10=15Ωであって150Ωではない(定数と最終値の混同と考えられる)。")
  (:claim
   "係数150を適用した場合の割り算の最終結果について再計算確認が必要である。"
   :kind fact :candidates ("cand-C") :verdict refuted :evidence
   "/home/madblack-21/Cowork/Notes/node/20250321220701-無人航空機操縦士.org:262 — 150÷10=15Ωは単純な算術であり再計算確認を要する余地はなく、提示された証拠(ドローン関連規定)も本主張と無関係で裏付けにならない。"))
 :fallback nil)
