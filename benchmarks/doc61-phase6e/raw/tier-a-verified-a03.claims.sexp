(:raw-claims
 ((:claim
   "使用電圧300Vを超える低圧電路の絶縁抵抗基準値は0.4MΩ以上である"
   :kind number :candidates ("cand-A"))
  (:claim
   "使用電圧300Vを超える低圧電路の絶縁抵抗基準値は40MΩ以上である"
   :kind number :candidates ("cand-B"))
  (:claim
   "電技省令第58条に三段階区分が定められており、0.4MΩはその最も厳しい値である"
   :kind citation :candidates ("cand-A")))
 :annotated
 ((:claim
   "使用電圧300Vを超える低圧電路の絶縁抵抗基準値は0.4MΩ以上である"
   :kind number :candidates ("cand-A") :verdict confirmed :evidence
   "/home/madblack-21/Cowork/Notes/node/20250518222708-絶縁抵抗測定.org:1286 — 電技省令第58条の絶縁抵抗基準は使用電圧300V以下150V以下0.1MΩ・150V超300V以下0.2MΩ・300V超0.4MΩであり、証拠(0.1/0.2/0.4MΩ)と確立した規定内容が主張を裏付ける。")
  (:claim
   "使用電圧300Vを超える低圧電路の絶縁抵抗基準値は40MΩ以上である"
   :kind number :candidates ("cand-B") :verdict refuted :evidence
   "/home/madblack-21/Cowork/Notes/node/20250521090644-年次点検.org:165 — 電気設備技術基準第58条では使用電圧300V超過の低圧電路の絶縁抵抗基準値は0.4MΩ以上であり、40MΩではない（桁が100倍誤り）。")
  (:claim
   "電技省令第58条に三段階区分が定められており、0.4MΩはその最も厳しい値である"
   :kind citation :candidates ("cand-A") :verdict confirmed :evidence
   "/home/madblack-21/Cowork/Notes/node/20250518222708-絶縁抵抗測定.org:1286 — 証拠(1236,1242,1286行)が電技省令第58条に0.1/0.2/0.4MΩの三段階区分を明記しており、300V超は最も高い(=絶縁性能として最も厳しい)0.4MΩに該当し主張と整合する。"))
 :fallback nil)
