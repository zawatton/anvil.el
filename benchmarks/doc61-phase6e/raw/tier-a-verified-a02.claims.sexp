(:raw-claims
 ((:claim
   "対地電圧が150Vを超え300V以下の場合、絶縁抵抗の最小値は0.2MΩ以上である"
   :kind number :candidates ("cand-A"))
  (:claim
   "対地電圧が150Vを超え300V以下の場合、絶縁抵抗の最小値は0.1MΩ以上である"
   :kind number :candidates ("cand-B")))
 :annotated
 ((:claim
   "対地電圧が150Vを超え300V以下の場合、絶縁抵抗の最小値は0.2MΩ以上である"
   :kind number :candidates ("cand-A") :verdict confirmed :evidence
   "/home/madblack-21/Cowork/Notes/node/20250520180536-絶縁耐力試験.org:344 — 電気設備技術基準第58条の絶縁抵抗値表において、使用電圧300V以下で対地電圧が150Vを超える場合の絶縁抵抗値は0.2MΩと明確に規定されており、主張と一致する。")
  (:claim
   "対地電圧が150Vを超え300V以下の場合、絶縁抵抗の最小値は0.1MΩ以上である"
   :kind number :candidates ("cand-B") :verdict refuted :evidence
   "/home/madblack-21/Cowork/Notes/node/20250520180536-絶縁耐力試験.org:344 — 電気設備技術基準（第58条）の絶縁抵抗値表では対地電圧150V以下は0.1MΩだが、150Vを超え300V以下の場合は0.2MΩ以上が正しく、主張の0.1MΩは誤り。"))
 :fallback nil)
