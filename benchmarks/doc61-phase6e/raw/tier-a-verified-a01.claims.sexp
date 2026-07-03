(:raw-claims
 ((:claim
   "対地電圧150V以下の低圧電路における絶縁抵抗最小値は0.1MΩである"
   :kind number :candidates ("A"))
  (:claim
   "対地電圧150V以下の低圧電路における絶縁抵抗最小値は100MΩである"
   :kind number :candidates ("B"))
  (:claim
   "低圧電路の絶縁抵抗基準は対地電圧の区分によって異なり、正確な数値は資料により表記が揺れることがある"
   :kind fact :candidates ("C"))
  (:claim "根拠法令は電気設備に関する技術基準を定める省令第58条である"
	  :kind citation :candidates ("A")))
 :annotated
 ((:claim
   "対地電圧150V以下の低圧電路における絶縁抵抗最小値は0.1MΩである"
   :kind number :candidates ("A") :verdict confirmed :evidence
   "/home/madblack-21/Cowork/Notes/node/20250518222320-接地抵抗測定.org:125 — 電気設備技術基準第58条(低圧電路の絶縁性能)は対地電圧150V以下で0.1MΩ以上と定めており、この値は確立された規定値と一致する(ローカル証拠は接地抵抗に関するもので直接の裏付けにはならないが、主張と矛盾もしない)。")
  (:claim
   "対地電圧150V以下の低圧電路における絶縁抵抗最小値は100MΩである"
   :kind number :candidates ("B") :verdict refuted :evidence
   "/home/madblack-21/Cowork/Notes/node/20250518222320-接地抵抗測定.org:125 — 電気設備技術基準第58条では対地電圧150V以下の低圧電路の絶縁抵抗最小値は0.1MΩであり、主張の100MΩは単位・桁が誤り(1000倍過大)。")
  (:claim
   "低圧電路の絶縁抵抗基準は対地電圧の区分によって異なり、正確な数値は資料により表記が揺れることがある"
   :kind fact :candidates ("C") :verdict refuted :evidence
   "/home/madblack-21/Cowork/Notes/node/20250518222708-絶縁抵抗測定.org:1286 — 証拠は対地電圧150V以下の場合0.1MΩ(電技省令第58条)という一意で明確な基準を示しており、数値が資料により揺れるとする根拠は証拠中になく、主張は不当な曖昧化である。")
  (:claim "根拠法令は電気設備に関する技術基準を定める省令第58条である"
	  :kind citation :candidates ("A") :verdict confirmed
	  :evidence
	  "/home/madblack-21/Cowork/Notes/node/20250421212419-電気設備に関する技術基準を定める省令.org:16 — 電気設備技術基準を定める省令第58条は「低圧の電路の絶縁性能」を規定し、対地電圧150V以下の場合の最小絶縁抵抗値0.1MΩを定める条文として広く知られており、確立した知識と整合する（ローカル証拠は高圧に関する第5条止まりで直接の裏付けはないが、低圧絶縁抵抗値を定めるのは第58条であることは確実な知識で裏付けられる）。"))
 :fallback nil)
