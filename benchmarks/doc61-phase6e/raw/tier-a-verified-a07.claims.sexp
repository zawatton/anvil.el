(:raw-claims
 ((:claim "5%タップ整定時の動作電圧は190.5Vである" :kind number
	  :candidates ("cand-A"))
  (:claim "5%タップ整定時の動作電圧は76.2Vである" :kind number
	  :candidates ("cand-B")))
 :annotated
 ((:claim "5%タップ整定時の動作電圧は190.5Vである" :kind number
	  :candidates ("cand-A") :verdict confirmed :evidence
	  "/home/madblack-21/Cowork/Notes/node/20251227000920-リレー試験.org:29 — 3810V×5%=190.5Vと計算が一致し、KB該当行(29行目・73行目)も同じ値を明記している。")
  (:claim "5%タップ整定時の動作電圧は76.2Vである" :kind number
	  :candidates ("cand-B") :verdict refuted :evidence
	  "/home/madblack-21/Cowork/Notes/node/20251227000920-リレー試験.org:71 — 3810V×5%=190.5Vであり、76.2Vは3810V×2%の値(証拠にある2%タップ)と一致するため、5%整定時の動作電圧という主張の数値は誤り。"))
 :fallback nil)
