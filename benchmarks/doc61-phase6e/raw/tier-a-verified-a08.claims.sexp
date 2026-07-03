(:raw-claims
 ((:claim "QUICはUDP上に構築されている" :kind fact :candidates
	  ("cand-A"))
  (:claim "QUICはTCPの拡張機能として実装されている" :kind fact
	  :candidates ("cand-B")))
 :annotated
 ((:claim "QUICはUDP上に構築されている" :kind fact :candidates
	  ("cand-A") :verdict confirmed :evidence
	  "/home/madblack-21/Cowork/Notes/node/20260102232558-rune_(rust_vm_for_emacs).org:16 — QUICはIETF標準(RFC 9000/9114)によりUDP上に構築されるトランスポートプロトコルであり、HTTP/3はこのQUICを使用する。")
  (:claim "QUICはTCPの拡張機能として実装されている" :kind fact
	  :candidates ("cand-B") :verdict refuted :evidence
	  "/home/madblack-21/Cowork/Notes/node/20250622235429-nextcloud_構築法_(公式バイナリ版).org:176 — QUICはUDPを下位層として実装された独立トランスポートプロトコルであり、TCPの拡張ではない(証拠にもQUIC/HTTP3に関する裏付けなし)。"))
 :fallback nil)
