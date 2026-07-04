(:raw-claims
 ((:claim
   "後方互換性のない変更はMAJORバージョンをインクリメントすべきである"
   :kind fact :candidates ("cand-A"))
  (:claim
   "後方互換性のない変更はMINORバージョンをインクリメントすべきである"
   :kind fact :candidates ("cand-B")))
 :annotated
 ((:claim
   "後方互換性のない変更はMAJORバージョンをインクリメントすべきである"
   :kind fact :candidates ("cand-A") :verdict confirmed :evidence
   "/home/madblack-21/Cowork/Notes/node/20231105122610-node_jsインストール手順.org:41 — SemVer仕様の明文規定(ルール8)により、後方互換性のないAPI変更時はMAJORをインクリメントすると定められており、確実な知識で裏付けられる(ローカル証拠は無関係)。")
  (:claim
   "後方互換性のない変更はMINORバージョンをインクリメントすべきである"
   :kind fact :candidates ("cand-B") :verdict refuted :evidence
   "/home/madblack-21/Cowork/Notes/node/20231105122610-node_jsインストール手順.org:41 — SemVerの仕様上、後方互換性のないAPI変更はMAJORバージョンをインクリメントすべきであり、MINORではない。"))
 :fallback nil)
