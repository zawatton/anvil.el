(:raw-claims
 ((:claim
   "Promiseはネイティブ機能としてES6（ECMAScript 2015）で標準化された"
   :kind fact :candidates ("cand-A"))
  (:claim
   "Promiseはネイティブ機能としてES5（ECMAScript 5.1、2011年）で既に標準機能として含まれていた"
   :kind fact :candidates ("cand-B")))
 :annotated
 ((:claim
   "Promiseはネイティブ機能としてES6（ECMAScript 2015）で標準化された"
   :kind fact :candidates ("cand-A") :verdict confirmed :evidence
   "/home/madblack-21/Cowork/Notes/node/20260102233125-emacs-ng.org:16 — PromiseはECMAScript 2015(ES6)仕様でネイティブのグローバルオブジェクトとして標準化されたことは広く確立された事実であり、確実な知識で裏付けられる(ローカルKBの証拠はEmacs関連で無関係)。")
  (:claim
   "Promiseはネイティブ機能としてES5（ECMAScript 5.1、2011年）で既に標準機能として含まれていた"
   :kind fact :candidates ("cand-B") :verdict refuted :evidence
   "/home/madblack-21/Cowork/Notes/node/20260102233125-emacs-ng.org:16 — Promiseがネイティブ標準機能として仕様化されたのはES2015(ES6)であり、ES5.1(2011)には含まれていない。"))
 :fallback nil)
