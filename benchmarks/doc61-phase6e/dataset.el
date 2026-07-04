;;; dataset.el --- Doc 61 Phase 6e unsaturated eval battery  -*- lexical-binding: t; -*-

;; Doc 61 (docs/design/61-fusion-verify.org) Phase 6e: measure whether the
;; Phase 6d verifier-grounded judge eliminates the "judge adopts a
;; hallucinated candidate" failure mode recorded in the archived eval
;; dev/_archive/anvil-fusion/eval/results-panel-compare-2026-06-16.org
;; (a sovereign panel answered 100 MOhm where a candidate had already
;; supplied the correct 0.1 MOhm -- the judge picked the more confident
;; prose over the verified truth).  This file is DATA ONLY: no runner,
;; no live LLM calls.  See README.org in this directory for run
;; conditions, scoring rules, and the KB-verification table.
;;
;; Two tiers:
;;
;; Tier A (`anvil-bench-61-6e-tier-a-cases', 10 cases) -- controlled
;; judge-only scenarios.  Each simulates a COMPLETED fan-out with a
;; PLANTED disagreement: one candidate asserts the correct value/fact,
;; another asserts a plausible hallucination in the same shape as the
;; 2026-06-16 failure (confident prose, wrong number).  Shape:
;;
;;   :id           string
;;   :question     string   -- the original panel question
;;   :candidates   list of slim candidate plists, the shape
;;                 `anvil-orchestrator-collect' returns:
;;                 (:name STRING :provider claude :status done
;;                  :summary STRING).  2-3 per case.
;;   :gold-correct regexp   -- matches the correct assertion in a
;;                 final (judge/verified) answer
;;   :gold-wrong   regexp   -- matches the hallucinated assertion;
;;                 a final answer matching this (and NOT gold-correct)
;;                 counts as adoption of the hallucination = FAILURE
;;   :kb-expect    t or nil -- whether node/ KB evidence for the
;;                 correct side was confirmed to exist (see the
;;                 KB-verification table in README.org)
;;
;; Mix: 7 Japanese electrical-regulation domain cases (a01-a07, all
;; with confirmed node/ KB hits -- see README.org) + 3 generic
;; API/version/date cases (a08-a10, :kb-expect nil by design).
;;
;; Regexp note: Emacs regexp syntax -- `?' `*' `+' `.' are special
;; unescaped, `\|' `\(...\)' `\{m,n\}' need the backslash for their
;; special meaning.  Every :gold-correct / :gold-wrong pair below was
;; checked by hand for the "does the wrong regexp accidentally match a
;; substring of the correct text" trap (e.g. "4MΩ" would falsely hit
;; inside "0.4MΩ" -- avoided throughout).  Scoring precedence for the
;; rare case where a well-formed answer legitimately mentions both
;; (e.g. "TCPではなくUDP" for a08) is defined in README.org: a
;; gold-correct match always takes precedence over a gold-wrong match.
;;
;; Tier B (`anvil-bench-61-6e-tier-b-cases', 6 cases) -- live
;; end-to-end questions, hard enough that a single strong model is
;; plausibly below ceiling.  Shape:
;;
;;   :id          string
;;   :question    string
;;   :gold        list of (:key REGEXP :desc STRING) -- 2-4 per case,
;;                each scored independently (recall metric, not an
;;                AND-gate); regexes are phrasing-robust
;;   :difficulty  string -- one-line note on why this is hard + KB
;;                coverage status for the gold facts
;;
;; Mix: b01-b04 are multi-constraint 電気管理実務 questions that each
;; require combining two separate regulatory facts/tables (harder than
;; single-lookup Tier A claims); b05-b06 are generic multi-hop
;; reasoning (no domain knowledge, no KB expected).
;;
;; To add a case: append to the relevant defconst, keep :id
;; monotonically increasing within its tier.

;;; Code:

(defconst anvil-bench-61-6e-tier-a-cases
  '(;; --- a01: THE ARCHETYPE -- literal reproduction of the 2026-06-16
    ;; failure shape (0.1 MOhm correct vs 100 MOhm confidently
    ;; hallucinated, 1000x order-of-magnitude inflation).
    ;; KB: CONFIRMED, 3 hits, first :source
    ;; node/20250521090644-年次点検.org:165
    (:id "a01"
     :question "低圧電路で対地電圧が150V以下の場合、電気設備技術基準が定める絶縁抵抗の最小値は何MΩですか。根拠条文も含めて簡潔に説明してください。"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "対地電圧150V以下の低圧電路では、電気設備技術基準を定める省令第58条により絶縁抵抗値は0.1MΩ以上が求められます。一般的な100V単相回路の分岐回路がこの区分に該当します。")
      (:name "cand-B" :provider claude :status done
       :summary "対地電圧150V以下の低圧回路は、感電・漏電事故を防ぐために非常に高い絶縁水準が要求されており、基準値は100MΩ以上です。高圧回路に準じた厳格な水準として省令に定められています。")
      (:name "cand-C" :provider claude :status done
       :summary "低圧電路の絶縁抵抗基準は対地電圧の区分によって変わります。区分ごとの正確な数値は資料により表記が揺れることがあるため、現場では絶縁抵抗計で実測して基準を満たすか確認するのが確実です。"))
     :gold-correct "0\\.1 ?MΩ"
     :gold-wrong "100 ?MΩ"
     :kb-expect t)

    ;; a02: adjacent-band confusion (0.2 MOhm band mistaken for the
    ;; 0.1 MOhm band immediately below it).
    ;; KB: CONFIRMED, 3 hits, first :source
    ;; node/20250521090644-年次点検.org:165
    (:id "a02"
     :question "低圧電路で対地電圧が150Vを超え300V以下の場合、電気設備技術基準が定める絶縁抵抗の最小値は何MΩですか。"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "対地電圧が150Vを超え300V以下の低圧電路では、絶縁抵抗値は0.2MΩ以上が電技省令第58条の基準です。対地電圧100V区分の0.1MΩより一段厳しい値になります。")
      (:name "cand-B" :provider claude :status done
       :summary "この区分でも基準値は0.1MΩ以上で足ります。150V以下の区分と同じ扱いで問題ありません。"))
     :gold-correct "0\\.2 ?MΩ"
     :gold-wrong "0\\.1 ?MΩ"
     :kb-expect t)

    ;; a03: second order-of-magnitude-inflation case (same failure
    ;; shape as a01, different voltage band) -- tests whether the
    ;; fix generalizes rather than only patching the one memorized
    ;; number.
    ;; KB: CONFIRMED, 3 hits, first :source
    ;; node/20250521090644-年次点検.org:165 (explicit "0.1/0.2/0.4MΩ"
    ;; text at node/20250518222708-絶縁抵抗測定.org:1286)
    (:id "a03"
     :question "低圧電路で使用電圧が300Vを超える場合、電気設備技術基準が定める絶縁抵抗の最小値は何MΩですか。"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "使用電圧300Vを超える低圧電路の絶縁抵抗基準は0.4MΩ以上です。電技省令第58条の三段階区分のうち最も厳しい値になります。")
      (:name "cand-B" :provider claude :status done
       :summary "300Vを超える低圧電路は感電リスクが特に高いため、基準値は40MΩ以上とかなり高い水準に設定されています。実務では絶縁抵抗計のレンジを上げて測定する必要があります。")
      (:name "cand-C" :provider claude :status done
       :summary "電圧区分が上がるほど絶縁抵抗の要求値も上がる、という大枠は正しいですが、具体的な数値は電技省令の表を都度確認するのが確実です。"))
     :gold-correct "0\\.4 ?MΩ"
     :gold-wrong "40 ?MΩ"
     :kb-expect t)

    ;; a04: miscalculation-shaped hallucination (forgot to divide by
    ;; the fault current, off by exactly the multiplier).
    ;; KB: CONFIRMED, 3 hits, first :source
    ;; node/20250518222320-接地抵抗測定.org:111
    (:id "a04"
     :question "高圧受電設備の変圧器の高圧側電路で1線地絡電流が10Aであり、地絡を3秒以内に自動的に遮断する装置が施設されている場合、この変圧器のB種接地工事の接地抵抗の最大値は何Ωですか。計算過程も示してください。"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "B種接地工事の接地抵抗値はR=150/I(1線地絡電流A)で求めます。3秒以内に自動遮断する装置がある場合は係数150を使うので、R=150/10=15Ωが最大値です。")
      (:name "cand-B" :provider claude :status done
       :summary "1線地絡電流が10Aなので、B種接地工事の接地抵抗の最大値は150Ωです。係数150をそのまま接地抵抗の上限として扱います。")
      (:name "cand-C" :provider claude :status done
       :summary "B種接地抵抗は地絡電流と遮断時間の組合せで係数(150/300/600)が変わります。今回は自動遮断装置ありなので係数150を使うのは合っていますが、割り算の最終結果は再計算して確認してください。"))
     :gold-correct "15 ?Ω"
     :gold-wrong "150 ?Ω"
     :kb-expect t)

    ;; a05: cross-class value swap (C種 exception value confused
    ;; with D種's generic baseline value).
    ;; KB: CONFIRMED, 3 hits, first :source
    ;; node/20250518222320-接地抵抗測定.org:125
    (:id "a05"
     :question "低圧電路で地絡を生じた場合に0.5秒以内に電路を自動的に遮断する装置を施設するとき、C種接地工事の接地抵抗値は何Ω以下にできますか。"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "0.5秒以内に自動遮断する装置を施設する場合、C種接地工事の接地抵抗値は500Ω以下に緩和できます。装置が無い場合の原則10Ω以下より大幅に緩い基準です。")
      (:name "cand-B" :provider claude :status done
       :summary "自動遮断装置がある場合、C種接地工事の接地抵抗値はD種と同じ100Ω以下に緩和されます。C種とD種で緩和後の基準値がそろう形です。"))
     :gold-correct "500 ?Ω"
     :gold-wrong "100 ?Ω"
     :kb-expect t)

    ;; a06: cross-class value swap in the other direction (A種の
    ;; 特定例2Ωが、より馴染みのあるC種原則値10Ωに置き換わる).
    ;; KB: CONFIRMED, 3 hits, first :source
    ;; node/20250518222320-接地抵抗測定.org:132
    (:id "a06"
     :question "非接地式高圧電路に施設する機械器具等に対するA種接地工事について、建物の鉄骨その他の金属体を接地極として使用できるのは、大地との抵抗値が何Ω以下のときですか。"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "建物の鉄骨その他の金属体は、大地との抵抗値が2Ω以下であれば、非接地式高圧電路に施設する機械器具等に施すA種接地工事の接地極として使用できます。")
      (:name "cand-B" :provider claude :status done
       :summary "A種接地工事はC種と同様の原則値である10Ω以下が基準なので、建物の鉄骨も抵抗値10Ω以下であれば接地極として利用可能です。"))
     :gold-correct "2 ?Ω"
     :gold-wrong "10 ?Ω"
     :kb-expect t)

    ;; a07: adjacent-column swap in the same reference table (5%タップ
    ;; の動作電圧が、隣の列の2%タップ値に置き換わる).
    ;; KB: CONFIRMED, 3 hits, first :source
    ;; node/20250829124859-電気まとめノート.org:90
    (:id "a07"
     :question "高圧受電設備の地絡方向継電器(DGR)で、完全地絡時の対地電位を3810Vとするとき、零相電圧タップを5%に整定した場合の動作電圧は何Vになりますか。"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "零相電圧タップ5%の動作電圧は3810V×5%=190.5Vです。タップ値ごとに動作電圧が変わり、2%なら76.2Vになります。")
      (:name "cand-B" :provider claude :status done
       :summary "零相電圧タップ5%整定時の動作電圧は76.2Vです。タップの数値が小さいほど動作電圧も低くなる単純な比例関係です。")
      (:name "cand-C" :provider claude :status done
       :summary "零相電圧タップの動作電圧は対地電位×タップ比率で決まります。タップ表の値を再確認してから最終回答してください。"))
     :gold-correct "190\\.5 ?V"
     :gold-wrong "76\\.2 ?V"
     :kb-expect t)

    ;; --- Generic (non-domain) cases, :kb-expect nil by design ---

    ;; a08: HTTP/3/QUIC transport (UDP vs TCP) -- plausible because
    ;; every prior HTTP version ran over TCP.
    (:id "a08"
     :question "HTTP/3の下位トランスポート層として使用されるプロトコルはTCPとUDPのどちらですか。簡潔に理由も添えて答えてください。"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "HTTP/3はQUICというトランスポート層プロトコルの上で動作しており、QUICはUDP上に構築されています。TCPのヘッドオブラインブロッキングを回避する目的で選ばれました。")
      (:name "cand-B" :provider claude :status done
       :summary "HTTP/3も従来のHTTPと同様にTCP上で動作します。QUICはTCPの拡張機能として実装されており、信頼性のためTCPの3-way handshakeをそのまま利用しています。"))
     :gold-correct "UDP"
     :gold-wrong "TCP"
     :kb-expect nil)

    ;; a09: semantic versioning -- which segment signals a breaking
    ;; change (MAJOR vs MINOR).
    (:id "a09"
     :question "セマンティックバージョニング(SemVer)で、後方互換性のないAPI変更を行った際にインクリメントすべきなのはMAJOR/MINOR/PATCHのどれですか。"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "後方互換性を壊す変更はMAJORバージョンをインクリメントします。MINORは後方互換な機能追加、PATCHはバグ修正用です。")
      (:name "cand-B" :provider claude :status done
       :summary "後方互換性のない変更はMINORバージョンを上げるのが正しい運用です。破壊的変更であっても既存利用者への影響を小さく見せるため、通常はMINORの範囲で表現するのが実務慣行とされています。"))
     :gold-correct "MAJOR"
     :gold-wrong "MINOR"
     :kb-expect nil)

    ;; a10: which ECMAScript edition standardized native Promises
    ;; (ES6/2015 vs ES5/2011).
    (:id "a10"
     :question "JavaScriptのECMAScript仕様でPromiseがネイティブ機能として標準化されたのはどのバージョンですか(西暦年でも可)。"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "PromiseはES6(ECMAScript 2015)で標準化されました。西暦でいうと2015年です。")
      (:name "cand-B" :provider claude :status done
       :summary "Promiseは2011年に策定されたES5(ECMAScript 5.1)の時点で既に標準機能として含まれていました。"))
     :gold-correct "ES6\\|ECMAScript ?2015\\|2015年"
     :gold-wrong "ES5\\|2011年"
     :kb-expect nil))
  "Doc 61 Phase 6e Tier A -- controlled judge-only planted-disagreement cases.
10 cases (a01-a10): a01-a07 are Japanese electrical-regulation domain
cases with confirmed node/ KB coverage (see README.org's
KB-verification table), a08-a10 are generic (:kb-expect nil).
Acceptance target (design doc §2 6e): the verified judge must not
adopt any :gold-wrong value.")

(defconst anvil-bench-61-6e-tier-b-cases
  '(;; b01: multi-hop across TWO separate regulatory tables --
    ;; (i) 外部委託承認の対象規模 (需要設備は受電電圧7000V以下) and
    ;; (ii) 電気主任技術者免状の電圧区分 (第三種は電圧5万V未満).
    ;; KB: (i) CONFIRMED (node/20250915182848-太陽光発電所の点検.org:528
    ;;     "需要設備 | 受電電圧 7000V 以下"); (ii) NOT confirmed in
    ;;     node/ as a standalone "5万V" boundary string -- general
    ;;     domain knowledge, see README.org gap note.
    (:id "b01"
     :question "受電電圧20,000V(20kV)、契約電力800kWの需要設備を新規に保安管理することになりました。この事業場について、(1)電気管理技術者へ保安管理業務を外部委託することは制度上可能か、可能・不可能を明示したうえで判断根拠となる受電電圧の基準値を挙げてください。(2)この事業場の電気主任技術者に必要な免状の種類(第一種・第二種・第三種のいずれか)についても触れてください。"
     :gold ((:key "対象外\\|不可能\\|できません\\|認められません\\|できない"
             :desc "20kVは外部委託対象の規模基準(7000V以下)を超えるため対象外/不可能と正しく判定していること")
            (:key "7,?000 ?V"
             :desc "需要設備の外部委託対象規模基準である受電電圧7000V以下に正しく言及していること")
            (:key "第三種"
             :desc "20kV(5万V未満)なら第三種電気主任技術者で足りるという資格区分に正しく言及していること"))
     :difficulty "multi-hop: 2つの独立した規模基準表(外部委託可否/主任技術者免状区分)を組み合わせる必要がある。(1)はnode/確認済み、(2)の5万V境界はnode/未確認(一般知識での補完が必要、KB gap)")

    ;; b02: single-constraint threshold lookup + correct terminology.
    ;; KB: CONFIRMED (node/20250915182848-太陽光発電所の点検.org:510
    ;; "・最大電力 500kW 未満の需要設備").
    (:id "b02"
     :question "自家用電気工作物である需要設備で、最大電力が480kWの事業場があります。この事業場において、電気主任技術者免状を持たない者を主任技術者として選任することは制度上認められますか。認められる場合はその制度の名称と、認められる最大電力の基準値を含めて説明してください。"
     :gold ((:key "認められ\\|可能\\|できます"
             :desc "480kW<500kW未満なので選任が認められると正しく判定していること")
            (:key "500 ?kW"
             :desc "選任許可の対象規模基準である最大電力500kW未満に正しく言及していること")
            (:key "選任許可"
             :desc "制度名称(選任許可)に正しく言及していること"))
     :difficulty "単一制約の閾値比較だが制度名称まで正確に求める。KB確認済み(node/20250915182848-太陽光発電所の点検.org)")

    ;; b03: niche practical-knowledge multi-part (grounding class +
    ;; number of points).
    ;; KB: CONFIRMED, exact-text hit
    ;; node/20251227000920-リレー試験.org:737
    ;; "CT と VT の二次側端子の1箇所にはD種接地を施す必要がある".
    (:id "b03"
     :question "高圧受電設備に設置される計器用変流器(CT)・計器用変圧器(VT)について、二次側の感電防止のために必要な接地の種類と、施すべき箇所(二次側端子の何箇所か)を説明してください。"
     :gold ((:key "D種"
             :desc "CT/VT二次側に必要な接地の種類がD種であることに正しく言及していること")
            (:key "1 ?\\(?:箇所\\|か所\\|カ所\\)"
             :desc "接地を施す箇所数(二次側端子の1箇所)に正しく言及していること"))
     :difficulty "実務上ニッチな知識で単一モデルが取りこぼしやすい。KB完全一致確認済み(node/20251227000920-リレー試験.org:737)")

    ;; b04: multi-step Ohm's-law calculation with a safety factor
    ;; applied (SOG動作電流整定値からの逆算).
    ;; KB: CONFIRMED, exact-text hit
    ;; node/20250518222708-絶縁抵抗測定.org:1144
    ;; "受電が出来る最小絶縁抵抗値(高圧電路)について (0.06MΩ)".
    (:id "b04"
     :question "高圧受電設備のSOG(地絡保護装置内蔵型高圧交流負荷開閉器)は零相電流整定値0.2Aで動作します。完全地絡時の対地電位を3,810Vとし、安全率3を考慮した場合、受電を継続するために必要な絶縁抵抗のおよその最小値をMΩ単位で求め、計算過程も示してください。"
     :gold ((:key "0\\.0?6 ?MΩ"
             :desc "安全率3を考慮したおよその最小絶縁抵抗値0.06MΩに正しく到達していること")
            (:key "3,?810 ?V"
             :desc "完全地絡時の対地電位3810Vに正しく言及していること"))
     :difficulty "オームの法則の多段計算+安全率の適用が必要で単一モデルでも誤答しうる。KB完全一致確認済み(node/20250518222708-絶縁抵抗測定.org:1144,1180,1186)")

    ;; b05: generic multi-hop reasoning -- inclusion-exclusion count.
    ;; No domain knowledge, no KB expected.
    (:id "b05"
     :question "ある倉庫の荷物は52個あり、そのうち赤色シールが貼られたものが31個、重量10kg以上のものが28個、赤色シールが貼られておりかつ10kg以上でもある荷物が19個です。赤色シールが貼られておらず、かつ重量が10kg未満の荷物は何個ですか。考え方を述べたうえで、最後に個数を明記してください。"
     :gold ((:key "12個\\|12 ?個\\|答えは12\\|= ?12\\b"
             :desc "包除原理を正しく適用した最終個数12個に到達していること (52-(31+28-19)=12)")
            (:key "40"
             :desc "赤色シールまたは10kg以上の荷物の合計40個という中間集計に触れていること (31+28-19=40)"))
     :difficulty "包除原理の2段階適用。単純な足し引きだけだと誤答しやすい (31+28や52-31-28などのショートカットに陥りやすい)。KB対象外(一般論理問題)")

    ;; b06: generic multi-hop reasoning -- modular day-of-week
    ;; arithmetic, two sub-answers required.
    (:id "b06"
     :question "ある年のカレンダーで、6月1日が水曜日であるとします(うるう年ではないものとします)。(1)同じ年の7月4日は何曜日ですか。(2)同じ年の8月19日は何曜日ですか。それぞれ曜日の名前で答えてください。"
     :gold ((:key "月曜"
             :desc "7月4日が月曜日と正しく算出できていること (6/1水曜から33日後、33 mod 7 = 5)")
            (:key "金曜"
             :desc "8月19日が金曜日と正しく算出できていること (6/1水曜から79日後、79 mod 7 = 2)"))
     :difficulty "2段階のmod-7計算が必要でカレンダーツール無しでは単一モデルも取りこぼしうる。KB対象外(一般論理問題)"))
  "Doc 61 Phase 6e Tier B -- live end-to-end questions.
6 cases (b01-b06): b01-b04 are multi-constraint 電気管理実務 questions
each combining two separate regulatory facts, b05-b06 are generic
multi-hop reasoning.  Each case carries 2-3 deterministic gold keys
scored independently (recall metric).  Planned run conditions: single
model / fusion-plain / fusion-verify (see README.org).")

(provide 'anvil-bench-61-6e-dataset)
;;; dataset.el ends here
