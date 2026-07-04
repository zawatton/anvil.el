;;; dataset.el --- Doc 61 section 9 claim-extraction fidelity cases  -*- lexical-binding: t; -*-

;; 8 synthetic Fusion fan-out cases for measuring
;; `anvil-fusion-verify-extract-claims' extraction fidelity across
;; provider/model tiers (Doc 61 section 9: "measure haiku vs sonnet
;; extraction fidelity").  Each case is a plist:
;;
;;   :id          integer  -- stable identifier
;;   :question    string   -- the original Fusion-panel question
;;   :candidates  list of slim candidate plists, the same shape
;;                `anvil-orchestrator-collect' returns:
;;                (:name STRING :provider claude :status done
;;                 :summary STRING).  2-3 per case, with a
;;                deliberately planted disagreement (or, for the
;;                :id 5 case, deliberately NO disagreement).
;;   :gold        list of (:key REGEXP :kind SYMBOL) -- a gold
;;                contested claim counts as recalled when ANY
;;                extracted claim's :claim text matches REGEXP
;;                (case-insensitive).  KIND is one of
;;                fact/number/citation/code, matched against the
;;                recalled claim's own :kind for the kind-accuracy
;;                metric.  Empty :gold means the case has no
;;                contested claims -- the correct extraction output
;;                is the literal NONE sentinel.
;;
;; Mix: cases 1-4 are Japanese electrical-domain (insulation
;; resistance threshold, grounding-work class, protective relay
;; type, 電技解釈 article citation), case 5 is the deliberate
;; no-contested-claims control (JP, tests false-positive behaviour),
;; cases 6-8 are generic English (release date, protocol version,
;; stdlib API name).  :kind is mixed across number/fact/citation,
;; plus one :code pair (case 8) to exercise all four kinds the
;; parser recognizes.  14 gold claims total across the 7 contested
;; cases (2 each); case 5 contributes 0.
;;
;; To add a case: append to `anvil-bench-61-6a-cases', keep `:id'
;; monotonically increasing.

;;; Code:

(defconst anvil-bench-61-6a-cases
  '((:id 1
     :category "jp-insulation-resistance"
     :question "低圧電路(使用電圧100V、対地電圧100V)の絶縁抵抗を測定しました。電気設備技術基準の解釈における合格基準値はいくつですか？"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "対地電圧が150V以下の低圧電路では、絶縁抵抗値は0.1MΩ以上であれば電技解釈第58条の基準を満たします。今回のケース(対地電圧100V)はこの区分に該当します。")
      (:name "cand-B" :provider claude :status done
       :summary "使用電圧300V以下の低圧電路は一律0.2MΩ以上が基準です。今回のケースでも0.2MΩ以上が必要になります。")
      (:name "cand-C" :provider claude :status done
       :summary "対地電圧100Vなら0.1メグオーム以上あれば合格と判定できます。"))
     :gold ((:key "0\\.1 ?MΩ\\|0\\.1 ?メグオーム" :kind number)
            (:key "0\\.2 ?MΩ" :kind number)))

    (:id 2
     :category "jp-grounding-class"
     :question "使用電圧300Vを超える低圧用電動機の金属製外箱に必要な接地工事の種類を教えてください。"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "300Vを超える低圧用機器の金属製外箱にはC種接地工事が必要です。接地抵抗値は10Ω以下が基準です。")
      (:name "cand-B" :provider claude :status done
       :summary "300Vを超える機器でもD種接地工事で足ります。接地抵抗値は100Ω以下です。")
      (:name "cand-C" :provider claude :status done
       :summary "C種接地工事が正しく、抵抗値10Ω以下が原則です(0.5秒以内に地絡を遮断する装置があれば500Ω以下でも可)。"))
     :gold ((:key "C種" :kind fact)
            (:key "D種" :kind fact)))

    (:id 3
     :category "jp-relay-type"
     :question "高圧受電設備で地絡事故を検出するために設置する継電器は何ですか？"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "地絡継電器(GR)を設置し、地絡電流を検出して遮断器を動作させます。")
      (:name "cand-B" :provider claude :status done
       :summary "地絡方向継電器(DGR、67G)が必要です。地絡電流の大きさだけでなく方向も判別して不要動作を防ぎます。")
      (:name "cand-C" :provider claude :status done
       :summary "DGR(地絡方向継電器)を推奨します。GRのみでは方向性が無く、他回路の地絡でも不要動作するリスクがあります。"))
     :gold ((:key "地絡継電器" :kind fact)
            (:key "地絡方向継電器\\|DGR\\|67G" :kind fact)))

    (:id 4
     :category "jp-code-citation"
     :question "低圧電路の絶縁抵抗の判定基準は、電気設備技術基準の解釈の何条に規定されていますか？"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "電技解釈第58条(低圧電路の絶縁性能)に規定されています。")
      (:name "cand-B" :provider claude :status done
       :summary "電技解釈第14条に規定されています。")
      (:name "cand-C" :provider claude :status done
       :summary "第58条が該当します。低圧電路の絶縁性能を定めた条文です。"))
     :gold ((:key "第58条" :kind citation)
            (:key "第14条" :kind citation)))

    (:id 5
     :category "jp-none-control"
     :question "自家用電気工作物の月次点検は、誰が実施しますか？"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "選任された電気主任技術者、またはその指示を受けた者が月次点検を実施します。")
      (:name "cand-B" :provider claude :status done
       :summary "電気主任技術者が自ら行うか、その指揮監督の下で点検員が実施するのが一般的です。")
      (:name "cand-C" :provider claude :status done
       :summary "外部委託の場合も、選任された電気主任技術者の指示のもとで点検業務を担当する実務者が月次点検を行います。"))
     :gold ())

    (:id 6
     :category "en-release-date"
     :question "What year was Python 3.12 released?"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "Python 3.12 was released in October 2023.")
      (:name "cand-B" :provider claude :status done
       :summary "Python 3.12 came out in October 2022.")
      (:name "cand-C" :provider claude :status done
       :summary "Released in October 2023, per the official changelog."))
     :gold ((:key "2023" :kind number)
            (:key "2022" :kind number)))

    (:id 7
     :category "en-protocol-version"
     :question "Which version of the HTTP protocol introduced multiplexed streams over a single TCP connection?"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "HTTP/2 introduced multiplexing over a single TCP connection.")
      (:name "cand-B" :provider claude :status done
       :summary "That capability was introduced in HTTP/1.1 through persistent connections.")
      (:name "cand-C" :provider claude :status done
       :summary "Correct, HTTP/2 added stream multiplexing over one connection."))
     :gold ((:key "HTTP/2" :kind fact)
            (:key "HTTP/1\\.1" :kind fact)))

    (:id 8
     :category "en-api-name"
     :question "In the Python standard library, which module provides the Path class for object-oriented filesystem paths?"
     :candidates
     ((:name "cand-A" :provider claude :status done
       :summary "The `pathlib' module provides the `Path' class.")
      (:name "cand-B" :provider claude :status done
       :summary "It's part of the `os.path' module.")
      (:name "cand-C" :provider claude :status done
       :summary "pathlib.Path is correct -- os.path only has functions, no Path class."))
     :gold ((:key "pathlib" :kind code)
            (:key "os\\.path" :kind code)))))

(provide 'anvil-bench-61-6a-dataset)
;;; dataset.el ends here
