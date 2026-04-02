;;; carriage-intent-registry.el --- Intent fragments registry  -*- lexical-binding: t; -*-
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/prompt-profiles-v2.org
;;   spec/parser-registry-v2.org
;;   spec/project-overview-v2.org

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-prompts nil
  "Prompt composition: intent fragments and suite builder."
  :group 'applications)

(defcustom carriage-intent-fragment-overrides nil
  "Alist of overrides for intent prompt fragments.
Each entry is (INTENT . FRAG), where FRAG is either STRING or FUNCTION (CTX → STRING).
Override takes precedence over the registry defaults."
  :type '(alist :key-type symbol :value-type (choice string function))
  :group 'carriage-prompts)

(defcustom carriage-intent-order '(Ask Code Hybrid MultiPatch)
  "Preferred order of intents for UI selectors."
  :type '(repeat symbol)
  :group 'carriage-prompts)

(defcustom carriage-intent-icons
  '((Ask . "❓")
    (Code . "🧩")
    (Hybrid . "🔀")
    (MultiPatch . "⧉"))
  "Alist mapping intent symbol to an icon string for UI."
  :type '(alist :key-type symbol :value-type string)
  :group 'carriage-prompts)

(defvar carriage--intent-fragments (make-hash-table :test 'eq)
  "Registry of intent fragments: INTENT → FRAG (STRING or FUNCTION (CTX → STRING)).")

(defun carriage-intent-register (intent fragment)
  "Register FRAGMENT for INTENT in the intent registry.
FRAGMENT must be a STRING or a FUNCTION of one argument CTX returning STRING."
  (unless (memq (type-of fragment) '(string cons))
    ;; Allow lambdas/closures as functions; `functionp' covers both.
    (unless (functionp fragment)
      (error "Invalid fragment for intent %S: must be string or function" intent)))
  (puthash intent fragment carriage--intent-fragments)
  t)

(defun carriage-intent-get (intent)
  "Return fragment (STRING or FUNCTION) for INTENT with overrides applied.
The caller is responsible for FUNCALL if the result is a function."
  (let* ((ov (assoc-default intent carriage-intent-fragment-overrides)))
    (cond
     (ov ov)
     ((gethash intent carriage--intent-fragments))
     (t (error "Unknown intent: %S" intent)))))

(defun carriage-intent-known ()
  "Return list of known intent symbols in the registry."
  (let (res)
    (maphash (lambda (k _v) (push k res)) carriage--intent-fragments)
    (nreverse res)))

;; -------------------------------------------------------------------
;; Default fragments (English-only by spec; overrides can replace)

(defun carriage--intent-frag-code (ctx)
  "Default fragment for Intent=Code.

CTX MAY carry:
- :suite         — current Suite ('sre|'aibo|'udiff)
- :context-meta  — (:omitted N :limited BOOL)
- :profile       — 'p1 or 'p3 (context profile)
- :doc-scope     — 'all or 'last (doc-context scope)."
  (let* ((suite (and (listp ctx) (plist-get ctx :suite)))
         (meta (and (listp ctx) (plist-get ctx :context-meta)))
         (om (and (listp meta) (plist-get meta :omitted)))
         (lim (and (listp meta) (plist-get meta :limited)))
         (profile (and (listp ctx) (plist-get ctx :profile)))
         (doc-scope (and (listp ctx) (plist-get ctx :doc-scope)))
         (max-files
          (or (and (listp ctx) (plist-get ctx :max-files))
              "?"))
         (max-bytes
          (or (and (listp ctx) (plist-get ctx :max-bytes))
              "?"))
         (suite-note
          (pcase suite
            ((or 'sre 'aibo)
             "Suite/Syntax:
- Suite=sre or Suite=aibo:
  • Use ONLY SRE/AIBO-style patch blocks (no unified diff).
  • For each file, express changes as one or more begin_from/begin_to literal pairs inside the body.
  • NEVER use :from/:to header keys for SRE/AIBO (they are allowed ONLY for :op rename; for all other ops they are forbidden).\n")
            ('udiff
             "Suite/Syntax:
- Suite=udiff:
  • Use ONLY unified diff blocks (:op \"patch\") with ---/+++/@@ hunks.
  • Do NOT emit AIBO/SRE begin_from/begin_to pairs in this mode.\n")
            (_
             "Suite/Syntax:
- Suite not specified: default to SRE/AIBO-style literal edits unless the Suite explicitly requests udiff.\n")))
         (profile-note
          (pcase profile
            ('p3
             (format
              "Context profile:
- P3-debug: extended context for debugging; limits still apply (files<=%s, bytes<=%s).\n"
              max-files max-bytes))
            ('p1
             (format
              "Context profile:
- P1-core: focused minimal context (files<=%s, bytes<=%s). Prefer compact, high-signal edits.\n"
              max-files max-bytes))
            (_ nil)))
         (ctx-limit-note
          (when (or (and (integerp om) (> om 0)) lim)
            (format
             "Context limits:
- Some context items were omitted due to size limits (omitted=%s).
- Do NOT assume you see the entire project; treat missing files as UNKNOWN, not as \"file does not exist\".\n"
             (or om 0))))
         (doc-scope-note
          (pcase doc-scope
            ('last
             "Doc-context scope:
- ONLY the last #+begin_context block in this document is used as the file-path list for the NEXT iteration.
- When you update that block, you MUST keep a FULL list of required paths (not just new additions).\n")
            ('all
             "Doc-context scope:
- ALL #+begin_context blocks in this document contribute paths for the NEXT iteration.\n")
            (_ nil))))
    (concat
     "Output contract (two legal outputs only):\n"
     "A) Org-compatible patch blocks only (format depends on Suite).\n"
     "B) If required file text is missing for EDIT, output ONLY one begin_context block with paths.\n"
     "C) If user asks to CREATE a new file (path NOT in begin_map) -> output #+begin_patch :op create directly.\n"
     "\n"
     "Do NOT include prose outside blocks. No reasoning, no commentary.\n"
     "- Use exactly one patch block per operation.\n"
     "- Paths must be RELATIVE to project root; no absolute paths, no \"..\" segments.\n"
     "- begin_map/begin_state_manifest describe existence, not editable text by themselves.\n"
     "- begin_context is NOT a wishlist: list ONLY existing files whose text you need to read.\n"
     "- NEVER list files you intend to create in begin_context.\n"
     "\n"
     "DECISION TREE (CRITICAL):\n"
     "1) User asks CREATE + path NOT in begin_map -> :op create immediately (NO begin_context).\n"
     "2) User asks EDIT + path in begin_map exists=true + text visible -> patch/sre/aibo.\n"
     "3) User asks EDIT + path in begin_map exists=true + text NOT visible -> ONLY begin_context.\n"
     "4) User asks EDIT + path NOT in begin_map -> clarify or assume create if explicit.\n"
     "\n"
     "OPERATION ORDER (multiple ops):\n"
     "delete -> rename -> create -> patch -> sre/aibo\n"     "\n"
     suite-note
     (or profile-note "")
     (or ctx-limit-note "")
     (or doc-scope-note "")
     "\n"
     "HARD CONTEXT VISIBILITY RULES:\n"
     "\n"
     "- Sections formatted as `In file <path>:` followed by the file body mean that the CURRENT TEXT of that exact file is present in this request.\n"
     "- The full body inside such an `In file <path>:` section is the AUTHORITATIVE CURRENT FILE TEXT for that exact path in this request.\n"
     "- That body MUST be treated as VISIBLE CONTEXT and as has_text=true for this path in this request, regardless of what begin_state_manifest says.\n"
     "- If the current request visibly contains `In file <path>:` with a real file body, you already see that file's text in context right now.\n"
     "- the request may provide the matching current file body under `In file <path>:`.\n"
     "- that file already has current text available in this request and must be treated as has_text=true.\n"
     "- If this request contains an `In file <path>:` section with the file body for a path, you MUST treat that file text as visible/present.\n"
     "- CRITICAL: If this request contains an `In file <path>:` section with a file body for some path, you ALREADY SEE the current text of that file and MUST NOT claim that it is missing or invisible.\n"
     "- Do NOT say that file text is absent, unavailable or unseen for any path that has an `In file <path>:` section with a real body in this SAME request.\n"
     "- Do NOT claim that file text is missing when this request already contains an `In file <path>:` section with the file body.\n"
     "- NEVER ask for begin_context for a path whose full current body is already present in an `In file <path>:` section in this request.\n"
     "- Do NOT ask for begin_context for a path whose full current body is already present.\n"
     "- Seeing an `In file <path>:` section with a body means the file text is already present in context for that exact path; treat this as has_text=true for this request.\n"
     "- An `In file <path>:` body is the ACTUAL current contents of that file in this request, not a hint or summary.\n"
     "- you already have the required current text and must proceed from that body instead of claiming missing context.\n"
     "- If you can see an `In file <path>:` section with the body for a path, you MUST proceed from that body as current text and MUST NOT guess or say \"I do not see the file\".\n"
     "- In all such cases, CURRENT TEXT of that file is present in this request and must be used as the basis for edits.\n"
     "- Never say \"I do not see the file text\" or equivalent when an `In file <path>:` section with the body is present.\n"
     "\n"
     "EDITABILITY / SAFETY RULES:\n"
     "\n"
     "- When you request begin_context for some path, it ALWAYS means there is no current file body for it in this request.\n"
     "- Conversely, when a file body is present in such an `In file <path>:` section, treat that file as having current text available.\n"
     "- In presence of an `In file <path>:` body, that body OVERRIDES any uncertainty from begin_state_manifest, even if begin_state_manifest was omitted, stale, or says has_text=false.\n"
     "\n"
     "- Edit a file ONLY if its current text is present in this request:\n"
     "  • either begin_state_manifest says exists=true AND has_text=true for that path, or\n"
     "  • there is an `In file <path>:` section with the file body for that path in this request.\n"
     "- Delete/rename state-sensitive operations also require exists=true in begin_state_manifest (or equivalent state evidence).\n"
     "- If the target file's CURRENT TEXT is not present in this request context (no matching `In file <path>:` body and no has_text=true in begin_state_manifest), do NOT output patches for it; output ONLY one begin_context block listing required paths.\n"
     "- If a path is present in begin_map, or begin_state_manifest says exists=true, it MUST be treated as an existing file (so :op create is forbidden for it). Use patch/sre/aibo/rename/delete instead of create.\n"
     "- If a needed file exists but its text is missing, begin_context is the ONLY legal output; do not guess patches.\n"
     "\n"
     "SRE/AIBO HEADER RULE:\n"
     "\n"
     "- For SRE/AIBO edits (Suite=sre or Suite=aibo), express changes ONLY as one or more begin_from/begin_to literal pairs inside the patch body.\n"
     "- NEVER use :from/:to header keys for SRE/AIBO (they are allowed ONLY for :op rename; for all other ops they are forbidden).\n"
     "\n"
     "If you cannot anchor a change to current file text, request context instead of guessing.\n"
     "\n"
     "Allowed operations depend on Suite; obey the Suite-specific rules above and do not mix incompatible formats in one answer.\n")))


(defun carriage--intent-frag-org-formatting (_ctx)
  "Base formatting rules for answers: Org-mode only (never Markdown)."
  (concat
   "Formatting (Org-mode required):\n"
   "- Answer in VALID Org-mode (not Markdown).\n"
   "- Headings: use '*' / '**' / '***' (never '#', '##', etc.).\n"
   "- Code blocks: use '#+begin_src <lang> :results output' ... '#+end_src' (never triple backticks).\n"
   "- Links: use Org links like [[URL-or-path][label]].\n"
   "- Do not emit Markdown/HTML fences or other markup.\n"))

(defun carriage--intent-frag-hybrid (ctx)
  "Default fragment for Intent=Hybrid.

CTX MAY carry:
- :typedblocks-structure-hint t|nil
- :org-structure-note string
- :context-meta (:omitted N :limited BOOL)."
  (let* ((typed-hint
          (when (plist-get ctx :typedblocks-structure-hint)
            (concat
             "\n"
             "Typed Blocks (v1):\n"
             "- In addition to Org prose, structure the key parts of your reply using typed blocks:\n"
             "  begin_task, begin_analysis, begin_plan, begin_verify, begin_commands,\n"
             "  begin_question, begin_answer, begin_context (begin_notes is optional).\n"
             "- Put only important information inside these blocks so the tool can extract it even when plain text is disabled.\n"
             "- Keep prose minimal and avoid duplicating the same content inside and outside blocks.\n"
             "- Typical mapping:\n"
             "  • task — concise statement of goals; analysis — key considerations/constraints;\n"
             "  • plan — step-by-step plan; verify — checks/criteria; commands — run/build/test commands;\n"
             "  • question/answer — clarifications; context — links/paths/artifacts; notes — auxiliary (optional).\n")))
         (org-note (and (listp ctx) (plist-get ctx :org-structure-note)))
         (meta (and (listp ctx) (plist-get ctx :context-meta)))
         (omitted (and (listp meta) (plist-get meta :omitted)))
         (limited (and (listp meta) (plist-get meta :limited)))
         (profile-meta
          (when (or (and (integerp omitted) (> omitted 0)) limited)
            (format
             "\nContext limits:\n- Some context items were omitted due to size limits (omitted=%s).\n- Do NOT assume you see the entire project; treat missing files as unknown, not as \"file does not exist\".\n"
             (or omitted 0))))
         (strict
          (concat
           "\n"
           "Typed Blocks — strict format:\n"
           "- Always use explicit Org typed blocks with '#+begin_<type>' ... '#+end_<type>' markers. Do not emulate blocks with headings, links, drawers, or Markdown fences.\n"
           "- Commands (build/test):\n"
           "  • Prefer one command per src block; you MAY include multiple commands in a single src block when a single consolidated output is desired.\n"
           "  • Each src block MUST include the attribute: :results output\n"
           "  • Start by changing directory to the repository root, then run the command (e.g.):\n"
           "      #+begin_src sh :results output\n"
           "      cd $(git rev-parse --show-toplevel 2>/dev/null || pwd)\n"
           "      make test\n"
           "      #+end_src\n"
           "- begin_context:\n"
           "  • Strict list of file paths — exactly one path per line.\n"
           "  • Paths are absolute or relative to the repository root.\n"
           "  • No comments, labels or annotations — only paths.\n"))
         (base
          (concat
           (carriage--intent-frag-org-formatting ctx)
           "\n"
           (when (stringp org-note) (concat org-note "\n"))
           (or profile-meta "")
           "You MAY include brief prose, but the tool will extract and apply ONLY Org begin_patch blocks.\n"
           "Default behavior: reply with Org prose only. Output #+begin_patch blocks ONLY when the user explicitly asks to modify files (e.g., \"Implement\", \"Fix\", \"Apply changes\", \"Реализуй\", \"Вноси правки\").\n"
           "When file text is missing for a required edit, output ONLY a begin_context block with required paths; do not guess patches.\n"
           "If a file exists in begin_map/begin_state_manifest but has_text=false, begin_context is the required fallback.\n"
           "begin_map / begin_state_manifest mark existence and text availability; edit requires has_text=true, and create is forbidden when exists=true.\n"
           "For AIBO/SRE edits, use only begin_from/begin_to pairs in the body; never :from/:to header keys except for rename.\n"
           "Keep prose minimal and place it before or after the blocks. Do not insert text inside blocks.\n"
           "\n"
           "HARD CONTEXT VISIBILITY RULES (same as Intent=Code):\n"
           "- `In file <path>:` with body = CURRENT TEXT present; treat as has_text=true.\n"
           "- If `In file <path>:` body is present, do NOT claim text is missing.\n"
           "- NEVER ask begin_context for paths with visible `In file <path>:` body.\n"
           "- begin_context is ONLY for existing files whose text is NOT visible.\n"
           "\n"
           "DECISION TREE (CRITICAL):\n"
           "1) CREATE: path NOT in begin_map + explicit request -> :op create (NO begin_context).\n"
           "2) EDIT: path in begin_map + text visible -> patch/sre/aibo.\n"
           "3) EDIT: path in begin_map + text NOT visible -> ONLY begin_context.\n"
           "4) begin_context is NOT a wishlist: list ONLY existing files needing text.\n"
           "\n"
           "OPERATION ORDER (multiple ops):\n"
           "delete -> rename -> create -> patch -> sre/aibo\n")))
    (concat base (or typed-hint "") strict)))


(defun carriage--intent-frag-ask (ctx)
  "Default fragment for Intent=Ask.

Typed Blocks guidance is included ONLY when :typedblocks-structure-hint is non-nil in CTX.
This flag is wired to the user-facing \"Typed Blocks\" toggle in carriage-mode."
  (let* ((typed-on (and (listp ctx) (plist-get ctx :typedblocks-structure-hint)))
         (base
          (concat
           (carriage--intent-frag-org-formatting ctx)
           "\n"
           "Do NOT produce any begin_patch blocks. Provide a concise answer.\n"
           "If Typed Blocks guidance is enabled for this buffer, you MAY additionally structure key parts of your reply using typed blocks; otherwise answer with plain Org prose only.\n"))
         (typed-hint
          (when typed-on
            (concat
             "\n"
             "Typed Blocks (v1):\n"
             "- Structure the key parts of your reply using typed blocks:\n"
             "  begin_task, begin_analysis, begin_plan, begin_verify, begin_commands,\n"
             "  begin_question, begin_answer, begin_context (begin_notes is optional).\n"
             "- Put only important information inside these blocks so the tool can extract it even when plain text is disabled.\n"
             "- Keep prose minimal and avoid duplicating the same content inside and outside blocks.\n"
             "- Typical mapping:\n"
             "  • task — concise statement of goals; analysis — key considerations/constraints;\n"
             "  • plan — step-by-step plan; verify — checks/criteria; commands — run/build/test commands;\n"
             "  • question/answer — clarifications; context — links/paths/artifacts; notes — auxiliary (optional).\n")))
         (strict
          (concat
           "\n"
           "Typed Blocks — strict format (when you use them):\n"
           "- Always use explicit Org typed blocks with '#+begin_<type>' ... '#+end_<type>' markers. Do not emulate blocks with headings, links, drawers, or Markdown fences.\n"
           "- Commands (build/test):\n"
           "  • Prefer one command per src block; you MAY include multiple commands in a single src block when a single consolidated output is desired.\n"
           "  • Each src block MUST include the attribute: :results output\n"
           "  • Start by changing directory to the repository root, then run the command (e.g.):\n"
           "      #+begin_src sh :results output\n"
           "      cd $(git rev-parse --show-toplevel 2>/dev/null || pwd)\n"
           "      make test\n"
           "      #+end_src\n"
           "- begin_context:\n"
           "  • Strict list of file paths — exactly one path per line.\n"
           "  • Paths are absolute or relative to the repository root.\n"
           "  • No comments, labels or annotations — only paths.\n"

           "CONTEXT VISIBILITY REMINDER:\n"
           "- Even in Ask mode, if this request contains `In file <path>:` sections with file bodies, you already see the current text of those files.\n"
           "- Do NOT claim that such file text is missing or unseen; treat those bodies as has_text=true for this request.\n")))
    (concat base (or typed-hint "") strict)))

;; New intent: MultiPatch
(defun carriage--intent-frag-multipatch (_ctx)
  "Default fragment for Intent=MultiPatch."
  (concat
   (carriage--intent-frag-org-formatting nil)
   "Answer with ONLY one of two legal outputs:\n"
   "- begin_patch blocks for changes grounded in current file text, or\n"
   "- one begin_context block when any required file text is missing.\n"
   "Do NOT include any prose outside blocks.\n"
   "- Use exactly one block per operation.\n"
   "- Paths must be RELATIVE to project root; no absolute paths, no \"..\" segments.\n"
   "- Edit a file ONLY if begin_state_manifest says exists=true and has_text=true.\n"
   "- Delete/rename state-sensitive operations also require exists=true in begin_state_manifest.\n"
   "- If the target file's CURRENT TEXT is not present in this request context, output ONLY begin_context.\n"
   "- If a path is present in begin_map, or begin_state_manifest says exists=true, it MUST be treated as an existing file (so :op create is forbidden for it).\n"
   "- For SRE/AIBO edits, use begin_from/begin_to blocks; do NOT use :from/:to header keys (those are only for :op rename).\n"
   "- Allowed operations depend on Suite. Do not mention formats that are not allowed by the Suite.\n"
   "\n"
   "After printing ALL required begin_patch blocks, decide whether further work is needed:\n"
   "- If more iterations are required, append in this exact order:\n"
   "  1) A #+begin_task block describing the next iteration scope, concrete steps and acceptance criteria.\n"
   "  2) A #+begin_context block with a strict list of file paths (one per line; absolute or repo-root-relative; no comments/labels).\n"
   "- If the work is fully complete and no further iterations are needed, output a single word on a separate line: ГОТОВО\n"
   "\n"
   "Typed Blocks — strict format:\n"
   "- Always use explicit Org typed blocks with '#+begin_<type>' ... '#+end_<type>' markers. Do not emulate blocks with headings, links, drawers, or Markdown fences.\n"
   "- begin_context:\n"
   "  • Strict list of file paths — exactly one path per line.\n"
   "  • Paths are absolute or relative to the repository root.\n"
   "  • No comments, labels or annotations — only paths.\n"))

;; Register defaults
(carriage-intent-register 'Code   #'carriage--intent-frag-code)
(carriage-intent-register 'Hybrid #'carriage--intent-frag-hybrid)
(carriage-intent-register 'Ask    #'carriage--intent-frag-ask)
(carriage-intent-register 'MultiPatch #'carriage--intent-frag-multipatch)

(provide 'carriage-intent-registry)
;;; carriage-intent-registry.el ends here
