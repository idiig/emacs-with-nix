;; -*- lexical-binding: t; -*-
;;
;; Scratch test: make pyim behave like SKK's model.
;;   - lowercase letters and punctuation pass through untouched as
;;     plain ASCII -- pyim doesn't intercept at all.
;;   - an uppercase letter (shift) starts a pinyin composition, fed to
;;     pyim as its lowercase form (mirrors SKK using the shifted key's
;;     own romaji value to start a reading).
;;   - while composing, the inline preview shows the raw entered code
;;     prefixed with "▽" (SKK's own visual convention is cursor-color
;;     based, not literal characters -- this is our own choice of how
;;     to render the same "collecting a reading" idea).
;;   - the candidate page (posframe/popup/minibuffer/whatever
;;     `pyim-page-tooltip' is set to) stays hidden while composing.
;;   - pressing SPC the first time reveals the candidate page, and the
;;     inline preview switches to "▼" + the currently-selected
;;     candidate; press SPC again (or with nothing entered) to fall
;;     through to the normal `pyim-select-word' (confirm/cycle).
;;
;; How to try it:
;;   1. `M-x eval-buffer' in this file.
;;   2. Switch to pyim (`toggle-input-method' / your usual binding).
;;   3. Type a lowercase letter -- it should insert literally, no pyim
;;      UI at all.
;;   4. Type an uppercase letter to start a word, e.g. "Muqian" --
;;      first letter capital, rest lowercase -- you should see
;;      "▽muqian" inline and no popup.
;;   5. Press SPC -- popup appears, inline preview switches to "▼" +
;;      the first candidate.
;;   6. Press SPC again to confirm as usual.
;;
;; To fully undo without restarting Emacs, call
;; `idiig/pyim-skk-style-disable'; the one piece it can't cleanly undo
;; is the `pyim-preview-string' override (see the commented-out
;; original at the bottom -- eval that to restore it, or just restart
;; Emacs).

(require 'pyim)
(require 'completion-preview)

(defvar-local idiig/pyim-page-revealed nil
  "Non-nil once SPC has revealed the candidate page for the current
composition.  Buffer-local; reset to nil whenever a composition ends
-- see `idiig/pyim-reset-reveal-flag' on `pyim-process-ui-hide-hook'.")

;; 1. Only letters are shift-gated: an uppercase ASCII letter starts a
;;    pyim composition, a lowercase one passes straight through as
;;    plain ASCII.  Everything else (digits, punctuation, nil) goes to
;;    ORIG-FN unchanged, so pyim's own punctuation handling (e.g.
;;    converting "," to "，") keeps working exactly as before -- only
;;    letters are affected by the shift distinction.
;;
;;    `pyim-outcome-trigger' (default "v") is excluded from the
;;    lowercase-passthrough case even though it's a lowercase letter:
;;    it's pyim's own built-in trigger key for magic features (toggle
;;    punctuation width after a punctuation char, delete/create-word
;;    after a numbered Chinese word, etc.), all gated by pyim's own
;;    context-sensitive predicates in `pyim-process--trigger-feature-run'
;;    -- not by whether shift was held.  Routing it to ORIG-FN as
;;    usual preserves that; when none of those predicates match, pyim
;;    just inserts it literally anyway, so plain English words
;;    containing "v" are unaffected.
(defun idiig/pyim-process-input-method-shift-gate (orig-fn key)
  (cond
   ((and (integerp key) (<= ?A key) (<= key ?Z))
    (funcall orig-fn (+ key (- ?a ?A))))
   ((and (integerp key) (<= ?a key) (<= key ?z)
         (not (eq key (aref pyim-outcome-trigger 0))))
    (char-to-string key))
   (t (funcall orig-fn key))))
(advice-add 'pyim-process-input-method :around
            #'idiig/pyim-process-input-method-shift-gate)

;; 1b. Once the candidate page is revealed (▼), typing anything that
;;     isn't a recognized candidate-navigation key (SPC, digits, page
;;     nav -- none of which go through this path) should confirm the
;;     currently-displayed candidate first, THEN process the new key
;;     as a fresh keystroke -- mirroring SKK's kakutei-before-anything-
;;     else behavior in ▼ mode, instead of folding new input into the
;;     same composition.  This is exactly the pattern pyim's own
;;     `pyim-process--auto-select-word' already uses internally: push
;;     the key back for the next read cycle, then confirm/terminate --
;;     `pyim-process--translating-p' becoming nil makes the enclosing
;;     `pyim-process-input-method' while loop exit on its next check,
;;     so the requeued key gets processed by a brand new top-level
;;     `pyim-input-method' call, hitting the shift-gate logic fresh.
(defun idiig/pyim-confirm-then-redispatch ()
  (pyim-add-unread-command-events last-command-event)
  (pyim-select-word))

;; A second shift mid-composition (before reveal) marks a syllable
;; boundary, mirroring SKK's use of a second shifted letter mid-word
;; to mark the boundary before trailing okurigana -- here it's the
;; boundary between two pinyin syllables, e.g. typing "DiEr" should
;; enter "di'er", not the ambiguous "dier".  This only ever fires
;; mid-composition: `pyim-process-input-method' dispatches every key
;; after the first through `pyim-mode-map' via its own internal
;; `read-key-sequence' loop, never back through `pyim-input-method'
;; itself, so it doesn't overlap with the shift-gate advice above
;; (which only ever sees the first key of a fresh composition).
(defun idiig/pyim-syllable-boundary-insert ()
  (interactive)
  (if idiig/pyim-page-revealed
      (idiig/pyim-confirm-then-redispatch)
    (pyim-process-with-entered-buffer
      (insert "'" (char-to-string (downcase last-command-event))))
    (pyim-process-run)))
(let ((i ?A))
  (while (<= i ?Z)
    (define-key pyim-mode-map (char-to-string i)
                #'idiig/pyim-syllable-boundary-insert)
    (setq i (1+ i))))

;; Same confirm-then-redispatch treatment for ordinary characters
;; (lowercase letters, punctuation pyim would otherwise keep folding
;; into the entered buffer) reaching pyim's own self-insert command
;; while the page is already revealed.
(defun idiig/pyim-self-insert-command-confirm-gate (orig-fn &rest args)
  (if idiig/pyim-page-revealed
      (idiig/pyim-confirm-then-redispatch)
    (apply orig-fn args)))
(advice-add 'pyim-self-insert-command :around
            #'idiig/pyim-self-insert-command-confirm-gate)

;; 2. Inline preview: "▽" + raw entered code while composing, "▼" +
;;    currently-selected candidate once SPC has revealed the page.
(defun idiig/pyim-pinyin-query-variants (entered)
  "Return ENTERED plus simple apostrophe split variants."
  (if (string-match-p "'" entered)
      (list entered)
    (let ((variants (list entered))
          (i 1)
          (len (length entered)))
      (while (< i len)
        (push (concat (substring entered 0 i)
                      "'"
                      (substring entered i))
              variants)
        (setq i (1+ i)))
      (nreverse variants))))

(defun idiig/pyim-pinyin-spellings (entered)
  "Return full pinyin spellings for pyim candidates matching ENTERED."
  (let* ((scheme (pyim-scheme-current))
         (queries (idiig/pyim-pinyin-query-variants entered))
         (candidates
          (delete-dups
           (apply #'append
                  (mapcar (lambda (query)
                            (pyim-candidates-create
                             (pyim-imobjs-create query scheme) scheme))
                          queries)))))
    (delete-dups
     (delq nil
           (mapcar (lambda (word)
                     (pyim-cstring-to-pinyin word nil "'" nil t))
                   candidates)))))

(defun idiig/pyim-pinyin-continuation-suffixes (entered)
  "Return pinyin continuation suffixes for ENTERED."
  (delq nil
        (mapcar (lambda (spelling)
                  (when (and (string-prefix-p entered spelling)
                             (> (length spelling) (length entered)))
                    (substring spelling (length entered))))
                (idiig/pyim-pinyin-spellings entered))))

(defun idiig/pyim-pinyin-preview-hint (entered)
  "Bracketed inline hint guessing ENTERED's full pinyin spelling.

E.g. entered \"wm\" shows \" [wo'men]\" once a matching word is in your
personal jianpin history.  This is deliberately the exact same first
element `idiig/pyim-pinyin-overlay-capf' offers as its top completion
candidate (both read `idiig/pyim-pinyin-spellings' directly, and its
order is preserved all the way to vertico by
`idiig/pyim-force-unsorted-consult') -- so the hint always previews
what accepting the first CAPF candidate would give you, not a
separate, possibly-different guess.

Only shown once the guess spans two or more syllables (i.e. its
spelling contains pyim's own \"'\" syllable separator) -- a single
partial syllable like \"w\" or \"bi\" always has SOME single-character
match, so requiring `equal' alone isn't enough to keep the hint from
firing on every ordinary keystroke; jianpin only ever needs one letter
per syllable, so a real multi-character expansion (what the hint is
actually for) can't appear until at least two initials have been
entered anyway."
  (let ((top (car (idiig/pyim-pinyin-spellings entered))))
    (if (and top (not (equal top entered)) (string-match-p "'" top))
        (format " [%s]" top)
      "")))

(cl-defmethod pyim-preview-string ((_scheme pyim-scheme-quanpin))
  (if idiig/pyim-page-revealed
      (let* ((candidates (pyim-process-get-candidates))
             (pos (min (pyim-process-word-position)
                       (1- (length candidates))))
             (preview (concat (pyim-process-get-select-result)
                              (nth pos candidates)))
             (rest (mapconcat
                    (lambda (py) (concat (nth 0 py) (nth 1 py)))
                    (nthcdr (length preview) (pyim-process-get-first-imobj))
                    "'")))
        (when (string< "" rest) (setq preview (concat preview rest)))
        (concat "▼" (pyim-process-magic-convert preview)))
    (let ((entered (pyim-process-get-entered 'point-before)))
      (concat "▽" entered (idiig/pyim-pinyin-preview-hint entered)))))

;; 3. Suppress the candidate page until revealed; the preview refresh
;;    above is a separate hook function and keeps firing on every
;;    keystroke regardless.
(defun idiig/pyim-page-refresh-gate (orig-fn &rest args)
  (when idiig/pyim-page-revealed
    (apply orig-fn args)))
(advice-add 'pyim-page--refresh :around #'idiig/pyim-page-refresh-gate)

;; 4. Reset the flag whenever a composition actually ends (word
;;    selected, or aborted) -- `pyim-process-terminate' always calls
;;    `pyim-process-ui-hide' exactly once per composition, regardless
;;    of how it ended, so this is a more reliable reset point than
;;    trying to detect "start of next composition" from the input loop.
(defun idiig/pyim-reset-reveal-flag (&rest _)
  (setq idiig/pyim-page-revealed nil))
(add-hook 'pyim-process-ui-hide-hook #'idiig/pyim-reset-reveal-flag)

;; 5. SPC: first press reveals the candidate page instead of
;;    confirming/cycling; once revealed (or with nothing entered),
;;    fall through to the normal `pyim-select-word'.
(defun idiig/pyim-space-reveal-or-select ()
  (interactive)
  (if (or idiig/pyim-page-revealed (pyim-process-without-entered-p))
      (pyim-select-word)
    (setq idiig/pyim-page-revealed t)
    (pyim-process-ui-refresh)))
(define-key pyim-mode-map " " #'idiig/pyim-space-reveal-or-select)

(defun idiig/pyim-skk-style-disable ()
  "Undo the shift-gate advice, SPC rebind, and page-hiding advice from
this test.  Does NOT restore the original `pyim-preview-string'
method -- see the commented-out original definition at the bottom of
this file."
  (interactive)
  (advice-remove 'pyim-process-input-method
                 #'idiig/pyim-process-input-method-shift-gate)
  (advice-remove 'pyim-page--refresh #'idiig/pyim-page-refresh-gate)
  (advice-remove 'pyim-self-insert-command
                 #'idiig/pyim-self-insert-command-confirm-gate)
  (advice-remove 'completion-at-point
                 #'idiig/pyim-completion-at-point-overlay-advice)
  (remove-hook 'pyim-process-ui-hide-hook #'idiig/pyim-reset-reveal-flag)
  (define-key pyim-mode-map " " #'pyim-select-word)
  (define-key pyim-mode-map (kbd "M-i") nil)
  (define-key pyim-mode-map (kbd "TAB") #'pyim-toggle-assistant-scheme)
  (define-key pyim-mode-map [?\t] #'pyim-toggle-assistant-scheme)
  (let ((i ?A))
    (while (<= i ?Z)
      (define-key pyim-mode-map (char-to-string i) #'pyim-self-insert-command)
      (setq i (1+ i))))
  (remove-hook 'pyim-activate-hook #'idiig/pyim-pinyin-capf-enable)
  (remove-hook 'pyim-deactivate-hook #'idiig/pyim-pinyin-capf-disable)
  (idiig/pyim-pinyin-capf-disable)
  (advice-remove 'consult--read #'idiig/pyim-consult-read-unsorted-advice)
  (remove-hook 'pyim-process-ui-refresh-hook
               #'idiig/pyim-composing-completion-preview-refresh)
  (remove-hook 'pyim-process-ui-hide-hook
               #'idiig/pyim-composing-completion-preview-hide)
  (when (bound-and-true-p completion-preview-active-mode)
    (completion-preview-active-mode -1))
  (message "pyim SKK-style test disabled (re-eval the original pyim-preview-string below, or restart Emacs, to fully restore the inline preview)"))

;; 6. While composing (▽, not yet revealed), offer full pinyin
;;    spellings for abbreviated/jianpin input as a CAPF, so e.g. typing
;;    "Br" can be completed to "bi'ru".  Reuses pyim's own candidate
;;    pipeline (`pyim-imobjs-create'/`pyim-candidates-create', same as
;;    `idiig/pyim-capf' in the real config) to get matching hanzi, then
;;    `pyim-cstring-to-pinyin' to turn each one back into its full
;;    spelling -- NOTE: the jianpin match itself comes from
;;    `ishortcode2word', which is built from your own selection
;;    history, not from the dictionary itself, so this only ever
;;    suggests words pyim has already learned you use.
;;
;;    Gate: must use `pyim-process--translating-p', NOT
;;    `(eq input-method-function 'pyim-input-method)' -- the latter is
;;    dynamically let-bound to nil by `pyim-process-input-method'
;;    itself while composing (pyim-process.el, around its main
;;    `(let* (... (input-method-function nil) ...)'), so it is *never*
;;    true at the same time as "entered is non-empty".  That combination
;;    made the earlier version of this CAPF a dead no-op.
;;
;;    Trigger: this CAPF is for manual full-spelling completion via
;;    `M-i'.  Automatic completion-preview below uses a separate suffix
;;    CAPF because pyim's entered code is preview overlay text, not real
;;    buffer text that CP can use as a prefix range.
(defun idiig/pyim-pinyin-capf ()
  (when (and (pyim-process--translating-p)
             (not idiig/pyim-page-revealed)
             (not (pyim-process-without-entered-p)))
    (let* ((entered (pyim-process-get-entered 'point-before))
           (len (length entered))
           (spellings (idiig/pyim-pinyin-spellings entered)))
      (list (- (point) len) (point)
            (lambda (string pred action)
              (complete-with-action action spellings "" pred))
            :exclusive 'yes
            :display-sort-function #'identity
            :cycle-sort-function #'identity
            :exit-function
            (lambda (spelling _status)
              ;; The buffer text is already replaced with SPELLING at
              ;; this point (that's the default completion machinery),
              ;; but `pyim-preview--overlay' doesn't know that: it was
              ;; sized around the OLD entered text, and its end
              ;; boundary has no rear-advance, so the completion's
              ;; insertion (landing exactly at that boundary) ends up
              ;; outside the tracked region.  Left alone, the next
              ;; preview refresh would only erase the stale "▽" marker
              ;; and reinsert fresh preview text after the untouched
              ;; SPELLING, producing a visible duplicate like
              ;; "bi'ru▽bi'ru".  Re-sync the overlay to the actual
              ;; completed text first so the refresh below cleanly
              ;; replaces the whole thing instead.
              (when (and (overlayp pyim-preview--overlay)
                         (overlay-start pyim-preview--overlay))
                (move-overlay pyim-preview--overlay
                              (overlay-start pyim-preview--overlay)
                              (point)))
              ;; Sync pyim's own hidden entered-buffer to match and
              ;; recompute candidates from it, so the inline preview
              ;; and a later SPC-reveal reflect the completed spelling.
              (pyim-process-with-entered-buffer
                (erase-buffer)
                (insert spelling))
              (pyim-process-run))))))

(defun idiig/pyim-pinyin-continuation-capf ()
  "CAPF for real completion-preview pinyin continuations while composing.
The pyim entered text is displayed by `pyim-preview--overlay', not stored
in the real buffer, so this CAPF completes an empty range at point and
offers only suffix strings."
  (when (and (pyim-process--translating-p)
             (not idiig/pyim-page-revealed)
             (not (pyim-process-without-entered-p)))
    (let* ((entered (pyim-process-get-entered 'point-before))
           (suffixes (idiig/pyim-pinyin-continuation-suffixes entered)))
      (list (point) (point)
            (lambda (string pred action)
              (complete-with-action action suffixes "" pred))
            :exclusive 'yes
            ;; Preserve `pyim-candidates-create's own frequency-based
            ;; order -- without this, vertico/consult (and the plain
            ;; *Completions* buffer) apply their own default sort
            ;; (roughly by length then alphabetically), which is
            ;; exactly why "wo'men" ended up last in a 17-candidate
            ;; list instead of first.  `completion-preview-complete'
            ;; itself does the same `:display-sort-function #'identity'
            ;; trick when it falls through to a full candidates list;
            ;; we bypass that function for the jianpin case below, so
            ;; we need to set this ourselves.
            :display-sort-function #'identity
            :cycle-sort-function #'identity
            :exit-function
            (lambda (suffix _status)
              ;; `completion-preview-insert' inserts SUFFIX into the real
              ;; buffer first.  Remove that insertion and append it to pyim's
              ;; hidden entered buffer instead, then let pyim redraw its own
              ;; preview from the synchronized state.
              (delete-region (- (point) (length suffix)) (point))
              (pyim-process-with-entered-buffer
                (goto-char (point-max))
                (insert suffix))
              (pyim-process-run))))))

(defun idiig/pyim-pinyin-overlay-capf ()
  "CAPF for choosing full pinyin spellings while pyim is composing.
Unlike `idiig/pyim-pinyin-capf', this completes an empty range at point
because pyim's entered code lives in `pyim-preview--overlay', not in the
real buffer."
  (when (and (pyim-process--translating-p)
             (not idiig/pyim-page-revealed)
             (not (pyim-process-without-entered-p)))
    (let* ((entered (pyim-process-get-entered 'point-before))
           (spellings (idiig/pyim-pinyin-spellings entered)))
      (list (point) (point)
            (lambda (string pred action)
              (complete-with-action action spellings "" pred))
            :exclusive 'yes
            :display-sort-function #'identity
            :cycle-sort-function #'identity
            :exit-function
            (lambda (spelling _status)
              ;; Completion inserts SPELLING into the real buffer.  Move that
              ;; text into pyim's hidden entered buffer instead.
              (delete-region (- (point) (length spelling)) (point))
              (pyim-process-with-entered-buffer
                (erase-buffer)
                (insert spelling))
              (pyim-process-run))))))

(defvar idiig/pyim-use-overlay-capf-for-completion nil
  "Non-nil means route `completion-at-point' to pyim's overlay CAPF.")

(defun idiig/pyim-completion-at-point-overlay-advice (orig-fn &rest args)
  "Use pyim's overlay CAPF while expanding a completion-preview menu."
  (if idiig/pyim-use-overlay-capf-for-completion
      (let ((completion-at-point-functions '(idiig/pyim-pinyin-overlay-capf)))
        (apply orig-fn args))
    (apply orig-fn args)))
(advice-add 'completion-at-point :around
            #'idiig/pyim-completion-at-point-overlay-advice)

(defvar idiig/pyim-force-unsorted-consult nil
  "Non-nil while a pyim pinyin CAPF list is open via `consult-completion-in-region'.

`pyim-candidates-create' already returns candidates in frequency
order, and `idiig/pyim-pinyin-spellings'/`idiig/pyim-pinyin-continuation-suffixes'
preserve that order, but it never survives to vertico: setting
`:display-sort-function'/`:cycle-sort-function' in the CAPF's plist
(the standard, documented mechanism) has no effect here, because
`consult--in-region' rebuilds `completion-extra-properties' from
scratch before handing off to `consult--read', keeping only
`:annotation-function'/`:affixation-function'/`:exit-function' and
silently dropping everything else -- verified by reading
`consult--in-region' directly.  The only lever `consult--read' itself
actually exposes for this is its own `:sort' keyword (\"SORT should be
set to nil if the candidates are already sorted\"), which
`consult--in-region' never passes, so it always defaults to `:sort t'.
Advising `consult--read' to force `:sort nil' while this flag is bound
is the only way found so far to make vertico display our candidates
in the order pyim's own frequency data puts them in -- without it,
\"wo'men\" (longer than most competing spellings) gets sorted to the
end of the list instead of appearing first.")

(defun idiig/pyim-consult-read-unsorted-advice (orig-fn table &rest options)
  (if idiig/pyim-force-unsorted-consult
      (apply orig-fn table (plist-put (copy-sequence options) :sort nil))
    (apply orig-fn table options)))
(advice-add 'consult--read :around #'idiig/pyim-consult-read-unsorted-advice)

;; `completion-at-point-functions' is local-variable-if-set, so a bare
;; `add-to-list' here would only ever affect whichever buffer happens
;; to be current when this file is eval'd, not every buffer where
;; pyim later gets activated.  Hook onto activate/deactivate instead,
;; same pattern as the wl-draft-capf/agent-shell CAPF registrations in
;; the real config.
(defun idiig/pyim-pinyin-capf-enable ()
  (add-hook 'completion-at-point-functions #'idiig/pyim-pinyin-capf nil t))
(defun idiig/pyim-pinyin-capf-disable ()
  (remove-hook 'completion-at-point-functions #'idiig/pyim-pinyin-capf t))
(add-hook 'pyim-activate-hook #'idiig/pyim-pinyin-capf-enable)
(add-hook 'pyim-deactivate-hook #'idiig/pyim-pinyin-capf-disable)

;; `M-i' and TAB should both stay inside completion-preview's own
;; command path.  If pyim has refreshed but CP has no active overlay
;; yet, create one from the pyim-only suffix CAPF first, then delegate
;; to `completion-preview-complete'.
(defun idiig/pyim-composing-completion-preview-complete ()
  (interactive)
  (unless (bound-and-true-p completion-preview-active-mode)
    (let ((completion-at-point-functions
           '(idiig/pyim-pinyin-continuation-capf)))
      (completion-preview--update)))
  ;; Both branches below may open a recursive minibuffer (vertico, via
  ;; the real config's `consult-completion-in-region' bridge) --
  ;; either through `completion-preview-complete' falling through to
  ;; `completion-at-point', or directly below for the jianpin case.
  ;; `pyim-process-input-method' dynamically binds
  ;; `overriding-terminal-local-map' to `pyim-mode-map' for the ENTIRE
  ;; composing loop, and that variable outranks every other keymap --
  ;; including the minibuffer's own -- for the whole duration that
  ;; binding is on the stack.  Left alone, RET inside the picker hits
  ;; pyim-mode-map's own RET (`pyim-quit-no-clear') instead of
  ;; confirming the minibuffer, and C-g likewise hits
  ;; `pyim-quit-clear' instead of aborting it.  Shadow it back to nil
  ;; for the duration of this call only; the dynamic let restores
  ;; pyim's own binding the moment we return, so the composing loop's
  ;; subsequent `read-key-sequence' calls are unaffected.
  (let ((overriding-terminal-local-map nil)
        (idiig/pyim-force-unsorted-consult t))
    (if (bound-and-true-p completion-preview-active-mode)
        (let ((idiig/pyim-use-overlay-capf-for-completion t))
          (completion-preview-complete))
      ;; No literal continuation to preview (e.g. entered "wm" has no
      ;; suffix relationship with its jianpin match "wo'men" --
      ;; established earlier: completion-preview's own filtering
      ;; requires a literal prefix, which jianpin abbreviations never
      ;; have).  Fall back to the full jianpin+spelling CAPF instead,
      ;; which always opens the vertico/consult picker listing every
      ;; match (e.g. "wo'men"), since these candidates never share a
      ;; common prefix with ENTERED either.
      (let ((completion-at-point-functions '(idiig/pyim-pinyin-overlay-capf)))
        (unless (completion-at-point)
          (pyim-toggle-assistant-scheme))))))
(define-key pyim-mode-map (kbd "M-i")
            #'idiig/pyim-composing-completion-preview-complete)

;; 7. Real `completion-preview-mode' for pinyin continuations while
;;    composing.  The raw entered code shown as "▽entered" is pyim
;;    preview overlay text rather than real buffer text, so the CP CAPF
;;    completes an empty range at the real point and offers suffixes
;;    only.  This keeps CP anchored after pyim's preview without letting
;;    ordinary buffer CAPFs contribute stale ghost text.
(defun idiig/pyim-composing-completion-preview-refresh (&rest _)
  (when (bound-and-true-p completion-preview-active-mode)
    (completion-preview-active-mode -1))
  (when (and (pyim-process--translating-p)
             (not idiig/pyim-page-revealed)
             (not (pyim-process-without-entered-p)))
    (let ((completion-at-point-functions
           '(idiig/pyim-pinyin-continuation-capf)))
      (completion-preview--update))))
(add-hook 'pyim-process-ui-refresh-hook
          #'idiig/pyim-composing-completion-preview-refresh 90)

(defun idiig/pyim-composing-completion-preview-hide (&rest _)
  (when (bound-and-true-p completion-preview-active-mode)
    (completion-preview-active-mode -1)))
(add-hook 'pyim-process-ui-hide-hook #'idiig/pyim-composing-completion-preview-hide)

;; TAB cycles through the currently showing CP candidates, mirroring
;; the README's global `completion-preview-active-mode-map' binding
;; (TAB/<tab> -> `idiig/completion-preview-tab', which cycles for the
;; first `idiig/completion-preview-tab-cycle-limit' consecutive presses
;; then escalates to `completion-preview-complete').  That global
;; binding can't reach us here though: `pyim-mode-map' is bound as
;; `overriding-terminal-local-map' for the whole composing loop, which
;; outranks every other keymap including minor-mode maps, so TAB has to
;; be rebound again right here or it would never even reach
;; `completion-preview-active-mode-map' while composing.
;;
;; We CANNOT just call `idiig/completion-preview-tab' directly here:
;; `pyim-process-input-method' tracks its own `this-command'/
;; `last-command' in a local `let*' that dynamically shadows the
;; globals for the whole composing loop (see pyim-process.el), so by
;; the time we're called, `last-command' is always
;; `idiig/pyim-composing-completion-preview-cycle' (whatever
;; `pyim-mode-map' actually dispatched), never
;; `idiig/completion-preview-tab' -- that function's own
;; consecutive-press check would never see two presses as consecutive
;; and would never escalate to `completion-preview-complete'.  So this
;; reimplements the same counting against its OWN command symbol
;; instead, while still sharing the README's
;; `idiig/completion-preview-tab-cycle-count'/`-limit' variables so
;; configuration and the press count stay unified across both
;; contexts.  Falls back to pyim's original assistant-scheme toggle
;; when no CP candidate is showing.
(defun idiig/pyim-composing-completion-preview-cycle ()
  (interactive)
  (if (bound-and-true-p completion-preview-active-mode)
      (progn
        ;; See the README's `idiig/completion-preview-tab' for why this
        ;; is needed: without it, `completion-preview--post-command'
        ;; (were it to run) would treat this wrapper as an unrecognized
        ;; command and hide the preview it just cycled.  Not actually
        ;; reachable mid-composition -- `post-command-hook' never runs
        ;; until `pyim-process-input-method's own loop returns -- but
        ;; kept here anyway so this mirrors the README version exactly
        ;; and stays correct if that ever changes.
        (completion-preview--inhibit-update)
        ;; Mirrors the README's `idiig/completion-preview-tab': skip
        ;; cycling entirely when there are fewer candidates than
        ;; `idiig/completion-preview-tab-cycle-limit' to begin with, and
        ;; bind the README's `idiig/completion-preview-force-unsorted-
        ;; consult' (its `consult--read' advice is already registered
        ;; globally from the `consult' package's own :config) so the
        ;; escalated candidate list doesn't get silently re-sorted by
        ;; vertico the same way the "wo'men" ordering did before that
        ;; was found -- no need for a second, separate advice here.
        (let ((total (length (completion-preview--get 'completion-preview-suffixes)))
              (idiig/completion-preview-force-unsorted-consult t))
          (if (< total idiig/completion-preview-tab-cycle-limit)
              (completion-preview-complete)
            (progn
              (setq idiig/completion-preview-tab-cycle-count
                    (if (eq last-command 'idiig/pyim-composing-completion-preview-cycle)
                        (1+ idiig/completion-preview-tab-cycle-count)
                      1))
              (if (> idiig/completion-preview-tab-cycle-count
                     idiig/completion-preview-tab-cycle-limit)
                  (completion-preview-complete)
                (completion-preview-next-candidate 1))))))
    (pyim-toggle-assistant-scheme)))
(define-key pyim-mode-map (kbd "TAB")
            #'idiig/pyim-composing-completion-preview-cycle)
(define-key pyim-mode-map [?\t]
            #'idiig/pyim-composing-completion-preview-cycle)

;; To restore the original inline-preview-of-selected-candidate
;; behavior without restarting Emacs, eval this:
;;
;; (cl-defmethod pyim-preview-string ((_scheme pyim-scheme-quanpin))
;;   "获得 preview 字符串，适用于全拼输入法。"
;;   (let* ((candidates (pyim-process-get-candidates))
;;          (pos (min (pyim-process-word-position)
;;                    (1- (length candidates))))
;;          (preview (concat (pyim-process-get-select-result)
;;                           (nth pos candidates)))
;;          (rest (mapconcat
;;                 (lambda (py)
;;                   (concat (nth 0 py) (nth 1 py)))
;;                 (nthcdr (length preview)
;;                         (pyim-process-get-first-imobj))
;;                 "'")))
;;     (when (string< "" rest)
;;       (setq preview (concat preview rest)))
;;     (pyim-process-magic-convert preview)))
