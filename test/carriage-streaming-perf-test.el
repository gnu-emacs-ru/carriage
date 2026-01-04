;; Simple ERT tests for streaming coalescing/flush behavior.
;; These tests are lightweight and intended to guard the core perf invariants:
;;  - multiple small stream chunks enqueued within a short time should result
;;    in a bounded number of buffer insert calls (i.e., coalesced flushes).
;;
;; NOTE: This file is a suggested test harness. Adjust namespace/names as needed.

(require 'ert)

(defun carriage-test--with-temp-buffer-and-mode (fn)
  "Create a temporary org buffer, enable carriage-mode and run FN there."
  (with-temp-buffer
    (org-mode)
    ;; Ensure carriage-mode can be toggled; if not available, skip tests.
    (when (fboundp 'carriage-mode)
      (carriage-mode 1))
    (funcall fn)))

(ert-deftest carriage-stream-coalesce-basic ()
  "Ensure N small chunks enqueued produce at most M actual buffer inserts during a short window.
This test assumes the implementation exposes `carriage--stream-pending-events' and
`carriage--stream-flush-timer' and that `carriage--stream-insert-at-end' performs the real insert.
If those symbols are not present in the runtime, the test skips."
  (unless (and (fboundp 'carriage-insert-stream-chunk)
               (boundp 'carriage--stream-pending-events))
    (ert-skip "Carriage streaming primitives not available; skipping perf test."))
  (carriage-test--with-temp-buffer-and-mode
   (lambda ()
     (let* ((insert-calls 0)
            ;; Advice the low-level inserter to count real inserts (best-effort).
            (orig (symbol-function 'carriage--stream-insert-at-end))
            ;; We must keep a reference to the advice function so it can be removed later.
            (ad-fn (lambda (orig-f s)
                     (setq insert-calls (1+ insert-calls))
                     (funcall orig-f s)))
            ;; Use short flush interval to speed test (if provided)
            (old-interval (and (boundp 'carriage-stream-flush-interval) carriage-stream-flush-interval)))
       (unwind-protect
           (progn
             (when (boundp 'carriage-stream-flush-interval)
               (setq carriage-stream-flush-interval 0.02))
             ;; Install advice using the local ad-fn object; remove it with the same object later.
             (advice-add 'carriage--stream-insert-at-end :around ad-fn)
             ;; Enqueue many tiny chunks quickly
             (dotimes (_ 50)
               (carriage-insert-stream-chunk "."))
             ;; Wait a bit longer than expected flush interval to allow background timer to run.
             (sleep-for 0.15)
             ;; At least one insert must have happened, and number of insert calls should be << 50.
             (should (>= insert-calls 1))
             (should (< insert-calls 10)))
         ;; cleanup: remove the exact advice function object and restore interval
         (when (advice-member-p ad-fn 'carriage--stream-insert-at-end)
           (advice-remove 'carriage--stream-insert-at-end ad-fn))
         (when (boundp 'carriage-stream-flush-interval)
           (setq carriage-stream-flush-interval old-interval)))))))

(ert-deftest carriage-stream-flush-on-finalize ()
  "When streaming finalizes, any pending events must be flushed synchronously.
This uses `carriage-stream-finalize' to trigger finalization."
  (unless (and (fboundp 'carriage-insert-stream-chunk)
               (fboundp 'carriage-stream-finalize)
               (boundp 'carriage--stream-pending-events))
    (ert-skip "Carriage streaming primitives not available; skipping flush-on-finalize test."))
  (carriage-test--with-temp-buffer-and-mode
   (lambda ()
     (let ((pending-before 0))
       ;; enqueue a few chunks
       (carriage-insert-stream-chunk "A")
       (carriage-insert-stream-chunk "B")
       ;; pending events should be non-empty
       (setq pending-before (length (or carriage--stream-pending-events '())))
       (should (> pending-before 0))
       ;; finalize should flush pending events
       (carriage-stream-finalize nil t)
       ;; allow a tick for synchronous finalize effects
       (sleep-for 0.01)
       (should (or (null carriage--stream-pending-events) (zerop (length carriage--stream-pending-events))))))))

(provide 'carriage-streaming-perf-test)
