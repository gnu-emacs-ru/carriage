Stage: RealityCheck

Purpose:
Make request-state handling explicit and fail-closed so patch generation is grounded in current file text, not in history, project-map existence alone, or speculative inference.

Invariants:
- INV-Core-IO-Boundary: core request-state meaning is independent from UI/history rendering.
- INV-Determinism: the same request state yields the same edit permission decision.
- INV-Compat-Policy: request-state rules evolve additively.
- INV-Traceability: request-state decisions are anchored in manifest/spec rules.
- INV-Surface-First: public request-state and edit permission rules are declared in SURFACE.md/specs before code changes.
- INV-Single-Intent: this change is only about reliable request-state and patch eligibility.
- INV-Exists-vs-HasText: file existence and file-text availability are distinct facts.
- INV-No-Edit-Without-Text: edit-like operations require has_text=true.
- INV-No-Create-If-Exists: create requires exists=false.
- INV-History-NonAuthoritative: patch history and narrative are not source of truth for current file text.

Decisions:
- [Done] Request state is modeled minimally as path|exists|has_text. Exit: Spec updated and code/tests enforce the distinction. Proof: spec/context-integration-v2.org, spec/file-ops-v2.org, spec/aibo-v2.org
- [Done] Missing file text must fail closed to begin_context instead of speculative patching. Exit: Prompt contract and parser/apply checks both enforce fallback. Proof: spec/context-integration-v2.org, lisp/carriage-apply.el (gatekeeper), test/carriage-gatekeeper-e2e-test.el
- [Done] Historical patch bodies must not act as current state in outgoing payload. Exit: Payload sanitization and tests confirm only history markers remain. Proof: test/transport/payload-summarize-patch-blocks-test.el, lisp/carriage-transport.el
