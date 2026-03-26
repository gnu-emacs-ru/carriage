Name: RequestStateManifest
Stability: [STABLE]
Spec: A request MAY include a begin_state_manifest block with rows path|exists|has_text. The system MUST distinguish file existence from file-text availability.
Proof: spec/context-integration-v2.org

Name: EditRequiresText
Stability: [STABLE]
Spec: Edit-like operations (patch/aibo/sre and other state-sensitive edits) are valid only when the target file exists in the current request state and its current full text is present in this request.
Proof: spec/context-integration-v2.org, lisp/carriage-apply.el (gatekeeper), test/carriage-gatekeeper-e2e-test.el

Name: CreateRequiresMissing
Stability: [STABLE]
Spec: Create is forbidden for any path that already exists in the current request state, including paths present in project map/state manifest.
Proof: spec/file-ops-v2.org, lisp/carriage-apply.el (gatekeeper), test/carriage-diff-test.el

Name: MissingTextFallback
Stability: [STABLE]
Spec: If a needed file exists but its current text is not present in the request, the preferred safe response is a begin_context request for that file instead of speculative edits.
Proof: spec/context-integration-v2.org, lisp/carriage-apply.el (gatekeeper)

Name: AiboBodyOnlyReplacements
Stability: [STABLE]
Spec: AIBO content replacements MUST be expressed only with begin_from/begin_to body segments; header-level :from/:to keys are forbidden except rename in file ops.
Proof: spec/aibo-v2.org, lisp/carriage-parser.el
