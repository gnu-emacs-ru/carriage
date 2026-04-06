# Plan refactoring carriage

## Status: In Progress

### Phase 1: Completed ✓

| Файл | Действие | Строк |
|------|----------|-------|
| carriage-mode.el | Removed duplicate reasoning-fold functions | -130 |
| carriage-mode.el | Removed legacy wrappers (carriage-send, carriage-send-region) | -160 |
| carriage-reasoning-fold.el | Added public API functions | +106 |
| carriage-ui.el | Extracted carriage-ui-faces.el | -99 |
| carriage-ui.el | Extracted carriage-ui-spinner.el | -55 |

---

## Phase 2: Large Files Splitting

### 2.1 carriage-context.el (3038 lines)
**Target: Split into 6 modules**

| New Module | Content | Est. Size | Status |
|------------|---------|-----------|--------|
| `carriage-context-file.el` | File reading, caching | 121 | ✓ Done |
| `carriage-context-map.el` | Project map (git ls-files) | 174 | ✓ Done |
| `carriage-context-visible.el` | Visible buffer collection | 122 | ✓ Done |
| `carriage-context-collect.el` | Doc-paths, patched files | 207 | ✓ Done |
| `carriage-context-parser.el` | Block parsing | ~300 | Pending |
| `carriage-context.el` | Main module (require all) | ~500 | Pending |

**Status: 4/6 modules done**

---

### 2.2 carriage-doc-state.el (1554 lines)
**Target: Split into 4 modules**

| New Module | Content | Est. Size | Status |
|------------|---------|-----------|--------|
| `carriage-doc-state-core.el` | Read/write, normalize | 221 | ✓ Done |
| `carriage-doc-state-sync.el` | Save hooks | 52 | ✓ Done |
| `carriage-doc-state-fold.el` | Fold overlays | 269 | ✓ Done |
| `carriage-doc-state.el` | Main module (require all) | 36 | ✓ Done |

**Status: 4/4 modules done**

---

### 2.3 carriage-ui.el (3461 lines)
**Target: Split into 6 modules**

| New Module | Content | Est. Size | Status |
|------------|---------|-----------|--------|
| `carriage-ui-faces.el` | defface definitions | 121 | ✓ Done |
| `carriage-ui-spinner.el` | Spinner logic | 72 | ✓ Done |
| `carriage-ui-modeline.el` | Segment builders, cache | ~1200 | Pending |
| `carriage-ui-header.el` | Header-line | ~600 | Pending |
| `carriage-ui-state.el` | State, tooltips | ~400 | Pending |
| `carriage-ui.el` | Main module | ~600 | Pending |

**Status: 2/6 modules done**

---

### 2.4 carriage-transport-gptel.el (2170 lines)
**Target: Split into 4 modules**

| New Module | Content | Est. Size | Status |
|------------|---------|-----------|--------|
| `carriage-transport-gptel-watchdog.el` | Watchdog timer logic | ~80 | ✓ Done |
| `carriage-transport-gptel-core.el` | Main callback logic | ~1900 | Pending |
| `carriage-transport-gptel-diagnostics.el` | Logging, diagnostics | ~200 | Pending |
| `carriage-transport-gptel-streaming.el` | Stream chunks | ~200 | Pending |

**Status: 1/4 modules done**

---

### 2.5 carriage-mode.el (5632 lines)
**Target: Split into 5 modules**

| New Module | Content | Est. Size | Status |
|------------|---------|-----------|--------|
| `carriage-mode-send.el` | Send buffer/subtree commands | 267 | ✓ Done |
| `carriage-mode-apply.el` | Apply commands | 193 | ✓ Done |
| `carriage-mode-core.el` | Mode definition, keymap | ~1500 | Pending |
| `carriage-mode-context.el` | Context building | ~500 | Pending |
| `carriage-mode.el` | Main module | ~1132 | Pending |

**Status: 2/5 modules done**

---

## Phase 3: Remove Deprecated

### 3.1 Candidates for removal
- `carriage-transport-echo.el` - check usage
- `carriage-stream-perf.el`, `carriage-stream-tune.el`, `carriage-stream-silence.el` - check usage

### 3.2 Fold modules - check duplicates
- `carriage-block-fold.el` (276)
- `carriage-patch-fold.el` (314)
- `carriage-reasoning-fold.el` (507)

Check for overlapping functionality.

---

## Phase 4: Import Optimization

1. Verify no circular requires
2. Use `require ... nil t` for optional deps
3. Add `declare-function` for externals

---

## Phase 5: Additional Files (New Analysis)

### Current State After Phase 1-2

| File | Lines | Functions | Ratio | Status |
|------|-------|-----------|-------|--------|
| carriage-mode.el | 5632 | 181 | 31.1 | **HIGH** - needs split |
| carriage-ui.el | 3461 | 140 | 24.7 | Needs modeline split |
| carriage-transport-gptel.el | 2174 | 115 | 18.9 | Partial |
| carriage-context.el | 3043 | 124 | 24.5 | Partial |
| carriage-web.el | 1649 | 64 | 25.7 | Check if needs split |
| carriage-apply.el | 1167 | 52 | 22.4 | OK |
| carriage-transport.el | 905 | 28 | 32.3 | **HIGH** - long functions |
| carriage-keyspec.el | 805 | 27 | 29.8 | Check if needs split |
| carriage-typedblocks.el | 769 | 32 | 24.0 | OK |
| carriage-op-sre.el | 765 | 38 | 20.1 | OK |
| carriage-task.el | 747 | 43 | 17.4 | OK |
| carriage-hub.el | 621 | 31 | 20.0 | OK |
| carriage-reasoning-fold.el | 507 | 23 | 22.0 | OK |

### Priority Additions

#### 5.1 carriage-transport.el (905 lines, ratio 32.3)
**Target: Split into 3 modules**

| New Module | Content | Est. Size | Status |
|------------|---------|-----------|--------|
| `carriage-transport-watchdog.el` | Watchdog timer logic | 132 | ✓ Done |
| `carriage-transport-payload.el` | Payload processing, conversation state | 130 | ✓ Done |
| `carriage-transport-core.el` | Main API (~600) | ~600 | Pending |

**Status: 2/3 modules done**

#### 5.2 carriage-web.el (1649 lines, ratio 25.7)
Split into:
- `carriage-web-core.el` - Web server core (~500)
- `carriage-web-handlers.el` - Request handlers (~500)
- `carriage-web-templates.el` - HTML templates (~400)
- `carriage-web.el` - Main module (~350)

#### 5.3 carriage-keyspec.el (805 lines, ratio 29.8)
Check for extractable parts:
- Key binding definitions
- Menu definitions
- Transient definitions

---

## Results

| Metric | Before | After (estimated) |
|--------|--------|------------------|
| Total lines | 32669 | ~24000 |
| Files | 57 | ~75 |
| Max file size | 5632 | ~1500 |

### Completed changes

| Файл | Действие | Строк |
|------|----------|-------|
| carriage-mode.el | Removed duplicate reasoning-fold functions | -130 |
| carriage-mode.el | Removed legacy wrappers | -160 |
| carriage-reasoning-fold.el | Added public API functions | +106 |
| carriage-ui.el | Extracted carriage-ui-faces.el | -99 |
| carriage-ui.el | Extracted carriage-ui-spinner.el | -55 |
| carriage-context.el | Extracted 4 modules | -624 |
| carriage-doc-state.el | Extracted 3 modules | -542 |
| carriage-mode.el | Extracted carriage-mode-send.el | -267 |
| carriage-mode.el | Extracted carriage-mode-apply.el | -193 |
| carriage-transport.el | Extracted 2 modules | -262 |

**Total: ~2224 lines refactored, 16 new files created**

---

## Priorities

1. **High**: carriage-transport.el (905 lines, ratio 32.3)
2. **High**: carriage-mode.el continue split
3. **Medium**: carriage-ui.el (modeline)
4. **Medium**: carriage-web.el (1649 lines)
5. **Low**: carriage-keyspec.el

---

## Code Quality Metrics to Track

- **Function length**: target < 50 lines
- **File length**: target < 1000 lines  
- **Functions per file**: target < 30
- **Cyclomatic complexity**: target < 10
- **Coupling**: minimize inter-module dependencies
