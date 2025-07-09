# Fix Await Identifier Bug

## Problem

The `await` operation has a critical bug where it incorrectly resolves promise identifiers. When attempting to await a promise, the system tries to resolve "promise:promise:1" instead of properly handling the promise ID.

## Current Behavior

```lisp
(await (spawn (lambda () 42)))  
;; Error: Unknown identifier: 'promise:promise:1'
```

## Expected Behavior

```lisp
(await (spawn (lambda () 42)))  
;; Should return: 42
```

## Root Cause

The VM's await implementation appears to be double-prefixing the promise identifier during resolution, causing the lookup to fail.

## Implementation Tasks

- [ ] Debug the identifier resolution in VM's await implementation
- [ ] Fix the promise ID handling to avoid double prefixing
- [ ] Add proper promise value extraction
- [ ] Ensure await blocks until promise resolves
- [ ] Add error handling for awaiting non-promises

## Test Cases

```lisp
;; Basic await
(await (spawn (lambda () 42))) ; => 42

;; Await with delay
(await (spawn (lambda () 
  (effect time:sleep 100)
  "delayed result"))) ; => "delayed result"

;; Error handling
(await 42) ; => Error: Expected promise, got integer

;; Nested awaits
(await (spawn (lambda ()
  (await (spawn (lambda () "nested")))))) ; => "nested"
```

## Priority

**Critical** - This blocks all async/await functionality

## Labels

- bug
- async-await
- high-priority