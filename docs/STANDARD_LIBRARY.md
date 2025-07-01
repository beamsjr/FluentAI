# ClaudeLang Standard Library

The ClaudeLang standard library provides a comprehensive set of functions for common programming tasks. All functions are designed to be pure when possible, with effects explicitly marked.

## Core Functions (`core`)

Basic operations and utilities.

### List Operations
- `length(lst)` - Get list length
- `append(lst1, lst2)` - Concatenate lists
- `reverse(lst)` - Reverse a list
- `nth(lst, n)` - Get nth element
- `take(n, lst)` - Take first n elements
- `drop(n, lst)` - Drop first n elements
- `range(start, end)` - Generate integer range

### Numeric Operations
- `abs(x)` - Absolute value
- `max(a, b)` - Maximum of two values
- `min(a, b)` - Minimum of two values
- `mod(a, b)` - Modulo operation (ERROR effect)

### Boolean Operations
- `xor(a, b)` - Logical XOR

### Comparison
- `>(a, b)`, `<(a, b)`, `>=(a, b)`, `<=(a, b)` - Numeric comparisons
- `!=(a, b)` - Not equal

### Type Checking
- `number?(x)` - Check if number
- `string?(x)` - Check if string  
- `list?(x)` - Check if list
- `bool?(x)` - Check if boolean

### Higher-Order Functions
- `map(fn, lst)` - Apply function to each element
- `filter(pred, lst)` - Keep elements matching predicate
- `fold(fn, init, lst)` - Fold/reduce list

## String Functions (`strings`)

String manipulation utilities.

- `string-length(s)` - String length
- `string-concat(s1, s2)` - Concatenate strings
- `string-upper(s)` - Convert to uppercase
- `string-lower(s)` - Convert to lowercase
- `string-trim(s)` - Remove whitespace
- `string-split(s, delim)` - Split string
- `string-join(lst, sep)` - Join strings
- `string-contains?(s, sub)` - Check substring
- `string-starts-with?(s, prefix)` - Check prefix
- `string-ends-with?(s, suffix)` - Check suffix
- `string-replace(s, old, new)` - Replace substring
- `string-slice(s, start, end)` - Extract substring
- `char-at(s, idx)` - Get character at index
- `string->list(s)` - Convert to character list
- `list->string(chars)` - Convert from character list

## IO Functions (`io`)

File and console I/O operations. All have IO/ERROR effects.

### File Operations
- `file-read(path)` - Read entire file
- `file-write(path, content)` - Write file
- `file-append(path, content)` - Append to file
- `file-exists?(path)` - Check if file exists
- `file-delete(path)` - Delete file
- `file-read-lines(path)` - Read file as lines
- `file-write-lines(path, lines)` - Write lines to file

### Directory Operations
- `dir-list(path)` - List directory contents
- `dir-create(path)` - Create directory
- `current-directory()` - Get current directory

### Console I/O
- `read-line()` - Read line from stdin
- `print-line(s)` - Print line to stdout

### JSON Operations
- `json-parse(s)` - Parse JSON string
- `json-stringify(obj)` - Convert to JSON

## Math Functions (`math`)

Mathematical operations.

### Trigonometric
- `sin(x)`, `cos(x)`, `tan(x)` - Basic trig functions
- `asin(x)`, `acos(x)`, `atan(x)` - Inverse trig (ERROR effect)
- `atan2(y, x)` - Two-argument arctangent
- `degrees(x)` - Radians to degrees
- `radians(x)` - Degrees to radians

### Exponential/Logarithmic
- `exp(x)` - e^x
- `log(x)` - Natural logarithm (ERROR effect)
- `log10(x)` - Base-10 logarithm (ERROR effect)
- `log2(x)` - Base-2 logarithm (ERROR effect)
- `pow(x, y)` - x^y
- `sqrt(x)` - Square root (ERROR effect)

### Rounding
- `ceil(x)` - Round up
- `floor(x)` - Round down
- `round(x)` - Round to nearest integer
- `round-to(x, places)` - Round to decimal places

### Constants
- `pi()` - π (3.14159...)
- `e()` - e (2.71828...)
- `tau()` - τ (2π)

### Other
- `hypot(x, y)` - Euclidean distance
- `factorial(n)` - n! (ERROR effect)
- `gcd(a, b)` - Greatest common divisor
- `lcm(a, b)` - Least common multiple
- `sum(lst)` - Sum of list
- `product(lst)` - Product of list
- `mean(lst)` - Average (ERROR effect)
- `clamp(x, low, high)` - Constrain value to range

## Data Structures (`data`)

Advanced data structure operations.

### Dictionary/Map Operations
- `dict-new()` - Create empty dictionary
- `dict-get(d, key)` - Get value
- `dict-get-or(d, key, default)` - Get with default
- `dict-set(d, key, value)` - Set value (returns new dict)
- `dict-remove(d, key)` - Remove key
- `dict-has?(d, key)` - Check if key exists
- `dict-keys(d)` - Get all keys
- `dict-values(d)` - Get all values
- `dict-items(d)` - Get key-value pairs
- `dict-size(d)` - Number of entries
- `dict-merge(d1, d2)` - Merge dictionaries

### Set Operations
- `set-new()` - Create empty set
- `set-from-list(lst)` - Create from list
- `set-add(s, elem)` - Add element
- `set-remove(s, elem)` - Remove element
- `set-has?(s, elem)` - Check membership
- `set-union(s1, s2)` - Set union
- `set-intersection(s1, s2)` - Set intersection
- `set-difference(s1, s2)` - Set difference
- `set-size(s)` - Number of elements
- `set-to-list(s)` - Convert to list

### Advanced List Operations
- `list-slice(lst, start, end)` - Extract sublist
- `list-reverse(lst)` - Reverse list
- `list-sort(lst)` - Sort list
- `list-flatten(lst)` - Flatten nested lists
- `list-zip(lst1, lst2)` - Zip two lists
- `list-unzip(pairs)` - Unzip list of pairs
- `list-partition(pred, lst)` - Partition by predicate
- `list-find(pred, lst)` - Find first match
- `list-index-of(elem, lst)` - Find element index
- `list-unique(lst)` - Remove duplicates

### Queue Operations
- `queue-new()` - Create empty queue
- `queue-enqueue(q, elem)` - Add to back
- `queue-dequeue(q)` - Remove from front (ERROR effect)
- `queue-peek(q)` - View front (ERROR effect)
- `queue-empty?(q)` - Check if empty

## Functional Programming (`functional`)

Higher-order functions and combinators.

### Function Composition
- `compose(f, g)` - Function composition (f ∘ g)
- `pipe(f, g)` - Pipeline composition (g ∘ f)
- `identity(x)` - Identity function
- `const(x)` - Constant function
- `flip(f)` - Flip arguments

### Partial Application
- `partial(f, x)` - Partial application
- `partial-right(f, x)` - Right partial application
- `curry(f)` - Curry binary function
- `uncurry(f)` - Uncurry function
- `apply(f, args)` - Apply function to argument list

### Memoization
- `memoize(f)` - Memoize function results

### List Operations
- `map-indexed(f, lst)` - Map with index
- `filter-map(f, lst)` - Filter and map
- `flat-map(f, lst)` - Map and flatten
- `fold-right(f, init, lst)` - Right fold
- `scan(f, init, lst)` - Scan (progressive fold)

### Predicates
- `all?(pred, lst)` - Check if all match
- `any?(pred, lst)` - Check if any match
- `none?(pred, lst)` - Check if none match

### Iteration
- `iterate(f, init, n)` - Iterate function n times
- `repeat(x, n)` - Repeat value n times
- `replicate(n, f)` - Generate n values with function

### Grouping
- `group-by(key-fn, lst)` - Group by key function
- `chunk(n, lst)` - Split into chunks
- `sliding-window(n, lst)` - Sliding window

## Date/Time Functions (`datetime`)

Date and time manipulation. Functions marked with TIME effect access current time.

### Current Time (TIME effect)
- `datetime:now()` - Current local datetime
- `datetime:utcnow()` - Current UTC datetime
- `datetime:today()` - Current date

### Creation
- `datetime:create(year, month, day, hour, min, sec)` - Create datetime
- `date:create(year, month, day)` - Create date

### Parsing (ERROR effect)
- `datetime:parse(str, format)` - Parse with format
- `datetime:parse-iso(str)` - Parse ISO format

### Formatting
- `datetime:format(dt, format)` - Format datetime
- `datetime:iso-format(dt)` - ISO format

### Components
- `datetime:year(dt)`, `datetime:month(dt)`, `datetime:day(dt)`
- `datetime:hour(dt)`, `datetime:minute(dt)`, `datetime:second(dt)`
- `datetime:weekday(dt)` - Day of week (0=Monday)

### Arithmetic
- `datetime:add-days(dt, n)` - Add days
- `datetime:add-hours(dt, n)` - Add hours
- `datetime:add-minutes(dt, n)` - Add minutes
- `datetime:add-seconds(dt, n)` - Add seconds

### Comparison
- `datetime:before?(dt1, dt2)` - Check if before
- `datetime:after?(dt1, dt2)` - Check if after
- `datetime:equal?(dt1, dt2)` - Check if equal

### Differences
- `datetime:diff-seconds(dt1, dt2)` - Difference in seconds
- `datetime:diff-days(dt1, dt2)` - Difference in days

### Unix Timestamps
- `datetime:to-timestamp(dt)` - Convert to timestamp
- `datetime:from-timestamp(ts)` - Create from timestamp

## Usage Examples

```lisp
; String manipulation
(string-join (map string-upper (string-split "hello world" " ")) "-")
; => "HELLO-WORLD"

; File I/O
(let ((lines (file-read-lines "data.txt")))
  (file-write-lines "output.txt" 
    (map (lambda (line) (string-upper line)) lines)))

; Math operations
(let ((numbers [1 2 3 4 5]))
  (io:print "Sum:" (sum numbers))
  (io:print "Mean:" (mean numbers))
  (io:print "Product:" (product numbers)))

; Data structures
(let ((scores (dict-new)))
  (let ((scores (dict-set scores "Alice" 95)))
    (let ((scores (dict-set scores "Bob" 87)))
      (io:print "Alice's score:" (dict-get scores "Alice")))))

; Functional programming
(let ((add-10 (partial + 10))
      (double (partial * 2))
      (add-10-and-double (compose double add-10)))
  (map add-10-and-double [1 2 3 4 5]))
; => [22 24 26 28 30]

; Date/time
(let ((now (datetime:now)))
  (io:print "Current time:" (datetime:iso-format now))
  (io:print "Tomorrow:" (datetime:iso-format 
    (datetime:add-days now 1))))
```

## Effect Summary

Most standard library functions are PURE, meaning they can be optimized and memoized. Functions with effects:

- **IO effect**: All file operations, console I/O
- **ERROR effect**: Operations that can fail (division, parsing, etc.)
- **TIME effect**: Functions that access current time
- **STATE effect**: None (state is handled separately)
- **RANDOM effect**: None (randomness handled by effect primitives)
- **NETWORK effect**: None (networking handled by effect primitives)