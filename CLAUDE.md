  1. Always run the full workspace test suite after making any changes, no matter how isolated they seem
  2. Before declaring any fix complete, verify that all tests across the entire codebase still pass
  3. If a change breaks tests in other modules, investigate why and either:
    - Fix the broken tests if they were relying on incorrect behavior
    - Reconsider the change if it breaks legitimate functionality
    - Find a solution that satisfies all parts of the system

  This is especially important in a complex system like FluentAI where modules are interconnected in subtle ways. The LoadGlobal change was a perfect example - what seemed like a simple fix in the VM module ended up
   breaking functionality in the effects module.

  I'll make sure to follow this workflow for all future changes:
  1. Make change
  2. Run local module tests
  3. Run full workspace tests
  4. Only proceed if ALL tests pass
  5. If any test fails, investigate and resolve before moving on

