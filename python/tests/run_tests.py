#!/usr/bin/env python3
"""
Test runner for FluentAI

Usage:
    python run_tests.py              # Run all tests
    python run_tests.py parser       # Run parser tests
    python run_tests.py vm           # Run VM tests
    python run_tests.py --verbose    # Run with verbose output
"""

import sys
import unittest
import argparse

# Test suite mappings
TEST_SUITES = {
    'parser': [
        'tests.test_parser_lexer',
        'tests.test_parser_sexpr',
        'tests.test_parser_optimized'
    ],
    'module': [
        'tests.test_module_system_simple'
    ],
    'codegen': [
        'tests.test_code_generation_simple'
    ],
    'vm': [
        'tests.test_vm_components'
    ],
    'error': [
        'tests.test_error_handling'
    ],
    'stdlib': [
        'tests.test_stdlib_modules'
    ],
    'all': []  # Will be populated with all test modules
}

# Populate 'all' with all test modules
TEST_SUITES['all'] = [module for suite in TEST_SUITES.values() for module in suite if suite]


def run_tests(suite_name='all', verbose=2):
    """Run the specified test suite"""
    
    if suite_name not in TEST_SUITES:
        print(f"Unknown test suite: {suite_name}")
        print(f"Available suites: {', '.join(TEST_SUITES.keys())}")
        return False
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    if suite_name == 'all':
        # Discover all tests
        suite = loader.discover('tests', pattern='test_*.py')
    else:
        # Load specific test modules
        for module_name in TEST_SUITES[suite_name]:
            try:
                module = loader.loadTestsFromName(module_name)
                suite.addTests(module)
            except Exception as e:
                print(f"Error loading {module_name}: {e}")
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=verbose)
    result = runner.run(suite)
    
    # Print summary
    print("\n" + "="*70)
    print(f"Tests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print(f"Skipped: {len(result.skipped)}")
    
    if result.wasSuccessful():
        print("\n✅ All tests passed!")
    else:
        print("\n❌ Some tests failed!")
    
    return result.wasSuccessful()


def main():
    parser = argparse.ArgumentParser(description='Run FluentAI tests')
    parser.add_argument('suite', nargs='?', default='all',
                      choices=list(TEST_SUITES.keys()),
                      help='Test suite to run (default: all)')
    parser.add_argument('-v', '--verbose', action='store_true',
                      help='Verbose output')
    parser.add_argument('-q', '--quiet', action='store_true',
                      help='Minimal output')
    
    args = parser.parse_args()
    
    # Determine verbosity level
    verbosity = 2  # Default
    if args.quiet:
        verbosity = 0
    elif args.verbose:
        verbosity = 2
    
    # Run tests
    success = run_tests(args.suite, verbosity)
    
    # Exit with appropriate code
    sys.exit(0 if success else 1)


if __name__ == '__main__':
    main()