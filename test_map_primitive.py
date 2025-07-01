#!/usr/bin/env python3
"""Check which primitives are available"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from src.core.primitives import PRIMITIVES

# Check what's available
print("Available primitives:")
for name in sorted(PRIMITIVES.primitives.keys()):
    print(f"  {name}")

print(f"\nTotal: {len(PRIMITIVES.primitives)} primitives")