"""
Setup script for FluentAI with Rust extensions
"""

from setuptools import setup, find_packages
from setuptools_rust import Binding, RustExtension
import os

# Read README for long description
def read_readme():
    readme_path = os.path.join(os.path.dirname(__file__), 'README.md')
    if os.path.exists(readme_path):
        with open(readme_path, 'r', encoding='utf-8') as f:
            return f.read()
    return "FluentAI - A modern Lisp dialect with advanced features"

setup(
    name="fluentai",
    version="0.1.0",
    author="FluentAI Contributors",
    description="A modern Lisp dialect with graph-based AST, effects system, and JIT compilation",
    long_description=read_readme(),
    long_description_content_type="text/markdown",
    url="https://github.com/yourusername/fluentai",
    packages=find_packages(where="python"),
    package_dir={"": "python"},
    rust_extensions=[
        RustExtension(
            "fluentai.fluentai_rust",
            path="rust/fluentai-py/Cargo.toml",
            binding=Binding.PyO3,
            # Specify build configuration
            debug=False,
        )
    ],
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Programming Language :: Rust",
        "Topic :: Software Development :: Compilers",
        "Topic :: Software Development :: Interpreters",
    ],
    python_requires=">=3.9",
    install_requires=[
        # Core dependencies (currently minimal)
    ],
    extras_require={
        "dev": [
            "pytest>=7.0.0",
            "pytest-cov>=4.0.0",
            "black>=22.0.0",
            "flake8>=4.0.0",
            "isort>=5.0.0",
            "mypy>=0.950",
            "setuptools-rust>=1.5.0",
        ],
        "llvm": [
            "llvmlite>=0.39.0",
        ],
        "ml": [
            "numpy>=1.20.0",
        ],
    },
    entry_points={
        "console_scripts": [
            "fluentai=repl.main:main",
            "cl-repl=repl.repl:main",
        ],
    },
    include_package_data=True,
    package_data={
        "stdlib": ["*.ai"],
    },
    zip_safe=False,  # Required for Rust extensions
)