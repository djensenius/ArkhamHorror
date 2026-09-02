#!/usr/bin/env python3
"""Executable wrapper for the importable emitted-key extractor."""

import sys

from extract_backend_i18n_keys import main


if __name__ == "__main__":
    sys.exit(main())
