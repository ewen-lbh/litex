"""

Usage:
  litex FILES...
"""
from __future__ import annotations
from typing import Any
from . import walk, parse
from docopt import docopt

def run():
  args: dict[str, Any] = docopt(__doc__)
  for filepath in args['FILES...']:
    with open(filepath, 'r') as file:
      walk(parse(file.read()))

