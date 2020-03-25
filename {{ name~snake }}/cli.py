"""

Usage:
  litex FILES...
"""
from typing import *
from . import walk, parse
from docopt import docopt

def run():
  args: Dict[str, Any] = docopt(__doc__)
  for filepath in args['FILES...']:
    with open(filepath, 'r') as file:
      walk(parse(file.read()))

