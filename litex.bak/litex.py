from os import path
from lark import Lark, Tree

def parse(contents) -> Tree:
  grammar: str = open(path.join(path.dirname(__file__), 'grammar.lark')).read()
  parser: Lark = Lark(grammar, start="start")
  return parser.parse(contents)

def walk(parsed):
  pass