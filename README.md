# LaTeX Preprocessor

***
> I'm following readme-driven development for this project.
> Nothing has been implemented yet, I'm still figuring out the language's design.
***

## Global structure

To eliminate the need of a stupid `\begin{document} ... \end{document}`, the structure is as follows:

```
preamble
--------

document content
```

example:

```
use tikz
use mathtools
--------

Lorem ipsum dolor sit amet.
```

## Environments

Environments are written as `@environment`, and indentation defines nesting

```
@eq
  @align
    thing &= stuff
```

Compiles to

```latex
\begin{eq}
  \begin{align}
    thing &= stuff
  \end{align}
\end{eq}
```

## Preamble-specific commands

### Using packages

To use a package, simply add a line `use mypackagename`

To add options, do `use mypackage [options]`, eg `use geometry [a4paper, total={6.5in, 9.5in}]` will compile to `\usepackage[a4paper, total={6.5in, 9.5in}]{geometry}`

`litex` also comes with a CLI tool to add packages automatically, leveraging `tlmgr`: to add a package, do `litex use tikz`  and `litex` will:

- Add a line to the preamble: `use tikz`

- If the package is missing, run `tlmgr install tikz`

Note that if you try to compile a file, `use` declarations will be compiled whether the package is installed or not. To automatically install missing packages upon compilation, use `litex --install my-file.litex`. You can also get a list of missing packages with `litex --list-missing-packages my-file.litex`.

### Defining commands and environments

```
torseur =
    tensor(_#1)({
        @array(cc)
            #2 & #5 \\
            #3 & #6 \\
            #4 & #7
    })_(vec(x); vec(y); vec(z))
```

This will compile to the following:

```latex
\newcommand{\torseur}[7]{
    \tensor[_{#1}]{\left{
        \begin{array}{cc}
            #2 & #5 \\
            #3 & #6 \\
            #4 & #7
        \end{array}
    \right}}_{\vec{x}; \vec{y}; \vec{z}}
}
```

You can then use the defined command:

```
And so the solution becomes:

    A = torseur(A)(1.5)(3)(Z)(L)(0)(N)
```

Will compile to

```latex
\paragraph{}
And so the solution becomes:
\begin{eq*}
\begin{align}
    A &= \torseur{A}{1.5}{3}{Z}{L}{0}{N}
\end{align}
\end{eq*}
```

Or, with `litex --resolve-commands`:

```latex
\paragraph{}
And so the solution becomes:
\begin{eq*}
\begin{align}
    A &= \tensor[_{A}]{\left{
        \begin{array}{cc}
            1.5 & L \\
            3 & 0 \\
            Z & N
        \end{array}
    \right}}_{\vec{x}; \vec{y}; \vec{z}}
\end{align}
\end{eq*}
```

### Global preambles

If you keep defining some configurations on the preamble of _all_ of your documents, litex can help you: do `litex config preamble.append` (or `litex config --global preamble.append` to change the _global_ configuration) and edit what will be appended to the preamble. You can also use the `preamble.prepend` config entry to modify what will be _prepended_ to the preamble.

A global preamble will result in your source `.litex` file being less portable, as people will be missing some preamble entries to compile your file and get the same exact result. Prefer a local configuration, which will create a `litex-config.yaml` file in the project's directory, and will be honored by the litex compiler.

## Commands

Commands use the more common `()` to specify arguments. Real parentheses should be surrounded by whitespace to prevent the parser thinking `this (totally) normal text` is `\this{totally}`. Multiple arguments are put in multiple parentheses, like in ![](http://www.texrendr.com/cgi-bin/mimetex?\LaTeX). We can't just separate arguments with commas, because some arguments use commas inside (eg. `siunitx`).



## Transformations

### Symbols

| Write      | Compiled to     | See               | Notes                                                                                                                                                                                 |
| ---------- | --------------- | ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| α          | \alpha          | ![α](http://www.texrendr.com/cgi-bin/mimetex?\alpha)          | Works with all greek letters                                                                                                                                                          |
| ÷          | \div            | ![÷](http://www.texrendr.com/cgi-bin/mimetex?\div)            |                                                                                                                                                                                       |
| ab/cd      | a\frac{b}{c}d   | ![](http://www.texrendr.com/cgi-bin/mimetex?a\frac{b}{c}d)   |                                                                                                                                                                                       |
| e ab / cdf | e\frac{ab}{cdf} | ![](http://www.texrendr.com/cgi-bin/mimetex?e\frac{ab}{cdf}) |                                                                                                                                                                                       |
| \* or ·    | \cdot           | ![](http://www.texrendr.com/cgi-bin/mimetex?\cdot)           | Can be set to compile to `\times` too                                                                                                                                                 |
| \*\*       | \ast            | ![](http://www.texrendr.com/cgi-bin/mimetex?\ast)            |                                                                                                                                                                                       |
| %          | \%              | ![](http://www.texrendr.com/cgi-bin/mimetex?\%)              |                                                                                                                                                                                       |
| ×          | \times          | ![](http://www.texrendr.com/cgi-bin/mimetex?\times)          |                                                                                                                                                                                       |
| ʌ          | \land           | ![](http://www.texrendr.com/cgi-bin/mimetex?\wedge)           | Also works with \lor & \lnot.                                                                                                                                                                                      |
| ⇒          | \implies        | ![](http://www.texrendr.com/cgi-bin/mimetex?\Rightarrow)        |                                                                                                                                                                           |
| ⇐          | \impliedby      | ![](http://www.texrendr.com/cgi-bin/mimetex?\Leftarrow)      |                                                                                                                                                                                       |
| ⇔          | \iff            | ![](http://www.texrendr.com/cgi-bin/mimetex?\Leftrightarrow)            |     
| →          | \to             | ![](http://www.texrendr.com/cgi-bin/mimetex?\to)             | All other arrows, single or double-struck, are compiled to their ![](http://www.texrendr.com/cgi-bin/mimetex?\LaTeX) commands. You can also use ascii approximation instead of the unicode characters: -> for →, <=> for ⇔, etc. |
| ∈          | \in             | ![](http://www.texrendr.com/cgi-bin/mimetex?\in)             |                                                                                                                                                                                       |
| ∀          | \forall         | ![](http://www.texrendr.com/cgi-bin/mimetex?\forall)         |                                                                                                                                                                                       |
| log        | \log            | ![](http://www.texrendr.com/cgi-bin/mimetex?\log)            | Works with all functions (sin, tan, cot, ln, etc.)                                                                                                                                    |
| \|->        | \mapsto         | ![](http://www.texrendr.com/cgi-bin/mimetex?\mapsto)         |                                                                                                                                                                          |
| LaTeX      | \LaTeX          | ![](http://www.texrendr.com/cgi-bin/mimetex?\LaTeX)          |                
| a..        | \ddot{a}        | ![](http://www.texrendr.com/cgi-bin/mimetex?\ddot{a})        | Also works with `.`, which expands to \dot. Needs 0 spaces before the dot(s) and at least one after.
| a->        | \vec{a}         | ![](http://www.texrendr.com/cgi-bin/mimetex?\vec{a})         |  Needs 0 spaces before the dot(s) and at least one after.
| AB->     |\overrightarrow{AB}|![](http://www.texrendr.com/cgi-bin/mimetex?\overrightarrow{AB}) | While `.` and `..` only applies to the previous character, `->` can work on one or two.
