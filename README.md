# Racket Showcase

A collection of Racket code examples covering algorithms, applications, Scribble documentation, utility snippets, and language features.

![Racket](https://img.shields.io/badge/Racket-9F1D20?logo=racket&logoColor=white)

**English** · [中文](README.zh-CN.md)

## Sections

### algo-showcase

Algorithm implementations in Racket, organized by source:

- **codewars** — Solutions to Codewars challenges
- **hello-algo** — Implementations from *Hello 算法* (Hello Algo)
- **leetcode** — Solutions to LeetCode problems

### app-showcase

Complete applications built with Racket:

| App | Description |
|-----|-------------|
| `2048` | The 2048 puzzle game |
| `7gui` | Implementations of the 7GUI tasks (MVC, macros, typed variants) |
| `artascope` | Artascope pattern generator |
| `covid-risk-demo` | COVID risk visualization demo |
| `hebi` | Snake-like game |
| `icon-viewer` | System icon viewer |
| `imgbox` | Image processing tool |
| `interpreters` | Programming language interpreters |
| `lcfu` | Let's Code For Understanding — educational examples |
| `puzzle` | Puzzle game with MVC architecture |
| `snake` | Classic snake game |
| `text-viewer` | Text file viewer |
| `web-tutorial` | Web applications built with the Racket web server (listit series) |
| `ydiff` | Diff tool with demos |

### scribble-showcase

Examples using [Scribble](https://docs.racket-lang.org/scribble/), Racket's documentation system:

- **racket-book** — A structured Racket book project with chapters on getting started, basics, practical programs, and advanced topics

### snippet-showcase

Standalone utility snippets:

- **json** — JSON configuration parsing (`json.rkt`, `config.json`)
- **tcp** — TCP client and server examples (`tcp-client.rkt`, `tcp-server.rkt`)

### syntax-showcase

Racket language feature explorations:

| Directory | Topic |
|-----------|-------|
| `oop` | Object-oriented programming in Racket |
| `racket-weekend` | Weekend study notes covering contracts, functional programming, macros, OOP, structs, types, and web programming |
| `recursive` | Recursion patterns and techniques |
| `scheme-mind` | Mind map of Scheme concepts |

## Requirements

- [Racket](https://racket-lang.org/) 7.0 or later
- Some apps may require additional packages (check individual directories)

## Usage

Open any `.rkt` file in DrRacket and click **Run**, or run from the command line:

```bash
racket path/to/file.rkt
```

For Scribble documents:

```bash
scribble --html path/to/document.scrbl
```

## License

This project does not currently include a license file.
