# Racket Showcase

Racket 代码示例合集，涵盖算法、应用程序、Scribble 文档、实用代码片段和语言特性。

![Racket](https://img.shields.io/badge/Racket-9F1D20?logo=racket&logoColor=white) [![License](https://img.shields.io/badge/license-MIT-blue)](LICENSE)

[English](README.md) · **中文**

## 分类

| 类别 | 说明 | 技术 |
|------|------|------|
| `algo-showcase` | 按来源分类的算法实现 | Racket |
| `app-showcase` | 用 Racket 构建的完整应用 | Racket、`racket/gui`、Web 服务器 |
| `scribble-showcase` | 使用 Scribble 编写的文档 | Scribble |
| `snippet-showcase` | 独立的实用代码片段 | Racket |
| `syntax-showcase` | Racket 语言特性探索 | Racket |

### algo-showcase

Racket 算法实现，按来源分类：

- **codewars** — Codewars 挑战题解
- **hello-algo** — 《Hello 算法》中的实现
- **leetcode** — LeetCode 题解

### app-showcase

用 Racket 构建的完整应用：

| 应用 | 说明 |
|------|------|
| `2048` | 2048 数字拼图游戏 |
| `7gui` | 7GUI 任务的实现（MVC、宏、类型化变体） |
| `artascope` | 万花尺图案生成器 |
| `covid-risk-demo` | COVID 风险可视化演示 |
| `hebi` | 贪吃蛇游戏 |
| `icon-viewer` | 系统图标查看器 |
| `imgbox` | 图片处理工具 |
| `interpreters` | 编程语言解释器 |
| `lcfu` | Let's Code For Understanding — 教学示例 |
| `puzzle` | 采用 MVC 架构的拼图游戏 |
| `snake` | 经典贪吃蛇游戏 |
| `text-viewer` | 文本文件查看器 |
| `web-tutorial` | 使用 Racket Web 服务器构建的 Web 应用（listit 系列） |
| `ydiff` | Diff 工具及演示 |

### scribble-showcase

使用 [Scribble](https://docs.racket-lang.org/scribble/)（Racket 文档系统）的示例：

- **racket-book** — 结构化的 Racket 书籍项目，包含入门、基础、实用编程和高级主题等章节

### snippet-showcase

独立的实用代码片段：

- **json** — JSON 配置解析（`json.rkt`、`config.json`）
- **tcp** — TCP 客户端和服务端示例（`tcp-client.rkt`、`tcp-server.rkt`）

### syntax-showcase

Racket 语言特性探索：

| 目录 | 主题 |
|------|------|
| `oop` | Racket 面向对象编程 |
| `racket-weekend` | 周末学习笔记，涵盖契约、函数式编程、宏、OOP、结构体、类型和 Web 编程 |
| `recursive` | 递归模式与技巧 |
| `scheme-mind` | Scheme 概念思维导图 |

## 环境要求

| 依赖 | 用途 / 版本 |
|------|-------------|
| [Racket](https://racket-lang.org/) | 7.0 或更高版本 |

部分应用可能需要额外安装包（请查看各目录）。

## 使用方法

### 1. 克隆

```bash
git clone https://github.com/turinglambdaai/racket-showcase.git
cd racket-showcase
```

### 2. 运行示例

在 DrRacket 中打开任意 `.rkt` 文件并点击 **Run**，或从命令行运行：

```bash
racket path/to/file.rkt
```

Scribble 文档：

```bash
scribble --html path/to/document.scrbl
```

## 许可证

基于 [MIT 许可证](LICENSE) 授权。
