// Markdown extension for a YAML frontmatter at the start of the document.
// Unlike `yamlFrontmatter` from `@codemirror/lang-yaml`,
// it also accepts `...` as the closing line, which `tasklite edit` uses.

import { yamlLanguage } from "@codemirror/lang-yaml"
import { parseMixed } from "@lezer/common"
import { tags } from "@lezer/highlight"

const delimiters = ["---", "..."]

export const frontmatter = {
  defineNodes: [
    { name: "Frontmatter", block: true },
    { name: "FrontmatterMark", style: tags.meta },
    { name: "FrontmatterContent" },
  ],
  parseBlock: [{
    name: "Frontmatter",
    // `---` would otherwise be parsed as a horizontal rule
    before: "HorizontalRule",
    parse(cx, line) {
      if (cx.lineStart !== 0 || line.text.trimEnd() !== "---") return false

      const children = [cx.elt("FrontmatterMark", 0, line.text.length)]
      let contentFrom = null
      let contentTo = null
      let end = line.text.length

      while (cx.nextLine()) {
        if (delimiters.includes(line.text.trimEnd())) {
          if (contentFrom !== null) {
            children.push(cx.elt("FrontmatterContent", contentFrom, contentTo))
          }
          end = cx.lineStart + line.text.length
          children.push(cx.elt("FrontmatterMark", cx.lineStart, end))
          cx.nextLine()
          cx.addElement(cx.elt("Frontmatter", 0, end, children))
          return true
        }
        contentFrom ??= cx.lineStart
        contentTo = cx.lineStart + line.text.length
        end = contentTo
      }

      // Unterminated frontmatter extends to the end of the document
      if (contentFrom !== null) {
        children.push(cx.elt("FrontmatterContent", contentFrom, contentTo))
      }
      cx.addElement(cx.elt("Frontmatter", 0, end, children))
      return true
    },
  }],
  wrap: parseMixed(node =>
    node.name === "FrontmatterContent" ? { parser: yamlLanguage.parser } : null,
  ),
}
