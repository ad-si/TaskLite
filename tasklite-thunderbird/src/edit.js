// Editor for a task in the same format as `tasklite edit`

import { indentWithTab } from "@codemirror/commands"
import { markdown } from "@codemirror/lang-markdown"
import { Compartment, EditorState, Prec } from "@codemirror/state"
import { oneDark } from "@codemirror/theme-one-dark"
import { EditorView, keymap } from "@codemirror/view"
import { basicSetup } from "codemirror"

import { frontmatter } from "./frontmatter.js"

const nativeHostName = "tasklite"
const ulid = new URLSearchParams(location.search).get("ulid")

const errorBox = document.getElementById("error")
const saveButton = document.getElementById("save")
const cancelButton = document.getElementById("cancel")

const darkMode = window.matchMedia("(prefers-color-scheme: dark)")
const themeCompartment = new Compartment()
const themeFor = isDark => isDark ? oneDark : []

let initialMarkdown = null

function createState(doc, { readOnly }) {
  return EditorState.create({
    doc,
    extensions: [
      basicSetup,
      markdown({ extensions: [frontmatter] }),
      themeCompartment.of(themeFor(darkMode.matches)),
      EditorView.lineWrapping,
      EditorState.readOnly.of(readOnly),
      // Tab indents with spaces, as tabs aren't allowed in YAML
      keymap.of([indentWithTab]),
      Prec.highest(keymap.of([{ key: "Mod-Enter", run: () => { save(); return true } }])),
      // Only if no other editor feature (e.g. autocompletion, search) handles it
      Prec.lowest(keymap.of([{ key: "Escape", run: () => { cancel(); return true } }])),
    ],
  })
}

const view = new EditorView({
  parent: document.getElementById("editor"),
  state: createState("Loading …", { readOnly: true }),
})

darkMode.addEventListener("change", (event) => {
  view.dispatch({ effects: themeCompartment.reconfigure(themeFor(event.matches)) })
})

function getMarkdown() {
  return view.state.doc.toString()
}

function showError(message) {
  errorBox.textContent = message
  errorBox.hidden = false
}

async function closeWindow() {
  const currentWindow = await messenger.windows.getCurrent()
  await messenger.windows.remove(currentWindow.id)
}

async function load() {
  try {
    const result = await messenger.runtime.sendNativeMessage(nativeHostName, {
      action: "getTask",
      ulid,
    })
    if (result.status !== "ok") {
      view.setState(createState("", { readOnly: true }))
      showError(result.message)
      return
    }
    document.title = `${result.message} – TaskLite`
    initialMarkdown = result.markdown
    // New state, so that the undo history doesn't contain the placeholder
    view.setState(createState(result.markdown, { readOnly: false }))
    saveButton.disabled = false
    view.focus()
  }
  catch (error) {
    view.setState(createState("", { readOnly: true }))
    showError(error.message)
  }
}

async function save() {
  if (saveButton.disabled) return
  saveButton.disabled = true
  try {
    const result = await messenger.runtime.sendNativeMessage(nativeHostName, {
      action: "updateTask",
      ulid,
      markdown: getMarkdown(),
    })
    if (["updated", "unchanged", "deleted"].includes(result.status)) {
      await closeWindow()
      return
    }
    showError(result.message)
  }
  catch (error) {
    showError(error.message)
  }
  saveButton.disabled = false
}

async function cancel() {
  const hasChanges = initialMarkdown !== null && getMarkdown() !== initialMarkdown
  if (hasChanges && !confirm("Discard your changes?")) return
  await closeWindow()
}

saveButton.addEventListener("click", save)
cancelButton.addEventListener("click", cancel)

// Escape inside the editor is handled by its keymap.
// Handled events are skipped, as e.g. closing the search panel
// removes the event's target from the editor.
document.addEventListener("keydown", (event) => {
  if (
    event.key === "Escape"
    && !event.defaultPrevented
    && !view.dom.contains(event.target)
  ) {
    event.preventDefault()
    cancel()
  }
})

load()
