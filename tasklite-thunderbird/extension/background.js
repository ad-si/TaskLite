const nativeHostName = "tasklite"
const menuItems = {
  "add-to-tasklite": { title: "Add to TaskLite", edit: false },
  "add-to-tasklite-and-edit": { title: "Add to TaskLite and Edit", edit: true },
}
const menuIds = Object.keys(menuItems)

messenger.runtime.onInstalled.addListener(async () => {
  await messenger.menus.removeAll()
  for (const [id, { title }] of Object.entries(menuItems)) {
    messenger.menus.create({
      id,
      title,
      // The other contexts are for the body of a displayed message
      contexts: ["message_list", "page", "selection", "link", "image"],
    })
  }
})

// The body contexts also apply to web pages, so only show the item
// if the tab displays a message
messenger.menus.onShown.addListener(async (info, tab) => {
  if (!info.menuIds.some(id => menuIds.includes(id)) || info.contexts.includes("message_list")) {
    return
  }

  const displayedCount = await messenger.messageDisplay
    .getDisplayedMessages(tab.id)
    .then(messageList => messageList.messages.length)
    .catch(() => 0)
  for (const id of menuIds) {
    await messenger.menus.update(id, { visible: displayedCount > 0 })
  }
  await messenger.menus.refresh()
})

messenger.menus.onHidden.addListener(async () => {
  for (const id of menuIds) {
    await messenger.menus.update(id, { visible: true })
  }
})

messenger.menus.onClicked.addListener(async (info, tab) => {
  const menuItem = menuItems[info.menuItemId]
  if (!menuItem) return

  const messageList = info.selectedMessages
    ?? await messenger.messageDisplay.getDisplayedMessages(tab.id)

  const results = []
  for await (const message of iterateMessageList(messageList)) {
    try {
      const rawFile = await messenger.messages.getRaw(message.id, { data_format: "File" })
      const rawBytes = new Uint8Array(await rawFile.arrayBuffer())
      results.push(await messenger.runtime.sendNativeMessage(nativeHostName, {
        action: "importEmail",
        emailBase64: rawBytes.toBase64(),
      }))
    }
    catch (error) {
      results.push({ status: "error", message: `${message.subject}: ${error.message}` })
    }
  }

  if (menuItem.edit) {
    for (const result of results) {
      if (result.ulid) await openEditor(result.ulid)
    }
    // Only notify about errors, as the opened editors show the rest
    if (results.some(result => result.status === "error")) await notify(results)
  }
  else {
    await notify(results)
  }
})

async function openEditor(ulid) {
  await messenger.windows.create({
    type: "popup",
    url: `edit.html?ulid=${encodeURIComponent(ulid)}`,
    width: 720,
    height: 720,
  })
}

async function* iterateMessageList(messageList) {
  let page = messageList
  while (true) {
    yield* page.messages
    if (!page.id) return
    page = await messenger.messages.continueList(page.id)
  }
}

async function notify(results) {
  const added = results.filter(result => result.status === "added")
  const existing = results.filter(result => result.status === "exists")
  const failed = results.filter(result => result.status === "error")

  const lines = []
  if (added.length > 0) lines.push(`Added ${added.length} task(s)`)
  if (existing.length > 0) lines.push(`${existing.length} already in TaskLite`)
  for (const result of failed) lines.push(`Error: ${result.message}`)

  await messenger.notifications.create({
    type: "basic",
    iconUrl: "icons/icon-64.png",
    title: failed.length > 0 ? "TaskLite: Import failed" : "TaskLite",
    message: lines.join("\n"),
  })
}
