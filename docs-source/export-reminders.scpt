use scripting additions

set rnames to {}
set cdates to {}
set fmt to ""

tell application "Reminders"
  launch

  try
    set theRemList to text returned of (¬
      display dialog "Enter the Reminder list name" ¬
        default answer "" with icon note¬
    )

  on error errmsg number errnbr
    if errnbr is equal to -128 then
      display alert "User cancelled... ending." giving up after 10
    end if

    return
  end try

  set {rnames, cdates} to {name, creation date}¬
    of every reminder of list theRemList
end tell


repeat with i from 1 to number of items in rnames
  set fmt to fmt & (item i of rnames) & " : " & (item i of cdates) & return
end repeat


tell application "System Events"
  display dialog fmt as text with title "Reminder Items Creation Dates"
end tell

return
