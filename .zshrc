# launch daily notes with `notes`
# launch specific notes with `notes woodworking`
# this is a cloud synced file, so it's live on all my devices
function notes(){
  SUBJECT="${1:=daily}"
  if [ -z "$SUBJECT" ]
  then
    (cd ~/Documents/notes && hx NOTES.md)
  else
    (cd ~/Documents/notes && hx "${SUBJECT:u}.md" NOTES.md)
  fi
}
