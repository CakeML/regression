import { format } from "https://esm.sh/v135/date-fns@3.3.1/format.mjs"
import { formatDistanceToNow } from "https://esm.sh/v135/date-fns@3.3.1/formatDistanceToNow.mjs"
import { formatDuration } from "https://esm.sh/v135/date-fns@3.3.1/formatDuration.mjs"
import { intervalToDuration } from "https://esm.sh/v135/date-fns@3.3.1/intervalToDuration.mjs"
import { addSeconds } from "https://esm.sh/v135/date-fns@3.3.1/addSeconds.mjs"

const dateFormat = "MMM dd HH:mm:ss"
const optionsAgo = {addSuffix: true}

const duration = pt => {
  const s = parseInt(pt.slice(2, -1))
  const d = new Date()
  return intervalToDuration({start: d, end: addSeconds(d, s)})
}

function localiseTimes(all) {
  for (const elt of Array.from(document.getElementsByTagName('time'))) {
    if (elt.classList.contains('ago')) {
      elt.title = elt.innerText.slice(elt.innerText.indexOf('[')+1,-1)
      elt.innerText = ` [${formatDistanceToNow(elt.dateTime, optionsAgo)}]`
    }
    else if (elt.classList.contains('since')) {
      elt.title = elt.innerText
      elt.innerText = `[elapsed: ${formatDistanceToNow(elt.dateTime)}]`
    }
    else if (elt.classList.contains('duration')) {
      elt.title = elt.innerText
      elt.innerText = `[average: ${formatDuration(duration(elt.dateTime))}]`
    }
    else if (all) {
      elt.title = elt.innerText
      elt.innerText = format(elt.dateTime, dateFormat)
    }
  }
}

const upds = new EventSource('/regression-updates.cgi')
upds.onmessage = function(e) {
  if (location.pathname.includes('/job/' + e.data) ||
      !(location.pathname.includes('/job/')))
    location.reload()
}

localiseTimes(true)
setInterval(localiseTimes, 60000, false)
