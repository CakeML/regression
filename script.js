function addZero(n) { if (n < 10) { n = "0" + n; } return n; }
function monthName(n) {
  if (n ==  0) { return "Jan"; }
  if (n ==  1) { return "Feb"; }
  if (n ==  2) { return "Mar"; }
  if (n ==  3) { return "Apr"; }
  if (n ==  4) { return "May"; }
  if (n ==  5) { return "Jun"; }
  if (n ==  6) { return "Jul"; }
  if (n ==  7) { return "Aug"; }
  if (n ==  8) { return "Sep"; }
  if (n ==  9) { return "Oct"; }
  if (n == 10) { return "Nov"; }
  if (n == 11) { return "Dec"; }
}
function formatDate(d) {
  return (monthName(d.getMonth()) + " " + addZero(d.getDate()) + " " + addZero(d.getHours()) + ":" + addZero(d.getMinutes()) + ":" + addZero(d.getSeconds()));
}
function localiseTimes() {
  var ls = document.getElementsByTagName("time");
  for (var i = 0; i < ls.length; i++) {
    ls[i].innerHTML = formatDate(new Date(ls[i].dateTime));
  }
}
