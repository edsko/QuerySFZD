function prefer(url) {
  console.log("Preferring " + url);
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
     console.log("Preferring " + url + ": " + this.status);
     if (this.status == 200) {
       location.reload(false);
     }
  }
  xhttp.open("POST", "/prefer", true);
  xhttp.send(url);
}

function selectAuthor(author) {
  console.log("Selecting author " + author);
  document.getElementById("author").value = author;
}

function addFallback(author) {
  console.log("Adding fallback " + author);
  var fallbacks = document.getElementById("fallbacks");
  var oldValue  = fallbacks.value;
  var newValue  = null;
  if(oldValue == "" || oldValue.substr(-1) == ",")
    newValue = oldValue + author;
  else
    newValue = oldValue + "," + author;
  fallbacks.value = newValue;
}
