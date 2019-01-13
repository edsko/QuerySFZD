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

function useQuery(query) {
  console.log("Using query " + query);
  document.getElementById("query").value = query;
}

function addOverlays(numChars, overlay) {
  console.log("Adding overlays..", numChars, overlay);

  for(var i = 1; i <= numChars; i++) {
    var canvas = document.getElementById("canvas" + i);
    // Canvas may be null for missing characters
    if(canvas != null) addOverlay(canvas, overlay);
  }
}

function addOverlay(canvas, overlay) {
  switch(overlay) {
    case "mizige":
      addMizige(canvas);
      break;
    case "":
      break;
    default:
      console.log("Unknown overlay " + overlay);
      break;
  }
}

function addMizige(canvas) {
  var ctx    = canvas.getContext("2d");
  var width  = canvas.width;
  var height = canvas.height;

  ctx.lineWidth = 2;
  ctx.strokeStyle = "#0000ff90";
  ctx.setLineDash([10,10]);

  ctx.beginPath();
  ctx.moveTo(0,0.5 * height);
  ctx.lineTo(width,0.5 * height);
  ctx.stroke();

  ctx.beginPath();
  ctx.moveTo(0.5 * width,0);
  ctx.lineTo(0.5 * width,height);
  ctx.stroke();

  ctx.beginPath();
  ctx.moveTo(0,0);
  ctx.lineTo(width,height);
  ctx.stroke();

  ctx.beginPath();
  ctx.moveTo(width,0);
  ctx.lineTo(0,height);
  ctx.stroke();

}
