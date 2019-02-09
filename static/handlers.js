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
  xhttp.setRequestHeader("Content-Type", "application/json; charset=utf-8");
  xhttp.send(JSON.stringify({"url": url}));
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

function addOverlays(urls, overlay) {
  console.log("Adding overlays..", urls, overlay);

  var i = 1;
  for(url of urls) {
    if(url != null) {
      let canvas = document.getElementById("canvas" + i);
      let img    = new Image();

      img.onload = function() { addOverlay(canvas, img, overlay); };
      img.src    = url;
    }
    i++;
  }
}

function addOverlay(canvas, url, overlay) {
  switch(overlay) {
    case "mizige":
      addMizige(canvas, url);
      break;
    case "":
      break;
    default:
      console.log("Unknown overlay " + overlay);
      break;
  }
}

function addMizige(canvas, img) {
  var ctx    = canvas.getContext("2d");
  var width  = canvas.width;
  var height = canvas.height;
  var rotate = false;

  if(rotate) {
    ctx.save();
    ctx.translate(0.5 * width, 0.5 * height);
    ctx.rotate(-0.5 * Math.PI);
    ctx.drawImage(img, -0.5 * height, -0.5 * width, height, width);
    ctx.restore();
  } else {
    ctx.drawImage(img, 0, 0, width, height);
  }

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
