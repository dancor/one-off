// ==UserScript==
// @name        New script 
// @namespace   Violentmonkey Scripts
// @match       https://www.rtp.pt/play/p*
// @grant       none
// @version     1.0
// @author      -
// @description 11/29/2020, 2:15:29 PM
// ==/UserScript==
window.addEventListener('load', function() {
  var url = document.URL;
  var vids = document.getElementsByClassName("rmp-video");
  if (vids.length == 1) {
    var duration = vids[0].duration;
    var hasCaptions = document.getElementsByClassName("rmp-captions").length > 0;
    url = "http://localhost:9084/?rtp+1+" + (hasCaptions ? "1" : "0") + "+" + duration + "+" + url;
    window.location.replace(url);
    return;
  }
  if (document.getElementById("img_rights") == null) {
    url = "http://localhost:9084/?rtp+?+?+?+" + url;
    window.location.replace(url);
    return;
  }
  url = "http://localhost:9084/?rtp+0+0+0+" + url;
  window.location.replace(url);
}, false);
