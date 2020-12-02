var link = document.createElement("a");
link.href = document.getElementsByClassName("rmp-video")[0].src;
link.download = "aDefaultFileName.txt";
link.innerHTML = "Click here to download the file";
document.body.appendChild(link); // needed?
link.click();
