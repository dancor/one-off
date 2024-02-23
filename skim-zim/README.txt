note: I switched to c version ~/p/o/zim

skim-zim serves a wiktionary zim file with runtime article modification.

todo:
- Nonbreaking spaces are mangled in our tagsoup parse and render. Fix this.
  eg http://localhost:3000/A/ch%C5%82opiec.html wrong "chłopiecmpers"
     http://localhost:3000/raw/A/ch%C5%82opiec.html right "chłopiec m pers"
