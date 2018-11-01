var entries = JSON.parse(LZString.decompressFromBase64(entriesLz));
var waitSecsIfWrong = 600;
var daySecs = 86400;
var workDaySecs = 28800;
var awaitStart = 0;
var awaitPlayAns = 1;
var awaitAns = 2;
var stage = awaitStart;
var waitDaysIfCorrect;

function subGet(i, v) {
        localStorage[subId + i] = v;
}
function subPut(i) {
        return localStorage[subId + i];
}
if (!('_version' in localStorage) || !(localStorage['_version'] >= 1)) {
    localStorage.clear();
    localStorage['_version'] = 1;
}

function speak(text, lang) {
    var u = new SpeechSynthesisUtterance();
    u.text = text;
    u.lang = lang;
    speechSynthesis.speak(u);
}
function zspeak(text) {
    return speak(text, 'zh-CN');
}
function espeak(text) {
    return speak(text, 'en-US');
}
function play() {
    if (stage == awaitStart)        newQ();
    else if (stage == awaitPlayAns) speakQ();
    else if (stage == awaitAns)     speakAns();
}
