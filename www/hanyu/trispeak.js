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
if (!('_version' in localStorage) || !(localStorage['_version'] >= 2)) {
    localStorage.clear();
    localStorage['_version'] = 2;
}

function speak(text, lang, rate) {
    var u = new SpeechSynthesisUtterance();
    u.text = text;
    u.lang = lang;
    u.rate = rate;
    speechSynthesis.speak(u);
}
function zspeak(text) {
    return speak(text, 'zh-CN', 0.5);
}
function espeak(text) {
    return speak(text, 'en-US', 1);
}
function play() {
    if (stage == awaitStart)        newQ();
    else if (stage == awaitPlayAns) speakQ();
    else if (stage == awaitAns)     speakAns();
}
