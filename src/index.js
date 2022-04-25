import { Elm } from "./Main.elm";
var storedData = localStorage.getItem('user-creds');
var user = storedData ? JSON.parse(storedData) : null;
const app = Elm.Main.init({ node: document.getElementById("root"), flags: user });

app.ports.logInfo.subscribe(function(message) {
  console.log(message)
})

app.ports.logError.subscribe(function(message) {
  console.error(message)
})

app.ports.logDebug.subscribe(function(message) {
  console.debug(message)
})

app.ports.setStorage.subscribe(function (state) {
  localStorage.setItem('user-creds', JSON.stringify(state));
});

if ('serviceWorker' in navigator) {
  navigator.serviceWorker
    .register('./service-worker.js')
    .then(function () { console.log("Service Worker Registered"); });
}
