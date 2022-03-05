import { Elm } from "./Main.elm";

const app = Elm.Main.init({ node: document.getElementById("root"), flags: [ process.env.SUPABASE_URL, process.env.SUPABASE_API ] });

app.ports.logInfo.subscribe(function(message) {
  console.log(message)
})

app.ports.logError.subscribe(function(message) {
  console.error(message)
})

app.ports.logDebug.subscribe(function(message) {
  console.debug(message)
})
