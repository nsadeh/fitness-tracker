import { Elm } from "./Main.elm";
import { createClient } from '@supabase/supabase-js';
import { v4 as uuidv4 } from 'uuid';

const app = Elm.Main.init({ node: document.getElementById("root") });

// Create a single supabase client for interacting with your database 
const supabase = createClient('https://zmwmosaxfkywgkueembd.supabase.co', 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inptd21vc2F4Zmt5d2drdWVlbWJkIiwicm9sZSI6ImFub24iLCJpYXQiOjE2NDUwNzExNTMsImV4cCI6MTk2MDY0NzE1M30.M3AI9OxwlNk97FoieuitzpBCCvVr7RDiCPtaXUQo6gM')

app.ports.logInfo.subscribe(function(message) {
  console.log(message)
})

app.ports.logError.subscribe(function(message) {
  console.error(message)
})

app.ports.logDebug.subscribe(function(message) {
  console.debug(message)
})


async function insertNewExercise(exercise, user_id) {
  const { data, error } = await supabase.from('strength_exercises').insert([
    { name: exercise.name, weight: exercise.weight, reps: exercise.reps, user_id: user_id, id: 1 }
  ])
  if (error) {
    console.log(`Encountered error while attempting to insert to database: ${JSON.stringify(error)}`)
    return null
  } else {
    return data
  }
}

async function signupNewUser(email, password) {
  const { user, session, error } = await supabase.auth.signUp({
    email,
    password
  })
  if (error) {
    console.log(JSON.stringify(error))
  } else {
    return { session, user }
  }
}
