module Api.Routes exposing (..)

import Api.Supabase exposing (ApiKey, AuthenticatedUser, Url)
import Api.User
import Http as H


-- supabaseClient url apiKey =
--     { auth =
--         { login = Api.User.login url apiKey
--         }
--     , exercises =
--         { get = Api.Exercises.getExercises url apiKey
--         }
--     }
