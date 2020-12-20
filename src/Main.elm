module Main exposing (..)

import Browser
import Bulma.Classes as Bulma
import GraphQLClient exposing (makeGraphQLQuery)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, div, figure, h1, img, p, text)
import Html.Attributes exposing (class, src)
import List
import Pokemon.Object
import Pokemon.Object.Pokemon as Pokemon
import Pokemon.Query as Query exposing (PokemonsRequiredArguments)
import Pokemon.ScalarCodecs
import RemoteData exposing (RemoteData)



---- MODEL ----


type alias Pokemon =
    { id : Pokemon.ScalarCodecs.Id
    , name : Maybe String
    , image : Maybe String
    }


type alias Pokemons =
    Maybe (List (Maybe Pokemon))


type alias PokemonData =
    RemoteData (Graphql.Http.Error Pokemons) Pokemons


type alias Model =
    { pokemons : PokemonData }


init : ( Model, Cmd Msg )
init =
    ( { pokemons = RemoteData.Loading
      }
    , fetchPokemons 151
    )



---- UPDATE ----


type Msg
    = FetchDataSuccess PokemonData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchDataSuccess response ->
            updatePokemonsData response model Cmd.none


updatePokemonsData : PokemonData -> Model -> Cmd Msg -> ( Model, Cmd Msg )
updatePokemonsData data model cmd =
    ( { model | pokemons = data }, cmd )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class Bulma.container ]
        [ img [ src "https://i.gyazo.com/480551bded5134ddacf08616b2595717.png" ] []
        , h1 [ class Bulma.title, class Bulma.is4, class Bulma.mb6 ] [ text "Pokemons with elm-graphql" ]
        , renderPokemons model.pokemons
        ]


renderPokemons : PokemonData -> Html Msg
renderPokemons pokemonData =
    case pokemonData of
        RemoteData.NotAsked ->
            p [ class Bulma.isSize4, class Bulma.hasTextCentered ] [ text "not" ]

        RemoteData.Success maybePokemonsList ->
            maybePokemonsList
                |> Maybe.withDefault []
                |> List.map renderPokemon
                |> div [ class Bulma.columns, class Bulma.isMultiline ]

        RemoteData.Loading ->
            p [ class Bulma.isSize4, class Bulma.hasTextCentered ] [ text "loading..." ]

        RemoteData.Failure err ->
            p [ class Bulma.isSize4, class Bulma.hasTextCentered ] [ text "Error" ]


renderPokemon : Maybe Pokemon -> Html Msg
renderPokemon maybePokemon =
    div [ class Bulma.column, class Bulma.is3 ]
        [ maybePokemon
            |> Maybe.map
                (\pokemon ->
                    div [ class Bulma.card ]
                        [ div [ class Bulma.cardImage ]
                            [ figure [ class Bulma.image, class Bulma.is16by9, class Bulma.mx5, class Bulma.mt5 ]
                                [ img [ src (pokemon.image |> Maybe.withDefault "") ] []
                                ]
                            ]
                        , div [ class Bulma.cardContent ]
                            [ p [ class Bulma.isSize4 ] [ text (pokemon.name |> Maybe.withDefault "") ]
                            ]
                        ]
                )
            |> Maybe.withDefault (text "")
        ]



---- GraphQL API ----


pokemonsRequiredArguments : Int -> PokemonsRequiredArguments
pokemonsRequiredArguments num =
    { first = num }


pokemonListSelection : SelectionSet Pokemon Pokemon.Object.Pokemon
pokemonListSelection =
    SelectionSet.map3 Pokemon
        Pokemon.id
        Pokemon.name
        Pokemon.image


fetchPokemonsQuery : Int -> SelectionSet Pokemons RootQuery
fetchPokemonsQuery num =
    Query.pokemons (pokemonsRequiredArguments num) pokemonListSelection


fetchPokemons : Int -> Cmd Msg
fetchPokemons num =
    makeGraphQLQuery (fetchPokemonsQuery num) (RemoteData.fromResult >> FetchDataSuccess)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
