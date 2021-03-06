-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Pokemon.Object.Pokemon exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Pokemon.InputObject
import Pokemon.Interface
import Pokemon.Object
import Pokemon.Scalar
import Pokemon.ScalarCodecs
import Pokemon.Union


{-| The ID of an object
-}
id : SelectionSet Pokemon.ScalarCodecs.Id Pokemon.Object.Pokemon
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Pokemon.ScalarCodecs.codecs |> Pokemon.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The identifier of this Pokémon
-}
number : SelectionSet (Maybe String) Pokemon.Object.Pokemon
number =
    Object.selectionForField "(Maybe String)" "number" [] (Decode.string |> Decode.nullable)


{-| The name of this Pokémon
-}
name : SelectionSet (Maybe String) Pokemon.Object.Pokemon
name =
    Object.selectionForField "(Maybe String)" "name" [] (Decode.string |> Decode.nullable)


{-| The minimum and maximum weight of this Pokémon
-}
weight :
    SelectionSet decodesTo Pokemon.Object.PokemonDimension
    -> SelectionSet (Maybe decodesTo) Pokemon.Object.Pokemon
weight object_ =
    Object.selectionForCompositeField "weight" [] object_ (identity >> Decode.nullable)


{-| The minimum and maximum weight of this Pokémon
-}
height :
    SelectionSet decodesTo Pokemon.Object.PokemonDimension
    -> SelectionSet (Maybe decodesTo) Pokemon.Object.Pokemon
height object_ =
    Object.selectionForCompositeField "height" [] object_ (identity >> Decode.nullable)


{-| The classification of this Pokémon
-}
classification : SelectionSet (Maybe String) Pokemon.Object.Pokemon
classification =
    Object.selectionForField "(Maybe String)" "classification" [] (Decode.string |> Decode.nullable)


{-| The type(s) of this Pokémon
-}
types : SelectionSet (Maybe (List (Maybe String))) Pokemon.Object.Pokemon
types =
    Object.selectionForField "(Maybe (List (Maybe String)))" "types" [] (Decode.string |> Decode.nullable |> Decode.list |> Decode.nullable)


{-| The type(s) of Pokémons that this Pokémon is resistant to
-}
resistant : SelectionSet (Maybe (List (Maybe String))) Pokemon.Object.Pokemon
resistant =
    Object.selectionForField "(Maybe (List (Maybe String)))" "resistant" [] (Decode.string |> Decode.nullable |> Decode.list |> Decode.nullable)


{-| The attacks of this Pokémon
-}
attacks :
    SelectionSet decodesTo Pokemon.Object.PokemonAttack
    -> SelectionSet (Maybe decodesTo) Pokemon.Object.Pokemon
attacks object_ =
    Object.selectionForCompositeField "attacks" [] object_ (identity >> Decode.nullable)


{-| The type(s) of Pokémons that this Pokémon weak to
-}
weaknesses : SelectionSet (Maybe (List (Maybe String))) Pokemon.Object.Pokemon
weaknesses =
    Object.selectionForField "(Maybe (List (Maybe String)))" "weaknesses" [] (Decode.string |> Decode.nullable |> Decode.list |> Decode.nullable)


fleeRate : SelectionSet (Maybe Float) Pokemon.Object.Pokemon
fleeRate =
    Object.selectionForField "(Maybe Float)" "fleeRate" [] (Decode.float |> Decode.nullable)


{-| The maximum CP of this Pokémon
-}
maxCP : SelectionSet (Maybe Int) Pokemon.Object.Pokemon
maxCP =
    Object.selectionForField "(Maybe Int)" "maxCP" [] (Decode.int |> Decode.nullable)


{-| The evolutions of this Pokémon
-}
evolutions :
    SelectionSet decodesTo Pokemon.Object.Pokemon
    -> SelectionSet (Maybe (List (Maybe decodesTo))) Pokemon.Object.Pokemon
evolutions object_ =
    Object.selectionForCompositeField "evolutions" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


{-| The evolution requirements of this Pokémon
-}
evolutionRequirements :
    SelectionSet decodesTo Pokemon.Object.PokemonEvolutionRequirement
    -> SelectionSet (Maybe decodesTo) Pokemon.Object.Pokemon
evolutionRequirements object_ =
    Object.selectionForCompositeField "evolutionRequirements" [] object_ (identity >> Decode.nullable)


{-| The maximum HP of this Pokémon
-}
maxHP : SelectionSet (Maybe Int) Pokemon.Object.Pokemon
maxHP =
    Object.selectionForField "(Maybe Int)" "maxHP" [] (Decode.int |> Decode.nullable)


image : SelectionSet (Maybe String) Pokemon.Object.Pokemon
image =
    Object.selectionForField "(Maybe String)" "image" [] (Decode.string |> Decode.nullable)
