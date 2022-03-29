module HomePage exposing (..)

import Browser.Dom exposing (Error)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Maybe exposing (..)
import Random
import Tuple exposing (first)


type alias Model =
    { status : Status
    , selectedSize : Size
    , hue : Int
    , ripple : Int
    , noise : Int
    }


type Status
    = Loading
    | Loaded (List Photo) (Maybe String)
    | Error String


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> Json.Decode.Pipeline.required "url" string
        |> Json.Decode.Pipeline.required "size" int
        |> optional "title" string "(Untitled)"


type Msg
    = ClickedPhoto (Maybe String)
    | ClickedChooseSize Size
    | ClickedSurpriseMe
    | RuntimeSelectedPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int


type Size
    = Small
    | Medium
    | Large


domainName : String
domainName =
    "http://elm-in-action.com/"


initialModel : Model
initialModel =
    { status = Loading
    , selectedSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (Json.Decode.list photoDecoder)
        }


view : Model -> Html Msg
view model =
    div
        [ class "content" ]
    <|
        case model.status of
            Loading ->
                [ text "Loading..." ]

            Loaded photosList selectedPhoto ->
                viewLoaded photosList selectedPhoto model

            Error errorMsg ->
                [ text ("Error! " ++ errorMsg) ]


viewLoaded : List Photo -> Maybe String -> Model -> List (Html Msg)
viewLoaded photosList selectedPhoto model =
    [ h1
        []
        [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ class "filters" ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , h3
        []
        [ text "Thumbnail Size: " ]
    , div
        [ id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , viewThumbnailsList photosList selectedPhoto model.selectedSize
    , viewLargeSelected selectedPhoto
    ]


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl photo =
    img
        [ src (domainName ++ photo.url)
        , title (photo.title ++ " [" ++ String.fromInt photo.size ++ " KB]")
        , classList [ ( "selected", Just photo.url == selectedUrl ) ]
        , onClick (ClickedPhoto (Just photo.url))
        ]
        []


viewThumbnailsList : List Photo -> Maybe String -> Size -> Html Msg
viewThumbnailsList photosList selectedPhoto selectedSize =
    div
        [ id "thumbnails"
        , class (sizeToString selectedSize)
        ]
        (List.map (viewThumbnail selectedPhoto) photosList)


viewLargeSelected : Maybe String -> Html msg
viewLargeSelected selectedUrl =
    img
        [ src (domainName ++ "large/" ++ withDefault "1.jpeg" selectedUrl)
        , class "large"
        ]
        []


viewSizeChooser : Size -> Html Msg
viewSizeChooser size =
    label []
        [ input
            [ type_ "Radio"
            , name "size"
            , onClick (ClickedChooseSize size)
            ]
            []
        , text (sizeToString size)
        ]


sizeToString : Size -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


cmdGetRandomPhoto : Photo -> List Photo -> Cmd Msg
cmdGetRandomPhoto firstPhoto rest =
    Random.generate
        RuntimeSelectedPhoto
        (Random.uniform firstPhoto rest)


setSelectedPhoto : Maybe String -> Status -> Status
setSelectedPhoto url status =
    case status of
        Loading ->
            status

        Loaded photosList _ ->
            Loaded photosList url

        Error _ ->
            status


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = setSelectedPhoto url model.status }
            , Cmd.none
            )

        ClickedChooseSize size ->
            ( { model | selectedSize = size }, Cmd.none )

        RuntimeSelectedPhoto photo ->
            ( { model | status = setSelectedPhoto (Just photo.url) model.status }
            , Cmd.none
            )

        ClickedSurpriseMe ->
            case model.status of
                Loading ->
                    ( model, Cmd.none )

                Loaded (firstPhoto :: rest) _ ->
                    ( model, cmdGetRandomPhoto firstPhoto rest )

                Loaded [] _ ->
                    ( model, Cmd.none )

                Error _ ->
                    ( model, Cmd.none )

        GotPhotos (Ok responseListPhotos) ->
            case responseListPhotos of
                first :: _ ->
                    ( { model | status = Loaded responseListPhotos (Just first.url) }, Cmd.none )

                [] ->
                    ( { model | status = Error "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Error "Server Error!" }, Cmd.none )

        SlidHue hue ->
            ( { model | hue = hue }
            , Cmd.none
            )

        SlidRipple ripple ->
            ( { model | ripple = ripple }
            , Cmd.none
            )

        SlidNoise noise ->
            ( { model | noise = noise }
            , Cmd.none
            )


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter intToMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide intToMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


onSlide : (Int -> msg) -> Attribute msg
onSlide intToMsg =
    at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map intToMsg
        |> on "slide"
