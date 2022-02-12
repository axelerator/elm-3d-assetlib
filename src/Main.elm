module Main exposing (..)

import Angle exposing (Angle)
import AssetLibrary exposing (Asset(..), AssetLibrary, Geometry, ViewMesh(..))
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import BoundingBox3d exposing (BoundingBox3d)
import Browser
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Direction3d
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Http exposing (Response(..))
import Length exposing (Meters)
import Obj.Decode exposing (ObjCoordinates)
import Physics.Body as Body
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World as World exposing (World)
import Pixels
import Point2d
import Point3d exposing (Point3d)
import Quantity
import Rectangle2d exposing (Rectangle2d)
import Scene3d exposing (Entity, translateBy)
import Scene3d.Material exposing (Texture)
import Time
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Texture exposing (Error(..))


type alias BaseItem =
    { base : ObjectId
    , baseOffset : Vector3d Meters ObjCoordinates
    , knob : ObjectId
    , knobOffset : Vector3d Meters ObjCoordinates
    , switchTo : Int -> Int -> Entity ObjCoordinates -> Entity ObjCoordinates
    , switchToBB : Int -> Int -> Block3d Meters BodyCoordinates -> Block3d Meters BodyCoordinates
    }


type alias Item =
    { x : Int
    , y : Int
    , baseItem : BaseItem
    , max : Int
    , value : Int
    }


toBodyCoords : BoundingBox3d Meters ObjCoordinates -> Block3d Meters BodyCoordinates
toBodyCoords bb =
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema bb
    in
    Block3d.fromBoundingBox <|
        BoundingBox3d.fromExtrema <|
            BoundingBox3d.extrema bb


v3dToBody : Vector3d Meters ObjCoordinates -> Vector3d Meters BodyCoordinates
v3dToBody v =
    Vector3d.fromMeters <| Vector3d.toMeters v


controlBlock : AssetLibrary Msg ObjectId -> Item -> Block3d Meters BodyCoordinates
controlBlock assetLibrary { baseItem, x, y, max, value } =
    let
        knobAsset =
            AssetLibrary.get baseItem.knob assetLibrary

        boundingBox =
            case knobAsset of
                JustMesh ( _, bb ) ->
                    bb

                ObjectWithTexture ( _, bb ) _ ->
                    bb

        originalBlockAtOrigin =
            Block3d.translateBy (v3dToBody baseItem.knobOffset) <|
                toBodyCoords boundingBox

        manipulatedBlock =
            baseItem.switchToBB max value originalBlockAtOrigin
    in
    Block3d.translateBy (itemDisplacement x y) manipulatedBlock


itemWidth =
    2.01


itemDisplacement x y =
    Vector3d.meters (itemWidth * toFloat x) (itemWidth * toFloat y) 0


renderItem : AssetLibrary Msg ObjectId -> Item -> List (Entity ObjCoordinates)
renderItem lib { x, y, baseItem, max, value } =
    List.map (translateBy <| itemDisplacement x y)
        [ assetToEntity <| AssetLibrary.get baseItem.base lib
        , knobAt lib baseItem max value
        ]


dial : BaseItem
dial =
    { base = BasePlate
    , baseOffset = Vector3d.meters 0 0 0
    , knob = Dial
    , knobOffset = Vector3d.meters 0 -2 0
    , switchTo = rotateDial
    , switchToBB = rotateDialBB
    }


slider : BaseItem
slider =
    { base = BasePlate
    , baseOffset = Vector3d.meters 0 0 0
    , knob = Slider
    , knobOffset = Vector3d.meters -3 -2 0
    , switchTo = slideSlider
    , switchToBB = slideSliderBB
    }


rotateDial : Int -> Int -> Entity coordinates -> Entity coordinates
rotateDial max current =
    let
        startAngle =
            90

        endAngle =
            -90

        delta =
            endAngle - startAngle

        step =
            delta // max

        angle =
            toFloat <| startAngle + (current * step)
    in
    Scene3d.rotateAround Axis3d.z (Angle.degrees angle)


rotateDialBB : Int -> Int -> Block3d Meters coordinates -> Block3d Meters coordinates
rotateDialBB max current =
    let
        startAngle =
            90

        endAngle =
            -90

        delta =
            endAngle - startAngle

        step =
            delta // max

        angle =
            toFloat <| startAngle + (current * step)
    in
    Block3d.rotateAround Axis3d.z (Angle.degrees angle)


slideSlider : Int -> Int -> Entity coordinates -> Entity coordinates
slideSlider max current =
    let
        startPos =
            -0.8

        endPos =
            0.8

        delta =
            endPos - startPos

        step =
            delta / toFloat max

        pos =
            startPos + (toFloat current * step)
    in
    translateBy (Vector3d.meters pos 0 0)


slideSliderBB : Int -> Int -> Block3d Meters coordinates -> Block3d Meters coordinates
slideSliderBB max current =
    let
        startPos =
            -0.8

        endPos =
            0.8

        delta =
            endPos - startPos

        step =
            delta / toFloat max

        pos =
            startPos + (toFloat current * step)
    in
    Block3d.translateBy (Vector3d.meters pos 0 0)


knobAt : AssetLibrary Msg ObjectId -> BaseItem -> Int -> Int -> Entity ObjCoordinates
knobAt lib { knob, switchTo, knobOffset } max value =
    switchTo max value <| Scene3d.translateBy knobOffset <| assetToEntity <| AssetLibrary.get knob lib


assetToEntity : Asset -> Entity ObjCoordinates
assetToEntity asset =
    case asset of
        ObjectWithTexture ( TexturedMesh texturedMesh, _ ) texture ->
            Scene3d.mesh (Scene3d.Material.texturedMatte texture) texturedMesh

        JustMesh ( UniformMesh m, _ ) ->
            Scene3d.mesh (Scene3d.Material.matte Color.blue) m

        JustMesh ( TexturedMesh m, _ ) ->
            Scene3d.mesh (Scene3d.Material.matte Color.blue) m

        ObjectWithTexture ( UniformMesh m, _ ) _ ->
            Scene3d.mesh (Scene3d.Material.matte Color.blue) m


type Msg
    = LoadedTexture (Result WebGL.Texture.Error (Texture Color))
    | LoadedMesh (Result String Geometry)
    | LoadedMeshFor ObjectId (Result String Geometry)
    | LoadedTextureFor ObjectId (Result WebGL.Texture.Error (Texture Color))
    | OnAssetError String
    | ForAssetLibrary (AssetLibrary.Msg ObjectId)
    | Orbit Time.Posix
    | FindItem Mouse.Event


type File3d
    = GeometryFile String (Maybe String)
    | GeometryAndTexture String String


type ObjectId
    = BasePlate
    | Dial
    | Slider


type LoadState a
    = Empty
    | Loaded a
    | Error String


type alias Model =
    { meshWithBoundingBox : LoadState Geometry
    , texture : LoadState (Texture Color)
    , partiallyLoaded : List ( ObjectId, Maybe Geometry, Maybe (Texture Color) )
    , azimuth : Angle
    , elevation : Angle
    , zoom : Float
    , orbiting : Bool
    , assetLibrary : AssetLibrary Msg ObjectId
    , pod : Asset
    , theta : Float
    , items : List Item
    }


libraryDescription : AssetLibrary.LibraryDescription Msg ObjectId
libraryDescription =
    { onProblem = OnAssetError
    , assetSources =
        [ AssetLibrary.ObjFileWithTexture BasePlate "/meshes/knobs_0_1.obj" "Pod.png" (Just "base_plate_Cube")
        , AssetLibrary.ObjFile Dial "/meshes/knobs_0_1.obj" (Just "Dial_Cylinder")
        , AssetLibrary.ObjFile Slider "/meshes/knobs_0_1.obj" (Just "slider_Cube.006")
        ]
    , forAssetLibrary = ForAssetLibrary
    }


main : Program () Model Msg
main =
    let
        ( assetLibrary, initAssetsCmd ) =
            AssetLibrary.init libraryDescription
    in
    Browser.element
        { init =
            always
                ( { meshWithBoundingBox = Empty
                  , texture = Empty
                  , azimuth = Angle.degrees 0
                  , elevation = Angle.degrees 35
                  , zoom = 0.1
                  , partiallyLoaded = []
                  , orbiting = False
                  , assetLibrary = assetLibrary
                  , pod = AssetLibrary.get BasePlate assetLibrary
                  , theta = 0
                  , items =
                        [ { x = 0, y = 0, baseItem = slider, max = 10, value = 5 }
                        , { x = 1, y = 0, baseItem = dial, max = 10, value = 5 }
                        , { x = 2, y = 0, baseItem = dial, max = 10, value = 9 }
                        , { x = 0, y = 1, baseItem = dial, max = 10, value = 3 }
                        , { x = 1, y = 1, baseItem = slider, max = 7, value = 3 }
                        , { x = 2, y = 1, baseItem = slider, max = 9, value = 8 }
                        ]
                  }
                , initAssetsCmd
                  --loadAssets
                )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Time.every 200 Orbit


toWorldAxis : Axis3d Meters ObjCoordinates -> Axis3d Meters WorldCoordinates
toWorldAxis input =
    let
        inputOrigin =
            Axis3d.originPoint input

        { x, y, z } =
            Point3d.unwrap inputOrigin

        outputOrigin =
            Point3d.meters x y z

        inputDir =
            Axis3d.direction input

        cs =
            Vector3d.unwrap <| Direction3d.toVector inputDir

        outputDir =
            Direction3d.unsafe <| Direction3d.unwrap inputDir
    in
    Axis3d.withDirection outputDir outputOrigin


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FindItem { offsetPos } ->
            let
                screen =
                    Rectangle2d.with
                        { x1 = Length.meters 0
                        , y1 = Length.meters 0
                        , x2 = Length.meters 640
                        , y2 = Length.meters 640
                        }

                ( x, y ) =
                    offsetPos

                point =
                    Point2d.meters x y

                ray =
                    Camera3d.ray (camera model) screen point

                mkBody i block =
                    Body.block block i

                controlBlocks =
                    List.map (controlBlock model.assetLibrary) model.items

                bodies =
                    List.indexedMap mkBody controlBlocks

                world =
                    List.foldl World.add World.empty bodies

                d =
                    case raycastResult of
                        Just res ->
                            Body.data res.body

                        Nothing ->
                            -1

                raycastResult =
                    World.raycast (toWorldAxis ray) world

                _ =
                    Debug.log "body data" d
            in
            ( model, Cmd.none )

        LoadedMeshFor _ _ ->
            ( model, Cmd.none )

        LoadedTextureFor _ _ ->
            ( model, Cmd.none )

        LoadedMesh result ->
            ( { model
                | meshWithBoundingBox =
                    case result of
                        Err err ->
                            Error err

                        Ok m ->
                            Loaded m
              }
            , Cmd.none
            )

        LoadedTexture result ->
            ( { model
                | texture =
                    case result of
                        Err LoadError ->
                            Error "Texture load error"

                        Err (SizeError _ _) ->
                            Error "Texture size error"

                        Ok texture ->
                            Loaded texture
              }
            , Cmd.none
            )

        OnAssetError _ ->
            ( model, Cmd.none )

        ForAssetLibrary assetLibMsg ->
            let
                assetLibrary =
                    AssetLibrary.update assetLibMsg model.assetLibrary
            in
            ( { model
                | assetLibrary = assetLibrary
                , pod = AssetLibrary.get Dial assetLibrary
              }
            , Cmd.none
            )

        Orbit _ ->
            let
                manipulate item =
                    if item.value == item.max then
                        { item | value = 0 }

                    else
                        { item | value = item.value + 1 }

                items =
                    List.map manipulate model.items
            in
            ( { model | items = items }, Cmd.none )


camera : Model -> Camera3d Meters ObjCoordinates
camera model =
    let
        boundingBox =
            case model.pod of
                JustMesh ( _, bb ) ->
                    bb

                ObjectWithTexture ( _, bb ) _ ->
                    bb

        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema boundingBox

        distance =
            List.map Quantity.abs [ minX, maxX, minY, maxY, minZ, maxZ ]
                |> List.foldl Quantity.max Quantity.zero
                |> Quantity.multiplyBy 3
                |> Quantity.multiplyBy (3 - model.zoom)
    in
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = BoundingBox3d.centerPoint boundingBox
                , azimuth = Angle.degrees -45
                , elevation = model.elevation
                , distance = distance
                }
        , verticalFieldOfView = Angle.degrees 30
        }


view : Model -> Html Msg
view model =
    centeredContents
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        ]
        [ centeredContents
            [ Html.Attributes.style "border" "3px dashed #ccc"
            , Html.Attributes.style "width" "640px"
            , Html.Attributes.style "height" "640px"
            , Html.Attributes.style "position" "relative"
            ]
            [ meshView (camera model) model.assetLibrary model.items
            ]
        ]


meshView : Camera3d Meters ObjCoordinates -> AssetLibrary Msg ObjectId -> List Item -> Html Msg
meshView cam lib items =
    Scene3d.sunny
        { upDirection = Direction3d.z
        , sunlightDirection = Direction3d.negativeZ
        , shadows = False
        , camera = cam
        , dimensions = ( Pixels.int 640, Pixels.int 640 )
        , background = Scene3d.transparentBackground
        , clipDepth = Length.meters 0.01
        , entities = List.concat <| List.map (renderItem lib) items
        }



--centeredContents : List (Html.Attribute msg) -> List (Html msg) -> Html msg


centeredContents attributes =
    Html.div
        ([ Html.Attributes.style "align-items" "center"
         , Html.Attributes.style "justify-content" "center"
         , Html.Attributes.style "display" "flex"
         , Html.Attributes.style "flex-direction" "column"
         , Mouse.onDown FindItem
         ]
            ++ attributes
        )
