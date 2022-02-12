module AssetLibrary exposing
    ( Asset(..)
    , AssetLibrary
    , AssetSrc(..)
    , Geometry
    , LibraryDescription
    , Msg
    , ViewMesh(..)
    , assets
    , get
    , init
    , update
    )

import Array
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color)
import Http exposing (Response(..))
import Length exposing (Meters)
import Obj.Decode exposing (Decoder, ObjCoordinates)
import Platform exposing (Task)
import Point3d exposing (Point3d)
import Scene3d.Material exposing (Texture)
import Scene3d.Mesh exposing (Textured, Uniform)
import Task
import TriangularMesh exposing (TriangularMesh)
import WebGL.Texture


type AssetLibrary msg objectId
    = AssetLibrary
        { libraryDescription : LibraryDescription msg objectId
        , assets : List ( objectId, Asset )
        , waitingForAssets : List (AssetSrc objectId)
        , rawLoaded : List RawFile
        }


type RawFile
    = RawTexture String (Texture Color)
    | RawGeometry String String


type AssetSrc objectId
    = ObjFile objectId String (Maybe String)
    | ObjFileWithTexture objectId String String (Maybe String)


type Asset
    = JustMesh Geometry
    | ObjectWithTexture Geometry (Texture Color)


type alias Geometry =
    ( ViewMesh, BoundingBox3d Meters ObjCoordinates )


type Msg objectId
    = LoadedMeshFor String (Result String String)
    | LoadedTextureFor String (Result WebGL.Texture.Error (Texture Color))


type alias LibraryDescription msg objectId =
    { onProblem : String -> msg
    , forAssetLibrary : Msg objectId -> msg
    , assetSources : List (AssetSrc objectId)
    }


assets : AssetLibrary msg objectId -> List ( objectId, Asset )
assets (AssetLibrary assetLibrary) =
    assetLibrary.assets


nothing : Uniform ObjCoordinates
nothing =
    Scene3d.Mesh.facets []


notFound : Asset
notFound =
    JustMesh ( UniformMesh nothing, BoundingBox3d.singleton <| Point3d.meters 0 0 0 )


get : objectId -> AssetLibrary msg objectId -> Asset
get objectId (AssetLibrary lib) =
    case find (\( oid, _ ) -> oid == objectId) lib.assets of
        Just ( _, asset ) ->
            asset

        Nothing ->
            notFound


type ViewMesh
    = TexturedMesh (Textured ObjCoordinates)
    | UniformMesh (Uniform ObjCoordinates)


find : (a -> Bool) -> List a -> Maybe a
find predicate =
    List.head << List.filter predicate


findRawGeometry : List RawFile -> String -> Maybe String -> Maybe Geometry
findRawGeometry rawLoaded filename objName =
    let
        isGeo raw =
            case raw of
                RawGeometry f _ ->
                    f == filename

                _ ->
                    False
    in
    case find isGeo rawLoaded of
        Just (RawGeometry _ objFileString) ->
            let
                decodedObj =
                    Obj.Decode.decodeString
                        Length.meters
                        (meshWithBoundingBoxDecoder objName)
                        objFileString
            in
            case decodedObj of
                Ok geo ->
                    Just geo

                Err e ->
                    let
                        _ =
                            Debug.log "obj decode err" e
                    in
                    Nothing

        _ ->
            Nothing


findRawTexture : List RawFile -> String -> Maybe (Texture Color)
findRawTexture rawLoaded filename =
    let
        isTexture raw =
            case raw of
                RawTexture f _ ->
                    f == filename

                _ ->
                    False
    in
    case find isTexture rawLoaded of
        Just (RawTexture _ texture) ->
            Just texture

        _ ->
            Nothing


resolve : List RawFile -> AssetSrc objectId -> Maybe ( objectId, Asset )
resolve rawLoaded assetSrc =
    case assetSrc of
        ObjFile objId filename objName ->
            case findRawGeometry rawLoaded filename objName of
                Just geo ->
                    Just ( objId, JustMesh geo )

                _ ->
                    Nothing

        ObjFileWithTexture objId objFilename textureFilename objName ->
            let
                geoAndTexture =
                    ( findRawGeometry rawLoaded objFilename objName
                    , findRawTexture rawLoaded textureFilename
                    )
            in
            case geoAndTexture of
                ( Just geo, Just texture ) ->
                    Just ( objId, ObjectWithTexture geo texture )

                _ ->
                    Nothing


completeAssets : List RawFile -> List (AssetSrc objectId) -> ( List ( objectId, Asset ), List (AssetSrc objectId) )
completeAssets rawLoaded waitingForAssets =
    let
        completeAssets_ assetSrc ( completed, stillWaiting ) =
            case resolve rawLoaded assetSrc of
                Just asset ->
                    ( asset :: completed
                    , stillWaiting
                    )

                Nothing ->
                    ( completed
                    , assetSrc :: stillWaiting
                    )
    in
    List.foldr completeAssets_ ( [], [] ) waitingForAssets


update : Msg objectId -> AssetLibrary msg objectId -> AssetLibrary msg objectId
update msg (AssetLibrary assetLibrary) =
    case msg of
        LoadedMeshFor filename (Ok mesh) ->
            let
                rawLoaded =
                    RawGeometry filename mesh :: assetLibrary.rawLoaded

                ( completed, stillWaiting ) =
                    completeAssets rawLoaded assetLibrary.waitingForAssets
            in
            AssetLibrary
                { assetLibrary
                    | assets = completed ++ assetLibrary.assets
                    , rawLoaded = rawLoaded
                    , waitingForAssets = stillWaiting
                }

        LoadedTextureFor filename (Ok texture) ->
            let
                rawLoaded =
                    RawTexture filename texture :: assetLibrary.rawLoaded

                ( completed, stillWaiting ) =
                    completeAssets rawLoaded assetLibrary.waitingForAssets
            in
            AssetLibrary
                { assetLibrary
                    | assets = completed ++ assetLibrary.assets
                    , rawLoaded = rawLoaded
                    , waitingForAssets = stillWaiting
                }

        e ->
            let
                _ =
                    Debug.log "what" e
            in
            Debug.todo "proper error handling"


loadAsset : LibraryDescription msg objectId -> AssetSrc objectId -> Cmd msg
loadAsset { forAssetLibrary } src =
    case src of
        ObjFile _ filename _ ->
            Cmd.map forAssetLibrary <| Task.attempt (LoadedMeshFor filename) (loadMeshToString filename)

        ObjFileWithTexture _ objFilename textureFilename _ ->
            Cmd.batch
                [ Cmd.map forAssetLibrary <| Task.attempt (LoadedMeshFor objFilename) (loadMeshToString objFilename)
                , Cmd.map forAssetLibrary <| Task.attempt (LoadedTextureFor textureFilename) (Scene3d.Material.loadWith Scene3d.Material.nearestNeighborFiltering textureFilename)
                ]


init : LibraryDescription msg objectId -> ( AssetLibrary msg objectId, Cmd msg )
init libDescr =
    let
        cmds : List (Cmd msg)
        cmds =
            List.map (loadAsset libDescr) libDescr.assetSources
    in
    ( AssetLibrary
        { libraryDescription = libDescr
        , assets = []
        , waitingForAssets = libDescr.assetSources
        , rawLoaded = []
        }
    , Cmd.batch cmds
    )


loadMeshToString : String -> Task String String
loadMeshToString filename =
    let
        response r =
            case r of
                GoodStatus_ _ body ->
                    Ok body

                _ ->
                    Err "Unable to GET obj file"
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = filename
        , body = Http.emptyBody
        , resolver = Http.stringResolver response
        , timeout = Nothing
        }


readObjFromString : String -> Task String Geometry
readObjFromString content =
    case
        Obj.Decode.decodeString
            Length.meters
            (meshWithBoundingBoxDecoder Nothing)
            content
    of
        Ok m ->
            Task.succeed m

        Err err ->
            Task.fail err


loadMeshTask : String -> Task String Geometry
loadMeshTask filename =
    loadMeshToString filename
        |> Task.andThen readObjFromString


{-| Because we don’t know the exect format of a mesh, we try decoding different
primitives: from the most specific to the most simple one.
-}
meshWithBoundingBoxDecoder : Maybe String -> Decoder ( ViewMesh, BoundingBox3d Meters ObjCoordinates )
meshWithBoundingBoxDecoder objectName =
    Obj.Decode.oneOf
        [ withBoundingBox objectName .position (Scene3d.Mesh.texturedFaces >> TexturedMesh) Obj.Decode.texturedFaces
        , withBoundingBox objectName .position (Scene3d.Mesh.indexedFaces >> UniformMesh) Obj.Decode.faces
        , withBoundingBox objectName .position (Scene3d.Mesh.texturedFacets >> TexturedMesh) Obj.Decode.texturedTriangles
        , withBoundingBox objectName identity (Scene3d.Mesh.indexedFacets >> UniformMesh) Obj.Decode.triangles
        ]


withBoundingBox :
    Maybe String
    -> (a -> Point3d Meters ObjCoordinates) -- a function that knows how to extract position of a vertex
    -> (TriangularMesh a -> ViewMesh) -- a function that knows how to create a ViewMesh
    -> Decoder (TriangularMesh a) -- a primitive decoder
    -> Decoder ( ViewMesh, BoundingBox3d Meters ObjCoordinates )
withBoundingBox objectName getPosition createMesh decoder =
    Obj.Decode.map
        (\triangularMesh ->
            ( createMesh triangularMesh
            , case List.map getPosition (Array.toList (TriangularMesh.vertices triangularMesh)) of
                first :: rest ->
                    BoundingBox3d.hull first rest

                [] ->
                    BoundingBox3d.singleton Point3d.origin
            )
        )
        (case objectName of
            Just name ->
                Obj.Decode.object name decoder

            Nothing ->
                decoder
        )
