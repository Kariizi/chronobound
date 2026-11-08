-- Background music: which track belongs to each scene, and loading the mp3s

module Audio where

import Model
import Sound.ProteaAudio (Sample, Sound, sampleFromFile, initAudio, soundLoop, soundStop)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (when)

-- Every piece of background music the game can play
data Track
    = MenuTheme
    | LimboTheme
    | HellTheme
    | Silence
    deriving (Eq, Show)

-- Decide which track should be playing for the current world
trackFor :: World -> Track
trackFor world =
    case gameState world of
        Menu            -> MenuTheme
        Paused          -> trackForLevel (currentLevel world)
        Playing         -> trackForLevel (currentLevel world)
        ChoosingUpgrade -> trackForLevel (currentLevel world)
        GameOver        -> Silence
        Victory         -> MenuTheme

-- Music for each dimension while playing
trackForLevel :: Level -> Track
trackForLevel level =
    case level of
        VictoriaLevel -> LimboTheme
        LimboLevel    -> HellTheme
        HellLevel     -> MenuTheme

-- The loaded mp3s, kept together so they are only loaded once
data Music = Music
    { motif         :: Sample
    , gothicPursuit :: Sample
    , descent       :: Sample
    }

-- Load every track from the music folder at startup
loadMusic :: FilePath -> IO Music
loadMusic folder =
    Music
        <$> sampleFromFile (folder ++ "/Motif.mp3") 1.0
        <*> sampleFromFile (folder ++ "/Gothic Pursuit.mp3") 1.0
        <*> sampleFromFile (folder ++ "/Descent.mp3") 1.0

-- The sample to play for a track (Nothing means play no music)
sampleFor :: Music -> Track -> Maybe Sample
sampleFor music track =
    case track of
        MenuTheme  -> Just (motif music)
        LimboTheme -> Just (gothicPursuit music)
        HellTheme  -> Just (descent music)
        Silence    -> Nothing


-- What is playing right now: the track and the sound handle needed to stop it
-- (kept in an IORef because the World is pure but the playing sound is real-world state)
type MusicPlayer = IORef (Track, Maybe Sound)

-- Background music volume (0 = silent, 1 = full)
musicVolume :: Float
musicVolume = 0.4

-- Start the audio system and load the music
-- Returns Nothing if there is no audio device, so the game still runs without sound
setupMusic :: FilePath -> IO (Maybe (Music, MusicPlayer))
setupMusic folder = do
    audioStarted <- initAudio 64 48000 1024

    if not audioStarted
        then do
            putStrLn "Audio could not start - playing without music"
            return Nothing
        else do
            music <- loadMusic folder
            musicPlayer <- newIORef (Silence, Nothing)
            return (Just (music, musicPlayer))

-- Called every frame: switches track when the scene changes, otherwise does nothing
updateMusic :: Music -> MusicPlayer -> World -> IO ()
updateMusic music musicPlayer world = do
    (currentTrack, currentSound) <- readIORef musicPlayer

    let wantedTrack = trackFor world

    when (wantedTrack /= currentTrack) $ do
        -- Stop the old track if one is playing
        mapM_ soundStop currentSound

        -- Loop the new track (or stay silent)
        newSound <-
            case sampleFor music wantedTrack of
                Just sample -> Just <$> soundLoop sample musicVolume musicVolume 0 1
                Nothing     -> return Nothing

        writeIORef musicPlayer (wantedTrack, newSound)
