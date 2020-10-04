import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf

-- A pulse is a beat of music
type Pulse = Float
-- The sample rate is the number of pulses per second
type SampleRate = Float

type Wave = [Pulse]

save :: FilePath -> Wave -> IO ()
save path = B.writeFile path . B.toLazyByteString . fold . map B.floatLE

play :: FilePath -> SampleRate -> Wave -> IO()
play path rate wave = do
  save path wave
  _ <- runCommand $ printf "aplay -f FLOAT_LE -c 1 -r %f %s" rate path
  return ()

data Tone = A | ASharp | B | C | CSharp | D | DSharp | E | F | FSharp | G | GSharp
    deriving (Eq, Show, Ord, Enum)

-- A note is a tone and an octave. Octaves are Floats to simplify type conversion
-- (C, 3.0) is middle C for consistency with musical convention
type Note = (Tone, Float)

type Duration = Float
type Beats = Float
type Melody = [(Note, Beats)]

durationOfBeats :: Float -> Beats -> Duration
durationOfBeats bpm b = 60.0 / bpm * b

waveOfNote :: SampleRate -> Float -> Note -> Beats -> Wave
waveOfNote r bpm n d = map (sin . ((*) $ freq n r)) [0.0 .. ((durationOfBeats bpm d) * r)]
  where freq :: Note -> SampleRate -> Pulse
        freq (t, o) r = let exp = ((o - 4.0) * 12.0 + (fromIntegral $ fromEnum t))
                        in  2.0 * pi * 440.0 * (2.0 ** (1.0 / 12.0)) ** exp / r

-- See https://www.reasonexperts.com/attack-decay-sustain-and-release.html for more on ADSR
data NoteEnvelope = AttackRelease Float       -- A simple /\ shape, the float being where the peak is in the duration of the note
                  | ADSR { maxAttack :: Float                  -- A full ADSR shape with a potentially high attack and variable sustain and release
                         , timings :: (Float, Float, Float) }  -- timings. (Sustain is set to 1 by default since the volume might reconfigure it.)

pointOnLine x0 y0 x1 y1 t = y0 + (y1 - y0) * (t - x0) / (x1 - x0)

amplitudeOfEnvelope :: NoteEnvelope -> Maybe (Float -> Float)
amplitudeOfEnvelope (AttackRelease peak)              = Just (\t -> if t < peak then t / peak else 2.0 - t/peak)
amplitudeOfEnvelope ADSR{maxAttack=m,timings=(d,s,r)}
                    | d < s && s < r                  = Just $ fullADSR m d s r
                    | otherwise                       = Nothing
  where fullADSR m d s r t
            | t < d     = pointOnLine 0.0 0.0 d   m   t
            | t < s     = pointOnLine d   m   s   1.0 t
            | t < r     = pointOnLine s   1.0 r   1.0 t
            | otherwise = pointOnLine r   1.0 1.0 0.0 t

createVolumeScaling :: SampleRate -> Float -> Beats -> NoteEnvelope -> Float -> [Float]
createVolumeScaling r bpm b env vol = map ((*) vol . (maybe (\t -> 1.0) id $ amplitudeOfEnvelope env))
　　　　　　　　　　　　　　　　　　　　　　    　 [0.0, (1.0/ ((durationOfBeats bpm b) * r)) .. 1.0]
