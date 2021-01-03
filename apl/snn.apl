⍝ From	https://tryapl.org/ (because I can't figure out the install on nixos without feeling like I'm betraying the system)
⍝ An SNN with STDP and Izekhevich neurons, but in APL

⍝ Configuration
training_samples ← 10000
sample_freq ← ÷1000
lags ← 0 4
buckets ← -500 0 500
⍝ Neuron constants
Ee ← 0
vr ← -60
vt ← -54
El ← -74
taum ← 10
taue ← 5

⍝ I have no clue if the precedence makes those parens superfluous
times ← (sample_freq∘×)⍳training_samples
sines ← 500 × 1○¨○times

bucketized ← {(<∘sines ¨ ⍺) ∧ (>∘ sines ¨ ⍵)}
⍝ Technically, this is ahead, so the most "lagging" is really the first one. Oh well.
input_neurons ← lags ∘.↓ ((1↓¯1⌽buckets) bucketized (1↓buckets))

next_izikhevich ← {
    ⍝ I think there's a destructuring syntax that'd help here.
    ppv ← 1⌷⍺
    pge ← 2⌷⍺ + 2⌷⍵
    ⍝ https://stackoverflow.com/a/15768129
    pv ← (1 + (ppv > vt))⊃vr ppv
    sample_freq × ((pge × (Ee - vr) + El - v)÷taum ¯ge÷taue)
    }

