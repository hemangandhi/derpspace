# TODO: replace 5 with a word limit param?

module sum_types
  struct Left{T}
    val::T
  end

  struct Right{T}
    val::T
  end

  const Sum{T, U} = Union{Left{T}, Right{U}}

  left_or_right_is(s::Left{T}, v::T) where {T} = s == v
  left_or_right_is(s::Right{T}, v::T) where {T} = s == v
end

function read_dictionary(path)
  Base.open(path, "r") do file
    Base.Set(filter(line -> length(line) == 5, Base.eachline(file)))
  end
end

# Perhaps better called "map_reduce" lol
# (c -> a -> c) -> [a] -> c -> (a -> b) -> Map b c
function group_by(merger::Function, iter, initialize, key_proj = x -> x)
  groups = Dict()
  for i in iter
    key = key_proj(i)
    groups[key] = merger(get(groups, key, initialize), i)
  end
  groups
end

function get_letter_counts(word)
  group_by(word, 0) do s, l
    s + 1
  end
end

@enum GuessScore begin
  absent = 1
  present = 2
  correct = 3
end

function score_wordle_guess(guess, wordle)
  scores = Array{Union{Nothing, GuessScore}}(nothing, 5)
  for i = 1:5
    if guess[i] == wordle[i]
      scores[i] = correct
    else
      scores[i] = absent
    end
  end
  letter_counts = get_letter_counts([w for (i, w) in enumerate(wordle) if scores[i] != correct])
  for i = 1:5
    if scores[i] == correct || !haskey(letter_counts, guess[i])
      continue
    end
    if letter_counts[guess[i]] > 0
      scores[i] = present
      letter_counts[guess[i]] -= 1
    end
  end
  scores
end

# TODO: s/Char/UTF-8 format/
mutable struct WordInfo
  # Letters and the number of times they must appear.
  # If the bool is true, that number is exact. Else, it's a lower bound
  letter_map::Dict{Char, Tuple{Bool, Int8}}
  word::Array{Union{Nothing, Char}}
  WordInfo() = new(Dict(), Array{Union{Nothing, Char}}(nothing, 5))
end

struct WordInfoIncorrectCharError <: Exception
  msg
  WordInfoIncorrectChar(pos, a, b) = new("Duplicate correct character at $pos: info '$a' vs. guess '$b'")
end

function update_info!(info, scores, guess)
  # put all the correctly spotted characters into the word we're building
  for i = 1:5
    if scores[i] != correct
      continue
    end
    if info.word[i] == nothing
      info.word[i] = guess[i]
    elseif info.word[i] != guess[i]
      throw(WordInfoIncorrectChar(i, info.word[i], guess[i]))
    end
  end
  guess_counts = group_by(zip(guess, scores), (false, 0), p -> p[1]) do (is_eq, limit), (_, score)
    if score == absent
      (true, limit)
    elseif score == present
      (is_eq, limit + 1)
    elseif score == correct
      (is_eq, limit + 1)
    end
  end
  info.letter_map = merge(info.letter_map, guess_counts)
end