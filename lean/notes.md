# Editor unicode entry

| Symbols | Unicode |
|---|---|
| `\lam` | λ |
| `\exists` | ∃ |
| `\n` | ¬ |
| `\all` | ∀ |
| `\iff` | ↔ |
| `\r` | → |
| `\l` | ← |
| `\and` | ∧ |
| `\or` | ∨ |

# Chapter 5

Tactics summary:

| Syntax | Note |
|---|---|
| `apply <term>` | Not sure why you'd use this. |
| `exact <term>` | Not sure why you'd use this. Identity function? |
| `case <tag> => <tactics>` | Useful for permuting the cases. |
| `intro <hypotheses>` | For lambdas? Again, why? |
| `intro | <case> => <tactics>` | For eliminators. |
| `assumption` | Apply an appropriate hypothesis. |
| `intros` | Introduces various variables anonymously. |
| `rfl` | `exact rfl` |
| `repeat <tactic>` | Repeat a tactic. |
| `revert <hypothesis>` | Un-introduce a hypothesis. |
| `generalize <hypothesis name> : <term>` | Replace a term with a variable. |
| `admit` | Weak sorry. |
| `rw [<hypotheses, ← to reverse>]` | Rewrite. |
| `cases <term> with | <ctor> <args> => <term>` | Elimination. Can just use `apply` to go into an eliminator. |
| `<left tactic> \<;\> <right tactic>` | Apply a right tactic to each goal of left. |
| `constructor <tactics>` | Apply the constructor with tactics per parameter. |
| `<combinator> | <tactic> | <tactics ...>` | Apply each of the tactics until one succeeds. |
| `skip` | Do nothing, just succeed. |
| `try <tactic>` | Do the argument, but just succeed. |
| `all_goals <tactic>` | Apply the argument to all goals. |
| `any_goals <tactic>` | Apply the argument to all goals, but succeeds only if at least one succeeded. |
| `simp` | Simplify the goal until it works. |
| `split` | Apply a tactic to each parameter one at a time. |

OK, but. Like why? And like... don't? (Sometimes.)
