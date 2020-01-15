# Schema for the synthesizer config file


  --min-length=MINLEN  Minimum number of statements in each generated program
                       [default: 1]
  --max-length=MAXLEN  Maximum number of statements in each generated program
                       [default: 10]
  --arg-mode=MODE      Argument generation mode: (1) generated: always generate new type declarations; (2) existing: attempt to search for constructs that apply to existing statements and switch to (1) after exhaustive search; (3) 50% chance of (1) or (2)
                       [default: mixed]