# logparser

LogParser is a utility application that counts the total number of lines that 
are found having a specific string.

## How to use it

logparse takes two arguments, first being path to a log file and the second 
being a json config file that specifies what strings to look for. 
Example below (with output):

```
user>./logparser /home/user/log.log /home/user/config.json
Log: /home/user/log.log
Config: /home/user/config.json

--------
Analyzing Log
--------

Fun times = 2
Hard times = 3
```

config file:

```
{
    "rules": [
        {
            "matcher" : "Fun times"
        },
        {
            "matcher" : "Hard times"
        }
    ]
}
```

log file:

```
Fun times
Fun times
Hard times
Hard times
Hard times
Okay times
```

## Development

(Assumming OSX with Homebrew)

- brew install haskell-stack
- clone the project
- cd to project root
- run "stack install" to verify install
- run "stack test" to verify tests
