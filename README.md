# chicken: a Haskell parser for my Toggl checkins

Just pass the Toggl summary report (in CSV format) to the command-line:

```sh
$ chicken path/to/Toggl_summary_report_2020-03-30_2020-03-30.csv
```

And you'll get

```sh
$ chicken path/to/Toggl_summary_report_2020-03-30_2020-03-30.csv

checkin
- 3.56 hrs #project-1 Annotate test set
- 1.39 hrs #project-1 Standups, meetings, and comms
- 1.33 hrs #opsandadmin Team Meeting
- 0.10 hrs #opsandadmin Self-organization and Weekly Planning
- 0.72 hrs #bizdev Comment on proposal
- 0.54 hrs #engineering Comment on Architecture Gallery
- 0.45 hrs #project-2 Review PRs
```

## Installation

The following instructions require stack resolver LTS 14.8, and the Glorious
Glasgow Haskell Compiler 8.6.5


First, clone this repo:

```sh
git clone git@github.com:ljvmiranda921/chicken.git
```

Then, build with stack:

```sh
stack build
stack install
```

## FAQ

- **This looks cool, can I use it?** Sure, but this uses the arcane Haskell
    tech stack. I included the installation instructions above so you can just
    follow them. However, I prefer that you use
    [thinkingmachines/toggl-checkin](https://github.com/thinkingmachines/toggl-checkin)
- **Why are you making this if there's already an extension?** For fun, and
   because I live in the terminal! Also I'm learning Haskell so why not make
   something useful out of it?  
- **Can I contribute to this tool?** Sure! But this is really a super internal
    tool and you might be better off contributing to my other open-source work!
    Check out [Pyswarms](github.com/ljvmiranda921/pyswarms) (Python),
    [Seagull](github.com/ljvmiranda921/seagull) (Python), and
    [Barometer](github.com/ljvmiranda921/burnout-barometer) (Go)!
