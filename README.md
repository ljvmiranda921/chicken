# chicken :chicken: : a Haskell parser for my Toggl checkins

> In Thinking Machines, we [time-track everything in
> Slack](https://stories.thinkingmachin.es/time-tracking-dashboard/).  I use
> Toggl for my internal day-to-day, but the generated report is not compatible
> with our #dailycheckin standards.  I made a simple parser to convert everything
> properly.

Just pass the Toggl summary report (in CSV format) to the command-line:

```sh
$ chicken path/to/Toggl_summary_report_2020-03-30_2020-03-30.csv
```

And you'll get

```sh
$ chicken path/to/Toggl_summary_report_2020-03-30_2020-03-30.csv

checkin
- 3.56 hrs #project-1 Model training, evaluation 
- 1.39 hrs #project-1 Standups, meetings, and comms
- 1.33 hrs #opsandadmin Team Meeting
- 0.10 hr #opsandadmin Self-organization and Weekly Planning
- 0.72 hr #bizdev Comment on proposal
- 0.54 hr #engineering Comment on architecture
- 0.45 hr #project-2 Review PRs
```

## Installation

The following instructions require [stack resolver LTS
14.8](https://docs.haskellstack.org/en/stable/README/#the-haskell-tool-stack),
and the [Glorious Glasgow Haskell Compiler 8.6.5](https://www.haskell.org/ghc/)


First, clone this repo:

```sh
git clone git@github.com:ljvmiranda921/chicken.git
```

Then, build with stack:

```sh
stack build
stack install
```

## Frequently Asked Questions

- **This looks cool, can I use it?** Sure, but this uses the arcane Haskell
    tech stack. I included the installation instructions above so you can just
    follow them. However, I prefer that you use
    [thinkingmachines/toggl-checkin](https://github.com/thinkingmachines/toggl-checkin)
- **Why are you making this if there's already an extension?** For fun, and
   because I live in the terminal! Also I'm learning Haskell so why not make
   something useful out of it?  
- **Can I contribute to this tool?** Sure! But this is really a super internal
    tool and you might be better off contributing to my other open-source work!
    Check out [Pyswarms](https://github.com/ljvmiranda921/pyswarms) (Python),
    [Seagull](https://github.com/ljvmiranda921/seagull) (Python), and
    [Barometer](https://github.com/ljvmiranda921/burnout-barometer) (Go)!
