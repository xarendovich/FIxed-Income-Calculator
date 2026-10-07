# The rerun from a clean checkout (contract 5.0.0)

A fresh `git clone` of branch `claude/clever-bardeen-qqkxez` at `16b8db9`, with nothing carried over from the working tree:

- **Self-tests:** 261 of 261, none skipped (`selftests.txt`).
- **Batteries:** dir-watch, disk-watch and meminfo-watch each PASS, 21 PASS and DB-24 N/A (`battery-*.txt`). Their workspaces were deliberately given a long path, which is how the first clean rerun found HF-42 (`../hf42_before.txt`).
- **Unit:** `unit --report` projected the installable unit from meminfo-watch's report (`spark-daemon-meminfo-watch.service`).
- **Contract:** `make check-contract` is up to date: 5.0.0, `2d080b407b6c82b230563670746f4a615b2c96660ec68f4509574562c282001d`.
- **Clone:** no file changed or added (`summary.txt`).

`<scratchpad>` stands for the session's scratch directory.
