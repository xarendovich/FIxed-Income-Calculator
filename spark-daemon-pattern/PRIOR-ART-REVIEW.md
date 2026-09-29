# Prior-art review: daemon classes, blindness, escalation and authentication

- **Status:** REVIEW FOR ADJUDICATION. It feeds the reopened PD-01 (`PD-01-DAEMON-CLASSES.md`), section 10. Nothing here changes code, and every proposed addition is PENDING.
- **Revision:** r3.7, 2026-09-29, by Claude, at the owner's request: "review any other technical documentation in GitHub repositories, frontier research, robotics or industrial manufacturing that focuses on this topic… anything we're missing."
- **Method and limits:** web search of primary and secondary sources. Most industrial standards (IEC, ISO, ISA, ASTM) are paywalled. For those, the claims below come from the standards bodies' public summaries, vendor white papers and published papers, not from the standards' full text. Anything cited here should be checked against the standard itself before it becomes a requirement.

## 1. Summary

Our design lines up with established practice in more places than it diverges. The two-stage escalation, the "stop needs a human" exit, a separate gate holding authority, and "alive is not the same as seeing" each have direct counterparts in flight software, robotics, industrial automation and operations tooling.

The review found **twelve things worth adding**. The three most important:
1. **Use the industrial "black channel" error model** (IEC 61784-3) as the basis of the connector gate's conformance suite, instead of our own list.
2. **Put an independent monitor on every daemon**, not only critical ones: a dead man's switch outside the daemon, plus a systemd `OnFailure=` hook. A daemon cannot report its own total failure.
3. **Derive consequence levels from a short, recorded risk assessment**, as every functional-safety standard does, instead of letting each daemon declare its own level.

One correction came out of the review: my description of HF-28 overstated it. See section 5.

## 2. Sources

| Area | Source | What it is |
| --- | --- | --- |
| Industrial safety communication | IEC 61784-3 ("black channel"), via [ODVA CIP Safety](https://www.odva.org/wp-content/uploads/2022/03/2022-ODVA-Conference_CIP_Safety_Embracing_IEC61784-3_Edition_4_Peng-Seidlitz-Crane-Guru_FINAL.pdf), [61508.org](https://61508.org/wp-content/uploads/2024/11/11A-Functional-Safety-and-Communications-V1-e092024.pdf), [PROFIsafe guide](https://scadaprotocols.com/profisafe-complete-technical-guide/) | Safety messages over an untrusted transport: a fixed error model and fixed countermeasures |
| Industrial device diagnostics | NAMUR NE 107, via [Endress+Hauser](https://netilion.endress.com/blog/namur-ne-107/), [NAMUR](https://www.namur.net/en/publications/news-archive/ne107-self-monitoring-and-diagnostics-of-field-devices-has-been-revised.html) | Four standard status signals for field devices |
| Alarm management | ISA-18.2 / IEC 62682, via [Yokogawa](https://www.yokogawa.com/us/library/resources/media-publications/implementing-alarm-management-per-the-ansi-isa-182-standard-control-engineering/), [Emerson](https://www.emerson.com/documents/automation/white-paper-alarm-rationalization-deltav-en-56654.pdf), [TiPS](https://tipsweb.com/taming-alarm-floods/) | Alarm rationalization, flood limits, shelving |
| Machine states | OMAC PackML / ISA-TR88.00.02, via [OPC Foundation](https://reference.opcfoundation.org/PackML/v101/docs/6.3.8), [PLCopen](https://www.plcopen.org/download_file/force/91a361c1-b61b-4e8f-a321-ead1d8e92136/342/) | Standard machine state model (Held, Suspended, Aborted, Clearing) |
| Industrial cybersecurity | IEC 62443, via [IEC SyC SE](https://syc-se.iec.ch/deliveries/cybersecurity-guidelines/security-standards-and-best-practices/iec-62443/), [MDPI zones and conduits](https://www.mdpi.com/2624-800X/6/2/52) | Zones, conduits, seven foundational requirements, security levels 1 to 4 |
| Flight software | NASA F´ [Svc::Health](https://fprime.jpl.nasa.gov/latest/Svc/Health/docs/sdd/), [health-checking pattern](https://fprime.jpl.nasa.gov/latest/docs/user-manual/design-patterns/health-checking/) | Component pings with a warning timeout, then a fatal timeout |
| Runtime assurance | [ASTM F3269](https://store.astm.org/f3269-21.html), [DLR paper](https://elib.dlr.de/144352/1/latestsubmission_v1_ASTM%20F3269_SciTech_Control-ID_3453655.pdf), [arXiv 2110.03506](https://arxiv.org/pdf/2110.03506) | Simplex architecture: a complex function bounded by a monitor and a recovery function |
| Automotive | AUTOSAR [Watchdog Manager](https://www.autosar.org/fileadmin/standards/R23-11/CP/AUTOSAR_CP_SWS_WatchdogManager.pdf); ISO 21448 SOTIF, via [ISO](https://www.iso.org/standard/70939.html), [CS Canada](https://www.cscanada.ca/sotif-introduction/) | Three kinds of supervision; hazards without faults |
| Robotics | ROS 2 [diagnostics](https://docs.ros.org/en/jazzy/p/diagnostic_aggregator/__README.html) ([message](https://docs.ros2.org/galactic/api/diagnostic_msgs/msg/DiagnosticStatus.html)); Autoware [fail-safe](https://tier4.github.io/autoware-documentation/latest/design/autoware-interfaces/ad-api/features/fail-safe/); [SROS2](https://arxiv.org/pdf/2208.02615), [ROS 2 DDS-Security](https://design.ros2.org/articles/ros2_dds_security.html) | STALE status; request to intervene, then minimal-risk manoeuvre; signed per-node permissions |
| Autonomy assurance | [UL 4600 safety performance indicators](https://arxiv.org/pdf/2410.00578), [overview](https://www.perforce.com/blog/qac/what-is-ul-4600) | An instrumented safety case with monitored indicators |
| Operations | [systemd.unit](https://www.freedesktop.org/software/systemd/man/latest/systemd.unit.html) (`OnFailure=`, start limits); [Erlang/OTP supervisor](https://www.erlang.org/doc/system/sup_princ.html); Prometheus dead man's switch ([DEV](https://dev.to/irinobservability/a-dead-mans-switch-for-your-monitoring-stack-2335), [pingcap/dead-mans-switch](https://github.com/pingcap/dead-mans-switch)) | Failure hooks; restart intensity that escalates; an independent heartbeat |
| Identity and capabilities | [SPIFFE/SPIRE](https://www.encryptionconsulting.com/spiffe-spire-explained/); Fuchsia [capability routing](https://fuchsia.dev/fuchsia-src/development/components/connect) | Short-lived attested workload identity; explicitly routed capabilities |
| AI agents | [CaMeL, "Defeating Prompt Injections by Design"](https://arxiv.org/abs/2503.18813); ["Levels of Autonomy for AI Agents"](https://arxiv.org/abs/2506.12469) | Untrusted data never controls program flow; autonomy designed separately from capability |

## 3. What the sources confirm (keep as designed)

| Our design | Established counterpart |
| --- | --- |
| Warn at half of `T`, stop at `T` (PD-01.3) | **F´ Svc::Health** raises WARNING_HI at the first ping timeout and FATAL at the second. **Autoware** issues a Request to Intervene before it switches to a minimal-risk manoeuvre |
| Exit 78 needs a human and is never restarted | **PackML** ABORTED leaves only on an explicit Clear after manual correction |
| Blind is a status of its own (S-1, S-2) | **ROS diagnostics** has a fourth level, STALE, next to OK, WARN and ERROR |
| The daemon requests; a separate gate decides (section 3 of PD-01) | **Simplex / ASTM F3269**: a monitor bounds the complex function and switches to a recovery function |
| Capabilities declared per class in the manifest | **Fuchsia** capability routing: every capability explicitly routed, least privilege by construction. **SROS2**: signed per-node permissions |
| Credentials only in the gate, short-lived (S-5) | **SPIFFE/SPIRE**: attested workload identity, certificates of about an hour, rotated at half-life |
| The start limit escalates a persistent fault | **Erlang/OTP**: past its restart intensity a supervisor terminates and its parent decides |
| Observed text never steers a model (PD-61) | **CaMeL**: control flow comes only from the trusted query; untrusted data carries capability tags |
| Capability separate from how autonomous it is | **Levels of Autonomy for AI Agents**: autonomy (operator … observer) is designed independently of capability |

## 4. What we are missing (proposed additions)

Each item is written as an amendment to PD-01, numbered A-1 to A-12, and is PENDING.

**A-1. Adopt the black-channel error model for the connector gate** (IEC 61784-3). Industrial safety protocols treat the transport as untrusted and defend against a fixed list of errors: corruption, unintended repetition, incorrect sequence, loss, unacceptable delay, insertion, masquerade and addressing. The fixed countermeasures are sequence numbers, timestamps or time expectation, connection identifiers, and CRC or checksum integrity.

Our section 6 table was assembled ad hoc and lacks integrity checks and masquerade. Replace it with this error-by-measure matrix, and make it the connector gate's conformance suite. This matches the transport contract's LTC CT-20 and CT-21, which cover the same ground from the other side.

**A-2. Independent monitoring for every class, not only critical** (dead man's switch; systemd `OnFailure=`). A component cannot report its own total failure: a hung interpreter, a killed process or a full disk looks the same as "all quiet". Two cheap additions:
- the generated unit sets `OnFailure=` to a separate notifier unit, so a failed daemon is announced without the daemon needing any network;
- a separate timer checks the age of each daemon's latest ledger record or digest stamp from outside the daemon.

Today a failed unit is visible only to someone who looks.

**A-3. Consequence levels come from a recorded risk assessment, not self-declaration.** Every functional-safety standard derives its level from the hazard:
- ISO 26262 uses severity, exposure and controllability;
- IEC 61508 uses a risk graph;
- ISO 13849 uses severity, frequency and avoidability;
- IEC 62304 bases classes A, B and C on possible injury.

If each daemon declares its own level, everything will be "standard". Amend PD-01.2: the level comes from a short, recorded assessment of severity, exposure and controllability, and the assessment is part of what the activation register (PD-40) approves.

**A-4. A standard status vocabulary for alerts** (NAMUR NE 107, ROS diagnostics). Instead of inventing states, map ours onto NE 107's four signals:

| NE 107 signal | Our meaning |
| --- | --- |
| **Failure** | `SENSE_BLIND` or another fail-closed exit |
| **Function check** | A declared maintenance or test mode |
| **Out of specification** | Degraded: unsettled past half of `T` |
| **Maintenance required** | Working, but nearing a cap such as `max_bytes` |

Keep OK, WARN, ERROR and STALE as the internal status. This makes 1b alerts readable by existing industrial and robotics tooling.

**A-5. Alarm management rules for Sentinels** (ISA-18.2 / IEC 62682). The alarm-management standards require:
- every alarm to have a defined operator response (rationalization) and a priority;
- flood limits: more than 10 alarms in 10 minutes per operator is a flood, and the goal is about 1;
- shelving with an expiry.

Add these as requirements for class 1b.

**A-6. Shelving: a bounded, recorded maintenance window** (from A-4 and A-5). This settles the long-build tension behind PD-63. `T` should not be set huge just to survive an occasional long build. Instead, a human declares a time-limited maintenance window, recorded in the ledger as "function check", during which the blind limit is extended to a stated maximum.

This fits S-3 exactly: only a human, or a rule approved in advance, extends `T`, and the extension is recorded and expires.

**A-7. Tell internal causes from external ones** (PackML Held versus Suspended). PackML separates an interruption caused inside the machine (Held) from one caused upstream or downstream (Suspended). Our blind record has `last_cause_kind` (unsettled or error) but not where the cause lies. Add an origin field:
- **external:** the observed system is busy. Wait, or tell the repository's owner.
- **internal:** the daemon, its capacity or its code. Fix the daemon.

The origin decides who the escalation goes to.

**A-8. Three kinds of supervision** (AUTOSAR Watchdog Manager).

| AUTOSAR supervision | What it checks | What the pattern has |
| --- | --- | --- |
| Alive | Something runs often enough | The watchdog |
| Deadline | A step finishes in time | `step_timeout_seconds` |
| Progress (no AUTOSAR equivalent) | Useful work is being accepted | The blind period |
| Logical | Steps run in the right order | Nothing |

Logical supervision is not needed for observers. For the Act family's executor it is essential: authorize, then ticket, then execute, then verify, in that order, and never skipping one. Add it to PD-01.5.

**A-9. Blindness without a fault is its own hazard class** (ISO 21448 SOTIF). SOTIF covers hazards from functional insufficiencies, where the system works as designed but its sensing cannot cope with the situation, as opposed to faults. That describes our unsettled samples exactly. Each daemon should list its known **triggering conditions**. For `git-watch` these are a long build, a rebase, and a large untracked dataset. The list does two jobs: `T` is chosen against it, and acceptance tests replay it.

**A-10. An instrumented safety case with monitored indicators** (UL 4600). Present the evidence as claims, arguments and evidence, and monitor field **safety performance indicators** with thresholds, so the case is re-tested in operation. For daemons, the indicators would be:
- the number and length of blind streaks;
- the rate of degraded alerts;
- restarts;
- digest staleness;
- how close each daemon runs to its caps.

**A-11. Zones, conduits and security levels for outward connections** (IEC 62443). Treat each declared connection as a **conduit** with its own target security level (1 to 4). Check the connector gate against the seven foundational requirements:
1. identification and authentication;
2. use control;
3. system integrity;
4. data confidentiality;
5. restricted data flow;
6. timely response to events;
7. resource availability.

This gives S-5 and section 6 an industrial yardstick.

**A-12. Freedom from interference between daemons of different consequence** (the mixed-criticality principle behind ISO 26262's freedom from interference). Several daemons share one DGX. CPU weight and memory caps exist today. **Disk** is not isolated: one noisy daemon filling the shared filesystem makes every other daemon's ledger commit fail with exit 70. A daemon with a consequence level above standard needs its own filesystem or a quota.

## 5. Correction made during this review

**HF-28 was overstated.** r3.5 described the missing `RestartPreventExitStatus=` as an endless restart loop for every exit 78, citing systemd's default start limit (5 in 10 s). The generated unit, however, already set its own limit of 5 starts in 300 s.
- Fast fail-closed exits (a policy violation, an unsafe output directory, a corrupt ledger) were restarted five times, then stopped.
- `SENSE_BLIND` exits only after `T`, so for any realistic `T` (above about 75 s) five restarts cannot fit in the 300 s window. The limit never trips, and a blind daemon would have restarted and re-waited `T` forever.

The fix stands. The description in `HARDENING.md` and the comment in `unitgen.py` are corrected, and HF-28's severity is lowered from High to Medium.

## 6. Not adopted, and why

- **A third axis for autonomy** (Levels of Autonomy for AI Agents). The paper is right that autonomy is separate from capability. For this pattern, though, autonomy only matters in the Act family, and it fits better as a per-action approval mode in the 3a runbook (always approve, approve on risk, pre-approved) than as a third axis on every daemon. This is recorded as an open question in PD-01.
- **Full SIL/ASIL machinery** (IEC 61508, ISO 26262 processes). It is out of proportion for observers. It becomes relevant only if the `critical` level is ever unlocked, which PD-01 already refuses until a certification path exists.
