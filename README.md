# mainframe-bank

A COBOL core-banking system — a fictional bank ("Meridian Trust Company Online Banking System") written in GnuCOBOL 3.2 free-format source, built like a 1980s/90s mainframe shop: indexed datasets, a CICS-style teller terminal, a single posting engine, double-entry general ledger, record locking, teller authorization, a parameter-driven fee/rate table, and a night-end batch job.

## Quick start

```sh
cd mainframe-bank
./bin/LOADLIB          # build/rebuild the datasets from scratch
./bin/TERMBANK         # interactive teller session (sign on with TELLER01/SUPVSR)
./bin/NIGHTEND         # run the night-end batch job
./bin/BUILD            # rebuild everything from source
```

## Architecture

```
src/
  cics/MBANMAIN.cbl    Teller terminal driver — sign-on, 8 transactions, screen I/O
  lib/MBPOSTNG.cbl     The posting engine — the only program that writes a balance
  lib/MBSCRN.cbl       24×80 screen manager (CICS-map style)
  lib/MBMONEY.cbl      Money parsing + exact fixed-point arithmetic
  lib/MBDATE.cbl       YYYYMMDD date arithmetic
  lib/MBINTCAL.cbl     Interest & loan amortisation
  lib/MBLOGN.cbl       Sign-on service (operator master, lockout)
  lib/MBPARM.cbl       Parameter service (fees, rates, limits)
  lib/MBGL.cbl         General ledger balance maintenance
  init/MBLD001.cbl     Dataset bootstrap (60 customers, 120 accounts, 4 loans)
  batch/MBJB001.cbl    Night-end batch job
  copy/                COMMAREAs, record layouts, constants
test/                  Unit + integration tests
bin/                   Compiled binaries + build scripts
data/                  GnuCOBOL datasets (generated)
```

### The posting engine

Every financial movement funnels through `MBPOSTNG`. It supports 9 transaction types and on each success writes, in order:

1. Balance file
2. Account master lifetime credit/debit counters
3. Immutable transaction journal (keyed by sequence)
4. Double-entry general ledger (one DR + one CR line per posting)
5. Control account balances
6. Append-only audit trail with old and new balances

### Double-entry general ledger

Each posting writes exactly two GL lines — one debit, one credit, same amount — so the signed sum of the ledger is always zero. The night-end job proves this and refuses to close the day if the ledger is out of balance.

### Record locking

A posting takes a lock on the account before reading it. A second task that finds the lock is told the account is in use. Locks older than a parameter-driven number of minutes are stale (a crashed task) and are swept by the night-end job.

### Teller authorization

Operators sign on with an ID and password. Each has an authority level and a transaction limit. A movement that exceeds the limit is rejected with RC 60 unless a supervisor authorizes it. Three failed passwords lock the operator out.

### Parameters

Fees, rates, and limits live in `PARMFILE`, not in program literals, so a rate change is a data change. The NSF fee, overdraft limits, minimum balances, interest rates, service fee, and lock timeout are all parameterized.

### Night-end batch job

`MBJB001` runs after the online day is over:

1. Interest accrual — a day's interest on every loan
2. Statement lines — one line per journal entry
3. Ledger proof — debits equal credits, net zero
4. Stale lock sweep
5. Job log — what ran, when, with what counts

The job is restartable: the run date is held in the counter dataset, so a re-run on the same day is refused unless the operator passes `OVERRIDE`.

## Datasets

| File | Organization | Key | Purpose |
|---|---|---|---|
| CUSTFILE | Indexed | CU-ID | Customer master |
| ACCTFILE | Indexed | AC-NO | Account master (static attributes) |
| BALFILE | Indexed | BA-NO | Account balance (mutable) |
| LOANFILE | Indexed | LN-NO | Loan master |
| LNBALFILE | Indexed | LB-NO | Loan balance & delinquency |
| TELLFILE | Indexed | TE-ID | Teller/operator master |
| PARMFILE | Indexed | PRM-NAME | Bank parameters |
| TXNFILE | Indexed | TX-SEQ | Transaction journal (immutable) |
| AUDFILE | Sequential | — | Audit trail (append-only) |
| GLFILE | Sequential | — | General ledger entries (append-only) |
| GLBFILE | Indexed | GB-ACCT | General ledger control balances |
| LOCKFILE | Indexed | LK-KEY | Record locks |
| SEQFILE | Indexed | SQ-NAME | Sequence counters |
| STMTFILE | Sequential | — | Statement lines |
| JOBLOG | Sequential | — | Batch job run log |

## Keying scheme

- Customers: `Cnnnnnnnnn`
- Checking: `1nnnn00000`
- Savings: `2nnnn00000`
- Loans: `Lnnnnnnnnnnn`
- Tellers: `Tnnnnnnn`

## Test suite

```sh
./bin/LOADLIB && for t in TDATE TMONEY TINTCAL TLOGN TPARM TPOST TJOB; do ./build/$t; done
```

| Test | Program | Assertions |
|---|---|---|
| TDATE | MBDATE date arithmetic | 31 |
| TMONEY | MBMONEY money parsing | 24 |
| TINTCAL | MBINTCAL interest | 11 |
| TLOGN | MBLOGN sign-on | 7 |
| TPARM | MBPARM parameters | 6 |
| TPOST | MBPOSTNG posting engine + GL | 29 |
| TJOB | MBJB001 night-end batch | 6 |

## Build

```sh
./bin/BUILD          # build everything
./bin/BUILD clean    # remove build/ first
```

Requires GnuCOBOL 3.x (`cobc`). The build uses `-free -I src/copy`.

## Design decisions

- **No binary floating point.** Balances are `S9(13)V99 COMP-3`. The annuity factor in MBINTCAL is computed by summation rather than `**`, because a float would put a long-term payment out by a cent.
- **Integer division is stepped, not inlined.** GnuCOBOL rounds a division inside a compound `COMPUTE` but truncates it when stepped separately. All integer division goes through a `DIVIDE ... REMAINDER` paragraph to keep leap-year and century handling exact.
- **A called module never carries session context in its own storage.** The screen manager reads the operator identity from the commarea, not from its own copy of a common area.
- **A `PERFORM UNTIL` tests before it runs.** Seeding the condition variable with zero makes the loop unreachable.
- **A literal moved into a commarea group wipes the constants that live inside it.** Messages go to the message field, not to the whole group.

## Known limitations

- The transfer's "PREVIOUS BALANCE / NEW BALANCE" on the receipt describes the receiving account, not the one debited.
- The deposit/withdrawal screen title row shows the previous message because the shared screen takes its title from `SC-MSG`.
- No CICS, no DB2, no JCL — the CICS idioms are simulated, not real.
