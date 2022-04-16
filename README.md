# ustc-hs

## Build

```
cabal build
```

## Bots

### yjsbot

| Env variable | Note                   | Format Example       |
|--------------|------------------------|----------------------|
| `EMAIL_USER` | Email address          | foo@mail.ustc.edu.cn |
| `EMAIL_PASS` | Email account password | [redacted]           |
| `USERNAME`   | YJS platform username  | SA........           |
| `PASSWORD`   | YJS platform password  | [redacted]           |

### epcbot

| Env variable | Note                                 | Format Example       |
|--------------|--------------------------------------|----------------------|
| `EMAIL_USER` | Email address                        | foo@mail.ustc.edu.cn |
| `EMAIL_PASS` | Email account password               | [redacted]           |
| `EPC_USER`   | EPC platform username                | SA........           |
| `EPC_PASS`   | EPC platform password                | [redacted]           |
| `TAG`        | Bot tag, used in email notifications | "number 1"           |

You probably also want to modify `epcConfig` in `epcbot.hs`.

