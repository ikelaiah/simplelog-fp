# SimpleLog-FP v1.1.0

Minor release: zip archiving of rotated logs and easier-to-read console colors.
No breaking API changes.

## Zip archiving

Thanks to donvenchenzo (#5), rotated logs can be kept as numbered zip archives:

```pascal
Log := TSimpleLog.FileLog('app.log')
  .SetMaxFileSize(1024 * 1024)
  .SetArchiveLogFiles(True)
  .SetMaxRotateFiles(5);
```

```text
app.log
app.log.1.zip   (newest)
...
app.log.5.zip   (oldest)
```

- Off by default; plain rotation (`app.log.1`) is unchanged.
- Runs in the calling thread; no extra threads and no `cthreads` requirement.
- The active log is cleared only after its archive is written. Failures follow
  `ErrorMode`.

## Level-only console colors

Thanks to rchastain2 (#3), only the `[LEVEL]` tag is colored by default, so
messages stay readable:

```pascal
Log := TSimpleLog.Console.SetColorScope(ccsLine); // 1.0 whole-line colors
```

## Upgrade notes

- Console output looks different by default (colored tag only). Call
  `SetColorScope(ccsLine)` to keep the old look.
- Log files are unchanged.
