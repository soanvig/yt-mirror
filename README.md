# yt-mirror

**This tool is still in active development, therefore it is not suitable for end-user**

Some necessary things are still missing for MVP. See Issues for more details.

Currently it works only on Linux (dependency on `mv` command - for Windows change `mv` to `move` in `Downloader.hs`)

## Requirements

- sqlite3
- ffmpeg
- youtube-dl

## Usage

Start by preparing bookmarks to synchronize:

```sh
yt-mirror-exe prepare -p ./process.sqlite -b ./places.sqlite
```

Then run synchronization:

```sh
yt-mirror-exe synchronize -p ./process.sqlite -t ~/music/synchronized --tmp /tmp
```

`--tmp` defaults to `/tmp` and describes where `youtube-dl` temporary files will be stored.
