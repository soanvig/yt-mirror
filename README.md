# yt-mirror

**This tool is still in active development, therefore it is not suitable for end-user**

Some necessary things are still missing for MVP. See Issues for more details.

Currently it works only on Linux (dependency on `mv` command - for Windows change `mv` to `move` in `Downloader.hs`)

## Supported browsers

- Firefox

## Requirements

- sqlite3
- ffmpeg
- yt-dlp

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

To show failed synchronizations:

```sh
yt-mirror-exe failed -p ./process.sqlite [-s/--short]
```

`-s`/`--short` prints only failed YouTube ids without any decorations

### Filtering videos to download

Using `--filter` user can utilise full power of [youtube-dl --match-filter](https://github.com/ytdl-org/youtube-dl/blob/master/README.md#video-selection).
It is useful to skip some videos, like those annoying 10h music videos that are sometimes added to bookmarks.

If video is filtered its process will be marked as "skipped".

Example - to download only videos with duration lower than 1000s:

```
yt-mirror-exe synchronize -p ./process.sqlite -t ~/music/synchronized --filter "duration < 1000"
```

## Bookmarks locations

```
. | Firefox | Chrome | Chromium
--- | --- | --- | ---
Windows | ? | ? | ?
Linux | ` ~/.mozilla/firefox/i2xv3ca2.default-default/places.sqlite` | `~/.config/google-chrome/Default/Bookmarks` | `~/.config/chromium/Default/Bookmarks`


