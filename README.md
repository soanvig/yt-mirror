# yt-mirror

Do you use YouTube to listen to music, and you save it in bookmarks? I do.
It would be cool to have all this music on your local PC as a backup or just offline access, wouldn't it?

`yt-mirror` parses your browser bookmarks, looks for YouTube URLs, downloads movies, and converts them to a music file.
It runs incrementally, so you can synchronize videos as they are added up - only new bookmarks will be downloaded.

**This tool is still in active development**

Currently it works only on Linux (dependency on `mv` command - for Windows change `mv` to `move` in `Downloader.hs`).

## Supported browsers

- Firefox
- Chrome/Chromium/Edge (and possibly other Chromium-based browsers)

## Requirements

- sqlite3
- ffmpeg
- yt-dlp

## Installation

Download binary from GitHub Release page or build from source using Cabal.

## Usage

Start by preparing bookmarks to synchronize (`places.sqlite` is treated as Firefox bookmarks, `Bookmarks[.json]` is treated as Chromium-alike bookmarks):

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

### Using multiple bookmarks sources

Because `prepare` step is separated, and videos to synchronize are stored with video id, you can use multiple sources of bookmarks to prepare, just by
running `prepare` step multiple times. For example to synchronize bookmarks from Firefox and Chrome:

```sh
yt-mirror-exe prepare -p ./process.sqlite -b [...]/places.sqlite
yt-mirror-exe prepare -p ./process.sqlite -b [...]/Bookmarks
```

### Filtering videos to download

Using `--filter` user can utilise full power of [youtube-dl --match-filter](https://github.com/ytdl-org/youtube-dl/blob/master/README.md#video-selection).
It is useful to skip some videos, like those annoying 10h music videos that are sometimes added to bookmarks.

If video is filtered its process will be marked as "skipped".

Example - to download only videos with duration lower than 1000s:

```
yt-mirror-exe synchronize -p ./process.sqlite -t ~/music/synchronized --filter "duration < 1000"
```

## Bookmarks locations

. | Firefox | Chrome | Chromium
--- | --- | --- | ---
Windows | `%appdata%\Mozilla\Firefox\Profiles\*\places.sqlite` | `%appdata%\..\Local\Google\Chrome\User Data\Default\Bookmarks` | ?
Linux | ` ~/.mozilla/firefox/*/places.sqlite` | `~/.config/google-chrome/Default/Bookmarks` | `~/.config/chromium/Default/Bookmarks`


