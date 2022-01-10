# yt-mirror

**This tool is still in active development, therefore it is not suitable for end-user**

Some necessary things are still missing for MVP. See Issues for more details.

## Usage

Start by preparing bookmarks to synchronize:

```sh
yt-mirror-exe prepare -p ./process.sqlite -b ./places.sqlite
```

Then run synchronization:

```sh
yt-mirror-exe synchronize -p ./process.sqlite
```