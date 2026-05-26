# Anvil Mobile Memory

Static thin client for Doc 41 Phase 4 Pattern A.

Use it from a phone that can reach the bridge over Tailscale or a trusted LAN.
Set the bridge URL, for example `http://100.x.y.z:8730`, and paste the bridge
Bearer token. Search uses `/memory/search`; saves use `/memory/save`.

When a save fails, the entry is stored in `localStorage` and replayed with the
Replay Queue button or the next browser `online` event. The app does not open an
SSE stream and does not poll in the background.
