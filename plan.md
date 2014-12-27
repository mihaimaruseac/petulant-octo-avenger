# Dev plan

- [x] tcpdump/libpcap + zlib and intercept traffic
- [x] eliminate DUP packets
- [x] check missed/ reordered packets
- [x] trim conversation to proper format
- [x] check duplicated chunks and chunks w/ multiple convos
- [x] extract HTTP layer
- [x] get HTTP layers
- [x] get HTTP main content
- [x] extract and unzip content
- [ ] parse content
  - [ ] Extract HTML info from HTML + HTTP tags.
- [ ] save to db
  - [ ] define db layout
  - [ ] ACID state?
  - [ ] db gc

# Stats to collect

- [ ] personal stats
- [ ] factions stats
- [ ] monsters kills stats
- [ ] pilots kills stats
- [ ] alliances stats
- [ ] top20 stats
- [ ] respected factions stats
- [ ] despised factions stats
- [ ] alliance members stats
- [ ] ships stats
- [ ] buildings stats
- [ ] pilot stats (name, faction, alliance...)
