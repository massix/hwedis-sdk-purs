docker := `which docker`
spago := `which spago`

[private]
@dockerup:
  {{docker}} compose up -d

[private]
@dockerdown:
  {{docker}} compose down

build:
  {{spago}} build

start: dockerdown dockerup
  {{spago}} run

repl:
  {{spago}} repl

test: dockerdown
  {{spago}} test
