docker := `which docker`
spago := `which spago`

[private]
default: start

clean:
  rm -fr output

[private]
@dockerup:
  {{docker}} compose up -d

[private]
@dockerdown:
  {{docker}} compose down

build:
  {{spago}} build

start: dockerdown dockerup
  @sleep 1
  {{spago}} run

stop: dockerdown

repl:
  {{spago}} repl

test: dockerdown
  {{spago}} test
