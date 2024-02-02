# copied-squares

my implementation of [this
thing](https://hachyderm.io/@vnglst/111828811496422610), live [here](https://am-katzest.duckdns.org/balls/index.html)

## developement

Run `lein figwheel` in your terminal. Wait for a while until you see `Successfully compiled "resources/public/js/main.js"`. Open [localhost:3449](http://localhost:3449) in your browser.

## building

run `lein do clean, cljsbuild once optimized`. compiled (hostable) files are in `resources/public/`

## License

AGPLv3
