## Building

### Docker image

Run the following command:

```bash
docker build --tag crypto-orderbook-db:latest . && docker build --tag orderbook-service:latest exe/app
```

## TODO

- [ ] Don't duplicate debugFilterMarkets in `Main` and `CryptoVenues.Fetch.Debug`
