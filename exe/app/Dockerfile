
FROM crypto-orderbook-db:latest

ENTRYPOINT crypto-orderbook-db --max-retries 10 --db-max-retries 20 --db-conn "$DATABASE_URL" --max-books 10
