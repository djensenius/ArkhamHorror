FROM postgres:14.6-alpine

COPY setup.sql /setup.sql
COPY migrations /migrations
COPY migrate.sh /migrate.sh

ENTRYPOINT ["sh", "/migrate.sh"]
