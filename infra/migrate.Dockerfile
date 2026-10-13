FROM postgres:14.6-alpine@sha256:a0f7890719c3c42b9f78f1ad98b2cd45aa2ee3472da12f9d2d3bfbc07f421146

COPY setup.sql /setup.sql
COPY migrations /migrations
COPY migrate.sh /migrate.sh

ENTRYPOINT ["sh", "/migrate.sh"]
