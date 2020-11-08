psql -h 95.217.217.10 -p 54320 -U joeexotic jira -c "COPY (SELECT * FROM kafka.jira_daily_statuses) TO STDOUT DELIMITER ',' CSV HEADER" > jira.csv
