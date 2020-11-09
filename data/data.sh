psql -h host -p port -U joeexotic jira -c "COPY (SELECT * FROM kafka.jira_daily_statuses) TO STDOUT DELIMITER ',' CSV HEADER" > jira.csv
