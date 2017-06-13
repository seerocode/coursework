#!/bin/bash
#
# add your solution after each of the 10 comments below
#

# count the number of unique stations
sed 1d 201402-citibike-tripdata.csv | cut -d, -f4,8 | tr , '\n' | sort | uniq -c | wc -l

# count the number of unique bikes
sed 1d 201402-citibike-tripdata.csv | cut -d, -f12 | sort | uniq -c | wc -l

# count the number of trips per day
sed 1d 201402-citibike-tripdata.csv | cut -d, -f2 | cut -c10-11 | uniq -c

#for full date: 
sed 1d 201402-citibike-tripdata.csv | cut -d, -f2 | cut -c2-11 | uniq -c

# find the day with the most rides
sed 1d 201402-citibike-tripdata.csv | cut -d, -f2 | cut -c10-11 | uniq -c | sort -rn | head -1

# find the day with the fewest rides
sed 1d 201402-citibike-tripdata.csv | cut -d, -f2 | cut -c10-11 | uniq -c | sort -rn | tail -1

# find the id of the bike with the most rides
sed 1d 201402-citibike-tripdata.csv | cut -d, -f12 | sort | uniq -c | sort -rn | head -1

# count the number of rides by gender and birth year
sed 1d 201402-citibike-tripdata.csv | cut -d, -f14,15 | sort | uniq -c | sort -n

# count the number of trips that start on cross streets that both contain numbers (e.g., "1 Ave & E 15 St", "E 39 St & 2 Ave", ...)
sed 1d 201402-citibike-tripdata.csv | cut -d, -f5 | grep '[0-9]' | sort | uniq -c

# compute the average trip duration
tr -d '"' < 201402-citibike-tripdata.csv | awk -F, 'NR>1 {total+=$1} END {print total/NR}
