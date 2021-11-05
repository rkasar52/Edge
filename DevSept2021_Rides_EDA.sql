SELECT CAST(["starttime"] AS DATETIME2) as Start_Time FROM dbo.[Sep21-bluebikes-tripdata]


select convert(datetime, ["starttime"], 126) as Start_Time FROM dbo.[Sep21-bluebikes-tripdata]

DROP TABLE IF EXISTS #SEP21;

SELECT * INTO #Sep21
FROM dbo.[Sep21-bluebikes-tripdata]


ALTER TABLE #Sep21
ALTER COLUMN ["starttime"] DATE;


-- Top Stations by Start Ride
SELECT ["start station name"], COUNT(["start station id"] as Num_Start_Rides FROM dbo.[Sep21-bluebikes-tripdata]
GROUP BY ["start station name"]
ORDER BY 2 DESC

