
.print ' '
.print 'Import the data'
--here I create a table called fl to import the csv file.
CREATE TABLE fl(
  "policyID" REAL,
  "statecode" TEXT,
  "county" TEXT,
  "eq_site_limit" REAL,
  "hu_site_limit" REAL,
  "fl_site_limit" REAL,
  "fr_site_limit" REAL,
  "tiv_2011" REAL,
  "tiv_2012" REAL,
  "eq_site_deductible" REAL,
  "hu_site_deductible" REAL,
  "fl_site_deductible" REAL,
  "fr_site_deductible" REAL,
  "point_latitude" REAL,
  "point_longitude" REAL,
  "line" TEXT,
  "construction" TEXT,
  "point_granularity" REAL
  );
  
 -- import the data
.mode csv
.import /home/ouecon013/DScourseS19/ProblemSets/PS3/FL_insurance_sample.csv fl


--Print out the first 10 observation
.print ' '
.print 'Print out the first 10 observation'
SELECT * FROM fl LIMIT 10;

--List of counties in the dataset
.print ' '
.print 'list of counties'
SELECT DISTINCT county FROM fl;


--Depriciation
.print ' '
.print 'Print out the first 10 observation'
SELECT AVG(tiv_2012) FROM fl;
SELECT AVG(tiv_2011) FROM fl;
SELECT  AVG(tiv_2012-tiv_2011) FROM fl;


--Frequency table of the construction variable
.print ' '
.print 'frequency table'
select construction, count(*) as frequency
from your_table
group by construction;

.output fl.sqlite3
.dump



