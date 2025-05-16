-- Summarizing IRHD Data
--- As outlined in http://aws-linux/mediawiki/index.php/Income-Restricted_Household_Data

DECLARE @vintage AS VARCHAR(100)='2021'

-- Summarize by County
--select *
--from irhd.summarize_by_county(@vintage)

--Summarize by Jurisdiction
select *
from irhd.summarize_by_jurisdiction(@vintage)