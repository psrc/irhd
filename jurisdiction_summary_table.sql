-- Summarize by County then jurisdiction/City, 2020 IRHD
--    Lines 6-8 group together the jurisdictions that are not cities (all unincorporated, such as PAA, UGA, etc). Includes cities without any IRH units
--    Lines 9 and 13 select the fields
--    Line 11 joins the two IRHD tables together

WITH cte AS(
    SELECT CASE WHEN r.feat_type='city' THEN r.juris
                ELSE CONCAT('Uninc.', r.cnty_name) END AS juris,
            p.total_restricted_units, r.cnty_name,f.ami_20,f.ami_25,f.ami_30,f.ami_35,f.ami_40,f.ami_45,f.ami_50,f.ami_60,f.ami_65,f.ami_70,f.ami_75,f.ami_80,f.ami_85,f.ami_90,f.ami_100,f.ami_unknown,f.bedroom_0,f.bedroom_1,f.bedroom_2,f.bedroom_3,f.bedroom_4,f.bedroom_5,f.bedroom_unknown
    FROM irhd.irhd_facts AS f
    JOIN irhd.property_dim AS p ON f.property_dim_id=p.property_dim_id
    RIGHT JOIN ElmerGeo.dbo.PSRC_REGION AS r ON p.Shape.STIntersects(r.Shape)=1)
SELECT juris, cnty_name, sum(total_restricted_units) AS TotalRestrictedUnits, sum(ami_20 + ami_25 + ami_30) AS AMI0_30, sum(ami_35 + ami_40 + ami_45 + ami_50) AS AMI31_50, sum(ami_60 + ami_65 + ami_70 + ami_75 + ami_80) AS AMI51_80, sum(ami_85 + ami_90 + ami_100) AS AMI81_100, sum(ami_unknown) AS AMI_Unknown, sum(bedroom_0) AS Bedroom0, sum(bedroom_1) AS Bedroom1, sum(bedroom_2) AS Bedroom2, sum(bedroom_3) AS Bedroom3, sum(bedroom_4) AS Bedroom4, sum(bedroom_5) AS Bedroom5, sum(bedroom_unknown) AS bedroom_Unknown
FROM cte 
GROUP BY juris, cnty_name 
ORDER BY cnty_name, juris;


-- Summarize by County - AMI breakdown
SELECT county, sum(p.total_restricted_units) AS TotalRestrictedUnits, sum(f.ami_20 + f.ami_25 + f.ami_30) AS AMI0_30, sum(f.ami_35 + f.ami_40 + f.ami_45 + f.ami_50) AS AMI31_50, sum(f.ami_60 + f.ami_65 + f.ami_70 + f.ami_75 + f.ami_80) AS AMI51_80, sum(f.ami_85 + f.ami_90 + f.ami_100) AS AMI81_100, sum(f.ami_unknown) AS AMI_Unknown
FROM Elmer.irhd.irhd_facts AS f
JOIN Elmer.irhd.property_dim AS p ON f.property_dim_id=p.property_dim_id
WHERE data_year = '2020'
GROUP BY county

-- Summarize by County - Bedroom breakdown
SELECT county, sum(p.total_restricted_units) AS TotalRestrictedUnits, sum(f.bedroom_0 + f.bedroom_1) AS Studio_1Bedroom, sum(f.bedroom_2 + f.bedroom_3) AS _2_and_3Bedrooms, sum(f.bedroom_4 + f.bedroom_5) AS _4Plus_Bedrooms, sum(f.bedroom_unknown) AS bedroom_Unknown
FROM Elmer.irhd.irhd_facts AS f
JOIN Elmer.irhd.property_dim AS p ON f.property_dim_id=p.property_dim_id
WHERE data_year = '2020'
GROUP BY county
