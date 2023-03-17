SELECT property_dim_id, reported_address, cleaned_address, project_name, property_name, city, zip, total_units, in_service_date
FROM irhd.property_dim
WHERE Shape IS NULL AND    dbo.rgx_find(cleaned_address, '^NA$|Various|Sites|Mul?tiple',1)=0;

UPDATE irhd.property_dim
SET cleaned_address='2605 15th St'
WHERE property_dim_id=7;

UPDATE irhd.property_dim
SET cleaned_address='2801 7th St'
WHERE property_dim_id=8;

UPDATE irhd.property_dim
SET cleaned_address='915 Columbia Ave'
WHERE property_dim_id=12;

UPDATE irhd.property_dim
SET zip='98223'
WHERE property_dim_id=30;

UPDATE irhd.property_dim
SET cleaned_address='3111 132nd St SE'
WHERE property_dim_id=31;

UPDATE irhd.property_dim
SET cleaned_address='1130 Rainer Ave'
WHERE property_dim_id=33;

UPDATE irhd.property_dim
SET cleaned_address='18326 Smokey Point Blvd'
WHERE property_dim_id=35;

UPDATE irhd.property_dim
SET cleaned_address='6031 208th St SW'
WHERE property_dim_id=36;

UPDATE irhd.property_dim
SET cleaned_address='2820 164th ST NE'
WHERE property_dim_id=38;

UPDATE irhd.property_dim
SET cleaned_address='500 E Fremont St'
WHERE property_dim_id=43;

UPDATE irhd.property_dim
SET cleaned_address='3232 Norton Ave'
WHERE property_dim_id=148;

UPDATE irhd.property_dim
SET cleaned_address='19815 Scriber Lake Rd'
WHERE property_dim_id=188;

UPDATE irhd.property_dim
SET cleaned_address='320 E Fremont St'
WHERE property_dim_id=223;

UPDATE irhd.property_dim
SET cleaned_address='12111 140th Ave E'
WHERE property_dim_id=239;

UPDATE irhd.property_dim
SET cleaned_address='3313 72nd Ave Ct W'
WHERE property_dim_id=242;

UPDATE irhd.property_dim
SET city='Tacoma', zip='98402'
WHERE property_dim_id=243;

UPDATE irhd.property_dim
SET cleaned_address='3018 N Highland St'
WHERE property_dim_id=245;

UPDATE irhd.property_dim
SET cleaned_address='3015 N Pearl St'
WHERE property_dim_id=246;

UPDATE irhd.property_dim
SET cleaned_address='15405 62nd St Ct E'
WHERE property_dim_id=250;

UPDATE irhd.property_dim
SET cleaned_address='1409 S Yakima Ave'
WHERE property_dim_id=372;

UPDATE irhd.property_dim
SET cleaned_address='1709 S G St'
WHERE property_dim_id=374;

UPDATE irhd.property_dim
SET cleaned_address='7522 10th Ave Crt E'
WHERE property_dim_id=380;

UPDATE irhd.property_dim
SET cleaned_address='5102 S 58th St'
WHERE property_dim_id=394;

UPDATE irhd.property_dim
SET cleaned_address='3811 E Arlington Dr'
WHERE property_dim_id=424;

UPDATE irhd.property_dim
SET cleaned_address='6849 NE Kwayachen Ct'
WHERE property_dim_id=446;

UPDATE irhd.property_dim
SET cleaned_address='385 Nollwood Ln'
WHERE property_dim_id=448;

UPDATE irhd.property_dim
SET cleaned_address='1319 Park Ave'
WHERE property_dim_id=470;

UPDATE irhd.property_dim
SET cleaned_address='3100 SE Orlando St'
WHERE property_dim_id=517;

UPDATE irhd.property_dim
SET cleaned_address='634 N Montgomery Ave'
WHERE property_dim_id=519;