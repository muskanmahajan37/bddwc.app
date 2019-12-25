app <- ShinyDriver$new("../", seed = 1)
app$snapshotInit("module-02-configure-dictionary-test")

app$uploadFile(`bdFileInput-inputFile` = "puma_concolor.csv")

# -------- 001 Configure Screen
app$setInputs(dataToDictionary = "click")
Sys.sleep(5)
app$snapshot()

# # -------- 002 Update Dictionary
app$setInputs(`bdDictionaryInput-updateCache` = "click")
# Input '`bdDictionaryInput-dictionaryView_rows_current`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryView_rows_all`' was set, but doesn't have an input binding.
Sys.sleep(8)
app$snapshot()

# # -------- 003 Use Cached Dictionary
app$setInputs(`bdDictionaryInput-cacheButton` = "click")
# Input '`bdDictionaryInput-dictionaryView_rows_current`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryView_rows_all`' was set, but doesn't have an input binding.
Sys.sleep(5)
app$snapshot()

# # -------- 004 Edit Panel
app$setInputs(`bdDictionaryInput-addDictPanel` = "Edit")
# Input '`bdDictionaryInput-dictionaryEditView_rows_current`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryEditView_rows_all`' was set, but doesn't have an input binding.
Sys.sleep(3)
app$snapshot()

# # -------- 005 Edit Dictionary
# Input '`bdDictionaryInput-dictionaryEditView_rows_selected`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryEditView_row_last_clicked`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryEditView_rows_selected`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryEditView_cell_clicked`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryEditView_rows_selected`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryEditView_rows_selected`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryEditView_cell_edit`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryEditView_rows_current`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryEditView_rows_all`' was set, but doesn't have an input binding.
app$setInputs(`bdDictionaryInput-edit_text` = "fieldname	standard
 ACCEPTEDNAMEUSAGE	test
 ACCEPTEDNAMEUSAGEID	acceptedNameUsageID
 ACCESSRIGHTS	accessRights
 test	year
 AÑO	year
 ASSOCIATEDMEDIA	associatedMedia
 ASSOCIATEDOCCURRENCES	associatedOccurrences
 ASSOCIATEDORGANISMS	associatedOrganisms
 ASSOCIATEDREFERENCES	associatedReferences
 ASSOCIATEDSEQUENCES	associatedSequences
 ASSOCIATEDTAXA	associatedTaxa
 BASISOFRECORD	basisOfRecord
 BED	bed
 BEHAVIOR	behavior
 BIBLIOGRAPHICCITATION	bibliographicCitation
 CATALOGNUMBER	catalogNumber
 CATNUM	catalogNumber
 CLASE	class
 CLASS	class
 COLECTOR	recordedBy
 COLLECTIONCODE	collectionCode
 COLLECTIONID	collectionID
 COLLECTOR	recordedBy
 CONTINENT	continent
 CONTINENTE	continent
 COORDENADAS	verbatimCoordinates
 COORDINATEPRECISION	coordinatePrecision
 COORDINATES	verbatimCoordinates
 COORDINATEUNCERTAINTYINMETERS	coordinateUncertaintyInMeters
 COUNTRY	country
 COUNTRYCODE	countryCode
 COUNTY	county
 CUENCA	waterBody
 DATAGENERALIZATIONS	dataGeneralizations
 DATASETID	datasetID
 DATASETNAME	datasetName
 DATEIDENTIFIED	dateIdentified
 DATUM	geodeticDatum
 DAY	day
 DAYCOLLECTED	day
 DAYIDENTIFIED	dayIdentified
 DCTERMS:ACCESSRIGHTS	accessRights
 DCTERMS:BIBLIOGRAPHICCITATION	bibliographicCitation
 DCTERMS:LANGUAGE	language
 DCTERMS:LICENSE	license
 DCTERMS:MODIFIED	modified
 DCTERMS:REFERENCES	references
 DCTERMS:RIGHTSHOLDER	rightsHolder
 DCTERMS:TYPE	type
 DECIMALLATITUDE	decimalLatitude
 DECIMALLONGITUDE	decimalLongitude
 DETERMINED BY	identifiedBy
 DIA	day
 DÍA	day
 DISPOSITION	disposition
 DRAINAGE	waterBody
 DWC:ACCEPTEDNAMEUSAGE	acceptedNameUsage
 DWC:ACCEPTEDNAMEUSAGEID	acceptedNameUsageID
 DWC:ASSOCIATEDMEDIA	associatedMedia
 DWC:ASSOCIATEDOCCURRENCES	associatedOccurrences
 DWC:ASSOCIATEDORGANISMS	associatedOrganisms
 DWC:ASSOCIATEDREFERENCES	associatedReferences
 DWC:ASSOCIATEDSEQUENCES	associatedSequences
 DWC:ASSOCIATEDTAXA	associatedTaxa
 DWC:BASISOFRECORD	basisOfRecord
 DWC:BED	bed
 DWC:BEHAVIOR	behavior
 DWC:CATALOGNUMBER	catalogNumber
 DWC:CLASS	class
 DWC:COLLECTIONCODE	collectionCode
 DWC:COLLECTIONID	collectionID
 DWC:CONTINENT	continent
 DWC:COORDINATEPRECISION	coordinatePrecision
 DWC:COORDINATEUNCERTAINTYINMETERS	coordinateUncertaintyInMeters
 DWC:COUNTRY	country
 DWC:COUNTRYCODE	countryCode
 DWC:COUNTY	county
 DWC:DATAGENERALIZATIONS	dataGeneralizations
 DWC:DATASETID	datasetID
 DWC:DATASETNAME	datasetName
 DWC:DATEIDENTIFIED	dateIdentified
 DWC:DAY	day
 DWC:DECIMALLATITUDE	decimalLatitude
 DWC:DECIMALLONGITUDE	decimalLongitude
 DWC:DISPOSITION	disposition
 DWC:DYNAMICPROPERTIES	dynamicProperties
 DWC:EARLIESTAGEORLOWESTSTAGE	earliestAgeOrLowestStage
 DWC:EARLIESTEONORLOWESTEONOTHEM	earliestEonOrLowestEonothem
 DWC:EARLIESTEPOCHORLOWESTSERIES	earliestEpochOrLowestSeries
 DWC:EARLIESTERAORLOWESTERATHEM	earliestEraOrLowestErathem
 DWC:EARLIESTPERIODORLOWESTSYSTEM	earliestPeriodOrLowestSystem
 DWC:ENDDAYOFYEAR	endDayOfYear
 DWC:ESTABLISHMENTMEANS	establishmentMeans
 DWC:EVENTDATE	eventDate
 DWC:EVENTID	eventID
 DWC:EVENTREMARKS	eventRemarks
 DWC:EVENTTIME	eventTime
 DWC:FAMILY	family
 DWC:FIELDNOTES	fieldNotes
 DWC:FIELDNUMBER	fieldNumber
 DWC:FOOTPRINTSPATIALFIT	footprintSpatialFit
 DWC:FOOTPRINTSRS	footprintSRS
 DWC:FOOTPRINTWKT	footprintWKT
 DWC:FORMATION	formation
 DWC:GENUS	genus
 DWC:GEODETICDATUM	geodeticDatum
 DWC:GEOLOGICALCONTEXTID	geologicalContextID
 DWC:GEOREFERENCEDBY	georeferencedBy
 DWC:GEOREFERENCEDDATE	georeferencedDate
 DWC:GEOREFERENCEPROTOCOL	georeferenceProtocol
 DWC:GEOREFERENCEREMARKS	georeferenceRemarks
 DWC:GEOREFERENCESOURCES	georeferenceSources
 DWC:GEOREFERENCEVERIFICATIONSTATUS	georeferenceVerificationStatus
 DWC:GROUP	group
 DWC:HABITAT	habitat
 DWC:HIGHERCLASSIFICATION	higherClassification
 DWC:HIGHERGEOGRAPHY	higherGeography
 DWC:HIGHERGEOGRAPHYID	higherGeographyID
 DWC:HIGHESTBIOSTRATIGRAPHICZONE	highestBiostratigraphicZone
 DWC:IDENTIFICATIONID	identificationID
 DWC:IDENTIFICATIONQUALIFIER	identificationQualifier
 DWC:IDENTIFICATIONREFERENCES	identificationReferences
 DWC:IDENTIFICATIONREMARKS	identificationRemarks
 DWC:IDENTIFICATIONVERIFICATIONSTATUS	identificationVerificationStatus
 DWC:IDENTIFIEDBY	identifiedBy
 DWC:INDIVIDUALCOUNT	individualCount
 DWC:INFORMATIONWITHHELD	informationWithheld
 DWC:INFRASPECIFICEPITHET	infraspecificEpithet
 DWC:INSTITUTIONCODE	institutionCode
 DWC:INSTITUTIONID	institutionID
 DWC:ISLAND	island
 DWC:ISLANDGROUP	islandGroup
 DWC:KINGDOM	kingdom
 DWC:LATESTAGEORHIGHESTSTAGE	latestAgeOrHighestStage
 DWC:LATESTEONORHIGHESTEONOTHEM	latestEonOrHighestEonothem
 DWC:LATESTEPOCHORHIGHESTSERIES	latestEpochOrHighestSeries
 DWC:LATESTERAORHIGHESTERATHEM	latestEraOrHighestErathem
 DWC:LATESTPERIODORHIGHESTSYSTEM	latestPeriodOrHighestSystem
 DWC:LIFESTAGE	lifeStage
 DWC:LITHOSTRATIGRAPHICTERMS	lithostratigraphicTerms
 DWC:LOCALITY	locality
 DWC:LOCATIONACCORDINGTO	locationAccordingTo
 DWC:LOCATIONID	locationID
 DWC:LOCATIONREMARKS	locationRemarks
 DWC:LOWESTBIOSTRATIGRAPHICZONE	lowestBiostratigraphicZone
 DWC:MATERIALSAMPLEID	materialSampleID
 DWC:MAXIMUMDEPTHINMETERS	maximumDepthInMeters
 DWC:MAXIMUMDISTANCEABOVESURFACEINMETERS	maximumDistanceAboveSurfaceInMeters
 DWC:MAXIMUMELEVATIONINMETERS	maximumElevationInMeters
 DWC:MEMBER	member
 DWC:MINIMUMDEPTHINMETERS	minimumDepthInMeters
 DWC:MINIMUMDISTANCEABOVESURFACEINMETERS	minimumDistanceAboveSurfaceInMeters
 DWC:MINIMUMELEVATIONINMETERS	minimumElevationInMeters
 DWC:MONTH	month
 DWC:MUNICIPALITY	municipality
 DWC:NAMEACCORDINGTO	nameAccordingTo
 DWC:NAMEACCORDINGTOID	nameAccordingToID
 DWC:NAMEPUBLISHEDIN	namePublishedIn
 DWC:NAMEPUBLISHEDINID	namePublishedInID
 DWC:NAMEPUBLISHEDINYEAR	namePublishedInYear
 DWC:NOMENCLATURALCODE	nomenclaturalCode
 DWC:NOMENCLATURALSTATUS	nomenclaturalStatus
 DWC:OCCURRENCEID	occurrenceID
 DWC:OCCURRENCEREMARKS	occurrenceRemarks
 DWC:OCCURRENCESTATUS	occurrenceStatus
 DWC:ORDER	order
 DWC:ORGANISMID	organismID
 DWC:ORGANISMNAME	organismName
 DWC:ORGANISMQUANTITY	organismQuantity
 DWC:ORGANISMQUANTITYTYPE	organismQuantityType
 DWC:ORGANISMREMARKS	organismRemarks
 DWC:ORGANISMSCOPE	organismScope
 DWC:ORIGINALNAMEUSAGE	originalNameUsage
 DWC:ORIGINALNAMEUSAGEID	originalNameUsageID
 DWC:OTHERCATALOGNUMBERS	otherCatalogNumbers
 DWC:OWNERINSTITUTIONCODE	ownerInstitutionCode
 DWC:PARENTEVENTID	parentEventID
 DWC:PARENTNAMEUSAGE	parentNameUsage
 DWC:PARENTNAMEUSAGEID	parentNameUsageID
 DWC:PHYLUM	phylum
 DWC:POINTRADIUSSPATIALFIT	pointRadiusSpatialFit
 DWC:PREPARATIONS	preparations
 DWC:PREVIOUSIDENTIFICATIONS	previousIdentifications
 DWC:RECORDEDBY	recordedBy
 DWC:RECORDNUMBER	recordNumber
 DWC:REPRODUCTIVECONDITION	reproductiveCondition
 DWC:SAMPLESIZEUNIT	sampleSizeUnit
 DWC:SAMPLESIZEVALUE	sampleSizeValue
 DWC:SAMPLINGEFFORT	samplingEffort
 DWC:SAMPLINGPROTOCOL	samplingProtocol
 DWC:SCIENTIFICNAME	scientificName
 DWC:SCIENTIFICNAMEAUTHORSHIP	scientificNameAuthorship
 DWC:SCIENTIFICNAMEID	scientificNameID
 DWC:SEX	sex
 DWC:SPECIFICEPITHET	specificEpithet
 DWC:STARTDAYOFYEAR	startDayOfYear
 DWC:STATEPROVINCE	stateProvince
 DWC:SUBGENUS	subgenus
 DWC:TAXONCONCEPTID	taxonConceptID
 DWC:TAXONID	taxonID
 DWC:TAXONOMICSTATUS	taxonomicStatus
 DWC:TAXONRANK	taxonRank
 DWC:TAXONREMARKS	taxonRemarks
 DWC:TYPESTATUS	typeStatus
 DWC:VERBATIMCOORDINATES	verbatimCoordinates
 DWC:VERBATIMCOORDINATESYSTEM	verbatimCoordinateSystem
 DWC:VERBATIMDEPTH	verbatimDepth
 DWC:VERBATIMELEVATION	verbatimElevation
 DWC:VERBATIMEVENTDATE	verbatimEventDate
 DWC:VERBATIMLATITUDE	verbatimLatitude
 DWC:VERBATIMLOCALITY	verbatimLocality
 DWC:VERBATIMLONGITUDE	verbatimLongitude
 DWC:VERBATIMSRS	verbatimSRS
 DWC:VERBATIMTAXONRANK	verbatimTaxonRank
 DWC:VERNACULARNAME	vernacularName
 DWC:WATERBODY	waterBody
 DWC:YEAR	year
 DYNAMICPROPERTIES	dynamicProperties
 EARLIESTAGEORLOWESTSTAGE	earliestAgeOrLowestStage
 EARLIESTEONORLOWESTEONOTHEM	earliestEonOrLowestEonothem
 EARLIESTEPOCHORLOWESTSERIES	earliestEpochOrLowestSeries
 EARLIESTERAORLOWESTERATHEM	earliestEraOrLowestErathem
 EARLIESTPERIODORLOWESTSYSTEM	earliestPeriodOrLowestSystem
 ENDDAYOFYEAR	endDayOfYear
 ESPECIE	specificEpithet
 ESTABLISHMENTMEANS	establishmentMeans
 ESTADO	stateProvince
 EVENTDATE	eventDate
 EVENTID	eventID
 EVENTREMARKS	eventRemarks
 EVENTTIME	eventTime
 FAMILIA	family
 FAMILY	family
 FECHA DE COLECTA	eventDate
 FIELDNOTES	fieldNotes
 FIELDNUMBER	fieldNumber
 FILO	phylum
 FOOTPRINTSPATIALFIT	footprintSpatialFit
 FOOTPRINTSRS	footprintSRS
 FOOTPRINTWKT	footprintWKT
 FORMATION	formation
 GENERO	genus
 GÉNERO	genus
 GENUS	genus
 GEODETICDATUM	geodeticDatum
 GEOLOGICALCONTEXTID	geologicalContextID
 GEOREFERENCEDBY	georeferencedBy
 GEOREFERENCEDDATE	georeferencedDate
 GEOREFERENCEPROTOCOL	georeferenceProtocol
 GEOREFERENCEREMARKS	georeferenceRemarks
 GEOREFERENCESOURCES	georeferenceSources
 GEOREFERENCEVERIFICATIONSTATUS	georeferenceVerificationStatus
 GROUP	group
 HABITAT	habitat
 HIGHERCLASSIFICATION	higherClassification
 HIGHERGEOGRAPHY	higherGeography
 HIGHERGEOGRAPHYID	higherGeographyID
 HIGHESTBIOSTRATIGRAPHICZONE	highestBiostratigraphicZone
 IDENTIFICATIONID	identificationID
 IDENTIFICATIONQUALIFIER	identificationQualifier
 IDENTIFICATIONREFERENCES	identificationReferences
 IDENTIFICATIONREMARKS	identificationRemarks
 IDENTIFICATIONVERIFICATIONSTATUS	identificationVerificationStatus
 IDENTIFIEDBY	identifiedBy
 INDIVIDUALCOUNT	individualCount
 INFORMATIONWITHHELD	informationWithheld
 INFRASPECIFICEPITHET	infraspecificEpithet
 INSTITUTIONCODE	institutionCode
 INSTITUTIONID	institutionID
 ISLA	island
 ISLAND	island
 ISLANDGROUP	islandGroup
 KINGDOM	kingdom
 LANGUAGE	language
 LAT	verbatimLatitude
 LATESTAGEORHIGHESTSTAGE	latestAgeOrHighestStage
 LATESTEONORHIGHESTEONOTHEM	latestEonOrHighestEonothem
 LATESTEPOCHORHIGHESTSERIES	latestEpochOrHighestSeries
 LATESTERAORHIGHESTERATHEM	latestEraOrHighestErathem
 LATESTPERIODORHIGHESTSYSTEM	latestPeriodOrHighestSystem
 LATITUD	verbatimLatitude
 LATITUDE	verbatimLatitude
 LENGTH	length
 LICENSE	license
 LIFESTAGE	lifeStage
 LITHOSTRATIGRAPHICTERMS	lithostratigraphicTerms
 LOCALIDAD	locality
 LOCALITY	locality
 LOCATIONACCORDINGTO	locationAccordingTo
 LOCATIONID	locationID
 LOCATIONREMARKS	locationRemarks
 LON	verbatimLongitude
 LONG	verbatimLongitude
 LONGITUD	verbatimLongitude
 LONGITUDE	verbatimLongitude
 LOT COUNT	individualCount
 LOWESTBIOSTRATIGRAPHICZONE	lowestBiostratigraphicZone
 MATERIALSAMPLEID	materialSampleID
 MAXIMUMDEPTHINMETERS	maximumDepthInMeters
 MAXIMUMDISTANCEABOVESURFACEINMETERS	maximumDistanceAboveSurfaceInMeters
 MAXIMUMELEVATIONINMETERS	maximumElevationInMeters
 MEMBER	member
 MES	month
 MINIMUMDEPTHINMETERS	minimumDepthInMeters
 MINIMUMDISTANCEABOVESURFACEINMETERS	minimumDistanceAboveSurfaceInMeters
 MINIMUMELEVATIONINMETERS	minimumElevationInMeters
 MODIFIED	modified
 MONTH	month
 MONTHCOLLECTED	month
 MONTHIDENTIFIED	monthIdentified
 MUNICIPALITY	municipality
 MUNICIPIO	municipality
 NAMEACCORDINGTO	nameAccordingTo
 NAMEACCORDINGTOID	nameAccordingToID
 NAMEPUBLISHEDIN	namePublishedIn
 NAMEPUBLISHEDINID	namePublishedInID
 NAMEPUBLISHEDINYEAR	namePublishedInYear
 NOMBRE CIENTIFICO	scientificName
 NOMBRE CIENTÍFICO	scientificName
 NOMENCLATURALCODE	nomenclaturalCode
 NOMENCLATURALSTATUS	nomenclaturalStatus
 NUMCAT	catalogNumber
 OCCURRENCEID	occurrenceID
 OCCURRENCEREMARKS	occurrenceRemarks
 OCCURRENCESTATUS	occurrenceStatus
 OCEAN	waterBody
 OCEANO	waterBody
 OCÉANO	waterBody
 ORDEN	order
 ORDER	order
 ORGANISMID	organismID
 ORGANISMNAME	organismName
 ORGANISMQUANTITY	organismQuantity
 ORGANISMQUANTITYTYPE	organismQuantityType
 ORGANISMREMARKS	organismRemarks
 ORGANISMSCOPE	organismScope
 ORIGINALNAMEUSAGE	originalNameUsage
 ORIGINALNAMEUSAGEID	originalNameUsageID
 OTHERCATALOGNUMBERS	otherCatalogNumbers
 OWNERINSTITUTIONCODE	ownerInstitutionCode
 PAÍS	country
 PARENTEVENTID	parentEventID
 PARENTNAMEUSAGE	parentNameUsage
 PARENTNAMEUSAGEID	parentNameUsageID
 PHOTO	associatedMedia
 PHYLUM	phylum
 POINTRADIUSSPATIALFIT	pointRadiusSpatialFit
 PREPARATIONS	preparations
 PREVIOUSIDENTIFICATIONS	previousIdentifications
 PREVIOUSTISSUEID	otherCatalogNumbers
 PROVINCE	stateProvince
 PROVINCIA	stateProvince
 RECORDEDBY	recordedBy
 RECORDNUMBER	recordNumber
 REFERENCES	references
 REINO	kingdom
 REPRODUCTIVECONDITION	reproductiveCondition
 RIGHTSHOLDER	rightsHolder
 RIO	waterBody
 RÍO	waterBody
 RIVER	waterBody
 SAMPLESIZEUNIT	sampleSizeUnit
 SAMPLESIZEVALUE	sampleSizeValue
 SAMPLINGEFFORT	samplingEffort
 SAMPLINGPROTOCOL	samplingProtocol
 SCIENTIFICNAME	scientificName
 SCIENTIFICNAMEAUTHORSHIP	scientificNameAuthorship
 SCIENTIFICNAMEID	scientificNameID
 SEX	sex
 SEXO	sex
 SPECIES	specificEpithet
 SPECIFICEPITHET	specificEpithet
 STARTDAYOFYEAR	startDayOfYear
 STATE	stateProvince
 STATE/PROVINCE	stateProvince
 STATEPROVINCE	stateProvince
 SUBESPECIE	infraspecificEpithet
 SUBGENUS	subgenus
 SUBSPECIES	infraspecificEpithet
 TAXONCONCEPTID	taxonConceptID
 TAXONID	taxonID
 TAXONOMICSTATUS	taxonomicStatus
 TAXONRANK	taxonRank
 TAXONREMARKS	taxonRemarks
 TYPE	type
 TYPESTATUS	typeStatus
 VERBATIMCOORDINATES	verbatimCoordinates
 VERBATIMCOORDINATESYSTEM	verbatimCoordinateSystem
 VERBATIMDEPTH	verbatimDepth
 VERBATIMELEVATION	verbatimElevation
 VERBATIMEVENTDATE	verbatimEventDate
 VERBATIMLATITUDE	verbatimLatitude
 VERBATIMLOCALITY	verbatimLocality
 VERBATIMLONGITUDE	verbatimLongitude
 VERBATIMSRS	verbatimSRS
 VERBATIMTAXONRANK	verbatimTaxonRank
 VERNACULARNAME	vernacularName
 WATERBODY	waterBody
 WEIGHT	weight
 WORMSID	taxonID
 YEAR	year
 YEARCOLLECTED	year
 YEARIDENTIFIED	yearIdentified")
# Input '`bdDictionaryInput-dictionaryEditView_rows_current`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryEditView_rows_all`' was set, but doesn't have an input binding.
Sys.sleep(3)
app$snapshot()

# # -------- 006 Reset Edits
app$setInputs(`bdDictionaryInput-reset` = "click")
# Input '`bdDictionaryInput-dictionaryEditView_rows_current`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryEditView_rows_all`' was set, but doesn't have an input binding.
Sys.sleep(3)
app$snapshot()

# # -------- 007 Upload Panel
app$setInputs(`bdDictionaryInput-addDictPanel` = "Upload")
Sys.sleep(3)
app$snapshot()

# # -------- 008 Upload Dictionary
app$uploadFile(`bdDictionaryInput-inputFile` = "customDict.txt") # <-- This should be the path to the file, relative to the app's tests/ directory
# Input '`bdDictionaryInput-dictionaryView_rows_current`' was set, but doesn't have an input binding.
# Input '`bdDictionaryInput-dictionaryView_rows_all`' was set, but doesn't have an input binding.
Sys.sleep(3)
app$snapshot()
