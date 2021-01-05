#' Create new image
#'
#' Create a new image
#'
#' @return NULL
#' @param idno (required) Unique identifier
#' @param repositoryid Owner Collection ID
#' @param access_policy Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param published Set status for study - 0 = Draft, 1 = Published
#' @param overwrite Overwrite if a study with the same ID already exists? Valid values "yes", "no"
#' @param metadata \strong{(required)} Metadata using the IPTC Image Schema
#'
#' @examples
#'
#' metadata=list(
#'    "repositoryid"= "central",
#'    "published"= "0",
#'    "overwrite"= "no",
#'    "metadata_information"= list(
#'      "title"= "string",
#'      "idno"= "string",
#'      "producers"= list(
#'        list(
#'          "name"= "string",
#'          "abbr"= "string",
#'          "affiliation"= "string",
#'          "role"= "string"
#'        )
#'        ),
#'      "production_date"= "string",
#'      "version"= "string"
#'    ),
#'    "image_description"= list(
#'      "iptc"= list(
#'        "mediafragment"= list(
#'          "uri"= "http://example.com",
#'          "delimitertype"= "spatial",
#'          "description"= "string"
#'        ),
#'        "photoVideoMetadataIPTC"= list(
#'          "aboutCvTerms"= list(
#'            list(
#'              "cvId"= "http://example.com",
#'              "cvTermName"= "string",
#'              "cvTermId"= "http://example.com",
#'              "cvTermRefinedAbout"= "http://example.com"
#'            )
#'            ),
#'          "additionalModelInfo"= "string",
#'          "artworkOrObjects"= list(
#'            list(
#'              "circaDateCreated"= "string",
#'              "contentDescription"= "string",
#'              "contributionDescription"= "string",
#'              "copyrightNotice"= "string",
#'              "creatorIdentifiers"= list(
#'                "string"
#'                ),
#'              "creatorNames"= list(
#'                "string"
#'                ),
#'              "currentCopyrightOwnerIdentifier"= "http://example.com",
#'              "currentCopyrightOwnerName"= "string",
#'              "currentLicensorIdentifier"= "http://example.com",
#'              "currentLicensorName"= "string",
#'              "dateCreated"= "2020-08-31T20:06:54Z",
#'              "physicalDescription"= "string",
#'              "source"= "string",
#'              "sourceInventoryNr"= "string",
#'              "sourceInventoryUrl"= "http://example.com",
#'              "stylePeriod"= list(
#'                "string"
#'                ),
#'              "title"= "string"
#'            )
#'            ),
#'          "captionWriter"= "string",
#'          "cityName"= "string",
#'          "copyrightNotice"= "string",
#'          "copyrightOwners"= list(
#'            list(
#'              "name"= "string",
#'              "role"= list(
#'                "http://example.com"
#'                ),
#'              "identifiers"= list(
#'                "http://example.com"
#'                )
#'            )
#'            ),
#'          "countryCode"= "string",
#'          "countryName"= "string",
#'          "creatorContactInfo"= list(
#'            "country"= "string",
#'            "emailwork"= "string",
#'            "region"= "string",
#'            "phonework"= "string",
#'            "weburlwork"= "string",
#'            "address"= "string",
#'            "city"= "string",
#'            "postalCode"= "string"
#'          ),
#'          "creatorNames"= list(
#'            "string"
#'            ),
#'          "creditLine"= "string",
#'          "dateCreated"= "2020-08-31T20:06:54Z",
#'          "description"= "string",
#'          "digitalImageGuid"= "string",
#'          "digitalSourceType"= "http://example.com",
#'          "embdEncRightsExpr"= list(
#'            list(
#'              "encRightsExpr"= "string",
#'              "rightsExprEncType"= "string",
#'              "rightsExprLangId"= "http://example.com"
#'            )
#'            ),
#'          "eventName"= "string",
#'          "genres"= list(
#'            list(
#'              "cvId"= "http://example.com",
#'              "cvTermName"= "string",
#'              "cvTermId"= "http://example.com",
#'              "cvTermRefinedAbout"= "http://example.com"
#'            )
#'            ),
#'          "headline"= "string",
#'          "imageRating"= 0,
#'          "imageSupplierImageId"= "string",
#'          "instructions"= "string",
#'          "intellectualGenre"= "string",
#'          "jobid"= "string",
#'          "jobtitle"= "string",
#'          "keywords"= list(
#'            "string"
#'            ),
#'          "linkedEncRightsExpr"= list(
#'            list(
#'              "linkedRightsExpr"= "http://example.com",
#'              "rightsExprEncType"= "string",
#'              "rightsExprLangId"= "http://example.com"
#'            )
#'            ),
#'          "locationsShown"= list(
#'            list(
#'              "city"= "string",
#'              "countryCode"= "string",
#'              "countryName"= "string",
#'              "gpsAltitude"= 0,
#'              "gpsLatitude"= 0,
#'              "gpsLongitude"= 0,
#'              "identifiers"= list(
#'                "http://example.com"
#'                ),
#'              "name"= "string",
#'              "provinceState"= "string",
#'              "sublocation"= "string",
#'              "worldRegion"= "string"
#'            )
#'            ),
#'          "maxAvailHeight"= 0,
#'          "maxAvailWidth"= 0,
#'          "minorModelAgeDisclosure"= "http://example.com",
#'          "modelAges"= list(
#'            0
#'            ),
#'          "modelReleaseDocuments"= list(
#'            "string"
#'            ),
#'          "modelReleaseStatus"= list(
#'            "cvId"= "http://example.com",
#'            "cvTermName"= "string",
#'            "cvTermId"= "http://example.com",
#'            "cvTermRefinedAbout"= "http://example.com"
#'          ),
#'          "organisationInImageCodes"= list(
#'            "string"
#'            ),
#'          "organisationInImageNames"= list(
#'            "string"
#'            ),
#'          "personInImageNames"= list(
#'            "string"
#'            ),
#'          "personsShown"= list(
#'            list(
#'              "name"= "string",
#'              "description"= "string",
#'              "identifiers"= list(
#'                "http://example.com"
#'                ),
#'              "characteristics"= list(
#'                list(
#'                  "cvId"= "http://example.com",
#'                  "cvTermName"= "string",
#'                  "cvTermId"= "http://example.com",
#'                  "cvTermRefinedAbout"= "http://example.com"
#'                )
#'                )
#'            )
#'            ),
#'          "productsShown"= list(
#'            list(
#'              "description"= "string",
#'              "gtin"= "string",
#'              "name"= "string"
#'            )
#'            ),
#'          "propertyReleaseDocuments"= list(
#'            "string"
#'            ),
#'          "propertyReleaseStatus"= list(
#'            "cvId"= "http://example.com",
#'            "cvTermName"= "string",
#'            "cvTermId"= "http://example.com",
#'            "cvTermRefinedAbout"= "http://example.com"
#'          ),
#'          "provinceStatePhoto"= "string",
#'          "registryEntries"= list(
#'            list(
#'              "role"= "http://example.com",
#'              "assetIdentifier"= "string",
#'              "registryIdentifier"= "http://example.com"
#'            )
#'            ),
#'          "sceneCodes"= list(
#'            "string"
#'            ),
#'          "source"= "string",
#'          "subjectCodes"= list(
#'            "string"
#'            ),
#'          "sublocationName"= "string",
#'          "supplier"= list(
#'            list(
#'              "name"= "string",
#'              "identifiers"= list(
#'                "http://example.com"
#'                )
#'            )
#'            ),
#'          "title"= "string",
#'          "usageTerms"= "string",
#'          "webstatementRights"= "http://example.com"
#'        )
#'      ),
#'      "license"= list(
#'        list(
#'          "name"= "string",
#'          "uri"= "string"
#'        )
#'        ),
#'      "album"= list(
#'        list(
#'          "name"= "string",
#'          "description"= "string",
#'          "owner"= "string",
#'          "uri"= "string"
#'        )
#'        ),
#'      "files"= list(
#'        list(
#'          "file_uri"= "string",
#'          "format"= "string",
#'          "note"= "string",
#'          "show"= true
#'        )
#'        )
#'    )
#'  )
#'
#'
#'
#'  image_add (
#'   idno="image-idno",
#'   published = 1,
#'   overwrite = "yes",
#'   metadata = metadata,
#'   thumbnail ="path-to-thumbnail-file.jpg"
#' )
#'
#'
#'
#'
#' @export
image_add <- function(idno,
                         metadata,
                         repositoryid=NULL,
                         access_policy=NULL,
                         data_remote_url=NULL,
                         published=0,
                         overwrite="no",
                         thumbnail=NULL,
                         api_key=NULL,
                         api_base_url=NULL
){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  files=list()

  #change file_name value to file basename
  if(!is.null(metadata$image_description$files)){
    files=metadata$image_description$files
    for(i in seq_along(metadata$image_description$files)){
      metadata$image_description$files[[i]]$file_uri=basename(metadata$image_description$files[[i]]$file_uri)
    }
  }

  result = create(type= "image",
                  idno= idno,
                  repositoryid= repositoryid,
                  access_policy= access_policy,
                  data_remote_url= data_remote_url,
                  published= published,
                  overwrite= overwrite,
                  metadata= metadata,
                  thumbnail=thumbnail
  )

  if(result$status_code==200){
    if(!is.null(files)){
      print ("images found, processing....")
      for(f in files){
        if(file.exists(f$file_uri)){
          resource_result=external_resources_add(idno=idno,
                                                 dctype="Document [doc/oth]", #"Photo [pic]",
                                                 title=basename(f$file_uri),
                                                 file_path=f$file_uri,
                                                 overwrite="yes"
          )
          result$resources[[basename(f$file_uri)]]=resource_result
        } else{
          warning(paste("File not found:",f$file_uri))
        }
      }
    }
  }

  return (result)
}
