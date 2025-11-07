# NADAR Project AI Assistant Instructions

## Project Overview
NADAR is an R client library for interacting with the NADA (National Data Archive) API. It provides a comprehensive set of functions for both public (read-only) and administrative operations on NADA catalogs.

## Core Architecture

### Key Components
- **API Client Foundation**: Core HTTP utilities in `R/utils.R` and configuration management in `R/api_functions.R`
- **Global State**: Package-wide configuration managed through `pkg.globals` environment (`R/globals.R`)
- **Function Categories**:
  - Public (read-only) functions for catalog access
  - Administrative functions for content management
  - Utility functions for common operations

### Configuration Flow
1. Setup requires API URL and key:
```r
nada_set_api("http://catalog-url/api/", "your-api-key")
# or individually:
nada_set_api_url("http://catalog-url/api/")
nada_set_api_key("your-api-key")
```

## Common Patterns

### API Function Structure
1. Every API function follows this pattern:
```r
nada_*_function <- function(..., api_key=NULL, api_base_url=NULL) {
  if(is.null(api_key)) {
    api_key = nada_get_api_key()
  }
  # API call implementation
}
```

### HTTP Requests
- Use wrapper functions (`nada_http_*`) for consistency
- Always include `X-API-KEY` header
- Handle response status codes uniformly:
  ```r
  if(httpResponse$status_code!=200) {
    warning(content(httpResponse, "text"))
  }
  ```

### Resource Handling
- File operations use absolute paths
- Remote resources require URL validation via `nada_is_valid_url()`
- Use `upload_file()` for file uploads

## Development Workflows

### Function Documentation
- Use roxygen2 format for all exported functions
- Include examples in documentation
- Parameters should indicate if they are required
- Document return values and possible errors

### Error Handling
- Validate required parameters before API calls
- Use `stop()` for critical errors with clear messages
- Use `warning()` for non-critical issues

### Testing
- Test files should match function file names
- Mock API responses in tests
- Test both success and error cases

## Integration Points

### Key Dependencies
- HTTP: httr, curl
- JSON: jsonlite
- Data Processing: readr, haven
- String Manipulation: stringi, stringr

### External Systems
- NADA Catalog API endpoints
- File system for resource uploads
- PDF processing via pdftools
- XML handling through XML package

## Project Conventions

### Function Naming
- Public functions: `nada_*`
- Admin functions: `nada_admin_*`
- Internal functions: `.*` (prefixed with dot)

### Return Values
Standard response structure:
```r
list(
  "status_code" = httpResponse$status_code,
  "response" = fromJSON(content(httpResponse,"text"))
)
```

### Parameter Patterns
- Optional parameters default to NULL
- Use lists for complex options
- Boolean flags default to FALSE
- File paths should be absolute

## Tips
1. Always check API configuration before debugging API calls
2. Use verbose mode for debugging: `nada_set_api_verbose(TRUE)`
3. Check global state when functions behave unexpectedly
4. Validate URL and API key format before making requests