% page file parameters
-define(PAGE_SIZE, 256). % page size in bytes
% logger parameters
-define(LOG_LEVEL, debug). % log level
% process storage keys
-define(PAGE_FILE, pfs_file). % IO device for accessing page file
-define(PAGE_HEADER_SIZE, page_header_size). % page hedaer size in bytes.
-define(PAGE_DATA_SIZE, page_data_size). % page data size in bytes
-define(INTERNAL_QUEUE, pfs_internal_queue). % internal request queue
% messages
-define(GENERIC_ERROR, "Unknown error").
