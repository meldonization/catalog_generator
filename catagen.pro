PRO RA_PARSE_COLUMN_VALUES, line, types, p_vals, rec_count, $
    locs, lengths, missingValue, num_fields
    
  COMPILE_OPT HIDDEN, STRICTARR
  
  ON_IOERROR, column_cast_failed
  
  nf1 = num_fields - 1
  FOR i=0, nf1 DO BEGIN
  
    IF (types[i] NE 0) THEN BEGIN ; (0 == skip field.)
      token = (i EQ nf1) ? STRTRIM(STRMID(line, locs[i]),2) : $
        STRTRIM(STRMID(line, locs[i], lengths[i]),2)
      ; Assign substring to the variable. This will automatically do
      ; any necessary type conversions.
      (*p_vals[i])[rec_count] = (STRLEN(token) NE 0) ? token : $
        (types[i] EQ 7) ? token : missingValue
      CONTINUE
      column_cast_failed:
      MESSAGE, /reset
      (*p_vals[i])[rec_count] = missingValue
    ENDIF
  ENDFOR  ; i
END


; -----------------------------------------------------------------------------
;
;  Purpose:  Parse out values from a CSV file, taking into account escaped
;  quote characters, and commas within quoted fields.
;
;  For example, given a line:
;     ,"""abc,def""",ghi,"jkl,""mno"",pqr",,,
;
;  We need to split this into 7 fields:
;    <null>  <"abc,def">  <ghi>  <jkl,"mno",pqr>  <null>  <null>  <null>
;
FUNCTION RA_SPLIT_CSV, lineIn

  COMPILE_OPT idl2, hidden
  
  ; First replace escaped quote characters, which look like "",
  ; with a harmless upper-byte character.
  line = STRJOIN(STRTOK(lineIn, '""', /REGEX, /EXTRACT), STRING(254b))
  
  quoteLoc = STRTOK(line, '"', /PRESERVE_NULL)
  n = N_ELEMENTS(quoteLoc)
  
  ; Since we preserved nulls above, there should always be an odd
  ; number of quote-separated tokens... If the quotes are paired correctly.
  IF (n GT 2 && (n MOD 2) EQ 1) THEN BEGIN
  
    commaLoc = STRTOK(line, ',', /PRESERVE_NULL, LENGTH=length)
    line = STRMID(line, commaLoc, length)
    
    ; Add a backslash escape in front of all commas that lie within quotes.
    FOR i=1,n-1,2 DO BEGIN
      indx = WHERE(commaLoc GT quoteLoc[i] AND commaLoc LT quoteLoc[i+1], nInside)
      IF (nInside GT 0) THEN line[indx-1] += '\'
    ENDFOR
    
    line = STRJOIN(line, ',')
    
  ENDIF
  
  ; Remove quotes that are just surrounding fields.
  line = STRJOIN(STRTOK(line, '"', /EXTRACT))
  
  ; Put back the quotes that were in the actual field.
  line = STRJOIN(STRTOK(line, STRING(254b), /EXTRACT), '"')
  
  RETURN, STRTOK(line, ',', ESCAPE='\', /EXTRACT, /PRESERVE_NULL)
END


; -----------------------------------------------------------------------------
;
;  Purpose:  Parse out values from a line of text which are separated by
;        a given delimiter.
;
PRO RA_PARSE_DELIM_VALUES, line, types, p_vals, rec_count, $
    delimit, missing_value, whitespace_delimited, CSV=csvIn
    
  COMPILE_OPT HIDDEN, STRICTARR
  
  csv = KEYWORD_SET(csvIn)
  
  IF (csv) THEN BEGIN
    toks = RA_SPLIT_CSV(line)
  ENDIF ELSE BEGIN
    ; Remove whitespace from beginning and end.
    toks = whitespace_delimited ? STRTOK(line, /EXTRACT) : $
      STRTRIM(STRTOK(line, delimit, /EXTRACT, /PRESERVE_NULL), 2)
  ENDELSE
  
  length = STRLEN(toks)
  
  ON_IOERROR, delim_cast_failed
  
  n_types = N_ELEMENTS(types)
  n_toks = N_ELEMENTS(toks)
  nMin1 = (n_types < n_toks) - 1
  
  ; Loop up to the end of the tokens or the number of fields, whichever
  ; is smaller. Empty fields will be filled in after this loop.
  FOR i=0,nMin1 DO BEGIN
  
    IF (types[i] NE 0) THEN BEGIN ; (0 == skip field.)
      ; Assign the substring to the variable. This will automatically do
      ; any necessary type conversions.
      (*p_vals[i])[rec_count] = (length[i] NE 0) ? toks[i] : $
        ((types[i] EQ 7) ? toks[i] : missing_value)
      ; If successful conversion, then continue the loop.
      CONTINUE
      delim_cast_failed:
      ; If failed conversion, suppress the error and fill with missing.
      MESSAGE, /reset
      (*p_vals[i])[rec_count] = missing_value
    ENDIF
  ENDFOR
  
  
  ; Need to fill in extra fields with missing.
  IF (n_toks LT n_types) THEN BEGIN
    FOR i=n_toks, n_types-1 DO BEGIN
      IF (types[i] GT 0) THEN $
        (*p_vals[i])[rec_count] = missing_value
    ENDFOR
  ENDIF
  
END         ; ra_parse_delim_values

; -----------------------------------------------------------------------------
;
;  Purpose:  Read in the next n lines of text (skipping blank lines and
;        commented lines signified by template.commentSymbol at start;
;        also throw away comment portions of regular lines).
;
FUNCTION RA_GET_NEXT_RECORD, template, unit, lines
  ;
  COMPILE_OPT hidden, strictarr
  
  ON_IOERROR, end_of_file
  line = ''
  count = 0
  
  ;  Checking for comments...
  ;
  IF (TEMPLATE.commentSymbol NE '') THEN BEGIN
    WHILE (count LT N_ELEMENTS(lines)) DO BEGIN
      READF, unit, line
      pos = STRPOS(line, TEMPLATE.commentSymbol, 0)
      IF (STRTRIM(line,2) NE '' AND pos[0] NE 0) THEN BEGIN
        lines[count] = (pos[0] EQ -1) ? line : STRMID(line,0,pos[0])
        count = count + 1
      ENDIF
    ENDWHILE
    
  ;  NOT checking for comments...
  ;
  ENDIF ELSE BEGIN
    WHILE (count LT N_ELEMENTS(lines)) DO BEGIN
      READF, unit, line
      IF (STRLEN(STRTRIM(line,2)) NE 0) THEN BEGIN
        lines[count] = line
        count = count + 1
      ENDIF
    ENDWHILE
  ENDELSE
  
  RETURN, 0 ; success
  
  end_of_file:
  ; If read failed, suppress message and return EOF.
  MESSAGE, /reset
  RETURN, 1  ; failure, EOF
  
END   ; ra_get_next_record

; -----------------------------------------------------------------------------
;
;  Purpose:  Given a template structure, open an ASCII file and parse out the
;        numerical and string values based upon the parameters of the
;        given template.
;
;       (a) white space separates fields lined up in columns
;       (b) a delimiter character separates fields
;
;     Note:  When skipping to the start of the data, blank lines ARE included
;        as lines to skip, but once you get to the data, subsequent blank
;        lines (as well as comment lines) are ignored.
;
;        Function returns an array of pointers to the data read;
;        if no data read, 0 is returned.
;
FUNCTION RA_READ_FROM_TEMPL, $
    name, $     ; IN: name of ASCII file to read
    TEMPLATE, $     ; IN: ASCII file template
    start_record, $ ; IN: first record to read
    records_to_read, $  ; IN: number of records to read
    doVerbose, $    ; IN: 1B = print runtime messages
    num_fields_read, $  ; OUT: number of fields successfully read
    fieldNames, $       ; OUT: associated name of each field read
    rec_count, $    ; OUT: number of records successfully read
    num_blocks, $    ; OUT: number of blocks of data
    header=header, $   ; OUT: (opt) header read
    CSV=csvIn
    
  COMPILE_OPT hidden, strictarr
  
  ;  Set default numbers.
  ;
  num_fields_read = 0
  num_blocks = 0L
  
  ;  Catch errors.
  CATCH, error_status
  IF (error_status NE 0) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, /INFO, 'Unexpected Error: ' + !ERROR_STATE.msg
    MESSAGE, /RESET
    rec_count = 0l
    RETURN, 0
  ENDIF
  
  ;  Open the file.
  ;
  OPENR, unit, name, /get_lun
  
  ; Look for a byte-order marker and skip it.
  IF ((FSTAT(unit)).size GT 3) THEN BEGIN
    x = BYTARR(3)
    READU, unit, x
    POINT_LUN, unit, 0
    CASE (1) OF
      ARRAY_EQUAL(x, [239b, 187b, 191b]): POINT_LUN, unit, 3  ; UTF-8
      ARRAY_EQUAL(x[[0,1]], [254b, 255b]): POINT_LUN, unit, 2   ; UTF-16 (big endian)
      ARRAY_EQUAL(x[[0,1]], [255b, 254b]): POINT_LUN, unit, 2   ; UTF-16 (little endian)
      ELSE:
    ENDCASE
  ENDIF
  
  ;  Set various parameters.
  ;
  blk_size = 1000  ; each block holds this many records
  blk_count = 500  ; number of blocks we can have
  blk_grow  = 500
  current_block = 0L
  lines_per_record = N_ELEMENTS(TEMPLATE.fieldCount)
  num_fields = TEMPLATE.fieldCount
  tot_num_fields = TOTAL(TEMPLATE.fieldCount)
  types = TEMPLATE.fieldTypes
  locs = TEMPLATE.fieldLocations
  ; The length of the last field depends upon the line length,
  ; so here just make it some arbitrary number.
  fieldLengths = (N_ELEMENTS(locs) GT 1) ? [locs[1:*],0] - locs : 0
  
  ;  Define an array of variables for each field.
  ;
  p_vals = PTRARR(tot_num_fields, blk_count)
  FOR i=0, tot_num_fields-1 DO $
    IF (types[i] GT 0) THEN $
    p_vals[i, current_block] = PTR_NEW(MAKE_ARRAY(blk_size, type=types[i]))
    
  ;  Read the header and skip to the start of the data.
  ;
  dataStart = TEMPLATE.dataStart
  IF (dataStart GT 0) THEN BEGIN
    IF (doVerbose) THEN $
      PRINT, 'Reading header of ' + STRTRIM(STRING(dataStart), 2) + $
      ' lines ...', format='(A/)'
    header = STRARR(dataStart)
    READF, unit, header
  ENDIF ELSE $
    header = ''
    
  ;  Skip to the start of requested data.
  ;
  lines = STRARR(lines_per_record)
  IF ((doVerbose) AND (start_record GT 0)) THEN $
    PRINT, 'Skipping ' + STRTRIM(STRING(start_record), 2) + ' records ...', $
    format='(A/)'
  FOR i = 0L, start_record-1 DO $
    end_reached = RA_GET_NEXT_RECORD(template, unit, lines)
    
  IF TEMPLATE.delimiter EQ 32b THEN BEGIN
    delim_str = STRING([32b, 9b])
    whitespace_delimited = 1b
  END ELSE BEGIN
    delim_str = STRING(TEMPLATE.delimiter)
    whitespace_delimited = 0b
  ENDELSE
  
  nRecord1 = (records_to_read GT 0) ? records_to_read-1L : 2147483647L
  FOR rec_count = 0L, nRecord1 DO BEGIN
  
    ;  Read a record.
    ;
    end_reached = RA_GET_NEXT_RECORD(template, unit, lines)
    IF (end_reached) THEN BREAK ; out of the for loop
    
    IF (doVerbose) THEN $
      PRINT, 'Processing sequential record ' + $
      STRTRIM(STRING(rec_count+1), 2) + ' ...'
      
    anchor = 0
    rc = rec_count-current_block*blk_size
    
    ;  For each line in the record...
    ;
    FOR i=0,lines_per_record-1 DO BEGIN
    
      IF (TEMPLATE.delimiter EQ 0B) THEN BEGIN
        ; nice columned data...
        RA_PARSE_COLUMN_VALUES, lines[i], $
          types[ anchor:anchor+num_fields[i]-1], $
          p_vals[anchor:anchor+num_fields[i]-1, current_block], $
          rc, $
          locs[ anchor:anchor+num_fields[i]-1], $
          fieldLengths[anchor:anchor+num_fields[i]-1], $
          TEMPLATE.MISSINGVALUE, $
          num_fields[i]
      ENDIF ELSE BEGIN
        ; data separated by a delimiter...
        RA_PARSE_DELIM_VALUES, lines[i], $
          types[ anchor:anchor+num_fields[i]-1], $
          p_vals[anchor:anchor+num_fields[i]-1, current_block], $
          rc, $
          delim_str, $
          TEMPLATE.MISSINGVALUE, $
          whitespace_delimited, CSV=csvIn
      ENDELSE
      anchor = anchor + num_fields[i]
    ENDFOR  ; i
    
    ; If block is now full,
    ; Allocate and point to a new block
    ;
    IF ((rec_count+1) MOD blk_size EQ 0) THEN BEGIN
      current_block = current_block + 1
      IF (current_block EQ blk_count) THEN BEGIN
        p_vals = [[p_vals], [PTRARR(tot_num_fields, blk_grow)]]
        blk_count = blk_count + blk_grow
      ENDIF
      FOR i=0, tot_num_fields-1 DO IF (types[i] GT 0) THEN $
        p_vals[i, current_block] = $
        PTR_NEW(MAKE_ARRAY(blk_size, type=types[i]))
    ENDIF  ; new block
  ENDFOR  ; read
  
  ; ------------------------------------
  FREE_LUN, unit
  
  IF (doVerbose) THEN $
    PRINT, 'Total records read:  ' + STRTRIM(STRING(rec_count), 2), $
    format='(A/)'
    
  ; No records were read.
  IF (rec_count EQ 0) THEN BEGIN
    PTR_FREE, p_vals
    RETURN, 0
  ENDIF
  
  ;  If records were read ...
  ;  Set the output arrays to exactly the correct size.
  ;
  FOR i=0, tot_num_fields-1 DO BEGIN
    IF (p_vals[i,current_block] NE PTR_NEW()) THEN BEGIN
      IF (rec_count GT current_block*blk_size) THEN BEGIN
        *p_vals[i,current_block] = $
          (*p_vals[i,current_block])[0:rec_count-current_block*blk_size-1]
      ENDIF ELSE BEGIN ; block is allocated, but empty
        PTR_FREE, p_vals[i,current_block]
      ENDELSE
    ENDIF
  ENDFOR
  IF (rec_count GT current_block*blk_size) THEN BEGIN
    num_blocks = current_block + 1
  ENDIF ELSE BEGIN
    num_blocks = current_block
  ENDELSE
  
  ;  Check the groups array and arrange the output pointers into
  ;  (potentially) groups of 2-D arrays.
  ;
  groups = TEMPLATE.fieldGroups
  
  ;  Don't include any groups which are skipped fields.
  ;
  ptr = WHERE(types EQ 0, numSkip)
  FOR i=0, numSkip-1 DO groups[ptr[i]] = MAX(groups) + 1
  
  ;  Concatenate 1-D arrays into multi arrays based upon groupings.
  ;
  uptr = UNIQ(groups, SORT(groups))
  IF (N_ELEMENTS(uptr) LT N_ELEMENTS(groups)) THEN BEGIN
    FOR i=0, N_ELEMENTS(uptr)-1 DO BEGIN
      FOR b=0, num_blocks-1 DO BEGIN
        lptr = WHERE(groups EQ groups[uptr[i]], lcount)
        IF (lcount GT 1) THEN BEGIN
          p_new = p_vals[lptr[0],b]
          FOR j=1,lcount-1 DO BEGIN
            *p_new = [[TEMPORARY(*p_new)],[TEMPORARY(*p_vals[lptr[j],b])]]
            PTR_FREE, p_vals[lptr[j],b]
            p_vals[lptr[j],b] = PTR_NEW()
          ENDFOR
          *p_new = TRANSPOSE(TEMPORARY(*p_new))
        ENDIF
      ENDFOR
    ENDFOR
  ENDIF
  
  ;  Return the pointers that contain data, if any.
  ;  and the associated fieldNames for these pointers
  ;
  ptr = WHERE(p_vals[*,0] NE PTR_NEW(), num_fields_read)
  
  IF (num_fields_read GT 0) THEN BEGIN ; data successfully read
    fieldNames = TEMPLATE.fieldNames[ptr]
    RETURN, p_vals[ptr,*]
  ENDIF ELSE BEGIN                           ; no data read
    rec_count = 0l
    RETURN, 0
  ENDELSE
  
END         ; ra_read_from_templ

; -----------------------------------------------------------------------------
;
;  Purpose:  Return 1B if the template is valid, else 0B.
;
FUNCTION RA_VALID_TEMPLATE, $
    template, $       ; IN: template to check
    MESSAGE           ; OUT: error message if the template is not valid
    
  COMPILE_OPT hidden, strictarr
  
  message = ''
  
  ;  Make sure it's a structure.
  ;
  sz = SIZE(template)
  IF (sz[sz[0]+1] NE 8) THEN BEGIN
    message = 'Template is not a structure.'
    RETURN, 0B
  ENDIF
  
  ;  Get tag names and make sure version field is present.
  ;
  tagNamesFound = TAG_NAMES(template)
  void = WHERE(tagNamesFound EQ 'VERSION', count)
  IF (count NE 1) THEN BEGIN
    message = 'Version field missing from template.'
    RETURN, 0B
  ENDIF
  
  ;  Do checking based on version.
  ;
  CASE (TEMPLATE.version) OF
  
    1.0: BEGIN
    
      ;  Set the names of the required tags (version alread checked).
      ;
      tagNamesRequired = STRUPCASE([ $
        'dataStart', 'delimiter', 'missingValue', 'commentSymbol', $
        'fieldCount', 'fieldTypes', 'fieldNames', 'fieldLocations', $
        'fieldGroups'])
        
      ;  Check that all of the required tags are present.
      ;
      FOR seqTag = 0, N_ELEMENTS(tagNamesRequired)-1 DO BEGIN
        tag = tagNamesRequired[seqTag]
        void = WHERE(tagNamesFound EQ tag, count)
        IF (count NE 1) THEN BEGIN
          message = tag + ' field missing from template.'
          RETURN, 0B
        ENDIF
      ENDFOR
      
    END
    
    ELSE: BEGIN
      message = 'The only recognized template version is 1.0 (float).'
      RETURN, 0B
    END
  ENDCASE
  
  ; Check for valid structure tag names before we try to
  ; read the file.
  FOR i=0,N_ELEMENTS(TEMPLATE.fieldNames)-1 DO BEGIN
    IF (IDL_VALIDNAME(TEMPLATE.fieldNames[i], /CONVERT_SPACES) EQ '') THEN BEGIN
      message = 'Illegal field name: ' + TEMPLATE.fieldNames[i]
      RETURN, 0b  ; failure
    ENDIF
  ENDFOR
  
  
  ;  Return that the template is valid.
  ;
  RETURN, 1B
  
END         ; ra_valid_template

; -----------------------------------------------------------------------------
;
;  Purpose:  Convert to string and remove extra white space.
;
FUNCTION RA_STRINGIT, value

  COMPILE_OPT hidden, strictarr
  
  result = STRTRIM( STRCOMPRESS( STRING(value) ), 2 )
  
  num = N_ELEMENTS(result)
  
  IF (num LE 1) THEN RETURN, result
  
  ;  If two or more values, concatenate them.
  ;
  delim = ' '
  ret = result[0]
  FOR i = 1, num-1 DO $
    ret = ret + delim + result[i]
    
  RETURN, ret
  
END         ; ra_stringit

; -----------------------------------------------------------------------------
;
;  Purpose:  Guess at the number of columns in an ASCII file.
;
FUNCTION RA_GUESS_COLUMNS, fname, dataStart, commentSymbol, delimiter

  COMPILE_OPT hidden, strictarr
  
  ON_ERROR, 2 ; Return to caller on error.
  
  CATCH, err_stat
  IF err_stat NE 0 THEN BEGIN
    CATCH, /cancel
    IF N_ELEMENTS(lun) GT 0 THEN $
      IF (lun NE 0) THEN FREE_LUN, lun
    MESSAGE, !error_state.msg
  ENDIF
  
  GET_LUN, lun
  OPENR, lun, fname
  
  IF dataStart GT 0 THEN BEGIN
    header = STRARR(dataStart)
    READF, lun, header
  END
  
  line = ''
  end_reached = RA_GET_NEXT_RECORD({commentSymbol: commentSymbol}, $
    lun, line)
    
  IF end_reached THEN $
    MESSAGE, 'No columns found.'
    
  IF delimiter EQ ' ' THEN $
    positions = strtok(line) $
  ELSE $
    positions = strtok(line, delimiter, /preserve_null)
    
  CLOSE, lun
  FREE_LUN, lun
  RETURN, N_ELEMENTS(positions)
END

; -----------------------------------------------------------------------------
;
;  Purpose: Check that the input filename is a string, exists, and appears
;           to be ASCII.
;
FUNCTION RA_CHECK_FILE, fname

  COMPILE_OPT hidden, strictarr
  
  IF (SIZE(fname, /TYPE) NE 7) THEN $
    RETURN, -1 ; filename isn't a string
    
  info = FILE_INFO(fname)
  IF (~info.exists) THEN $
    RETURN, -2
  IF (~info.read) THEN $
    RETURN, -3
    
  success = QUERY_ASCII(fname)
  RETURN, success ? 1 : -4
  
END


; ------------------------------------------------------------------------
; Use a recursive approach to constructing the result structure.
; We used to do this by just concatanating each field onto the end of the
; existing structure, but this is extremely inefficient, as the entire
; structure must be copied over and over.
; The recursive approach builds up separate structures, and then
; concatanates them together.
;
FUNCTION READ_ASCII_CREATE_STRUCT, fieldnames, xData

  COMPILE_OPT idl2, hidden
  
  nFields = N_ELEMENTS(fieldnames)
  
  CASE (nFields) OF
  
    0: RETURN, 0   ; this shouldn't happen
    
    ; Create a structure with a single field.
    1: RETURN, CREATE_STRUCT(fieldnames[0], TEMPORARY(*xData[0]))
    
    ; Create a structure with two fields.
    2: RETURN, CREATE_STRUCT( $
      fieldnames[0], TEMPORARY(*xData[0]), $
      fieldnames[1], TEMPORARY(*xData[1]))
      
    ; Create a structure with three fields.
    3: RETURN, CREATE_STRUCT( $
      fieldnames[0], TEMPORARY(*xData[0]), $
      fieldnames[1], TEMPORARY(*xData[1]), $
      fieldnames[2], TEMPORARY(*xData[2]))
      
    ELSE: BEGIN
    
      ; Divide into 4 equal-sized sets of pointers, and concatanate
      ; the structures. Four is somewhat arbitrary, but on average,
      ; this will then require only 1/4 of the memory, as compared
      ; to simply concatanating the fields onto the end.
      d = nFields/4
      ; Recursive call.
      RETURN, CREATE_STRUCT( $
        READ_ASCII_CREATE_STRUCT(fieldnames[0:d-1], xData[0:d-1]), $
        READ_ASCII_CREATE_STRUCT(fieldnames[d:2*d-1], xData[d:2*d-1]), $
        READ_ASCII_CREATE_STRUCT(fieldnames[2*d:3*d-1], xData[2*d:3*d-1]), $
        READ_ASCII_CREATE_STRUCT(fieldnames[3*d:*], xData[3*d:*]))
    END
    
  ENDCASE
  
END


; -----------------------------------------------------------------------------
;
;  Purpose:  The main routine.
;

FUNCTION READ_ASCII, $
    file, $             ; IN:
    RECORD_START=recordStart, $     ; IN: (opt)
    NUM_RECORDS=numRecords, $       ; IN: (opt)
    TEMPLATE=template, $        ; IN: (opt)
    DATA_START=dataStart, $     ; IN: (opt)
    DELIMITER=delimiter, $      ; IN: (opt)
    MISSING_VALUE=missingValue, $   ; IN: (opt)
    COMMENT_SYMBOL=commentSymbol, $ ; IN: (opt)
    ;    FIELDS=fields, $           ; IN: (opt)    [not implemented]
    VERBOSE=verbose, $          ; IN: (opt)
    HEADER=header, $            ; OUT: (opt)
    COUNT=count, $             ; OUT: (opt)
    CSV=csvIn, $
    CANCEL=cancel, $
    WBOPEN=wbopen, $
    _EXTRA=_extra
    
  COMPILE_OPT strictarr, hidden
  ;xxx
  ;later add a VERSION kw ?
  
  ;  Set to return to caller on error.
  ;
  ON_ERROR, 2
  
  
  ;  Set some defaults.
  ;
  count = 0
  currentVersion = 1.0
  doVerbose = KEYWORD_SET(verbose)
  
  ;  If no file specified, use DIALOG_PICKFILE
  ;
  IF (N_ELEMENTS(file) EQ 0) THEN BEGIN
    file = DIALOG_PICKFILE(/MUST_EXIST)
    IF (file EQ '') THEN RETURN, 0
  ENDIF
  
  ; check that the file is readable and appears to be ASCII
  ;
  ret = RA_CHECK_FILE(file)
  CASE ret OF
    -1: MESSAGE, 'File name must be a string.'
    -2: MESSAGE, 'File "' + file + '" not found.'
    -3: MESSAGE, 'Error Reading from file "' + file + '"'
    -4: MESSAGE, 'File "' + file + '" is not a valid ASCII file.'
    ELSE:
  ENDCASE
  
  IF ((N_ELEMENTS(template) EQ 0) && KEYWORD_SET(wbopen)) THEN BEGIN
    template = ASCII_TEMPLATE(file, CANCEL=cancel, WBOPEN=wbopen, _EXTRA=_extra)
    IF ((N_ELEMENTS(cancel) NE 0) && (cancel NE 0)) THEN BEGIN
      ;; return gracefully
      RETURN, 0
    ENDIF
  ENDIF
  
  ;  Set which records to read.
  ;
  IF (N_ELEMENTS(recordStart) NE 0) THEN recordStartUse = recordStart $
  ELSE recordStartUse = 0
  IF (N_ELEMENTS(numRecords) NE 0)  THEN numRecordsUse = numRecords $
  ELSE numRecordsUse = 0
  IF (N_ELEMENTS(template) GT 0) THEN BEGIN
    IF (NOT RA_VALID_TEMPLATE(template, message)) THEN $
      MESSAGE, message
      
    versionUse      = TEMPLATE.version
    dataStartUse    = TEMPLATE.dataStart
    delimiterUse    = STRING(TEMPLATE.delimiter)
    missingValueUse = TEMPLATE.missingValue
    commentSymbolUse    = TEMPLATE.commentSymbol
  ENDIF ELSE BEGIN
    versionUse      = currentVersion
    dataStartUse    = 0L
    delimiterUse    = ' '
    missingValueUse = !values.f_nan
    commentSymbolUse    = ''
  ENDELSE
  
  IF N_ELEMENTS(dataStart) NE 0 THEN $
    dataStartUse = dataStart
  IF N_ELEMENTS(delimiter) NE 0 THEN $
    delimiterUse = delimiter
  IF N_ELEMENTS(missingValue) NE 0 THEN $
    missingValueUse = missingValue
  IF N_ELEMENTS(commentSymbol) NE 0 THEN $
    commentSymbolUse = commentSymbol
    
  IF N_ELEMENTS(dataStartUse) GT 1 THEN $
    MESSAGE, 'DATA_START must be a scalar.'
  IF N_ELEMENTS(BYTE(delimiterUse)) GT 1 THEN $ ; Might want to remove this
    MESSAGE, 'DELIMITER must be one character.' ; restriction in the future.
    
  ;  (For back version compatibility, we do not throw an error
  ;  here if n_elements(missingValueUse) gt 1).
  ;
    
  ;  The READ_ASCII that shipped with IDL 5.2.1 returns 0 when
  ;  an array of comment symbols is specified.  Set the error_state
  ;  in this case, but, for back version compatibility, continue
  ;  and reproduce the "return 0" behavior here.
  ;
  IF N_ELEMENTS(commentSymbolUse) GT 1 THEN BEGIN
    MESSAGE, $
      'Multiple comment symbols are not currently supported.', $
      /noprint, $
      /continue
    RETURN, 0
  ENDIF
  
  IF N_ELEMENTS(template) GT 0 THEN BEGIN
    fieldCountUse   = TEMPLATE.fieldCount
    fieldTypesUse   = TEMPLATE.fieldTypes
    fieldNamesUse   = TEMPLATE.fieldNames
    fieldLocationsUse   = TEMPLATE.fieldLocations
    fieldGroupsUse  = TEMPLATE.fieldGroups
  ENDIF ELSE BEGIN
    fieldCountUse = N_ELEMENTS(fieldTypes) $
      > N_ELEMENTS(fieldNames) $
      > N_ELEMENTS(fieldLocations) $
      > N_ELEMENTS(fieldGroups)
    IF fieldCountUse LE 0 THEN $
      fieldCountUse = RA_GUESS_COLUMNS( $
      file, $
      dataStartUse, $
      commentSymbolUse, $
      delimiterUse $
      )
      
    fieldTypesUse   = REPLICATE(4L, fieldCountUse)
    digits_str = STRTRIM(STRING(STRLEN(STRTRIM(STRING(fieldCountUse),2))),2)
    fstr = '(i' + digits_str + '.' + digits_str + ')'
    fieldNamesUse   = 'field' + STRING(INDGEN(fieldCountUse)+1, format=fstr)
    fieldLocationsUse   = LONARR(fieldCountUse)
    fieldGroupsUse  = INTARR(fieldCountUse)
  ENDELSE
  
  IF N_ELEMENTS(fieldTypes) NE 0 THEN $
    fieldTypesUse = fieldTypes
  IF N_ELEMENTS(fieldNames) NE 0 THEN $
    fieldNamesUse = fieldNames
  IF N_ELEMENTS(fieldLocations) NE 0 THEN $
    fieldLocationsUse = fieldLocations
  IF N_ELEMENTS(fieldGroups) NE 0 THEN $
    fieldGroupsUse = fieldGroups
    
  ;  Error check the field data.
  ;
  lengths = [ $
    N_ELEMENTS(fieldTypesUse), $
    N_ELEMENTS(fieldNamesUse), $
    N_ELEMENTS(fieldLocationsUse), $
    N_ELEMENTS(fieldGroupsUse) $
    ]
  IF (TOTAL(ABS(lengths - SHIFT(lengths, 1))) NE 0) THEN $
    MESSAGE, 'Field data (types/names/locs/groups) not the same length.'
    
  ;  Set the template to use.
  ;
  templateUse = { $
    version: versionUse, $
    dataStart: dataStartUse, $
    delimiter: BYTE(delimiterUse), $
    missingValue: missingValueUse, $
    commentSymbol: commentSymbolUse, $
    fieldCount: fieldCountUse, $
    fieldTypes: fieldTypesUse, $
    fieldNames: fieldNamesUse, $
    fieldLocations: fieldLocationsUse, $
    fieldGroups: fieldGroupsUse $
    }
    
  ;  Print verbose information.
  ;
  IF (doVerbose) THEN BEGIN
    PRINT, 'Using the following file attributes ...', FORMAT='(/A)'
    PRINT, '        Data Start:  ' + STRTRIM(STRING(dataStartUse), 2)
    PRINT, '         Delimiter:  ' + $
      STRTRIM(STRING(FIX(BYTE(delimiterUse))), 2) + 'B'
    PRINT, '     Missing Value:  ' + STRTRIM(STRING(missingValueUse), 2)
    PRINT, '    Comment Symbol:  ' + commentSymbolUse
    PRINT, '      Field Counts:  ' + RA_STRINGIT(fieldCountUse)
    PRINT, '      Field Types :  ' + RA_STRINGIT(fieldTypesUse)
    PRINT, '      Field Names :  ' + RA_STRINGIT(fieldNamesUse)
    PRINT, '      Field Locs  :  ' + RA_STRINGIT(fieldLocationsUse)
    PRINT, '      Field Groups:  ' + RA_STRINGIT(fieldGroupsUse)
    PRINT, '  Template Version:  ' + STRTRIM(STRING(versionUse), 2)
    PRINT
  ENDIF
  
  ;  Try to read the file.
  ;
  pData = RA_READ_FROM_TEMPL(file, templateUse, recordStartUse, $
    numRecordsUse, doVerbose, numFieldsRead, FieldNames, count, num_blocks, $
    header=header, CSV=csvIn)
    
  ;  Return zero if no records read.
  ;
  IF (count EQ 0) THEN $
    RETURN, 0
    
  ; Concatenate the blocks into fields.
  ;
  xData = PTRARR(numFieldsRead)
  
  ; If an error occurs, free up our temp pointers.
  CATCH, err
  IF (err NE 0) THEN BEGIN
    CATCH, /CANCEL
    PTR_FREE, xData, pData
    MESSAGE, /REISSUE_LAST
    RETURN, 0
  ENDIF
  
  FOR f=0L, numFieldsRead-1 DO BEGIN
    type = SIZE(*pData[F,0], /TYPE)
    dims = SIZE(*pData[F,0], /DIMENSIONS)
    n_dims = SIZE(*pData[F,0], /N_DIMENSIONS)
    IF (count EQ 1) THEN BEGIN
      ; if the file contains a single record, it is really
      ; two-dimensional: n fields x 1 record
      n_dims = 2
      dims = LONARR(2)
      dims[0] = SIZE(*pData[F,0],/N_ELEMENTS)
    ENDIF
    dims[n_dims-1] = count
    xData[f] = PTR_NEW(MAKE_ARRAY(DIMENSION=dims, TYPE=type))
    start=0L
    
    FOR b=0L, num_blocks-1 DO BEGIN
      sz = SIZE(*pData[F,b],/N_ELEMENTS)
      stop = start + sz - 1
      (*xData[F])[start:stop] = *pData[F,b]
      PTR_FREE, pData[F,b]
      start = stop + 1
    ENDFOR
  ENDFOR
  
  ;  Put the fields into a structure.
  data = READ_ASCII_CREATE_STRUCT(fieldnames, xData)
  
  ;  Clean up the heap data.
  ;
  FOR f = 0L, numFieldsRead-1 DO $
    PTR_FREE, xData[F]
    
  ;  Print verbose information.
  ;
  IF (doVerbose) THEN BEGIN
    PRINT, 'Output data ...'
    HELP, data, /STRUCTURES
    PRINT
  ENDIF
  
  ;  Return the structure.
  ;
  RETURN, data
  
END         ; read_ascii









; read_ascii
; read_ascii
; read_ascii
; read_ascii
; read_ascii
; read_ascii
; read_ascii
; read_ascii
; read_ascii











FUNCTION READ_SEX_CATA, catafn
  ;usage: a=read_sex_cata(catafn)
  ;+++++++++++++++++++++++++++++++++++++++
  ;purpose: read sextractor output catalog
  ;version 1.0 by mzy, 2013-8-13
  ;+++++++++++++++++++++++++++++++++++++++
  ;+catafn: filename of the catalog.
  ;+++++++++++++++++++++++++++++++++++++++
  ;
  ;catafn='/home/mengzy/Data/weihai1m/extra/20130918/cleanfits/solved_out/catalog/HATP37_130918_V+N_121910_00192.cat'

  delimiter=' ' ;field delimiter
  cs='#'        ;comment symbol
  hl=0ULL       ;head lines
  
  strtmp=''
  OPENR,lun,catafn,/get_lun
  FOR i=0ULL,FILE_LINES(catafn)-1ULL,1ULL DO BEGIN
    READF,lun,strtmp
    IF(STRMATCH(STRTRIM(strtmp,2),STRCOMPRESS(cs+'*',/remove_all)) NE 1) THEN BREAK
    hl+=1ULL
  ;PRINT,strtmp
  ENDFOR
  FREE_LUN,lun
  
  data=READ_ASCII(catafn,DELIMITER=delimiter,COMMENT_SYMBOL=cs,DATA_START=hl,$
    HEADER=header,COUNT=count,VERBOSE=0)
    
  RETURN, data
  
END

FUNCTION MK_MASTER_CAT, dr

  @var.inc
  
  fmt='(E18.10,1X,E18.10,1X,F8.4,1X,F10.7)'  ;FORMAT might be changable.
  
  
  IF(master_def EQ 1) THEN BEGIN
    OPENR,lun,masterfile,/get_lun
    mrt=DBLARR(4,FILE_LINES(masterfile))
    READF,lun,mrt,format=fmt
    FREE_LUN,lun
    GOTO,JUMP1
  ENDIF
  
  ;when master file not exists then create one below!
  
  catfile=file_search(cataloc + '*.cat')
  nfile=N_ELEMENTS(catfile)
  
  dr1=dr/3.6D3 ;convert dr to spherical rad unit
  
  FOR i=0LL,nfile-1,1 DO BEGIN
  
    lastr=strpos(catfile[i],'/',/reverse_search)
    PRINT,STRMID(catfile[i],lastr+1,strlen(catfile[i])-lastr)
    
    IF(i EQ 0) THEN BEGIN
      mastertmp=READ_SEX_CATA(catfile[i])
      master=mastertmp.FIELD01[*,*]
      master=master[*,WHERE(master[sexcol[6],*] EQ 0D0)]
      nhit=INTARR(N_ELEMENTS(master[0,*]))
      nhit+=1
    ENDIF
    
    IF(i NE 0) THEN BEGIN
      compare=READ_SEX_CATA(catfile[i])
      ;nl=N_ELEMENTS(compare.FIELD01[0,*])
      ;nc=N_ELEMENTS(compare.FIELD01[*,0])
      
      FOR j=0,N_ELEMENTS(compare.FIELD01[0,*])-1,1 DO BEGIN
        index=WHERE(SQRT((master[sexcol[0],*]-compare.FIELD01[sexcol[0],j])^2D0+$
          (master[sexcol[1],*]-compare.FIELD01[sexcol[1],j])^2D0) LE dr1)
        IF(index EQ [-1] AND compare.FIELD01[sexcol[6],j] EQ 0D0) THEN BEGIN
          nlm=N_ELEMENTS(master[0,*])
          ncm=N_ELEMENTS(master[*,0])
          mastertmp=DBLARR(ncm,nlm+1)
          mastertmp[*,0:nlm-1]=master
          mastertmp[*,nlm]=compare.FIELD01[*,j]
          master=mastertmp
          nhit=[nhit,1]
        ENDIF
        IF(index NE [-1] AND compare.FIELD01[sexcol[6],j] EQ 0D0) THEN BEGIN
          nlm=N_ELEMENTS(master[0,*])
          ncm=N_ELEMENTS(master[*,0])
          IF(N_ELEMENTS(index) EQ 1) THEN BEGIN
            nhit[index]+=1
            master[sexcol[0],index]=(master[sexcol[0],index]+compare.FIELD01[sexcol[0],j])/2D0
            master[sexcol[1],index]=(master[sexcol[1],index]+compare.FIELD01[sexcol[1],j])/2D0
            master[sexcol[4],index]=(master[sexcol[4],index]+compare.FIELD01[sexcol[4],j])/2D0
          ENDIF ELSE BEGIN
            dmin=MIN(SQRT((master[sexcol[0],index]-compare.FIELD01[sexcol[0],j])^2D0+$
              (master[sexcol[1],index]-compare.FIELD01[sexcol[1],j])^2D0))
            indextmp=index[WHERE(SQRT((master[sexcol[0],index]-compare.FIELD01[sexcol[0],j])^2D0+$
              (master[sexcol[1],index]-compare.FIELD01[sexcol[1],j])^2D0) EQ dmin)]
            nhit[indextmp]+=1
            master[sexcol[0],indextmp]=(master[sexcol[0],indextmp]+compare.FIELD01[sexcol[0],j])/2D0
            master[sexcol[1],indextmp]=(master[sexcol[1],indextmp]+compare.FIELD01[sexcol[1],j])/2D0
            master[sexcol[4],indextmp]=(master[sexcol[4],indextmp]+compare.FIELD01[sexcol[4],j])/2D0
          ENDELSE
        ENDIF
      ENDFOR
    ENDIF
  ENDFOR
  
  index=WHERE(nhit GT 1)
  master=master[*,index]
  nhit=nhit[index]
  indexsrt=SORT(master[sexcol[4],*])
  master=master[*,indexsrt]
  nhit=nhit[indexsrt]
  
  OPENW,lun,masterfile,/get_lun
  FOR m=0LL,N_ELEMENTS(index)-1,1 DO BEGIN
    PRINTF,lun,master[sexcol[0],m],master[sexcol[1],m],master[sexcol[4],m],nhit[m]*1D0/nfile,format=fmt
  ENDFOR
  FREE_LUN,lun
  
  mrt=DBLARR(4,N_ELEMENTS(index))
  mrt[0,*]=REFORM(master[sexcol[0],*])
  mrt[1,*]=REFORM(master[sexcol[1],*])
  mrt[2,*]=REFORM(master[sexcol[4],*])
  mrt[3,*]=nhit[*]*1D0/nfile
  
  JUMP1:
  RETURN, mrt
  
END


FUNCTION JULDAY, MONTH, DAY, YEAR, Hour, Minute, Second

  COMPILE_OPT idl2
  
  ON_ERROR, 2		; Return to caller if errors
  
  ; Gregorian Calander was adopted on Oct. 15, 1582
  ; skipping from Oct. 4, 1582 to Oct. 15, 1582
  GREG = 2299171L  ; incorrect Julian day for Oct. 25, 1582
  
  ; Process the input, if all are missing, use todays date.
  NP = N_PARAMS()
  IF (np EQ 0) THEN RETURN, SYSTIME(/JULIAN)
  IF (np LT 3) THEN MESSAGE, 'Incorrect number of arguments.'
  
  ; Find the dimensions of the Result:
  ;  1. Find all of the input arguments that are arrays (ignore scalars)
  ;  2. Out of the arrays, find the smallest number of elements
  ;  3. Find the dimensions of the smallest array
  
  ; Step 1: find all array arguments
  nDims = [SIZE(month,/N_DIMENSIONS), SIZE(day,/N_DIMENSIONS), $
    SIZE(year,/N_DIMENSIONS), SIZE(hour,/N_DIMENSIONS), $
    SIZE(minute,/N_DIMENSIONS), SIZE(second,/N_DIMENSIONS)]
  arrays = WHERE(nDims GE 1)
  
  nJulian = 1L    ; assume everything is a scalar
  IF (arrays[0] GE 0) THEN BEGIN
    ; Step 2: find the smallest number of elements
    nElement = [N_ELEMENTS(month), N_ELEMENTS(day), $
      N_ELEMENTS(year), N_ELEMENTS(hour), $
      N_ELEMENTS(minute), N_ELEMENTS(second)]
    nJulian = MIN(nElement[arrays], whichVar)
    ; step 3: find dimensions of the smallest array
    CASE arrays[whichVar] OF
      0: julianDims = SIZE(month,/DIMENSIONS)
      1: julianDims = SIZE(day,/DIMENSIONS)
      2: julianDims = SIZE(year,/DIMENSIONS)
      3: julianDims = SIZE(hour,/DIMENSIONS)
      4: julianDims = SIZE(minute,/DIMENSIONS)
      5: julianDims = SIZE(second,/DIMENSIONS)
    ENDCASE
  ENDIF
  
  d_Second = 0d  ; defaults
  d_Minute = 0d
  d_Hour = 0d
  ; convert all Arguments to appropriate array size & type
  SWITCH np OF  ; use switch so we fall thru all arguments...
    6: d_Second = (SIZE(second,/N_DIMENSIONS) GT 0) ? $
      second[0:nJulian-1] : second
    5: d_Minute = (SIZE(minute,/N_DIMENSIONS) GT 0) ? $
      minute[0:nJulian-1] : minute
    4: d_Hour = (SIZE(hour,/N_DIMENSIONS) GT 0) ? $
      hour[0:nJulian-1] : hour
    3: BEGIN ; convert m,d,y to type LONG
      L_MONTH = (SIZE(month,/N_DIMENSIONS) GT 0) ? $
        LONG(month[0:nJulian-1]) : LONG(month)
      L_DAY = (SIZE(day,/N_DIMENSIONS) GT 0) ? $
        LONG(day[0:nJulian-1]) : LONG(day)
      L_YEAR = (SIZE(year,/N_DIMENSIONS) GT 0) ? $
        LONG(year[0:nJulian-1]) : LONG(year)
    END
  ENDSWITCH
  
  
  min_calendar = -4716
  max_calendar = 5000000
  minn = MIN(l_year, MAX=maxx)
  IF (minn LT min_calendar) OR (maxx GT max_calendar) THEN MESSAGE, $
    'Value of Julian date is out of allowed range.'
  IF (MAX(L_YEAR EQ 0) NE 0) THEN MESSAGE, $
    'There is no year zero in the civil calendar.'
    
    
  bc = (L_YEAR LT 0)
  L_YEAR = TEMPORARY(L_YEAR) + TEMPORARY(bc)
  inJanFeb = (L_MONTH LE 2)
  JY = L_YEAR - inJanFeb
  JM = L_MONTH + (1b + 12b*TEMPORARY(inJanFeb))
  
  
  JUL = FLOOR(365.25d * JY) + FLOOR(30.6001d*TEMPORARY(JM)) + L_DAY + 1720995L
  
  
  ; Test whether to change to Gregorian Calandar.
  IF (MIN(JUL) GE GREG) THEN BEGIN  ; change all dates
    JA = LONG(0.01d * TEMPORARY(JY))
    JUL = TEMPORARY(JUL) + 2L - JA + LONG(0.25d * JA)
  ENDIF ELSE BEGIN
    gregChange = WHERE(JUL GE GREG, ngreg)
    IF (ngreg GT 0) THEN BEGIN
      JA = LONG(0.01d * JY[gregChange])
      JUL[gregChange] = JUL[gregChange] + 2L - JA + LONG(0.25d * JA)
    ENDIF
  ENDELSE
  
  
  ; hour,minute,second?
  IF (np GT 3) THEN BEGIN ; yes, compute the fractional Julian date
    ; Add a small offset so we get the hours, minutes, & seconds back correctly
    ; if we convert the Julian dates back. This offset is proportional to the
    ; Julian date, so small dates (a long, long time ago) will be "more" accurate.
    eps = (MACHAR(/DOUBLE)).eps
    eps = eps*ABS(jul) > eps
    ; For Hours, divide by 24, then subtract 0.5, in case we have unsigned ints.
    jul = TEMPORARY(JUL) + ( (TEMPORARY(d_Hour)/24d - 0.5d) + $
      TEMPORARY(d_Minute)/1440d + TEMPORARY(d_Second)/86400d + eps )
  ENDIF
  
  ; check to see if we need to reform vector to array of correct dimensions
  IF (N_ELEMENTS(julianDims) GT 1) THEN $
    JUL = REFORM(TEMPORARY(JUL), julianDims)
    
  RETURN, jul
  
END




FUNCTION LC_SUBTRACT, ra, dec, dr

  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;
  ;ra:  R.A. of the star in deg.
  ;dec: Dec. of the star in deg.
  ;dr:  within radius regarded to be matched in arcsec.
  ;     e.g. 3E-4 deg (or 2 arcsec) ^_^
  ;
  ;AUTHOR: Zeyang Meng, 2013-8-13
  ;
  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++

  @var.inc
  
  catfile=file_search(cataloc + '*.cat')
  nfile=N_ELEMENTS(catfile)
  lcdata=DBLARR(3,nfile)
  
  dr1=dr/3.6D3
  j=0LL
  strtmp=''

  FOR i=0LL,nfile-1,1 DO BEGIN
    filename=catfile[i]
    
    ;jday=julday(MM,DD,YYYY,HH,MM,SS)
    ;-----------HAT-P-37-----------
   
    OPENR,lun,filename,/get_lun
    READF,lun,strtmp
    FREE_LUN,lun
    
    
    jday=JULDAY(FIX(STRMID(strtmp,dateloc[1],2)),FIX(STRMID(strtmp,dateloc[2],2)),$
      FIX(STRMID(strtmp,dateloc[0],4)),FIX(STRMID(strtmp,dateloc[3],2)),$
      FIX(STRMID(strtmp,dateloc[4],2)),DOUBLE(STRMID(strtmp,dateloc[5],7)))
    ;-----------HAT-P-37-----------
      
    data=READ_SEX_CATA(filename)
    ;######FIELD01 might be changable!!######
    index=WHERE(SQRT((data.FIELD01[sexcol[0],*]-ra)^2D0+(data.FIELD01[sexcol[1],*]-dec)^2D0) LE dr1)
    IF(index NE [-1] AND N_ELEMENTS(index) LT 2) THEN BEGIN
      lcdata[0,j]=jday
      lcdata[1,j]=data.FIELD01[sexcol[4],index[0]]
      lcdata[2,j]=data.FIELD01[sexcol[5],index[0]]
      j++
    ENDIF
  ENDFOR
  
  lc=lcdata[*,WHERE(lcdata[0,*] NE 0D0)]
  RETURN, lc
  
;####################################################
;20150407   WASP-46    318.736900     -55.871700
;####################################################
  
END


FUNCTION LC_BIN, master, dr

  @var.inc
  fmt='(D40.30,1X,F9.5,1X,F8.4,1X,I010)'    ;FORMAT might be changable.
  
  IF(lcbin_def EQ 1) THEN BEGIN
    OPENR,lun,lcbinfile,/get_lun
    lcbin=DBLARR(4,FILE_LINES(lcbinfile))
    READF,lun,lcbin,format=fmt
    FREE_LUN,lun
    GOTO,JUMP1
  ENDIF
  
  
  numlc=N_ELEMENTS(master[0,*])
  catfile=findfile(cataloc + '*.cat')
  nfile=N_ELEMENTS(catfile)
  j=1ULL
  
  FOR i=0LL,numlc-1,1 DO BEGIN
    IF(i MOD 17LL EQ 0) THEN PRINT,'Subtracting lc: '+ STRCOMPRESS(STRING(i+1),/REMOVE_ALL) + $
      ' / ' + STRCOMPRESS(STRING(N_ELEMENTS(master[0,*])),/REMOVE_ALL)
    lc=LC_SUBTRACT(master[0,i],master[1,i],dr)
    IF(i EQ 0) THEN lcbin=DBLARR(4,nfile*1ULL*numlc)
    lcbin[0:2,i*1ULL*nfile:i*1ULL*nfile+N_ELEMENTS(lc[0,*])-1]=lc
    lcbin[3,i*1ULL*nfile:i*1ULL*nfile+N_ELEMENTS(lc[0,*])-1]=j++
  ENDFOR
  
  lcbin=lcbin[*,WHERE(lcbin[0,*] NE 0)]
  
  OPENW,lun,lcbinfile,/get_lun
  FOR m=0LL,N_ELEMENTS(lcbin[0,*])-1LL,1 DO BEGIN
    PRINTF,lun,lcbin[0,m],lcbin[1,m],lcbin[2,m],lcbin[3,m],format=fmt
  ENDFOR
  FREE_LUN,lun
  
  JUMP1:
  RETURN, lcbin
  
END



PRO CATAGEN

  @var.inc
  
  !PATH=!PATH + ':' + tfloc
  RESOLVE_ROUTINE,'lc_bin',/IS_FUNCTION,/COMPILE_FULL_FILE
  RESOLVE_ROUTINE,'mk_master_cat',/IS_FUNCTION,/COMPILE_FULL_FILE
  RESOLVE_ROUTINE,'lc_subtract',/IS_FUNCTION,/COMPILE_FULL_FILE
  
  
  PRINT,'DEALING WITH FOLDER: ' + cataloc
  
  FILE_DELETE,lcloc,/QUIET,/RECURSIVE
  IF(NOT FILE_TEST(lcloc,/DIRECTORY)) THEN FILE_MKDIR, lcloc
  
  master_all=MK_MASTER_CAT(dr)
  index=WHERE(master_all[3,*] GT mfreq)
  master=master_all[*,index]
  indmagsrt=SORT(master[2,*])
  PRINT,STRCOMPRESS(STRING(N_ELEMENTS(index)),/REMOVE_ALL)+' FRAMES ARE USED TO GENERATE TREND.'
  
  
  lcbin=LC_BIN(master,dr)
  
  FOR k=0LL,N_ELEMENTS(master[0,*])-1,1 DO BEGIN
    i=indmagsrt[k]
    IF(WHERE(lcbin[3,*] EQ i+1) EQ [-1]) THEN CONTINUE
    lctmp=lcbin[0:2,WHERE(lcbin[3,*] EQ i+1)]
    filename=STRCOMPRESS(lcloc+STRING(k,format='(I05)')+'_'+STRING(master[2,i],format='(F09.5)')+'_'+$
      STRING(master[0,i],format='(F012.8)')+'_'+STRING(master[1,i],format='(F012.8)')+'.slc',/REMOVE_ALL)
;    lcnorm=lctmp[1,*]
;    FOR m=0LL,N_ELEMENTS(lctmp[0,*])-1,1 DO BEGIN
;      lcnorm[m]/=trend[1,WHERE(trend[0,*] EQ lctmp[0,m])]
;    ENDFOR
    OPENW,lun,filename,/get_lun
    FOR j=0LL,N_ELEMENTS(lctmp[0,*])-1,1 DO BEGIN
      PRINTF,lun,lctmp[0,j],lctmp[1,j],lctmp[2,j],format='(D40.30,1X,F9.5,1X,F8.4)'
    ENDFOR
    FREE_LUN,lun
  ENDFOR
  
  PRINT,'+++(^____DONE!____^)+++'
  
END


