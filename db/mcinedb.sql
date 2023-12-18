--
-- PostgreSQL database dump
--

-- Dumped from database version 16.1
-- Dumped by pg_dump version 16.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a";


ALTER SCHEMA "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a" OWNER TO postgres;

--
-- Name: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA "68caa687-b8d8-46f0-b5a9-79bfc36eb456";


ALTER SCHEMA "68caa687-b8d8-46f0-b5a9-79bfc36eb456" OWNER TO postgres;

--
-- Name: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA "8afa6a12-cf3d-42bd-98b5-4961debd9bf3";


ALTER SCHEMA "8afa6a12-cf3d-42bd-98b5-4961debd9bf3" OWNER TO postgres;

--
-- Name: defaultschema; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA defaultschema;


ALTER SCHEMA defaultschema OWNER TO postgres;

--
-- Name: public; Type: SCHEMA; Schema: -; Owner: postgres
--

-- *not* creating schema, since initdb creates it


ALTER SCHEMA public OWNER TO postgres;

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS '';


--
-- Name: adminpack; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS adminpack WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION adminpack; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION adminpack IS 'administrative functions for PostgreSQL';


--
-- Name: obj_type; Type: TYPE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE TYPE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3".obj_type AS ENUM (
    'TABLE',
    'VIEW',
    'COLUMN',
    'SEQUENCE',
    'FUNCTION',
    'SCHEMA',
    'DATABASE'
);


ALTER TYPE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3".obj_type OWNER TO postgres;

--
-- Name: perm_type; Type: TYPE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE TYPE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3".perm_type AS ENUM (
    'SELECT',
    'INSERT',
    'UPDATE',
    'DELETE',
    'TRUNCATE',
    'REFERENCES',
    'TRIGGER',
    'USAGE',
    'CREATE',
    'EXECUTE',
    'CONNECT',
    'TEMPORARY'
);


ALTER TYPE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3".perm_type OWNER TO postgres;

--
-- Name: obj_type; Type: TYPE; Schema: defaultschema; Owner: postgres
--

CREATE TYPE defaultschema.obj_type AS ENUM (
    'TABLE',
    'VIEW',
    'COLUMN',
    'SEQUENCE',
    'FUNCTION',
    'SCHEMA',
    'DATABASE'
);


ALTER TYPE defaultschema.obj_type OWNER TO postgres;

--
-- Name: perm_type; Type: TYPE; Schema: defaultschema; Owner: postgres
--

CREATE TYPE defaultschema.perm_type AS ENUM (
    'SELECT',
    'INSERT',
    'UPDATE',
    'DELETE',
    'TRUNCATE',
    'REFERENCES',
    'TRIGGER',
    'USAGE',
    'CREATE',
    'EXECUTE',
    'CONNECT',
    'TEMPORARY'
);


ALTER TYPE defaultschema.perm_type OWNER TO postgres;

--
-- Name: cloneparms; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.cloneparms AS ENUM (
    'DATA',
    'NODATA',
    'DDLONLY',
    'NOOWNER',
    'NOACL',
    'VERBOSE',
    'DEBUG',
    'FILECOPY'
);


ALTER TYPE public.cloneparms OWNER TO postgres;

--
-- Name: clone_schema(text, text, public.cloneparms[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.clone_schema(source_schema text, dest_schema text, VARIADIC arr public.cloneparms[] DEFAULT '{}'::public.cloneparms[]) RETURNS void
    LANGUAGE plpgsql
    AS $_$

--  This function will clone all sequences, tables, data, views & functions from any existing schema to a new one
-- SAMPLE CALL:
-- SELECT clone_schema('sample', 'sample_clone2');

DECLARE
  src_oid          oid;
  tbl_oid          oid;
  func_oid         oid;
  object           text;
  buffer           text;
  buffer2          text;
  buffer3          text;
  srctbl           text;
  aname            text;
  default_         text;
  column_          text;
  qry              text;
  ix_old_name      text;
  ix_new_name      text;
  relpersist       text;
  udt_name         text;
  udt_schema       text;
  bRelispart       bool;
  bChild           bool;
  relknd           text;
  data_type        text;
  ocomment         text;
  adef             text;
  dest_qry         text;
  v_def            text;
  part_range       text;
  src_path_old     text;
  src_path_new     text;
  aclstr           text;
  -- issue#80 initialize arrays properly
  tblarray         text[] := '{}';
  tblarray2        text[] := '{}';
  tblarray3        text[] := '{}';
  tblelement       text;
  grantor          text;
  grantee          text;
  privs            text;
  seqval           bigint;
  sq_last_value    bigint;
  sq_max_value     bigint;
  sq_start_value   bigint;
  sq_increment_by  bigint;
  sq_min_value     bigint;
  sq_cache_value   bigint;
  sq_is_called     boolean := True;
  sq_is_cycled     boolean;
  is_prokind       boolean;
  abool            boolean;
  sq_data_type     text;
  sq_cycled        char(10);
  sq_owned         text;
  sq_version        text;
  sq_server_version text;
  sq_server_version_num integer;
  bWindows         boolean;
  arec             RECORD;
  cnt              integer;
  cnt1             integer;
  cnt2             integer;
  cnt3             integer;
  cnt4             integer;
  pos              integer;
  tblscopied       integer := 0;
  l_child          integer;
  action           text := 'N/A';
  tblname          text;
  v_ret            text;
  v_diag1          text;
  v_diag2          text;
  v_diag3          text;
  v_diag4          text;
  v_diag5          text;
  v_diag6          text;
  v_dummy          text;
  spath            text;
  spath_tmp        text;
  -- issue#86 fix
  isGenerated      text;
  
  -- issue#91 fix
  tblowner         text;
  func_owner       text;
  func_name        text;
  func_args        text;
  func_argno       integer;
  view_owner       text; 

  -- issue#92    
  calleruser       text;
  
  -- issue#94
  bData            boolean := False;
  bDDLOnly         boolean := False;
  bVerbose         boolean := False;
  bDebug           boolean := False;
  bNoACL           boolean := False;
  bNoOwner         boolean := False;
  arglen           integer;
  vargs            text;
  avarg            public.cloneparms;

  -- issue#98
  mvarray          text[] := '{}';  
  mvscopied        integer := 0;
  
  -- issue#99 tablespaces
  tblspace         text;
  
  -- issue#101
  bFileCopy        boolean := False;
  
  t                timestamptz := clock_timestamp();
  r                timestamptz;
  s                timestamptz;
  lastsql          text := '';
  v_version        text := '1.19  September 07, 2023';

BEGIN
  -- Make sure NOTICE are shown
  SET client_min_messages = 'notice';
  RAISE NOTICE 'clone_schema version %', v_version;

  IF 'DEBUG'   = ANY ($3) THEN bDebug = True; END IF;
  IF 'VERBOSE' = ANY ($3) THEN bVerbose = True; END IF;
  
  -- IF bVerbose THEN RAISE NOTICE 'START: %',clock_timestamp() - t; END IF;
  
  arglen := array_length($3, 1);
  IF arglen IS NULL THEN
    -- nothing to do, so defaults are assumed
    NULL;
  ELSE
    -- loop thru args
    -- IF 'NO_TRIGGERS' = ANY ($3)
    -- select array_to_string($3, ',', '***') INTO vargs;
    IF bDebug THEN RAISE NOTICE 'DEBUG: arguments=%', $3; END IF;
    FOREACH avarg IN ARRAY $3 LOOP
      IF bDebug THEN RAISE NOTICE 'DEBUG: arg=%', avarg; END IF;
      IF avarg = 'DATA' THEN
        bData = True;
      ELSEIF avarg = 'NODATA' THEN
        -- already set to that by default
        bData = False;
      ELSEIF avarg = 'DDLONLY' THEN
        bDDLOnly = True;
      ELSEIF avarg = 'NOACL' THEN
        bNoACL = True;
      ELSEIF avarg = 'NOOWNER' THEN
        bNoOwner = True;        
      -- issue#101 fix
      ELSEIF avarg = 'FILECOPY' THEN
        bFileCopy = True;
      END IF;
    END LOOP;
    IF bData and bDDLOnly THEN 
      RAISE WARNING 'You can only specify DDLONLY or DATA, but not both.';
      RETURN;
    END IF;
  END IF;  
  
  -- Get server version info to handle certain things differently based on the version.
  SELECT setting INTO sq_server_version
  FROM pg_settings
  WHERE name = 'server_version';
  SELECT version() INTO sq_version;
  
  IF POSITION('compiled by Visual C++' IN sq_version) > 0 THEN
      bWindows = True;
      RAISE NOTICE 'Windows: %', sq_version;
  ELSE
      bWindows = False;
      RAISE NOTICE 'Linux: %', sq_version;
  END IF;
  SELECT setting INTO sq_server_version_num
  FROM pg_settings
  WHERE name = 'server_version_num';

  IF sq_server_version_num < 100000 THEN
    IF sq_server_version_num > 90600 THEN
        RAISE WARNING 'Server Version:%  Number:%  PG Versions older than v10 are not supported.  Will try however for PG 9.6...', sq_server_version, sq_server_version_num;
    ELSE
        RAISE WARNING 'Server Version:%  Number:%  PG Versions older than v10 are not supported.  You need to be at minimum version 9.6 to at least try', sq_server_version, sq_server_version_num;
        RETURN;
    END IF;
  END IF;

  -- Check that source_schema exists
  SELECT oid INTO src_oid
  FROM pg_namespace
  WHERE nspname = quote_ident(source_schema);

  IF NOT FOUND
    THEN
    RAISE NOTICE ' source schema % does not exist!', source_schema;
    RETURN ;
  END IF;

  -- Check for case-sensitive target schemas and reject them for now.
  SELECT lower(dest_schema) = dest_schema INTO abool;
  IF not abool THEN
      RAISE NOTICE 'Case-sensitive target schemas are not supported at this time.';
      RETURN;
  END IF;

  -- Check that dest_schema does not yet exist
  PERFORM nspname
  FROM pg_namespace
  WHERE nspname = quote_ident(dest_schema);

  IF FOUND
    THEN
    RAISE NOTICE ' dest schema % already exists!', dest_schema;
    RETURN ;
  END IF;
  IF bDDLOnly and bData THEN
    RAISE WARNING 'You cannot specify to clone data and generate ddl at the same time.';
    RETURN ;
  END IF;

  -- Issue#92
  SELECT current_user into calleruser;
  
  -- Set the search_path to source schema. Before exiting set it back to what it was before.
  -- In order to avoid issues with the special schema name "$user" that may be
  -- returned unquoted by some applications, we ensure it remains double quoted.
  -- MJV FIX: #47
  SELECT setting INTO v_dummy FROM pg_settings WHERE name='search_path';
  IF bDebug THEN RAISE NOTICE 'DEBUG: search_path=%', v_dummy; END IF;
  
  SELECT REPLACE(REPLACE(setting, '"$user"', '$user'), '$user', '"$user"') INTO src_path_old
  FROM pg_settings WHERE name = 'search_path';

  IF bDebug THEN RAISE NOTICE 'DEBUG: src_path_old=%', src_path_old; END IF;

  EXECUTE 'SET search_path = ' || quote_ident(source_schema) ;
  SELECT setting INTO src_path_new FROM pg_settings WHERE name='search_path';
  IF bDebug THEN RAISE NOTICE 'DEBUG: new search_path=%', src_path_new; END IF;

  -- Validate required types exist.  If not, create them.
  SELECT a.objtypecnt, b.permtypecnt INTO cnt, cnt2
  FROM (
      SELECT count(*) AS objtypecnt
      FROM pg_catalog.pg_type t
      LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
  WHERE (t.typrelid = 0
      OR (
          SELECT c.relkind = 'c'
          FROM pg_catalog.pg_class c
          WHERE c.oid = t.typrelid))
      AND NOT EXISTS (
          SELECT 1
          FROM pg_catalog.pg_type el
          WHERE el.oid = t.typelem
              AND el.typarray = t.oid)
          AND n.nspname <> 'pg_catalog'
          AND n.nspname <> 'information_schema'
          AND pg_catalog.pg_type_is_visible(t.oid)
          AND pg_catalog.format_type(t.oid, NULL) = 'obj_type') a, (
          SELECT count(*) AS permtypecnt
          FROM pg_catalog.pg_type t
          LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
      WHERE (t.typrelid = 0
          OR (
              SELECT c.relkind = 'c'
              FROM pg_catalog.pg_class c
              WHERE c.oid = t.typrelid))
          AND NOT EXISTS (
              SELECT 1
              FROM pg_catalog.pg_type el
              WHERE el.oid = t.typelem
                  AND el.typarray = t.oid)
              AND n.nspname <> 'pg_catalog'
              AND n.nspname <> 'information_schema'
              AND pg_catalog.pg_type_is_visible(t.oid)
              AND pg_catalog.format_type(t.oid, NULL) = 'perm_type') b;

  IF cnt = 0 THEN
    CREATE TYPE obj_type AS ENUM ('TABLE','VIEW','COLUMN','SEQUENCE','FUNCTION','SCHEMA','DATABASE');
  END IF;
  IF cnt2 = 0 THEN
    CREATE TYPE perm_type AS ENUM ('SELECT','INSERT','UPDATE','DELETE','TRUNCATE','REFERENCES','TRIGGER','USAGE','CREATE','EXECUTE','CONNECT','TEMPORARY');
  END IF;

  -- Issue#95
  SELECT pg_catalog.pg_get_userbyid(nspowner) INTO buffer FROM pg_namespace WHERE nspname = quote_ident(source_schema);

  IF bDDLOnly THEN
    RAISE NOTICE ' Only generating DDL, not actually creating anything...';
    -- issue#95
    IF bNoOwner THEN
        RAISE INFO 'CREATE SCHEMA %;', quote_ident(dest_schema);    
    ELSE
        RAISE INFO 'CREATE SCHEMA % AUTHORIZATION %;', quote_ident(dest_schema), buffer;    
    END IF;
    RAISE NOTICE 'SET search_path=%;', quote_ident(dest_schema);
  ELSE
    -- issue#95
    IF bNoOwner THEN
        EXECUTE 'CREATE SCHEMA ' || quote_ident(dest_schema) ;
    ELSE
        EXECUTE 'CREATE SCHEMA ' || quote_ident(dest_schema) || ' AUTHORIZATION ' || buffer;
    END IF;
  END IF;

  -- Do system table validations for subsequent system table queries
  -- Issue#65 Fix
  SELECT count(*) into cnt
  FROM pg_attribute
  WHERE  attrelid = 'pg_proc'::regclass AND attname = 'prokind';

  IF cnt = 0 THEN
      is_prokind = False;
  ELSE
      is_prokind = True;
  END IF;

  -- MV: Create Collations
  action := 'Collations';
  cnt := 0;
  -- Issue#96 Handle differently based on PG Versions (PG15 rely on colliculocale, not collcolocate)
  -- perhaps use this logic instead: COALESCE(c.collcollate, c.colliculocale) AS lc_collate, COALESCE(c.collctype, c.colliculocale) AS lc_type  
  IF sq_server_version_num > 150000 THEN 
    FOR arec IN
      SELECT n.nspname AS schemaname, a.rolname AS ownername, c.collname, c.collprovider, c.collcollate AS locale, 
             'CREATE COLLATION ' || quote_ident(dest_schema) || '."' || c.collname || '" (provider = ' || 
             CASE WHEN c.collprovider = 'i' THEN 'icu' WHEN c.collprovider = 'c' THEN 'libc' ELSE '' END || 
             ', locale = ''' || c.colliculocale || ''');' AS COLL_DDL
      FROM pg_collation c
          JOIN pg_namespace n ON (c.collnamespace = n.oid)
          JOIN pg_roles a ON (c.collowner = a.oid)
      WHERE n.nspname = quote_ident(source_schema)
      ORDER BY c.collname
    LOOP
      BEGIN
        cnt := cnt + 1;
        IF bDDLOnly THEN
          RAISE INFO '%', arec.coll_ddl;
        ELSE
          EXECUTE arec.coll_ddl;
        END IF;
      END;
    END LOOP;
  ELSIF sq_server_version_num > 100000 THEN   
    FOR arec IN
      SELECT n.nspname AS schemaname, a.rolname AS ownername, c.collname, c.collprovider, c.collcollate AS locale, 
             'CREATE COLLATION ' || quote_ident(dest_schema) || '."' || c.collname || '" (provider = ' || 
             CASE WHEN c.collprovider = 'i' THEN 'icu' WHEN c.collprovider = 'c' THEN 'libc' ELSE '' END || 
             ', locale = ''' || c.collcollate || ''');' AS COLL_DDL
      FROM pg_collation c
          JOIN pg_namespace n ON (c.collnamespace = n.oid)
          JOIN pg_roles a ON (c.collowner = a.oid)
      WHERE n.nspname = quote_ident(source_schema)
      ORDER BY c.collname
    LOOP
      BEGIN
        cnt := cnt + 1;
        IF bDDLOnly THEN
          RAISE INFO '%', arec.coll_ddl;
        ELSE
          EXECUTE arec.coll_ddl;
        END IF;
      END;
    END LOOP;
  ELSE
    -- handle 9.6 that is missing some columns in pg_collation
    FOR arec IN
      SELECT n.nspname AS schemaname, a.rolname AS ownername, c.collname, c.collcollate AS locale, 
             'CREATE COLLATION ' || quote_ident(dest_schema) || '."' || c.collname || '" (provider = ' || 
             ', locale = ''' || c.collcollate || ''');' AS COLL_DDL
      FROM pg_collation c
          JOIN pg_namespace n ON (c.collnamespace = n.oid)
          JOIN pg_roles a ON (c.collowner = a.oid)
      WHERE n.nspname = quote_ident(source_schema)
      ORDER BY c.collname
    LOOP
      BEGIN
        cnt := cnt + 1;
        IF bDDLOnly THEN
          RAISE INFO '%', arec.coll_ddl;
        ELSE
          EXECUTE arec.coll_ddl;
        END IF;
      END;
    END LOOP;
  END IF;
  RAISE NOTICE '  COLLATIONS cloned: %', LPAD(cnt::text, 5, ' ');

  -- MV: Create Domains
  action := 'Domains';
  cnt := 0;
  FOR arec IN
    SELECT n.nspname AS "Schema", t.typname AS "Name", pg_catalog.format_type(t.typbasetype, t.typtypmod) AS "Type", (
            SELECT c.collname
            FROM pg_catalog.pg_collation c, pg_catalog.pg_type bt
            WHERE c.oid = t.typcollation
                AND bt.oid = t.typbasetype
                AND t.typcollation <> bt.typcollation) AS "Collation", CASE WHEN t.typnotnull THEN
            'not null'
        END AS "Nullable", t.typdefault AS "Default", pg_catalog.array_to_string(ARRAY (
                SELECT pg_catalog.pg_get_constraintdef(r.oid, TRUE)
                FROM pg_catalog.pg_constraint r
                -- Issue#78 FIX: handle case-sensitive names with quote_ident() on t.typename
                WHERE t.oid = r.contypid), ' ') AS "Check", 'CREATE DOMAIN ' || quote_ident(dest_schema) || '.' || quote_ident(t.typname) || ' AS ' || pg_catalog.format_type(t.typbasetype, t.typtypmod) ||
                CASE WHEN t.typnotnull IS NOT NULL THEN
            ' NOT NULL '
        ELSE
            ' '
        END || CASE WHEN t.typdefault IS NOT NULL THEN
            'DEFAULT ' || t.typdefault || ' '
        ELSE
            ' '
        END || pg_catalog.array_to_string(ARRAY (
                SELECT pg_catalog.pg_get_constraintdef(r.oid, TRUE)
                FROM pg_catalog.pg_constraint r
                WHERE t.oid = r.contypid), ' ') || ';' AS DOM_DDL
    FROM pg_catalog.pg_type t
        LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
    WHERE t.typtype = 'd'
        AND n.nspname = quote_ident(source_schema)
        AND pg_catalog.pg_type_is_visible(t.oid)
    ORDER BY 1, 2
  LOOP
    BEGIN
      cnt := cnt + 1;
      IF bDDLOnly THEN
        RAISE INFO '%', arec.dom_ddl;
      ELSE
        EXECUTE arec.dom_ddl;
      END IF;
    END;
  END LOOP;
  RAISE NOTICE '     DOMAINS cloned: %', LPAD(cnt::text, 5, ' ');

  -- MV: Create types
  action := 'Types';
  cnt := 0;
  lastsql = '';
  FOR arec IN
    -- Fixed Issue#108:enclose double-quote roles with special characters for setting "OWNER TO"
    -- SELECT c.relkind, n.nspname AS schemaname, t.typname AS typname, t.typcategory, pg_catalog.pg_get_userbyid(t.typowner) AS owner, CASE WHEN t.typcategory = 'C' THEN
    SELECT c.relkind, n.nspname AS schemaname, t.typname AS typname, t.typcategory, '"' || pg_catalog.pg_get_userbyid(t.typowner) || '"' AS owner, CASE WHEN t.typcategory = 'C' THEN
            'CREATE TYPE ' || quote_ident(dest_schema) || '.' || t.typname || ' AS (' || array_to_string(array_agg(a.attname || ' ' || pg_catalog.format_type(a.atttypid, a.atttypmod)
                ORDER BY c.relname, a.attnum), ', ') || ');'
        WHEN t.typcategory = 'E' THEN
            'CREATE TYPE ' || quote_ident(dest_schema) || '.' || t.typname || ' AS ENUM (' || REPLACE(quote_literal(array_to_string(array_agg(e.enumlabel ORDER BY e.enumsortorder), ',')), ',', ''',''') || ');'
        ELSE
            ''
        END AS type_ddl
    FROM pg_type t
        JOIN pg_namespace n ON (n.oid = t.typnamespace)
        LEFT JOIN pg_enum e ON (t.oid = e.enumtypid)
        LEFT JOIN pg_class c ON (c.reltype = t.oid)
        LEFT JOIN pg_attribute a ON (a.attrelid = c.oid)
    WHERE n.nspname = quote_ident(source_schema)
        AND (c.relkind IS NULL
            OR c.relkind = 'c')
        AND t.typcategory IN ('C', 'E')
    GROUP BY 1, 2, 3, 4, 5
    ORDER BY n.nspname, t.typcategory, t.typname

  LOOP
    BEGIN
      cnt := cnt + 1;
      -- Keep composite and enum types in separate branches for fine tuning later if needed.
      IF arec.typcategory = 'E' THEN
        IF bDDLOnly THEN
          RAISE INFO '%', arec.type_ddl;
          
          --issue#95
          IF NOT bNoOwner THEN
            -- Fixed Issue#108: double-quote roles in case they have special characters
            RAISE INFO 'ALTER TYPE % OWNER TO  %;', quote_ident(dest_schema) || '.' || arec.typname, arec.owner;
          END IF;
        ELSE
          EXECUTE arec.type_ddl;

          --issue#95
          IF NOT bNoOwner THEN
              -- Fixed Issue#108: double-quote roles in case they have special characters
	            EXECUTE 'ALTER TYPE ' || quote_ident(dest_schema) || '.' || arec.typname || ' OWNER TO ' || arec.owner;
	        END IF;
        END IF;
      ELSIF arec.typcategory = 'C' THEN
        IF bDDLOnly THEN
          RAISE INFO '%', arec.type_ddl;
          --issue#95
          IF NOT bNoOwner THEN
            -- Fixed Issue#108: double-quote roles in case they have special characters
            RAISE INFO 'ALTER TYPE % OWNER TO  %;', quote_ident(dest_schema) || '.' || arec.typname, arec.owner;
          END IF;
        ELSE
          EXECUTE arec.type_ddl;
          --issue#95
          IF NOT bNoOwner THEN
              -- Fixed Issue#108: double-quote roles in case they have special characters
	            EXECUTE 'ALTER TYPE ' || quote_ident(dest_schema) || '.' || arec.typname || ' OWNER TO ' || arec.owner;
	        END IF;
        END IF;
      ELSE
          RAISE NOTICE ' Unhandled type:%-%', arec.typcategory, arec.typname;
      END IF;
    END;
  END LOOP;
  RAISE NOTICE '       TYPES cloned: %', LPAD(cnt::text, 5, ' ');

  -- Create sequences
  action := 'Sequences';
  
  cnt := 0;
  -- fix#63  get from pg_sequences not information_schema
  -- fix#63  take 2: get it from information_schema.sequences since we need to treat IDENTITY columns differently.
  -- fix#95  get owner as well by joining to pg_sequences
  -- fix#106 we can get owner info with pg_class, pg_user/pg_group, and information_schema.sequences, so we can avoid the hit to pg_sequences which is not available in 9.6
  FOR object, buffer IN
    -- Fixed Issue#108:
    -- SELECT s1.sequence_name::text, s2.sequenceowner FROM information_schema.sequences s1 JOIN pg_sequences s2 ON (s1.sequence_schema = s2.schemaname AND s1.sequence_name = s2.sequencename) AND s1.sequence_schema = quote_ident(source_schema)
    SELECT s.sequence_name::text, '"' || u.usename || '"' as owner FROM information_schema.sequences s JOIN pg_class c ON (s.sequence_name = c.relname AND s.sequence_schema = c.relnamespace::regnamespace::text) JOIN pg_user u ON (c.relowner = u.usesysid) 
    WHERE c.relkind = 'S' AND s.sequence_schema = quote_ident(source_schema)
    UNION SELECT s.sequence_name::text, g.groname as owner FROM information_schema.sequences s JOIN pg_class c ON (s.sequence_name = c.relname AND s.sequence_schema = c.relnamespace::regnamespace::text) JOIN pg_group g ON (c.relowner = g.grosysid) 
    WHERE c.relkind = 'S' AND s.sequence_schema = quote_ident(source_schema)
  LOOP
    cnt := cnt + 1;
    IF bDDLOnly THEN
      -- issue#95
      RAISE INFO '%', 'CREATE SEQUENCE ' || quote_ident(dest_schema) || '.' || quote_ident(object) || ';';
      IF NOT bNoOwner THEN    
        -- Fixed Issue#108: double-quote roles in case they have special characters
        RAISE INFO '%', 'ALTER  SEQUENCE ' || quote_ident(dest_schema) || '.' || quote_ident(object) || ' OWNER TO ' || buffer || ';';
      END IF;
    ELSE
      EXECUTE 'CREATE SEQUENCE ' || quote_ident(dest_schema) || '.' || quote_ident(object);
      -- issue#95
      IF NOT bNoOwner THEN    
        -- Fixed Issue#108: double-quote roles in case they have special characters
        EXECUTE 'ALTER SEQUENCE '  || quote_ident(dest_schema) || '.' || quote_ident(object) || ' OWNER TO ' || buffer;
      END IF;
    END IF;
    srctbl := quote_ident(source_schema) || '.' || quote_ident(object);

    IF sq_server_version_num < 100000 THEN
      EXECUTE 'SELECT last_value, is_called FROM ' || quote_ident(source_schema) || '.' || quote_ident(object) || ';' INTO sq_last_value, sq_is_called;
      EXECUTE 'SELECT maximum_value, start_value, increment, minimum_value, 1 cache_size, cycle_option, data_type
               FROM information_schema.sequences WHERE sequence_schema='|| quote_literal(source_schema) || ' AND sequence_name=' || quote_literal(object) || ';'
               INTO sq_max_value, sq_start_value, sq_increment_by, sq_min_value, sq_cache_value, sq_is_cycled, sq_data_type;
      IF sq_is_cycled
        THEN
          sq_cycled := 'CYCLE';
      ELSE
          sq_cycled := 'NO CYCLE';
      END IF;

      qry := 'ALTER SEQUENCE '   || quote_ident(dest_schema) || '.' || quote_ident(object)
             || ' INCREMENT BY ' || sq_increment_by
             || ' MINVALUE '     || sq_min_value
             || ' MAXVALUE '     || sq_max_value
             -- will update current sequence value after this
             || ' START WITH '   || sq_start_value
             || ' RESTART '      || sq_min_value
             || ' CACHE '        || sq_cache_value
             || ' '              || sq_cycled || ' ;' ;
    ELSE
      EXECUTE 'SELECT max_value, start_value, increment_by, min_value, cache_size, cycle, data_type, COALESCE(last_value, 1)
            FROM pg_catalog.pg_sequences WHERE schemaname='|| quote_literal(source_schema) || ' AND sequencename=' || quote_literal(object) || ';'
            INTO sq_max_value, sq_start_value, sq_increment_by, sq_min_value, sq_cache_value, sq_is_cycled, sq_data_type, sq_last_value;
      IF sq_is_cycled
        THEN
          sq_cycled := 'CYCLE';
      ELSE
          sq_cycled := 'NO CYCLE';
      END IF;

      qry := 'ALTER SEQUENCE '   || quote_ident(dest_schema) || '.' || quote_ident(object)
             || ' AS ' || sq_data_type
             || ' INCREMENT BY ' || sq_increment_by
             || ' MINVALUE '     || sq_min_value
             || ' MAXVALUE '     || sq_max_value
             -- will update current sequence value after this
             || ' START WITH '   || sq_start_value
             || ' RESTART '      || sq_min_value
             || ' CACHE '        || sq_cache_value
             || ' '              || sq_cycled || ' ;' ;
    END IF;

    IF bDDLOnly THEN
      RAISE INFO '%', qry;
    ELSE
      EXECUTE qry;
    END IF;

    buffer := quote_ident(dest_schema) || '.' || quote_ident(object);
    IF bData THEN
      EXECUTE 'SELECT setval( ''' || buffer || ''', ' || sq_last_value || ', ' || sq_is_called || ');' ;
    ELSE
      if bDDLOnly THEN
        -- fix#63
        --  RAISE INFO '%', 'SELECT setval( ''' || buffer || ''', ' || sq_start_value || ', ' || sq_is_called || ');' ;
        RAISE INFO '%', 'SELECT setval( ''' || buffer || ''', ' || sq_last_value || ', ' || sq_is_called || ');' ;
      ELSE
        -- fix#63
        -- EXECUTE 'SELECT setval( ''' || buffer || ''', ' || sq_start_value || ', ' || sq_is_called || ');' ;
        EXECUTE 'SELECT setval( ''' || buffer || ''', ' || sq_last_value || ', ' || sq_is_called || ');' ;
      END IF;

    END IF;
  END LOOP;
  RAISE NOTICE '   SEQUENCES cloned: %', LPAD(cnt::text, 5, ' ');


  -- Create tables including partitioned ones (parent/children) and unlogged ones.  Order by is critical since child partition range logic is dependent on it.
  action := 'Tables';
  SELECT setting INTO v_dummy FROM pg_settings WHERE name='search_path';
  IF bDebug THEN RAISE NOTICE 'DEBUG: search_path=%', v_dummy; END IF;
  
  cnt := 0;
  -- Issue#61 FIX: use set_config for empty string
  -- SET search_path = '';
  SELECT set_config('search_path', '', false) into v_dummy;
  IF bDebug THEN RAISE NOTICE 'DEBUG: setting search_path to empty string:%', v_dummy; END IF;
  -- Fix#86 add isgenerated to column list
  -- Fix#91 add tblowner for setting the table ownership to that of the source
  -- Fix#99 added join to pg_tablespace
  
  -- Handle PG versions greater than last major/minor version of PG 9.6.24
  IF sq_server_version_num > 90624 THEN
  FOR tblname, relpersist, bRelispart, relknd, data_type, udt_name, udt_schema, ocomment, l_child, isGenerated, tblowner, tblspace  IN
    -- 2021-03-08 MJV #39 fix: change sql to get indicator of user-defined columns to issue warnings
    -- select c.relname, c.relpersistence, c.relispartition, c.relkind
    -- FROM pg_class c, pg_namespace n where n.oid = c.relnamespace and n.nspname = quote_ident(source_schema) and c.relkind in ('r','p') and
    -- order by c.relkind desc, c.relname
    --Fix#65 add another left join to distinguish child tables by inheritance
    -- Fix#86 add is_generated to column select
    -- Fix#91 add tblowner to the select
    -- Fix#105 need a different kinda distint to avoid retrieving a table twice in the case of a table with multiple USER-DEFINED datatypes using DISTINCT ON instead of just DISTINCT
    --SELECT DISTINCT c.relname, c.relpersistence, c.relispartition, c.relkind, co.data_type, co.udt_name, co.udt_schema, obj_description(c.oid), i.inhrelid, 
    --                COALESCE(co.is_generated, ''), pg_catalog.pg_get_userbyid(c.relowner) as "Owner", CASE WHEN reltablespace = 0 THEN 'pg_default' ELSE ts.spcname END as tablespace
    -- fixed #108 by enclosing owner in double quotes to avoid errors for bad characters like #.@...
    -- SELECT DISTINCT ON (c.relname, c.relpersistence, c.relispartition, c.relkind, co.data_type) c.relname, c.relpersistence, c.relispartition, c.relkind, co.data_type, co.udt_name, co.udt_schema, obj_description(c.oid), i.inhrelid, 
    SELECT DISTINCT ON (c.relname, c.relpersistence, c.relispartition, c.relkind, co.data_type) c.relname, c.relpersistence, c.relispartition, c.relkind, co.data_type, co.udt_name, co.udt_schema, obj_description(c.oid), i.inhrelid, 
                    COALESCE(co.is_generated, ''), '"' || pg_catalog.pg_get_userbyid(c.relowner) || '"' as "Owner", CASE WHEN reltablespace = 0 THEN 'pg_default' ELSE ts.spcname END as tablespace                    
    FROM pg_class c
        JOIN pg_namespace n ON (n.oid = c.relnamespace
                AND n.nspname = quote_ident(source_schema)
                AND c.relkind IN ('r', 'p'))
        LEFT JOIN information_schema.columns co ON (co.table_schema = n.nspname
                AND co.table_name = c.relname
                AND (co.data_type = 'USER-DEFINED' OR co.is_generated = 'ALWAYS'))
        LEFT JOIN pg_inherits i ON (c.oid = i.inhrelid) 
        -- issue#99 added join
        LEFT JOIN pg_tablespace ts ON (c.reltablespace = ts.oid) 
    ORDER BY c.relkind DESC, c.relname
  LOOP
    cnt := cnt + 1;
    lastsql = '';
    IF l_child IS NULL THEN
      bChild := False;
    ELSE
      bChild := True;
    END IF;
    IF bDebug THEN RAISE NOTICE 'DEBUG: TABLE START --> table=%  bRelispart=%  relkind=%  bChild=%',tblname, bRelispart, relknd, bChild; END IF;

    IF data_type = 'USER-DEFINED' THEN
      -- RAISE NOTICE ' Table (%) has column(s) with user-defined types so using get_table_ddl() instead of CREATE TABLE LIKE construct.',tblname;
      cnt :=cnt;
    END IF;
    buffer := quote_ident(dest_schema) || '.' || quote_ident(tblname);
    buffer2 := '';
    IF relpersist = 'u' THEN
      buffer2 := 'UNLOGGED ';
    END IF;
    IF relknd = 'r' THEN
      IF bDDLOnly THEN
        IF data_type = 'USER-DEFINED' THEN
          -- FIXED #65, #67
          -- SELECT * INTO buffer3 FROM public.pg_get_tabledef(quote_ident(source_schema), tblname);
          SELECT * INTO buffer3 FROM public.get_table_ddl(quote_ident(source_schema), tblname, False);

          buffer3 := REPLACE(buffer3, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.');
          RAISE INFO '%', buffer3;
          -- issue#91 fix
          -- issue#95
          IF NOT bNoOwner THEN    
            -- Fixed Issue#108: double-quote roles in case they have special characters
            RAISE INFO 'ALTER TABLE IF EXISTS % OWNER TO %;', quote_ident(dest_schema) || '.' || tblname, tblowner;
          END IF;
        ELSE
          IF NOT bChild THEN
            RAISE INFO '%', 'CREATE ' || buffer2 || 'TABLE ' || buffer || ' (LIKE ' || quote_ident(source_schema) || '.' || quote_ident(tblname) || ' INCLUDING ALL);';
            -- issue#91 fix
             -- issue#95
            IF NOT bNoOwner THEN    
              -- Fixed Issue#108: double-quote roles in case they have special characters
              RAISE INFO 'ALTER TABLE IF EXISTS % OWNER TO %;', quote_ident(dest_schema) || '.' || tblname, tblowner;
            END IF;
            
            -- issue#99 
            IF tblspace <> 'pg_default' THEN
              -- replace with user-defined tablespace
              -- ALTER TABLE myschema.mytable SET TABLESPACE usrtblspc;
              RAISE INFO 'ALTER TABLE IF EXISTS % SET TABLESPACE %;', quote_ident(dest_schema) || '.' || tblname, tblspace;
            END IF;
          ELSE
            -- FIXED #65, #67
            -- SELECT * INTO buffer3 FROM public.pg_get_tabledef(quote_ident(source_schema), tblname);
            SELECT * INTO buffer3 FROM public.get_table_ddl(quote_ident(source_schema), tblname, False);
            buffer3 := REPLACE(buffer3, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.');
            RAISE INFO '%', buffer3;
            -- issue#91 fix
            -- issue#95
            IF NOT bNoOwner THEN    
              -- Fixed Issue#108: double-quote roles in case they have special characters
              RAISE INFO 'ALTER TABLE IF EXISTS % OWNER TO %;', quote_ident(dest_schema) || '.' || tblname, tblowner;
            END IF;
          END IF;
        END IF;
      ELSE
        IF data_type = 'USER-DEFINED' THEN
          -- FIXED #65, #67
          -- SELECT * INTO buffer3 FROM public.pg_get_tabledef(quote_ident(source_schema), tblname);
          SELECT * INTO buffer3 FROM public.get_table_ddl(quote_ident(source_schema), tblname, False);
          buffer3 := REPLACE(buffer3, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.');
          IF bDebug THEN RAISE NOTICE 'DEBUG: tabledef01:%', buffer3; END IF;
          -- #82: Table def should be fully qualified with target schema, 
          --      so just make search path = public to handle extension types that should reside in public schema
          v_dummy = 'public';
          SELECT set_config('search_path', v_dummy, false) into v_dummy;
          EXECUTE buffer3;
          -- issue#91 fix
          -- issue#95
          IF NOT bNoOwner THEN    
            -- Fixed Issue#108: double-quote roles in case they have special characters
            buffer3 = 'ALTER TABLE IF EXISTS ' || quote_ident(dest_schema) || '.' || tblname || ' OWNER TO ' || tblowner;
            lastsql = buffer3;
            EXECUTE buffer3;
          END IF;
        ELSE
          IF (NOT bChild OR bRelispart) THEN
            buffer3 := 'CREATE ' || buffer2 || 'TABLE ' || buffer || ' (LIKE ' || quote_ident(source_schema) || '.' || quote_ident(tblname) || ' INCLUDING ALL)';
            IF bDebug THEN RAISE NOTICE 'DEBUG: tabledef02:%', buffer3; END IF;
            EXECUTE buffer3;
            -- issue#91 fix
            -- issue#95
            IF NOT bNoOwner THEN    
              -- Fixed Issue#108: double-quote roles in case they have special characters
              buffer3 = 'ALTER TABLE IF EXISTS ' || quote_ident(dest_schema) || '.'  || quote_ident(tblname) || ' OWNER TO ' || tblowner;
              lastsql = buffer3;
              EXECUTE buffer3;
            END IF;
            
            -- issue#99
            IF tblspace <> 'pg_default' THEN
              -- replace with user-defined tablespace
              -- ALTER TABLE myschema.mytable SET TABLESPACE usrtblspc;
              buffer3 = 'ALTER TABLE IF EXISTS ' || quote_ident(dest_schema) || '.' || tblname || ' SET TABLESPACE ' || tblspace;
              EXECUTE buffer3;
            END IF;

          ELSE
            -- FIXED #65, #67
            -- SELECT * INTO buffer3 FROM public.pg_get_tabledef(quote_ident(source_schema), tblname);
            SELECT * INTO buffer3 FROM public.get_table_ddl(quote_ident(source_schema), tblname, False);

            buffer3 := REPLACE(buffer3, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.');
            -- set client_min_messages higher to avoid messages like this:
            -- NOTICE:  merging column "city_id" with inherited definition
            set client_min_messages = 'WARNING';
            IF bDebug THEN RAISE NOTICE 'DEBUG: tabledef03:%', buffer3; END IF;
            EXECUTE buffer3;
            -- issue#91 fix
            -- issue#95
            IF NOT bNoOwner THEN
              -- Fixed Issue#108: double-quote roles in case they have special characters
              buffer3 = 'ALTER TABLE IF EXISTS ' || quote_ident(dest_schema) || '.' || tblname || ' OWNER TO ' || tblowner;
              lastsql = buffer3;
              EXECUTE buffer3;
            END IF;

            -- reset it back, only get these for inheritance-based tables
            set client_min_messages = 'notice';
          END IF;
        END IF;
        -- Add table comment.
        IF ocomment IS NOT NULL THEN
          EXECUTE 'COMMENT ON TABLE ' || buffer || ' IS ' || quote_literal(ocomment);
        END IF;
      END IF;
    ELSIF relknd = 'p' THEN
      -- define parent table and assume child tables have already been created based on top level sort order.
      -- Issue #103 Put the complex query into its own function, get_table_ddl_complex()
      SELECT * INTO qry FROM public.get_table_ddl_complex(source_schema, dest_schema, tblname, sq_server_version_num);
      IF bDebug THEN RAISE NOTICE 'DEBUG: tabledef04 - %', buffer; END IF;
      
      -- consider replacing complicated query above with this simple call to get_table_ddl()...
      -- SELECT * INTO qry FROM public.get_table_ddl(quote_ident(source_schema), tblname, False);
      -- qry := REPLACE(qry, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.');

      IF bDDLOnly THEN
        RAISE INFO '%', qry;
        -- issue#95
        IF NOT bNoOwner THEN
            -- Fixed Issue#108: double-quote roles in case they have special characters
            RAISE INFO 'ALTER TABLE IF EXISTS % OWNER TO %;', quote_ident(dest_schema) || '.' || quote_ident(tblname), tblowner;
        END IF;
      ELSE
        -- Issue#103: we need to always set search_path priority to target schema when we execute DDL
        IF bDebug THEN RAISE NOTICE 'DEBUG: tabledef04 context: old search path=%  new search path=% current search path=%', src_path_old, src_path_new, v_dummy; END IF;
        SELECT setting INTO spath_tmp FROM pg_settings WHERE name = 'search_path';   
        IF spath_tmp <> dest_schema THEN
          -- change it to target schema and don't forget to change it back after we execute the DDL
          spath = 'SET search_path = "' || dest_schema || '"';
          IF bDebug THEN RAISE NOTICE 'DEBUG: changing search_path --> %', spath; END IF;
          EXECUTE spath;
          SELECT setting INTO v_dummy FROM pg_settings WHERE name = 'search_path';   
          IF bDebug THEN RAISE NOTICE 'DEBUG: search_path changed to %', v_dummy; END IF;
        END IF;
        IF bDebug THEN RAISE NOTICE 'DEBUG: tabledef04:%', qry; END IF;
        EXECUTE qry;
        
        -- Issue#103
        -- Set search path back to what it was
        spath = 'SET search_path = "' || spath_tmp || '"';
        EXECUTE spath;
        SELECT setting INTO v_dummy FROM pg_settings WHERE name = 'search_path';   
        IF bDebug THEN RAISE NOTICE 'DEBUG: search_path changed back to %', v_dummy; END IF;
        
        -- issue#91 fix
        -- issue#95
        IF NOT bNoOwner THEN
          -- Fixed Issue#108: double-quote roles in case they have special characters
          buffer3 = 'ALTER TABLE IF EXISTS ' || quote_ident(dest_schema) || '.' || quote_ident(tblname) || ' OWNER TO ' || tblowner;
          lastsql = buffer3;
          EXECUTE buffer3;
        END IF;
        
      END IF;
      -- loop for child tables and alter them to attach to parent for specific partition method.
      -- Issue#103 fix: only loop for the table we are currently processing, tblname!
      FOR aname, part_range, object IN
        SELECT quote_ident(dest_schema) || '.' || c1.relname as tablename, pg_catalog.pg_get_expr(c1.relpartbound, c1.oid) as partrange, quote_ident(dest_schema) || '.' || c2.relname as object
        FROM pg_catalog.pg_class c1, pg_namespace n, pg_catalog.pg_inherits i, pg_class c2
        WHERE n.nspname = quote_ident(source_schema) AND c1.relnamespace = n.oid AND c1.relkind = 'r' 
        -- Issue#103: added this condition to only work on current partitioned table.  The problem was regression testing previously only worked on one partition table clone case
        AND c2.relname = tblname AND 
        c1.relispartition AND c1.oid=i.inhrelid AND i.inhparent = c2.oid AND c2.relnamespace = n.oid ORDER BY pg_catalog.pg_get_expr(c1.relpartbound, c1.oid) = 'DEFAULT',
        c1.oid::pg_catalog.regclass::pg_catalog.text
      LOOP
        qry := 'ALTER TABLE ONLY ' || object || ' ATTACH PARTITION ' || aname || ' ' || part_range || ';';
        IF bDebug THEN RAISE NOTICE 'DEBUG: %',qry; END IF;
        -- issue#91, not sure if we need to do this for child tables
        -- issue#95 we dont set ownership here
        IF bDDLOnly THEN
          RAISE INFO '%', qry;
          IF NOT bNoOwner THEN
            NULL;
          END IF;
        ELSE
          EXECUTE qry;
          IF NOT bNoOwner THEN
            NULL;
          END IF;
        END IF;
      END LOOP;
    END IF;
        
    -- INCLUDING ALL creates new index names, we restore them to the old name.
    -- There should be no conflicts since they live in different schemas
    FOR ix_old_name, ix_new_name IN
      SELECT old.indexname, new.indexname
      FROM pg_indexes old, pg_indexes new
      WHERE old.schemaname = source_schema
        AND new.schemaname = dest_schema
        AND old.tablename = new.tablename
        AND old.tablename = tblname
        AND old.indexname <> new.indexname
        AND regexp_replace(old.indexdef, E'.*USING','') = regexp_replace(new.indexdef, E'.*USING','')
        ORDER BY old.indexdef, new.indexdef
    LOOP
      IF bDDLOnly THEN
        RAISE INFO '%', 'ALTER INDEX ' || quote_ident(dest_schema) || '.'  || quote_ident(ix_new_name) || ' RENAME TO ' || quote_ident(ix_old_name) || ';';
      ELSE
        -- The SELECT query above may return duplicate names when a column is
        -- indexed twice the same manner with 2 different names. Therefore, to
        -- avoid a 'relation "xxx" already exists' we test if the index name
        -- is in use or free. Skipping existing index will fallback on unused
        -- ones and every duplicate will be mapped to distinct old names.
        IF NOT EXISTS (
            SELECT TRUE
            FROM pg_indexes
            WHERE schemaname = dest_schema
              AND tablename = tblname
              AND indexname = quote_ident(ix_old_name))
          AND EXISTS (
            SELECT TRUE
            FROM pg_indexes
            WHERE schemaname = dest_schema
              AND tablename = tblname
              AND indexname = quote_ident(ix_new_name))
          THEN
          EXECUTE 'ALTER INDEX ' || quote_ident(dest_schema) || '.' || quote_ident(ix_new_name) || ' RENAME TO ' || quote_ident(ix_old_name) || ';';
        END IF;
      END IF;
    END LOOP;

    lastsql = '';
    IF bData THEN
      -- Insert records from source table

      -- 2021-03-03  MJV FIX
      buffer := dest_schema || '.' || quote_ident(tblname);

      -- 2020/06/18 - Issue #31 fix: add "OVERRIDING SYSTEM VALUE" for IDENTITY columns marked as GENERATED ALWAYS.
      select count(*) into cnt2 from pg_class c, pg_attribute a, pg_namespace n
          where a.attrelid = c.oid and c.relname = quote_ident(tblname) and n.oid = c.relnamespace and n.nspname = quote_ident(source_schema) and a.attidentity = 'a';
      buffer3 := '';
      IF cnt2 > 0 THEN
          buffer3 := ' OVERRIDING SYSTEM VALUE';
      END IF;
      -- BUG for inserting rows from tables with user-defined columns
      -- INSERT INTO sample_clone.address OVERRIDING SYSTEM VALUE SELECT * FROM sample.address;
      -- ERROR:  column "id2" is of type sample_clone.udt_myint but expression is of type udt_myint
      
      -- Issue#86 fix:
      -- IF data_type = 'USER-DEFINED' THEN
      IF bDebug THEN RAISE NOTICE 'DEBUG: includerecs branch  table=%  data_type=%  isgenerated=%  buffer3=%', tblname, data_type, isGenerated, buffer3; END IF;
      IF data_type = 'USER-DEFINED' OR isGenerated = 'ALWAYS' THEN

        -- RAISE WARNING 'Bypassing copying rows for table (%) with user-defined data types.  You must copy them manually.', tblname;
        -- wont work --> INSERT INTO clone1.address (id2, id3, addr) SELECT cast(id2 as clone1.udt_myint), cast(id3 as clone1.udt_myint), addr FROM sample.address;
        -- Issue#101 --> INSERT INTO clone1.address2 (id2, id3, addr) SELECT id2::text::clone1.udt_myint, id3::text::clone1.udt_myint, addr FROM sample.address; 

        -- Issue#79 implementation follows        
        -- COPY sample.statuses(id, s) TO '/tmp/statuses.txt' WITH DELIMITER AS ',';
        -- COPY sample_clone1.statuses FROM '/tmp/statuses.txt' (DELIMITER ',', NULL '');
        -- Issue#101 fix: use text cast to get around the problem.
        IF bFileCopy THEN
          IF bWindows THEN
              buffer2   := 'COPY ' || quote_ident(source_schema) || '.' || quote_ident(tblname) || ' TO  ''C:\WINDOWS\TEMP\cloneschema.tmp'' WITH DELIMITER AS '','';';
              tblarray2 := tblarray2 || buffer2;
              -- Issue #81 reformat COPY command for upload
              -- buffer2:= 'COPY ' || quote_ident(dest_schema) || '.' || quote_ident(tblname) || '  FROM  ''C:\WINDOWS\TEMP\cloneschema.tmp'' (DELIMITER '','', NULL '''');';
              buffer2   := 'COPY ' || quote_ident(dest_schema) || '.' || quote_ident(tblname) || '  FROM  ''C:\WINDOWS\TEMP\cloneschema.tmp'' (DELIMITER '','', NULL ''\N'', FORMAT CSV);';
              tblarray2 := tblarray2 || buffer2;
          ELSE
              buffer2   := 'COPY ' || quote_ident(source_schema) || '.' || quote_ident(tblname) || ' TO ''/tmp/cloneschema.tmp'' WITH DELIMITER AS '','';';
              tblarray2 := tblarray2 || buffer2;
              -- Issue #81 reformat COPY command for upload
              -- buffer2   := 'COPY ' || quote_ident(dest_schema) || '.' || quote_ident(tblname) || '  FROM ''/tmp/cloneschema.tmp'' (DELIMITER '','', NULL '''');';
              -- works--> COPY sample.timestamptbl2  FROM '/tmp/cloneschema.tmp' WITH (DELIMITER ',', NULL '\N', FORMAT CSV) ;
              buffer2   := 'COPY ' || quote_ident(dest_schema) || '.' || quote_ident(tblname) || '  FROM ''/tmp/cloneschema.tmp'' (DELIMITER '','', NULL ''\N'', FORMAT CSV);';
              tblarray2 := tblarray2 || buffer2;
          END IF;
        ELSE
          -- Issue#101: assume direct copy with text cast, add to separate array
          SELECT * INTO buffer3 FROM public.get_insert_stmt_ddl(quote_ident(source_schema), quote_ident(dest_schema), quote_ident(tblname), True);
          tblarray3 := tblarray3 || buffer3;
        END IF;
      ELSE
        -- bypass child tables since we populate them when we populate the parents
        IF bDebug THEN RAISE NOTICE 'DEBUG: tblname=%  bRelispart=%  relknd=%  l_child=%  bChild=%', tblname, bRelispart, relknd, l_child, bChild; END IF;
        IF NOT bRelispart AND NOT bChild THEN
          -- Issue#75: Must defer population of tables until child tables have been added to parents
          -- Issue#101 Offer alternative of copy to/from file. Although originally intended for tables with UDTs, it is now expanded to handle all cases for performance improvement perhaps for large tables.
          -- Issue#106 buffer3 shouldnt be in the mix
          -- revisited:  buffer3 should be in play for PG versions that handle IDENTITIES
          buffer2 := 'INSERT INTO ' || buffer || buffer3 || ' SELECT * FROM ' || quote_ident(source_schema) || '.' || quote_ident(tblname) || ';';
          -- buffer2 := 'INSERT INTO ' || buffer || ' SELECT * FROM ' || quote_ident(source_schema) || '.' || quote_ident(tblname) || ';';
          IF bDebug THEN RAISE NOTICE 'DEBUG: buffer2=%',buffer2; END IF;
          IF bFileCopy THEN
            tblarray2:= tblarray2 || buffer2;
          ELSE
            tblarray := tblarray || buffer2;
          END IF;
        END IF;
      END IF;
    END IF;

    -- Issue#61 FIX: use set_config for empty string
    -- SET search_path = '';
    SELECT set_config('search_path', '', false) into v_dummy;

    FOR column_, default_ IN
      SELECT column_name::text,
             REPLACE(column_default::text, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.')
      FROM information_schema.COLUMNS
      WHERE table_schema = source_schema
          AND TABLE_NAME = tblname
          AND column_default LIKE 'nextval(%' || quote_ident(source_schema) || '%::regclass)'
    LOOP
      -- Issue#78 FIX: handle case-sensitive names with quote_ident() on column name
      buffer2 = 'ALTER TABLE ' || buffer || ' ALTER COLUMN ' || quote_ident(column_) || ' SET DEFAULT ' || default_ || ';';
      IF bDDLOnly THEN
        -- May need to come back and revisit this since previous sql will not return anything since no schema as created!
        RAISE INFO '%', buffer2;
      ELSE
        EXECUTE buffer2;
      END IF;
    END LOOP;
    
    EXECUTE 'SET search_path = ' || quote_ident(source_schema) ;
  END LOOP;
  ELSE 
  -- Handle 9.6 versions 90600
  FOR tblname, relpersist, relknd, data_type, udt_name, udt_schema, ocomment, l_child, isGenerated, tblowner, tblspace  IN
    -- 2021-03-08 MJV #39 fix: change sql to get indicator of user-defined columns to issue warnings
    -- select c.relname, c.relpersistence, c.relispartition, c.relkind
    -- FROM pg_class c, pg_namespace n where n.oid = c.relnamespace and n.nspname = quote_ident(source_schema) and c.relkind in ('r','p') and
    -- order by c.relkind desc, c.relname
    --Fix#65 add another left join to distinguish child tables by inheritance
    -- Fix#86 add is_generated to column select
    -- Fix#91 add tblowner to the select
    -- Fix#105 need a different kinda distint to avoid retrieving a table twice in the case of a table with multiple USER-DEFINED datatypes using DISTINCT ON instead of just DISTINCT
    -- Fixed Issue#108: double quote roles to avoid problems with special characters in OWNER TO statements
    --SELECT DISTINCT c.relname, c.relpersistence, c.relispartition, c.relkind, co.data_type, co.udt_name, co.udt_schema, obj_description(c.oid), i.inhrelid, 
    --                COALESCE(co.is_generated, ''), pg_catalog.pg_get_userbyid(c.relowner) as "Owner", CASE WHEN reltablespace = 0 THEN 'pg_default' ELSE ts.spcname END as tablespace
    -- SELECT DISTINCT ON (c.relname, c.relpersistence, c.relkind, co.data_type) c.relname, c.relpersistence, c.relkind, co.data_type, co.udt_name, co.udt_schema, obj_description(c.oid), i.inhrelid, 
    --                 COALESCE(co.is_generated, ''), pg_catalog.pg_get_userbyid(c.relowner) as "Owner", CASE WHEN reltablespace = 0 THEN 'pg_default' ELSE ts.spcname END as tablespace                    
    SELECT DISTINCT ON (c.relname, c.relpersistence, c.relkind, co.data_type) c.relname, c.relpersistence, c.relkind, co.data_type, co.udt_name, co.udt_schema, obj_description(c.oid), i.inhrelid, 
                    COALESCE(co.is_generated, ''), '"' || pg_catalog.pg_get_userbyid(c.relowner) || '"' as "Owner", CASE WHEN reltablespace = 0 THEN 'pg_default' ELSE ts.spcname END as tablespace                    
    FROM pg_class c
        JOIN pg_namespace n ON (n.oid = c.relnamespace
                AND n.nspname = quote_ident(source_schema)
                AND c.relkind IN ('r', 'p'))
        LEFT JOIN information_schema.columns co ON (co.table_schema = n.nspname
                AND co.table_name = c.relname
                AND (co.data_type = 'USER-DEFINED' OR co.is_generated = 'ALWAYS'))
        LEFT JOIN pg_inherits i ON (c.oid = i.inhrelid) 
        -- issue#99 added join
        LEFT JOIN pg_tablespace ts ON (c.reltablespace = ts.oid) 
    ORDER BY c.relkind DESC, c.relname
  LOOP
    cnt := cnt + 1;
    IF l_child IS NULL THEN
      bChild := False;
    ELSE
      bChild := True;
    END IF;
    IF bDebug THEN RAISE NOTICE 'DEBUG: TABLE START --> table=%  bRelispart=NA  relkind=%  bChild=%',tblname, relknd, bChild; END IF;

    IF data_type = 'USER-DEFINED' THEN
      -- RAISE NOTICE ' Table (%) has column(s) with user-defined types so using get_table_ddl() instead of CREATE TABLE LIKE construct.',tblname;
      cnt :=cnt;
    END IF;
    buffer := quote_ident(dest_schema) || '.' || quote_ident(tblname);
    buffer2 := '';
    IF relpersist = 'u' THEN
      buffer2 := 'UNLOGGED ';
    END IF;
    IF relknd = 'r' THEN
      IF bDDLOnly THEN
        IF data_type = 'USER-DEFINED' THEN
          -- FIXED #65, #67
          -- SELECT * INTO buffer3 FROM public.pg_get_tabledef(quote_ident(source_schema), tblname);
          SELECT * INTO buffer3 FROM public.get_table_ddl(quote_ident(source_schema), tblname, False);

          buffer3 := REPLACE(buffer3, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.');
          RAISE INFO '%', buffer3;
          -- issue#91 fix
          -- issue#95
          IF NOT bNoOwner THEN    
            -- Fixed Issue#108: double-quote roles in case they have special characters
            RAISE INFO 'ALTER TABLE IF EXISTS % OWNER TO %;', quote_ident(dest_schema) || '.' || tblname, tblowner;
          END IF;
        ELSE
          IF NOT bChild THEN
            RAISE INFO '%', 'CREATE ' || buffer2 || 'TABLE ' || buffer || ' (LIKE ' || quote_ident(source_schema) || '.' || quote_ident(tblname) || ' INCLUDING ALL);';
            -- issue#91 fix
             -- issue#95
            IF NOT bNoOwner THEN    
              -- Fixed Issue#108: double-quote roles in case they have special characters
              RAISE INFO 'ALTER TABLE IF EXISTS % OWNER TO %;', quote_ident(dest_schema) || '.' || tblname, tblowner;
            END IF;
            
            -- issue#99 
            IF tblspace <> 'pg_default' THEN
              -- replace with user-defined tablespace
              -- ALTER TABLE myschema.mytable SET TABLESPACE usrtblspc;
              RAISE INFO 'ALTER TABLE IF EXISTS % SET TABLESPACE %;', quote_ident(dest_schema) || '.' || tblname, tblspace;
            END IF;
          ELSE
            -- FIXED #65, #67
            -- SELECT * INTO buffer3 FROM public.pg_get_tabledef(quote_ident(source_schema), tblname);
            SELECT * INTO buffer3 FROM public.get_table_ddl(quote_ident(source_schema), tblname, False);
            buffer3 := REPLACE(buffer3, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.');
            RAISE INFO '%', buffer3;
            -- issue#91 fix
            -- issue#95
            IF NOT bNoOwner THEN    
              -- Fixed Issue#108: double-quote roles in case they have special characters
              RAISE INFO 'ALTER TABLE IF EXISTS % OWNER TO %;', quote_ident(dest_schema) || '.' || tblname, tblowner;
            END IF;
          END IF;
        END IF;
      ELSE
        IF data_type = 'USER-DEFINED' THEN
          -- FIXED #65, #67
          -- SELECT * INTO buffer3 FROM public.pg_get_tabledef(quote_ident(source_schema), tblname);
          SELECT * INTO buffer3 FROM public.get_table_ddl(quote_ident(source_schema), tblname, False);
          buffer3 := REPLACE(buffer3, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.');
          IF bDebug THEN RAISE NOTICE 'DEBUG: tabledef01:%', buffer3; END IF;
          -- #82: Table def should be fully qualified with target schema, 
          --      so just make search path = public to handle extension types that should reside in public schema
          v_dummy = 'public';
          SELECT set_config('search_path', v_dummy, false) into v_dummy;
          EXECUTE buffer3;
          -- issue#91 fix
          -- issue#95
          IF NOT bNoOwner THEN    
            -- Fixed Issue#108: double-quote roles in case they have special characters
            buffer3 = 'ALTER TABLE IF EXISTS ' || quote_ident(dest_schema) || '.' || tblname || ' OWNER TO ' || tblowner;
            lastsql = buffer3;
            EXECUTE buffer3;
          END IF;
        ELSE
          IF (NOT bChild) THEN
            buffer3 := 'CREATE ' || buffer2 || 'TABLE ' || buffer || ' (LIKE ' || quote_ident(source_schema) || '.' || quote_ident(tblname) || ' INCLUDING ALL)';
            IF bDebug THEN RAISE NOTICE 'DEBUG: tabledef02:%', buffer3; END IF;
            EXECUTE buffer3;
            -- issue#91 fix
            -- issue#95
            IF NOT bNoOwner THEN    
              -- Fixed Issue#108: double-quote roles in case they have special characters
              buffer3 = 'ALTER TABLE IF EXISTS ' || quote_ident(dest_schema) || '.'  || quote_ident(tblname) || ' OWNER TO ' || tblowner;
              lastsql = buffer3;
              EXECUTE buffer3;
            END IF;
            
            -- issue#99
            IF tblspace <> 'pg_default' THEN
              -- replace with user-defined tablespace
              -- ALTER TABLE myschema.mytable SET TABLESPACE usrtblspc;
              buffer3 = 'ALTER TABLE IF EXISTS ' || quote_ident(dest_schema) || '.' || tblname || ' SET TABLESPACE ' || tblspace;
              EXECUTE buffer3;
            END IF;

          ELSE
            -- FIXED #65, #67
            -- SELECT * INTO buffer3 FROM public.pg_get_tabledef(quote_ident(source_schema), tblname);
            SELECT * INTO buffer3 FROM public.get_table_ddl(quote_ident(source_schema), tblname, False);

            buffer3 := REPLACE(buffer3, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.');
            -- set client_min_messages higher to avoid messages like this:
            -- NOTICE:  merging column "city_id" with inherited definition
            set client_min_messages = 'WARNING';
            IF bDebug THEN RAISE NOTICE 'DEBUG: tabledef03:%', buffer3; END IF;
            EXECUTE buffer3;
            -- issue#91 fix
            -- issue#95
            IF NOT bNoOwner THEN
              -- Fixed Issue#108: double-quote roles in case they have special characters
              buffer3 = 'ALTER TABLE IF EXISTS ' || quote_ident(dest_schema) || '.' || tblname || ' OWNER TO ' || tblowner;
              lastsql = buffer3;
              EXECUTE buffer3;
            END IF;

            -- reset it back, only get these for inheritance-based tables
            set client_min_messages = 'notice';
          END IF;
        END IF;
        -- Add table comment.
        IF ocomment IS NOT NULL THEN
          EXECUTE 'COMMENT ON TABLE ' || buffer || ' IS ' || quote_literal(ocomment);
        END IF;
      END IF;
    ELSIF relknd = 'p' THEN
      -- define parent table and assume child tables have already been created based on top level sort order.
      -- Issue #103 Put the complex query into its own function, get_table_ddl_complex()
      SELECT * INTO qry FROM public.get_table_ddl_complex(source_schema, dest_schema, tblname, sq_server_version_num);
      IF bDebug THEN RAISE NOTICE 'DEBUG: tabledef04 - %', buffer; END IF;
      
      -- consider replacing complicated query above with this simple call to get_table_ddl()...
      -- SELECT * INTO qry FROM public.get_table_ddl(quote_ident(source_schema), tblname, False);
      -- qry := REPLACE(qry, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.');

      IF bDDLOnly THEN
        RAISE INFO '%', qry;
        -- issue#95
        IF NOT bNoOwner THEN
            -- Fixed Issue#108: double-quote roles in case they have special characters
            RAISE INFO 'ALTER TABLE IF EXISTS % OWNER TO %;', quote_ident(dest_schema) || '.' || quote_ident(tblname), tblowner;
        END IF;
      ELSE
        -- Issue#103: we need to always set search_path priority to target schema when we execute DDL
        IF bDebug THEN RAISE NOTICE 'DEBUG: tabledef04 context: old search path=%  new search path=% current search path=%', src_path_old, src_path_new, v_dummy; END IF;
        SELECT setting INTO spath_tmp FROM pg_settings WHERE name = 'search_path';   
        IF spath_tmp <> dest_schema THEN
          -- change it to target schema and don't forget to change it back after we execute the DDL
          spath = 'SET search_path = "' || dest_schema || '"';
          IF bDebug THEN RAISE NOTICE 'DEBUG: changing search_path --> %', spath; END IF;
          EXECUTE spath;
          SELECT setting INTO v_dummy FROM pg_settings WHERE name = 'search_path';   
          IF bDebug THEN RAISE NOTICE 'DEBUG: search_path changed to %', v_dummy; END IF;
        END IF;
        IF bDebug THEN RAISE NOTICE 'DEBUG: tabledef04:%', qry; END IF;
        EXECUTE qry;
        
        -- Issue#103
        -- Set search path back to what it was
        spath = 'SET search_path = "' || spath_tmp || '"';
        EXECUTE spath;
        SELECT setting INTO v_dummy FROM pg_settings WHERE name = 'search_path';   
        IF bDebug THEN RAISE NOTICE 'DEBUG: search_path changed back to %', v_dummy; END IF;
        
        -- issue#91 fix
        -- issue#95
        IF NOT bNoOwner THEN
          -- Fixed Issue#108: double-quote roles in case they have special characters
          buffer3 = 'ALTER TABLE IF EXISTS ' || quote_ident(dest_schema) || '.' || quote_ident(tblname) || ' OWNER TO ' || tblowner;
          EXECUTE buffer3;
        END IF;
        
      END IF;
      -- loop for child tables and alter them to attach to parent for specific partition method.
      -- Issue#103 fix: only loop for the table we are currently processing, tblname!
      FOR aname, part_range, object IN
        SELECT quote_ident(dest_schema) || '.' || c1.relname as tablename, pg_catalog.pg_get_expr(c1.relpartbound, c1.oid) as partrange, quote_ident(dest_schema) || '.' || c2.relname as object
        FROM pg_catalog.pg_class c1, pg_namespace n, pg_catalog.pg_inherits i, pg_class c2
        WHERE n.nspname = quote_ident(source_schema) AND c1.relnamespace = n.oid AND c1.relkind = 'r' 
        -- Issue#103: added this condition to only work on current partitioned table.  The problem was regression testing previously only worked on one partition table clone case
        AND c2.relname = tblname AND 
        c1.relispartition AND c1.oid=i.inhrelid AND i.inhparent = c2.oid AND c2.relnamespace = n.oid ORDER BY pg_catalog.pg_get_expr(c1.relpartbound, c1.oid) = 'DEFAULT',
        c1.oid::pg_catalog.regclass::pg_catalog.text
      LOOP
        qry := 'ALTER TABLE ONLY ' || object || ' ATTACH PARTITION ' || aname || ' ' || part_range || ';';
        IF bDebug THEN RAISE NOTICE 'DEBUG: %',qry; END IF;
        -- issue#91, not sure if we need to do this for child tables
        -- issue#95 we dont set ownership here
        IF bDDLOnly THEN
          RAISE INFO '%', qry;
          IF NOT bNoOwner THEN
            NULL;
          END IF;
        ELSE
          EXECUTE qry;
          IF NOT bNoOwner THEN
            NULL;
          END IF;
        END IF;
      END LOOP;
    END IF;
        
    -- INCLUDING ALL creates new index names, we restore them to the old name.
    -- There should be no conflicts since they live in different schemas
    FOR ix_old_name, ix_new_name IN
      SELECT old.indexname, new.indexname
      FROM pg_indexes old, pg_indexes new
      WHERE old.schemaname = source_schema
        AND new.schemaname = dest_schema
        AND old.tablename = new.tablename
        AND old.tablename = tblname
        AND old.indexname <> new.indexname
        AND regexp_replace(old.indexdef, E'.*USING','') = regexp_replace(new.indexdef, E'.*USING','')
        ORDER BY old.indexdef, new.indexdef
    LOOP
      lastsql = '';
      IF bDDLOnly THEN
        RAISE INFO '%', 'ALTER INDEX ' || quote_ident(dest_schema) || '.'  || quote_ident(ix_new_name) || ' RENAME TO ' || quote_ident(ix_old_name) || ';';
      ELSE
        -- The SELECT query above may return duplicate names when a column is
        -- indexed twice the same manner with 2 different names. Therefore, to
        -- avoid a 'relation "xxx" already exists' we test if the index name
        -- is in use or free. Skipping existing index will fallback on unused
        -- ones and every duplicate will be mapped to distinct old names.
        IF NOT EXISTS (
            SELECT TRUE
            FROM pg_indexes
            WHERE schemaname = dest_schema
              AND tablename = tblname
              AND indexname = quote_ident(ix_old_name))
          AND EXISTS (
            SELECT TRUE
            FROM pg_indexes
            WHERE schemaname = dest_schema
              AND tablename = tblname
              AND indexname = quote_ident(ix_new_name))
          THEN
          EXECUTE 'ALTER INDEX ' || quote_ident(dest_schema) || '.' || quote_ident(ix_new_name) || ' RENAME TO ' || quote_ident(ix_old_name) || ';';
        END IF;
      END IF;
    END LOOP;

    IF bData THEN
      -- Insert records from source table

      -- 2021-03-03  MJV FIX
      buffer := dest_schema || '.' || quote_ident(tblname);
      
      -- Issue#86 fix:
      -- IF data_type = 'USER-DEFINED' THEN
      IF bDebug THEN RAISE NOTICE 'DEBUG: includerecs branch  table=%  data_type=%  isgenerated=%', tblname, data_type, isGenerated; END IF;
      IF data_type = 'USER-DEFINED' OR isGenerated = 'ALWAYS' THEN

        -- RAISE WARNING 'Bypassing copying rows for table (%) with user-defined data types.  You must copy them manually.', tblname;
        -- wont work --> INSERT INTO clone1.address (id2, id3, addr) SELECT cast(id2 as clone1.udt_myint), cast(id3 as clone1.udt_myint), addr FROM sample.address;
        -- Issue#101 --> INSERT INTO clone1.address2 (id2, id3, addr) SELECT id2::text::clone1.udt_myint, id3::text::clone1.udt_myint, addr FROM sample.address; 

        -- Issue#79 implementation follows        
        -- COPY sample.statuses(id, s) TO '/tmp/statuses.txt' WITH DELIMITER AS ',';
        -- COPY sample_clone1.statuses FROM '/tmp/statuses.txt' (DELIMITER ',', NULL '');
        -- Issue#101 fix: use text cast to get around the problem.
        IF bFileCopy THEN
          IF bWindows THEN
              buffer2   := 'COPY ' || quote_ident(source_schema) || '.' || quote_ident(tblname) || ' TO  ''C:\WINDOWS\TEMP\cloneschema.tmp'' WITH DELIMITER AS '','';';
              tblarray2 := tblarray2 || buffer2;
              -- Issue #81 reformat COPY command for upload
              -- buffer2:= 'COPY ' || quote_ident(dest_schema) || '.' || quote_ident(tblname) || '  FROM  ''C:\WINDOWS\TEMP\cloneschema.tmp'' (DELIMITER '','', NULL '''');';
              buffer2   := 'COPY ' || quote_ident(dest_schema) || '.' || quote_ident(tblname) || '  FROM  ''C:\WINDOWS\TEMP\cloneschema.tmp'' (DELIMITER '','', NULL ''\N'', FORMAT CSV);';
              tblarray2 := tblarray2 || buffer2;
          ELSE
              buffer2   := 'COPY ' || quote_ident(source_schema) || '.' || quote_ident(tblname) || ' TO ''/tmp/cloneschema.tmp'' WITH DELIMITER AS '','';';
              tblarray2 := tblarray2 || buffer2;
              -- Issue #81 reformat COPY command for upload
              -- buffer2   := 'COPY ' || quote_ident(dest_schema) || '.' || quote_ident(tblname) || '  FROM ''/tmp/cloneschema.tmp'' (DELIMITER '','', NULL '''');';
              -- works--> COPY sample.timestamptbl2  FROM '/tmp/cloneschema.tmp' WITH (DELIMITER ',', NULL '\N', FORMAT CSV) ;
              buffer2   := 'COPY ' || quote_ident(dest_schema) || '.' || quote_ident(tblname) || '  FROM ''/tmp/cloneschema.tmp'' (DELIMITER '','', NULL ''\N'', FORMAT CSV);';
              tblarray2 := tblarray2 || buffer2;
          END IF;
        ELSE
          -- Issue#101: assume direct copy with text cast, add to separate array
          SELECT * INTO buffer3 FROM public.get_insert_stmt_ddl(quote_ident(source_schema), quote_ident(dest_schema), quote_ident(tblname), True);
          tblarray3 := tblarray3 || buffer3;
        END IF;
      ELSE
        -- bypass child tables since we populate them when we populate the parents
        IF bDebug THEN RAISE NOTICE 'DEBUG: tblname=%  bRelispart=NA relknd=%  l_child=%  bChild=%', tblname, relknd, l_child, bChild; END IF;

        IF NOT bChild THEN
          -- Issue#75: Must defer population of tables until child tables have been added to parents
          -- Issue#101 Offer alternative of copy to/from file. Although originally intended for tables with UDTs, it is now expanded to handle all cases for performance improvement perhaps for large tables.
          -- buffer2 := 'INSERT INTO ' || buffer || buffer3 || ' SELECT * FROM ' || quote_ident(source_schema) || '.' || quote_ident(tblname) || ';';
          buffer2 := 'INSERT INTO ' || buffer || ' SELECT * FROM ' || quote_ident(source_schema) || '.' || quote_ident(tblname) || ';';
          IF bDebug THEN RAISE NOTICE 'DEBUG: buffer2=%',buffer2; END IF;
          IF bFileCopy THEN
            tblarray2:= tblarray2 || buffer2;
          ELSE
            tblarray := tblarray || buffer2;
          END IF;
        END IF;
      END IF;
    END IF;

    -- Issue#61 FIX: use set_config for empty string
    -- SET search_path = '';
    SELECT set_config('search_path', '', false) into v_dummy;

    FOR column_, default_ IN
      SELECT column_name::text,
             REPLACE(column_default::text, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.')
      FROM information_schema.COLUMNS
      WHERE table_schema = source_schema
          AND TABLE_NAME = tblname
          AND column_default LIKE 'nextval(%' || quote_ident(source_schema) || '%::regclass)'
    LOOP
      -- Issue#78 FIX: handle case-sensitive names with quote_ident() on column name
      buffer2 = 'ALTER TABLE ' || buffer || ' ALTER COLUMN ' || quote_ident(column_) || ' SET DEFAULT ' || default_ || ';';
      IF bDDLOnly THEN
        -- May need to come back and revisit this since previous sql will not return anything since no schema as created!
        RAISE INFO '%', buffer2;
      ELSE
        EXECUTE buffer2;
      END IF;
    END LOOP;
    
    EXECUTE 'SET search_path = ' || quote_ident(source_schema) ;
  END LOOP;      
  END IF;
  -- end of 90600 branch
  
  RAISE NOTICE '      TABLES cloned: %', LPAD(cnt::text, 5, ' ');

  SELECT setting INTO v_dummy FROM pg_settings WHERE name = 'search_path';
  IF bDebug THEN RAISE NOTICE 'DEBUG: search_path=%', v_dummy; END IF;

  -- Assigning sequences to table columns.
  action := 'Sequences assigning';
  cnt := 0;
  FOR object IN
    SELECT sequence_name::text
    FROM information_schema.sequences
    WHERE sequence_schema = quote_ident(source_schema)
  LOOP
    cnt := cnt + 1;
    srctbl := quote_ident(source_schema) || '.' || quote_ident(object);

    -- Get owning column, inspired from Sadique Ali post at:
    -- https://sadique.io/blog/2019/05/07/viewing-sequence-ownership-information-in-postgres/
    -- Fixed via pull request#109
    SELECT ' OWNED BY '
      || quote_ident(dest_schema)
      || '.'
      || quote_ident(dc.relname)
      || '.'
      || quote_ident(a.attname)
    INTO sq_owned
    FROM pg_class AS c
      JOIN pg_namespace n ON c.relnamespace = n.oid
      JOIN pg_depend AS d ON c.relfilenode = d.objid
      JOIN pg_class AS dc ON (
        d.refobjid = dc.relfilenode
        AND dc.relnamespace = n.oid
      )
      JOIN pg_attribute AS a ON (
        a.attnum = d.refobjsubid
        AND a.attrelid = d.refobjid
      )
    WHERE n.nspname = quote_ident(source_schema)
      AND c.relkind = 'S'
      AND c.relname = object;

    IF sq_owned IS NOT NULL THEN
      qry := 'ALTER SEQUENCE '
        || quote_ident(dest_schema)
        || '.'
        || quote_ident(object)
        || sq_owned
        || ';';

      IF bDDLOnly THEN
        RAISE NOTICE 'DEBUG: %',qry;
        RAISE INFO '%', qry;
      ELSE
        EXECUTE qry;
      END IF;

    END IF;

  END LOOP;
  RAISE NOTICE '    SEQUENCES set:   %', LPAD(cnt::text, 5, ' ');

  -- Update IDENTITY sequences to the last value, bypass 9.6 versions
  IF sq_server_version_num > 90624 THEN
      action := 'Identity updating';
      cnt := 0;
      FOR object, sq_last_value IN
        SELECT sequencename::text, COALESCE(last_value, -999) from pg_sequences where schemaname = quote_ident(source_schema)
        AND NOT EXISTS
        (select 1 from information_schema.sequences where sequence_schema = quote_ident(source_schema) and sequence_name = sequencename)
      LOOP
        IF sq_last_value = -999 THEN
          continue;
        END IF;
        cnt := cnt + 1;
        buffer := quote_ident(dest_schema) || '.' || quote_ident(object);
        IF bData THEN
          EXECUTE 'SELECT setval( ''' || buffer || ''', ' || sq_last_value || ', ' || sq_is_called || ');' ;
        ELSE
          if bDDLOnly THEN
            -- fix#63
            RAISE INFO '%', 'SELECT setval( ''' || buffer || ''', ' || sq_last_value || ', ' || sq_is_called || ');' ;
          ELSE
            -- fix#63
            EXECUTE 'SELECT setval( ''' || buffer || ''', ' || sq_last_value || ', ' || sq_is_called || ');' ;
          END IF;
        END IF;
      END LOOP;
      -- Fixed Issue#107: set lpad from 2 to 5
      RAISE NOTICE '   IDENTITIES set:   %', LPAD(cnt::text, 5, ' ');
  ELSE
    -- Fixed Issue#107: set lpad from 2 to 5
    RAISE NOTICE '   IDENTITIES set:   %', LPAD('-1'::text, 5, ' ');    
  END IF;

  -- Issue#78 forces us to defer FKeys until the end since we previously did row copies before FKeys
  --  add FK constraint
  -- action := 'FK Constraints';

  -- Issue#62: Add comments on indexes, and then removed them from here and reworked later below.

  -- Issue 90: moved functions to here, before views or MVs that might use them
  -- Create functions
    action := 'Functions';
    cnt := 0;
    -- MJV FIX per issue# 34
    -- SET search_path = '';
    EXECUTE 'SET search_path = ' || quote_ident(source_schema) ;
    
    -- Fixed Issue#65
    -- Fixed Issue#97
    -- FOR func_oid IN SELECT oid FROM pg_proc WHERE pronamespace = src_oid AND prokind != 'a'
    IF is_prokind THEN
      FOR func_oid, func_owner, func_name, func_args, func_argno, buffer3 IN 
          SELECT p.oid, pg_catalog.pg_get_userbyid(p.proowner), p.proname, oidvectortypes(p.proargtypes), p.pronargs,
          CASE WHEN prokind = 'p' THEN 'PROCEDURE' WHEN prokind = 'f' THEN 'FUNCTION' ELSE '' END 
          FROM pg_proc p WHERE p.pronamespace = src_oid AND p.prokind != 'a'          
      LOOP
        cnt := cnt + 1;
        SELECT pg_get_functiondef(func_oid)
        INTO qry;
  
        SELECT replace(qry, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.') INTO dest_qry;
        IF bDDLOnly THEN
          RAISE INFO '%;', dest_qry;
          -- Issue#91 Fix
          -- issue#95 
          IF NOT bNoOwner THEN
            IF func_argno = 0 THEN
                -- Fixed Issue#108: double-quote roles in case they have special characters
                RAISE INFO 'ALTER % %() OWNER TO %', buffer3, quote_ident(dest_schema) || '.' || quote_ident(func_name), '"' || func_owner || '";';
            ELSE
                -- Fixed Issue#108: double-quote roles in case they have special characters
                RAISE INFO 'ALTER % % OWNER TO %', buffer3, quote_ident(dest_schema) || '.' || quote_ident(func_name) || '(' || func_args || ')', '"' || func_owner || '";';
            END IF;
          END IF;
        ELSE
          IF bDebug THEN RAISE NOTICE 'DEBUG: %', dest_qry; END IF;
          EXECUTE dest_qry;

          -- Issue#91 Fix
          -- issue#95 
          IF NOT bNoOwner THEN
            IF func_argno = 0 THEN
                -- Fixed Issue#108: double-quote roles in case they have special characters
                dest_qry = 'ALTER ' || buffer3 || ' ' || quote_ident(dest_schema) || '.' || quote_ident(func_name) || '() OWNER TO ' || '"' || func_owner || '";';
            ELSE
                -- Fixed Issue#108: double-quote roles in case they have special characters
                dest_qry = 'ALTER ' || buffer3 || ' ' || quote_ident(dest_schema) || '.' || quote_ident(func_name) || '(' || func_args || ') OWNER TO ' || '"' || func_owner || '";';
            END IF;
          END IF;
          EXECUTE dest_qry;
        END IF;
      END LOOP;
    ELSE
      FOR func_oid IN SELECT oid
                      FROM pg_proc
                      WHERE pronamespace = src_oid AND not proisagg
      LOOP
        cnt := cnt + 1;
        SELECT pg_get_functiondef(func_oid) INTO qry;
        SELECT replace(qry, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.') INTO dest_qry;
        IF bDDLOnly THEN
          RAISE INFO '%;', dest_qry;
        ELSE
          EXECUTE dest_qry;
        END IF;
      END LOOP;
    END IF;
  
    -- Create aggregate functions.
    -- Fixed Issue#65
    -- FOR func_oid IN SELECT oid FROM pg_proc WHERE pronamespace = src_oid AND prokind = 'a'
    IF is_prokind THEN
      FOR func_oid IN
          SELECT oid
          FROM pg_proc
          WHERE pronamespace = src_oid AND prokind = 'a'
      LOOP
        cnt := cnt + 1;
        SELECT
          'CREATE AGGREGATE '
          || dest_schema
          || '.'
          || p.proname
          || '('
          -- || format_type(a.aggtranstype, NULL)
          -- Issue#65 Fixes for specific datatype mappings
          || CASE WHEN format_type(a.aggtranstype, NULL) = 'double precision[]' THEN 'float8'
                  WHEN format_type(a.aggtranstype, NULL) = 'anyarray'           THEN 'anyelement'
             ELSE format_type(a.aggtranstype, NULL) END
          || ') (sfunc = '
          || regexp_replace(a.aggtransfn::text, '(^|\W)' || quote_ident(source_schema) || '\.', '\1' || quote_ident(dest_schema) || '.')
          || ', stype = '
          -- || format_type(a.aggtranstype, NULL)
          -- Issue#65 Fixes for specific datatype mappings
          || CASE WHEN format_type(a.aggtranstype, NULL) = 'double precision[]' THEN 'float8[]' ELSE format_type(a.aggtranstype, NULL) END
          || CASE
              WHEN op.oprname IS NULL THEN ''
              ELSE ', sortop = ' || op.oprname
            END
          || CASE
              WHEN a.agginitval IS NULL THEN ''
              ELSE ', initcond = ''' || a.agginitval || ''''
            END
          || ')'
        INTO dest_qry
        FROM pg_proc p
        JOIN pg_aggregate a ON a.aggfnoid = p.oid
        LEFT JOIN pg_operator op ON op.oid = a.aggsortop
        WHERE p.oid = func_oid;
  
        IF bDDLOnly THEN
          RAISE INFO '%;', dest_qry;
        ELSE
          EXECUTE dest_qry;
        END IF;
  
      END LOOP;
      RAISE NOTICE '   FUNCTIONS cloned: %', LPAD(cnt::text, 5, ' ');
  
    ELSE
      FOR func_oid IN SELECT oid FROM pg_proc WHERE pronamespace = src_oid AND proisagg
      LOOP
        cnt := cnt + 1;
        SELECT
          'CREATE AGGREGATE '
          || dest_schema
          || '.'
          || p.proname
          || '('
          -- || format_type(a.aggtranstype, NULL)
          -- Issue#65 Fixes for specific datatype mappings
          || CASE WHEN format_type(a.aggtranstype, NULL) = 'double precision[]' THEN 'float8'
                  WHEN format_type(a.aggtranstype, NULL) = 'anyarray'           THEN 'anyelement'
             ELSE format_type(a.aggtranstype, NULL) END
          || ') (sfunc = '
          || regexp_replace(a.aggtransfn::text, '(^|\W)' || quote_ident(source_schema) || '\.', '\1' || quote_ident(dest_schema) || '.')
          || ', stype = '
          -- || format_type(a.aggtranstype, NULL)
          -- Issue#65 Fixes for specific datatype mappings
          || CASE WHEN format_type(a.aggtranstype, NULL) = 'double precision[]' THEN 'float8[]' ELSE format_type(a.aggtranstype, NULL) END
          || CASE
              WHEN op.oprname IS NULL THEN ''
              ELSE ', sortop = ' || op.oprname
            END
          || CASE
              WHEN a.agginitval IS NULL THEN ''
              ELSE ', initcond = ''' || a.agginitval || ''''
            END
          || ')'
        INTO dest_qry
        FROM pg_proc p
        JOIN pg_aggregate a ON a.aggfnoid = p.oid
        LEFT JOIN pg_operator op ON op.oid = a.aggsortop
        WHERE p.oid = func_oid;
  
        IF bDDLOnly THEN
          RAISE INFO '%;', dest_qry;
        ELSE
          EXECUTE dest_qry;
        END IF;
  
      END LOOP;
      RAISE NOTICE '   FUNCTIONS cloned: %', LPAD(cnt::text, 5, ' ');
    END IF;
  
  -- Create views
  action := 'Views';

  -- Issue#61 FIX: use set_config for empty string
  -- MJV FIX #43: also had to reset search_path from source schema to empty.
  -- SET search_path = '';
  SELECT set_config('search_path', '', false)
  INTO v_dummy;

  cnt := 0;
  --FOR object IN
    -- SELECT table_name::text, view_definition
    -- FROM information_schema.views
    -- WHERE table_schema = quote_ident(source_schema)

  -- Issue#73 replace loop query to handle dependencies
  -- Issue#91 get view_owner
  FOR srctbl, aname, view_owner, object IN
    WITH RECURSIVE views AS (
       SELECT n.nspname as schemaname, v.relname as tablename, v.oid::regclass AS viewname,
              v.relkind = 'm' AS is_materialized, pg_catalog.pg_get_userbyid(v.relowner) as owner, 
              1 AS level
       FROM pg_depend AS d
          JOIN pg_rewrite AS r
             ON r.oid = d.objid
          JOIN pg_class AS v
             ON v.oid = r.ev_class
          JOIN pg_namespace n
             ON n.oid = v.relnamespace
       -- WHERE v.relkind IN ('v', 'm')
       WHERE v.relkind IN ('v')
         AND d.classid = 'pg_rewrite'::regclass
         AND d.refclassid = 'pg_class'::regclass
         AND d.deptype = 'n'
    UNION
       -- add the views that depend on these
       SELECT n.nspname as schemaname, v.relname as tablename, v.oid::regclass AS viewname,
              v.relkind = 'm', pg_catalog.pg_get_userbyid(v.relowner) as owner, 
              views.level + 1
       FROM views
          JOIN pg_depend AS d
             ON d.refobjid = views.viewname
          JOIN pg_rewrite AS r
             ON r.oid = d.objid
          JOIN pg_class AS v
             ON v.oid = r.ev_class
          JOIN pg_namespace n
             ON n.oid = v.relnamespace
       -- WHERE v.relkind IN ('v', 'm')
       WHERE v.relkind IN ('v')
         AND d.classid = 'pg_rewrite'::regclass
             AND d.refclassid = 'pg_class'::regclass
         AND d.deptype = 'n'
         AND v.oid <> views.viewname
    )
    SELECT tablename, viewname, owner, format('CREATE OR REPLACE%s VIEW %s AS%s',
                  CASE WHEN is_materialized
                       THEN ' MATERIALIZED'
                       ELSE ''
                  END,
                  viewname,
                  pg_get_viewdef(viewname))
    FROM views
    WHERE schemaname = quote_ident(source_schema)
    GROUP BY schemaname, tablename, viewname, owner, is_materialized
    ORDER BY max(level), schemaname, tablename
  LOOP
    cnt := cnt + 1;
    -- Issue#73 replace logic based on new loop sql
    buffer := quote_ident(dest_schema) || '.' || quote_ident(aname);
    -- MJV FIX: #43
    -- SELECT view_definition INTO v_def
    -- SELECT REPLACE(view_definition, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.') INTO v_def
    -- FROM information_schema.views
    -- WHERE table_schema = quote_ident(source_schema)
    --   AND table_name = quote_ident(object);
    SELECT REPLACE(object, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.') INTO v_def;
    -- NOTE: definition already includes the closing statement semicolon
    SELECT REPLACE(aname, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.') INTO buffer3;
    IF bDDLOnly THEN
      RAISE INFO '%', v_def;
      -- Issue#91 Fix
      -- issue#95 
      IF NOT bNoOwner THEN
        -- Fixed Issue#108: double-quote roles in case they have special characters
        -- RAISE INFO 'ALTER TABLE % OWNER TO %', buffer3, view_owner || ';';
        RAISE INFO 'ALTER TABLE % OWNER TO %', buffer3, '"' ||view_owner || '";';
      END IF;        
    ELSE
      -- EXECUTE 'CREATE OR REPLACE VIEW ' || buffer || ' AS ' || v_def;
      EXECUTE v_def;
      -- Issue#73: commented out comment logic for views since we do it elsewhere now.
      -- Issue#91 Fix
      -- issue#95 
      IF NOT bNoOwner THEN      
        -- Fixed Issue#108: double-quote roles in case they have special characters
        v_def = 'ALTER TABLE ' || buffer3 || ' OWNER TO ' || '"' || view_owner || '";';
        EXECUTE v_def;
      END IF;
    END IF;
  END LOOP;
  RAISE NOTICE '       VIEWS cloned: %', LPAD(cnt::text, 5, ' ');

  -- Create Materialized views
  action := 'Mat. Views';
  cnt := 0;
  -- Issue#91 get view_owner
  FOR object, view_owner, v_def IN
      SELECT matviewname::text, '"' || matviewowner::text || '"', replace(definition,';','') FROM pg_catalog.pg_matviews WHERE schemaname = quote_ident(source_schema)
  LOOP
      cnt := cnt + 1;
      -- Issue#78 FIX: handle case-sensitive names with quote_ident() on target schema and object
      buffer := quote_ident(dest_schema) || '.' || quote_ident(object);

      -- MJV FIX: #72 remove source schema in MV def
      SELECT REPLACE(v_def, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.') INTO buffer2;

      IF bData THEN
        -- issue#98 defer creation until after regular tables are populated. Also defer the ownership as well.
        -- EXECUTE 'CREATE MATERIALIZED VIEW ' || buffer || ' AS ' || buffer2 || ' WITH DATA;' ;
        buffer3 = 'CREATE MATERIALIZED VIEW ' || buffer || ' AS ' || buffer2 || ' WITH DATA;';
        mvarray := mvarray || buffer3;
        
        -- issue#95 
        IF NOT bNoOwner THEN      
          -- buffer3 = 'ALTER MATERIALIZED VIEW ' || buffer || ' OWNER TO ' || view_owner || ';' ;
          -- EXECUTE buffer3;
          -- Fixed Issue#108: double-quote roles in case they have special characters
          buffer3 = 'ALTER MATERIALIZED VIEW ' || buffer || ' OWNER TO ' || view_owner || ';' ;
          mvarray := mvarray || buffer3;
        END IF;
      ELSE
        IF bDDLOnly THEN
          RAISE INFO '%', 'CREATE MATERIALIZED VIEW ' || buffer || ' AS ' || buffer2 || ' WITH NO DATA;' ;
          -- Issue#91
          -- issue#95 
          IF NOT bNoOwner THEN      
            -- Fixed Issue#108: double-quote roles in case they have special characters
            RAISE INFO '%', 'ALTER MATERIALIZED VIEW ' || buffer || ' OWNER TO ' || view_owner || ';' ;
          END IF;
        ELSE
          EXECUTE 'CREATE MATERIALIZED VIEW ' || buffer || ' AS ' || buffer2 || ' WITH NO DATA;' ;
          -- Issue#91
          -- issue#95 
          IF NOT bNoOwner THEN      
            -- Fixed Issue#108: double-quote roles in case they have special characters
            buffer3 = 'ALTER MATERIALIZED VIEW ' || buffer || ' OWNER TO ' || view_owner || ';' ;
            EXECUTE buffer3;
          END IF;
        END IF;
      END IF;
      SELECT coalesce(obj_description(oid), '') into adef from pg_class where relkind = 'm' and relname = object;
      IF adef <> '' THEN
        IF bDDLOnly THEN
          RAISE INFO '%', 'COMMENT ON MATERIALIZED VIEW ' || quote_ident(dest_schema) || '.' || object || ' IS ''' || adef || ''';';
        ELSE
          -- Issue#$98: also defer if copy rows is on since we defer MVIEWS in that case
          IF bData THEN
            buffer3 = 'COMMENT ON MATERIALIZED VIEW ' || quote_ident(dest_schema) || '.' || object || ' IS ''' || adef || ''';';
            mvarray = mvarray || buffer3;
          ELSE
            EXECUTE 'COMMENT ON MATERIALIZED VIEW ' || quote_ident(dest_schema) || '.' || object || ' IS ''' || adef || ''';';
          END IF;
          
        END IF;
      END IF;

      FOR aname, adef IN
        SELECT indexname, replace(indexdef, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.') as newdef FROM pg_indexes where schemaname = quote_ident(source_schema) and tablename = object order by indexname
      LOOP
        IF bDDLOnly THEN
          RAISE INFO '%', adef || ';';
        ELSE
          EXECUTE adef || ';';
        END IF;
      END LOOP;

  END LOOP;
  RAISE NOTICE '   MAT VIEWS cloned: %', LPAD(cnt::text, 5, ' ');

  -- Issue 90 Move create functions to before views
  
  -- MV: Create Triggers

  -- MJV FIX: #38
  -- EXECUTE 'SET search_path = ' || quote_ident(source_schema) ;

  -- Issue#61 FIX: use set_config for empty string
  -- SET search_path = '';
  SELECT set_config('search_path', '', false) into v_dummy;

  action := 'Triggers';
  cnt := 0;
  FOR arec IN
    -- 2021-03-09 MJV FIX: #40 fixed sql to get the def using pg_get_triggerdef() sql
    SELECT n.nspname, c.relname, t.tgname, p.proname, REPLACE(pg_get_triggerdef(t.oid), quote_ident(source_schema), quote_ident(dest_schema)) || ';' AS trig_ddl
    FROM pg_trigger t, pg_class c, pg_namespace n, pg_proc p
    WHERE n.nspname = quote_ident(source_schema)
      AND n.oid = c.relnamespace
      AND c.relkind in ('r','p')
      AND n.oid = p.pronamespace
      AND c.oid = t.tgrelid
      AND p.oid = t.tgfoid
      ORDER BY c.relname, t.tgname
  LOOP
    BEGIN
      cnt := cnt + 1;
      IF bDDLOnly THEN
        RAISE INFO '%', arec.trig_ddl;
      ELSE
        EXECUTE arec.trig_ddl;
      END IF;

    END;
  END LOOP;
  RAISE NOTICE '    TRIGGERS cloned: %', LPAD(cnt::text, 5, ' ');


  -- MV: Create Rules
  -- Fixes Issue#59 Implement Rules
  action := 'Rules';
  cnt := 0;
  FOR arec IN
    SELECT regexp_replace(definition, E'[\\n\\r]+', ' ', 'g' ) as definition
    FROM pg_rules
    WHERE schemaname = quote_ident(source_schema)
  LOOP
    cnt := cnt + 1;
    buffer := REPLACE(arec.definition, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.');
    IF bDDLOnly THEN
      RAISE INFO '%', buffer;
    ELSE
      EXECUTE buffer;
    END IF;
  END LOOP;
  RAISE NOTICE '    RULES    cloned: %', LPAD(cnt::text, 5, ' ');


  -- MV: Create Policies
  -- Fixes Issue#66 Implement Security policies for RLS
  action := 'Policies';
  cnt := 0;
  -- #106 Handle 9.6 which doesn't have "permissive"
  IF sq_server_version_num > 90624 THEN
    FOR arec IN
      -- Issue#78 FIX: handle case-sensitive names with quote_ident() on policy, tablename
      SELECT schemaname as schemaname, tablename as tablename, 'CREATE POLICY ' || policyname || ' ON ' || quote_ident(dest_schema) || '.' || quote_ident(tablename) || ' AS ' || permissive || ' FOR ' || cmd || ' TO '
      ||  array_to_string(roles, ',', '*') || ' USING (' || regexp_replace(qual, E'[\\n\\r]+', ' ', 'g' ) || ')'
      || CASE WHEN with_check IS NOT NULL THEN ' WITH CHECK (' ELSE '' END || coalesce(with_check, '') || CASE WHEN with_check IS NOT NULL THEN ');' ELSE ';' END as definition
      FROM pg_policies
      WHERE schemaname = quote_ident(source_schema)
      ORDER BY policyname
    LOOP
      cnt := cnt + 1;
      IF bDDLOnly THEN
        RAISE INFO '%', arec.definition;
      ELSE
        EXECUTE arec.definition;
      END IF;
    
      -- Issue#76: Enable row security if indicated
      SELECT c.relrowsecurity INTO abool FROM pg_class c, pg_namespace n where n.nspname = quote_ident(arec.schemaname) AND n.oid = c.relnamespace AND c.relname = quote_ident(arec.tablename) and c.relkind = 'r';
      IF abool THEN
        buffer = 'ALTER TABLE ' || dest_schema || '.' || arec.tablename || ' ENABLE ROW LEVEL SECURITY;';
        IF bDDLOnly THEN
          RAISE INFO '%', buffer;
        ELSE
          EXECUTE buffer;
        END IF;
      END IF;
    END LOOP;
  ELSE
    -- handle 9.6 versions
    FOR arec IN
      -- Issue#78 FIX: handle case-sensitive names with quote_ident() on policy, tablename
      SELECT schemaname as schemaname, tablename as tablename, 'CREATE POLICY ' || policyname || ' ON ' || quote_ident(dest_schema) || '.' || quote_ident(tablename) || ' FOR ' || cmd || ' TO '
      ||  array_to_string(roles, ',', '*') || ' USING (' || regexp_replace(qual, E'[\\n\\r]+', ' ', 'g' ) || ')'
      || CASE WHEN with_check IS NOT NULL THEN ' WITH CHECK (' ELSE '' END || coalesce(with_check, '') || CASE WHEN with_check IS NOT NULL THEN ');' ELSE ';' END as definition
      FROM pg_policies
      WHERE schemaname = quote_ident(source_schema)
      ORDER BY policyname
    LOOP
      cnt := cnt + 1;
      IF bDDLOnly THEN
        RAISE INFO '%', arec.definition;
      ELSE
        EXECUTE arec.definition;
      END IF;
    
      -- Issue#76: Enable row security if indicated
      SELECT c.relrowsecurity INTO abool FROM pg_class c, pg_namespace n where n.nspname = quote_ident(arec.schemaname) AND n.oid = c.relnamespace AND c.relname = quote_ident(arec.tablename) and c.relkind = 'r';
      IF abool THEN
        buffer = 'ALTER TABLE ' || dest_schema || '.' || arec.tablename || ' ENABLE ROW LEVEL SECURITY;';
        IF bDDLOnly THEN
          RAISE INFO '%', buffer;
        ELSE
          EXECUTE buffer;
        END IF;
      END IF;
    END LOOP;  
  END IF;
  RAISE NOTICE '    POLICIES cloned: %', LPAD(cnt::text, 5, ' ');


  -- MJV Fixed #62 for comments (PASS 1)
  action := 'Comments1';
  cnt := 0;
  FOR qry IN
    -- Issue#74 Fix: Change schema from source to target. Also, do not include comments on foreign tables since we do not clone foreign tables at this time.
    SELECT 'COMMENT ON ' || CASE WHEN c.relkind in ('r','p') AND a.attname IS NULL THEN 'TABLE ' WHEN c.relkind in ('r','p') AND
    a.attname IS NOT NULL THEN 'COLUMN ' WHEN c.relkind = 'f' THEN 'FOREIGN TABLE ' WHEN c.relkind = 'm' THEN 'MATERIALIZED VIEW ' WHEN c.relkind = 'v' THEN 'VIEW '
    WHEN c.relkind = 'i' THEN 'INDEX ' WHEN c.relkind = 'S' THEN 'SEQUENCE ' ELSE 'XX' END || quote_ident(dest_schema) || '.' || CASE WHEN c.relkind in ('r','p') AND
    -- Issue#78: handle case-sensitive names with quote_ident()
    a.attname IS NOT NULL THEN quote_ident(c.relname) || '.' || a.attname ELSE quote_ident(c.relname) END ||
    -- Issue#74 Fix
    -- ' IS ''' || d.description || ''';' as ddl
    ' IS '   || quote_literal(d.description) || ';' as ddl
    FROM pg_class c
    JOIN pg_namespace n ON (n.oid = c.relnamespace)
    LEFT JOIN pg_description d ON (c.oid = d.objoid)
    LEFT JOIN pg_attribute a ON (c.oid = a.attrelid
      AND a.attnum > 0 and a.attnum = d.objsubid)
    WHERE c.relkind <> 'f' AND d.description IS NOT NULL AND n.nspname = quote_ident(source_schema)
    ORDER BY ddl
  LOOP
    cnt := cnt + 1;
    
    -- BAD : "COMMENT ON SEQUENCE sample_clone2.CaseSensitive_ID_seq IS 'just a comment on CaseSensitive sequence';"
    -- GOOD: "COMMENT ON SEQUENCE "CaseSensitive_ID_seq" IS 'just a comment on CaseSensitive sequence';"
    
    -- Issue#98 For MVs we create comments when we create the MVs
    IF substring(qry,1,28) = 'COMMENT ON MATERIALIZED VIEW' THEN
      IF bDebug THEN RAISE NOTICE 'DEBUG: deferring comments on MVs'; END IF;
      cnt = cnt - 1;
      continue;
    END IF;
    
    IF bDDLOnly THEN
      RAISE INFO '%', qry;
    ELSE
      EXECUTE qry;
    END IF;
  END LOOP;
  RAISE NOTICE ' COMMENTS(1) cloned: %', LPAD(cnt::text, 5, ' ');

  -- MJV Fixed #62 for comments (PASS 2)
  action := 'Comments2';
  cnt2 := 0;
  IF is_prokind THEN
  FOR qry IN
    -- Issue#74 Fix: Change schema from source to target.
    SELECT 'COMMENT ON SCHEMA ' || dest_schema ||
    -- Issue#74 Fix
    -- ' IS ''' || d.description || ''';' as ddl
    ' IS '   || quote_literal(d.description) || ';' as ddl
    from pg_namespace n, pg_description d where d.objoid = n.oid and n.nspname = quote_ident(source_schema)
    UNION
    -- Issue#74 Fix: need to replace source schema inline
    -- SELECT 'COMMENT ON TYPE ' || pg_catalog.format_type(t.oid, NULL) || ' IS ''' || pg_catalog.obj_description(t.oid, 'pg_type') || ''';' as ddl
    SELECT 'COMMENT ON TYPE ' || REPLACE(pg_catalog.format_type(t.oid, NULL), quote_ident(source_schema), quote_ident(dest_schema)) || ' IS ''' || pg_catalog.obj_description(t.oid, 'pg_type') || ''';' as ddl
    FROM pg_catalog.pg_type t
    JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
    WHERE (t.typrelid = 0 OR (SELECT c.relkind = 'c' FROM pg_catalog.pg_class c WHERE c.oid = t.typrelid))
      AND NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type el WHERE el.oid = t.typelem AND el.typarray = t.oid)
      AND n.nspname = quote_ident(source_schema) COLLATE pg_catalog.default
      AND pg_catalog.obj_description(t.oid, 'pg_type') IS NOT NULL and t.typtype = 'c'
    UNION
    -- Issue#78: handle case-sensitive names with quote_ident()
    SELECT 'COMMENT ON COLLATION ' || quote_ident(dest_schema) || '.' || quote_ident(c.collname) || ' IS ''' || pg_catalog.obj_description(c.oid, 'pg_collation') || ''';' as ddl
    FROM pg_catalog.pg_collation c, pg_catalog.pg_namespace n
    WHERE n.oid = c.collnamespace AND c.collencoding IN (-1, pg_catalog.pg_char_to_encoding(pg_catalog.getdatabaseencoding()))
      AND n.nspname = quote_ident(source_schema) COLLATE pg_catalog.default AND pg_catalog.obj_description(c.oid, 'pg_collation') IS NOT NULL
    UNION
    SELECT 'COMMENT ON ' || CASE WHEN p.prokind = 'f' THEN 'FUNCTION ' WHEN p.prokind = 'p' THEN 'PROCEDURE ' WHEN p.prokind = 'a' THEN 'AGGREGATE ' END ||
    dest_schema || '.' || p.proname || ' (' || oidvectortypes(p.proargtypes) || ')'
    -- Issue#74 Fix
    -- ' IS ''' || d.description || ''';' as ddl
    ' IS '   || quote_literal(d.description) || ';' as ddl
    FROM pg_catalog.pg_namespace n
    JOIN pg_catalog.pg_proc p ON p.pronamespace = n.oid
    JOIN pg_description d ON (d.objoid = p.oid)
    WHERE n.nspname = quote_ident(source_schema)
    UNION
    SELECT 'COMMENT ON POLICY ' || p1.policyname || ' ON ' || dest_schema || '.' || p1.tablename ||
    -- Issue#74 Fix
    -- ' IS ''' || d.description || ''';' as ddl
    ' IS '   || quote_literal(d.description) || ';' as ddl
    FROM pg_policies p1, pg_policy p2, pg_class c, pg_namespace n, pg_description d
    WHERE p1.schemaname = n.nspname AND p1.tablename = c.relname AND n.oid = c.relnamespace
      AND c.relkind in ('r','p') AND p1.policyname = p2.polname AND d.objoid = p2.oid AND p1.schemaname = quote_ident(source_schema)
    UNION
    SELECT 'COMMENT ON DOMAIN ' || dest_schema || '.' || t.typname ||
    -- Issue#74 Fix
    -- ' IS ''' || d.description || ''';' as ddl
    ' IS '   || quote_literal(d.description) || ';' as ddl
    FROM pg_catalog.pg_type t
    LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
    JOIN pg_catalog.pg_description d ON d.classoid = t.tableoid AND d.objoid = t.oid AND d.objsubid = 0
    WHERE t.typtype = 'd' AND n.nspname = quote_ident(source_schema) COLLATE pg_catalog.default
    ORDER BY 1
  LOOP
    cnt2 := cnt2 + 1;
    IF bDDLOnly THEN
      RAISE INFO '%', qry;
    ELSE
      EXECUTE qry;
    END IF;
  END LOOP;
  ELSE -- must be v 10 or less
  FOR qry IN
    -- Issue#74 Fix: Change schema from source to target.
    SELECT 'COMMENT ON SCHEMA ' || dest_schema ||
    -- Issue#74 Fix
    -- ' IS ''' || d.description || ''';' as ddl
    ' IS '   || quote_literal(d.description) || ';' as ddl
    from pg_namespace n, pg_description d where d.objoid = n.oid and n.nspname = quote_ident(source_schema)
    UNION
    -- Issue#74 Fix: need to replace source schema inline
    -- SELECT 'COMMENT ON TYPE ' || pg_catalog.format_type(t.oid, NULL) || ' IS ''' || pg_catalog.obj_description(t.oid, 'pg_type') || ''';' as ddl
    SELECT 'COMMENT ON TYPE ' || REPLACE(pg_catalog.format_type(t.oid, NULL), quote_ident(source_schema), quote_ident(dest_schema)) || ' IS ''' || pg_catalog.obj_description(t.oid, 'pg_type') || ''';' as ddl
    FROM pg_catalog.pg_type t
    JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
    WHERE (t.typrelid = 0 OR (SELECT c.relkind = 'c'
                              FROM pg_catalog.pg_class c
                              WHERE c.oid = t.typrelid))
      AND NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type el
                     WHERE el.oid = t.typelem AND el.typarray = t.oid)
      AND n.nspname = quote_ident(source_schema) COLLATE pg_catalog.default
      AND pg_catalog.obj_description(t.oid, 'pg_type') IS NOT NULL and t.typtype = 'c'
    UNION
    -- FIX Isse#87 by adding double quotes around collation name
    SELECT 'COMMENT ON COLLATION ' || dest_schema || '."' || c.collname || '" IS ''' || pg_catalog.obj_description(c.oid, 'pg_collation') || ''';' as ddl
    FROM pg_catalog.pg_collation c, pg_catalog.pg_namespace n
    WHERE n.oid = c.collnamespace AND c.collencoding IN (-1, pg_catalog.pg_char_to_encoding(pg_catalog.getdatabaseencoding()))
      AND n.nspname = quote_ident(source_schema) COLLATE pg_catalog.default AND pg_catalog.obj_description(c.oid, 'pg_collation') IS NOT NULL
    UNION
    SELECT 'COMMENT ON ' || CASE WHEN proisagg THEN 'AGGREGATE ' ELSE 'FUNCTION ' END ||
    dest_schema || '.' || p.proname || ' (' || oidvectortypes(p.proargtypes) || ')'
    -- Issue#74 Fix
    -- ' IS ''' || d.description || ''';' as ddl
    ' IS '   || quote_literal(d.description) || ';' as ddl
    FROM pg_catalog.pg_namespace n
    JOIN pg_catalog.pg_proc p ON p.pronamespace = n.oid
    JOIN pg_description d ON (d.objoid = p.oid)
    WHERE n.nspname = quote_ident(source_schema)
    UNION
    SELECT 'COMMENT ON POLICY ' || p1.policyname || ' ON ' || dest_schema || '.' || p1.tablename ||
    -- Issue#74 Fix
    -- ' IS ''' || d.description || ''';' as ddl
    ' IS '   || quote_literal(d.description) || ';' as ddl
    FROM pg_policies p1, pg_policy p2, pg_class c, pg_namespace n, pg_description d
    WHERE p1.schemaname = n.nspname AND p1.tablename = c.relname AND n.oid = c.relnamespace
      AND c.relkind in ('r','p') AND p1.policyname = p2.polname AND d.objoid = p2.oid AND p1.schemaname = quote_ident(source_schema)
    UNION
    SELECT 'COMMENT ON DOMAIN ' || dest_schema || '.' || t.typname ||
    -- Issue#74 Fix
    -- ' IS ''' || d.description || ''';' as ddl
    ' IS '   || quote_literal(d.description) || ';' as ddl
    FROM pg_catalog.pg_type t
    LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
    JOIN pg_catalog.pg_description d ON d.classoid = t.tableoid AND d.objoid = t.oid AND d.objsubid = 0
    WHERE t.typtype = 'd' AND n.nspname = quote_ident(source_schema) COLLATE pg_catalog.default
    ORDER BY 1
  LOOP
    cnt2 := cnt2 + 1;
    IF bDDLOnly THEN
      RAISE INFO '%', qry;
    ELSE
      EXECUTE qry;
    END IF;
  END LOOP;
  END IF;
  RAISE NOTICE ' COMMENTS(2) cloned: %', LPAD(cnt2::text, 5, ' ');


  -- Issue#95 bypass if No ACL specified.
  IF NOT bNoACL THEN
    -- ---------------------
    -- MV: Permissions: Defaults
    -- ---------------------
    EXECUTE 'SET search_path = ' || quote_ident(source_schema) ;
    action := 'PRIVS: Defaults';
    cnt := 0;
    FOR arec IN
      SELECT pg_catalog.pg_get_userbyid(d.defaclrole) AS "owner", n.nspname AS schema,
      CASE d.defaclobjtype WHEN 'r' THEN 'table' WHEN 'S' THEN 'sequence' WHEN 'f' THEN 'function' WHEN 'T' THEN 'type' WHEN 'n' THEN 'schema' END AS atype,
      d.defaclacl as defaclacl, pg_catalog.array_to_string(d.defaclacl, ',') as defaclstr
      FROM pg_catalog.pg_default_acl d LEFT JOIN pg_catalog.pg_namespace n ON (n.oid = d.defaclnamespace)
      WHERE n.nspname IS NOT NULL AND n.nspname = quote_ident(source_schema)
      ORDER BY 3, 2, 1
    LOOP
      BEGIN
        -- RAISE NOTICE ' owner=%  type=%  defaclacl=%  defaclstr=%', arec.owner, arec.atype, arec.defaclacl, arec.defaclstr;

        FOREACH aclstr IN ARRAY arec.defaclacl
        LOOP
            cnt := cnt + 1;
            -- RAISE NOTICE ' aclstr=%', aclstr;
            -- break up into grantor, grantee, and privs, mydb_update=rwU/mydb_owner
            SELECT split_part(aclstr, '=',1) INTO grantee;
            SELECT split_part(aclstr, '=',2) INTO grantor;
            SELECT split_part(grantor, '/',1) INTO privs;
            SELECT split_part(grantor, '/',2) INTO grantor;
            -- RAISE NOTICE ' grantor=%  grantee=%  privs=%', grantor, grantee, privs;

            IF arec.atype = 'function' THEN
              -- Just having execute is enough to grant all apparently.
              buffer := 'ALTER DEFAULT PRIVILEGES FOR ROLE ' || grantor || ' IN SCHEMA ' || quote_ident(dest_schema) || ' GRANT ALL ON FUNCTIONS TO "' || grantee || '";';
            
              -- Issue#92 Fix
              -- set role = cm_stage_ro_grp;
              -- ALTER DEFAULT PRIVILEGES FOR ROLE cm_stage_ro_grp IN SCHEMA cm_stage GRANT REFERENCES, TRIGGER ON TABLES TO cm_stage_ro_grp;
              IF grantor = grantee THEN
                  -- append set role to statement
                  buffer = 'SET ROLE = ' || grantor || '; ' || buffer;
              END IF;
            
              IF bDDLOnly THEN
                RAISE INFO '%', buffer;
              ELSE
                EXECUTE buffer;
              END IF;
              -- Issue#92 Fix:
              EXECUTE 'SET ROLE = ' || calleruser;
            
            ELSIF arec.atype = 'sequence' THEN
              IF POSITION('r' IN privs) > 0 AND POSITION('w' IN privs) > 0 AND POSITION('U' IN privs) > 0 THEN
                -- arU is enough for all privs
                buffer := 'ALTER DEFAULT PRIVILEGES FOR ROLE ' || grantor || ' IN SCHEMA ' || quote_ident(dest_schema) || ' GRANT ALL ON SEQUENCES TO "' || grantee || '";';
              
                -- Issue#92 Fix
                IF grantor = grantee THEN
                    -- append set role to statement
                    buffer = 'SET ROLE = ' || grantor || '; ' || buffer;
                END IF;

                IF bDDLOnly THEN
                  RAISE INFO '%', buffer;
                ELSE
                  EXECUTE buffer;
                END IF;
                -- Issue#92 Fix:
                EXECUTE 'SET ROLE = ' || calleruser;

              ELSE
                -- have to specify each priv individually
                buffer2 := '';
                IF POSITION('r' IN privs) > 0 THEN
                      buffer2 := 'SELECT';
                END IF;
                IF POSITION('w' IN privs) > 0 THEN
                  IF buffer2 = '' THEN
                    buffer2 := 'UPDATE';
                  ELSE
                    buffer2 := buffer2 || ', UPDATE';
                  END IF;
                END IF;
                IF POSITION('U' IN privs) > 0 THEN
                      IF buffer2 = '' THEN
                    buffer2 := 'USAGE';
                  ELSE
                    buffer2 := buffer2 || ', USAGE';
                  END IF;
                END IF;
                buffer := 'ALTER DEFAULT PRIVILEGES FOR ROLE ' || grantor || ' IN SCHEMA ' || quote_ident(dest_schema) || ' GRANT ' || buffer2 || ' ON SEQUENCES TO "' || grantee || '";';

                -- Issue#92 Fix
                IF grantor = grantee THEN
                    -- append set role to statement
                    buffer = 'SET ROLE = ' || grantor || '; ' || buffer;
                END IF;
              
                IF bDDLOnly THEN
                  RAISE INFO '%', buffer;
                ELSE
                  EXECUTE buffer;
                END IF;
                select current_user into buffer;
                -- Issue#92 Fix:
                EXECUTE 'SET ROLE = ' || calleruser;
              END IF;

            ELSIF arec.atype = 'table' THEN
              -- do each priv individually, jeeeesh!
              buffer2 := '';
              IF POSITION('a' IN privs) > 0 THEN
                buffer2 := 'INSERT';
              END IF;
              IF POSITION('r' IN privs) > 0 THEN
                IF buffer2 = '' THEN
                  buffer2 := 'SELECT';
                ELSE
                  buffer2 := buffer2 || ', SELECT';
                END IF;
              END IF;
              IF POSITION('w' IN privs) > 0 THEN
                IF buffer2 = '' THEN
                  buffer2 := 'UPDATE';
                ELSE
                  buffer2 := buffer2 || ', UPDATE';
                END IF;
              END IF;
              IF POSITION('d' IN privs) > 0 THEN
                IF buffer2 = '' THEN
                  buffer2 := 'DELETE';
                ELSE
                  buffer2 := buffer2 || ', DELETE';
                END IF;
              END IF;
              IF POSITION('t' IN privs) > 0 THEN
                IF buffer2 = '' THEN
                  buffer2 := 'TRIGGER';
                ELSE
                  buffer2 := buffer2 || ', TRIGGER';
                END IF;
              END IF;
              IF POSITION('T' IN privs) > 0 THEN
                IF buffer2 = '' THEN
                  buffer2 := 'TRUNCATE';
                ELSE
                  buffer2 := buffer2 || ', TRUNCATE';
                END IF;
              END IF;
              buffer := 'ALTER DEFAULT PRIVILEGES FOR ROLE ' || grantor || ' IN SCHEMA ' || quote_ident(dest_schema) || ' GRANT ' || buffer2 || ' ON TABLES TO "' || grantee || '";';
            
              -- Issue#92 Fix
              IF grantor = grantee THEN
                  -- append set role to statement
                  buffer = 'SET ROLE = ' || grantor || '; ' || buffer;
              END IF;
            
              IF bDDLOnly THEN
                RAISE INFO '%', buffer;
              ELSE
                EXECUTE buffer;
              END IF;
              select current_user into buffer;
              -- Issue#92 Fix:
              EXECUTE 'SET ROLE = ' || calleruser;

            ELSIF arec.atype = 'type' THEN
              IF POSITION('r' IN privs) > 0 AND POSITION('w' IN privs) > 0 AND POSITION('U' IN privs) > 0 THEN
                -- arU is enough for all privs
                buffer := 'ALTER DEFAULT PRIVILEGES FOR ROLE ' || grantor || ' IN SCHEMA ' || quote_ident(dest_schema) || ' GRANT ALL ON TYPES TO "' || grantee || '";';
                
                -- Issue#92 Fix
                IF grantor = grantee THEN
                    -- append set role to statement
                    buffer = 'SET ROLE = ' || grantor || '; ' || buffer;
                END IF;
              
                IF bDDLOnly THEN
                  RAISE INFO '%', buffer;
                ELSE
                  EXECUTE buffer;
                END IF;
                -- Issue#92 Fix:
                EXECUTE 'SET ROLE = ' || calleruser;
              
              ELSIF POSITION('U' IN privs) THEN
                buffer := 'ALTER DEFAULT PRIVILEGES FOR ROLE ' || grantor || ' IN SCHEMA ' || quote_ident(dest_schema) || ' GRANT USAGE ON TYPES TO "' || grantee || '";';
              
                -- Issue#92 Fix
                IF grantor = grantee THEN
                    -- append set role to statement
                    buffer = 'SET ROLE = ' || grantor || '; ' || buffer;
                END IF;
              
                IF bDDLOnly THEN
                  RAISE INFO '%', buffer;
                ELSE
                  EXECUTE buffer;
                END IF;
                -- Issue#92 Fix:
                EXECUTE 'SET ROLE = ' || calleruser;
              
              ELSE
                RAISE WARNING 'Unhandled TYPE Privs:: type=%  privs=%  owner=%   defaclacl=%  defaclstr=%  grantor=%  grantee=% ', arec.atype, privs, arec.owner, arec.defaclacl, arec.defaclstr, grantor, grantee;
            END IF;
          ELSE
            RAISE WARNING 'Unhandled Privs:: type=%  privs=%  owner=%   defaclacl=%  defaclstr=%  grantor=%  grantee=% ', arec.atype, privs, arec.owner, arec.defaclacl, arec.defaclstr, grantor, grantee;
          END IF;
        END LOOP;
      END;
    END LOOP;
  
    RAISE NOTICE '  DFLT PRIVS cloned: %', LPAD(cnt::text, 5, ' ');    
  END IF; -- NO ACL BRANCH

  -- Issue#95 bypass if No ACL specified
  IF NOT bNoACL THEN
    -- MV: PRIVS: schema
    -- crunchy data extension, check_access
    -- SELECT role_path, base_role, as_role, objtype, schemaname, objname, array_to_string(array_agg(privname),',') as privs  FROM all_access()
    -- WHERE base_role != CURRENT_USER and objtype = 'schema' and schemaname = 'public' group by 1,2,3,4,5,6;

    action := 'PRIVS: Schema';
    cnt := 0;
    FOR arec IN
      SELECT 'GRANT ' || p.perm::perm_type || ' ON SCHEMA ' || quote_ident(dest_schema) || ' TO "' || r.rolname || '";' as schema_ddl
      FROM pg_catalog.pg_namespace AS n
      CROSS JOIN pg_catalog.pg_roles AS r
      CROSS JOIN (VALUES ('USAGE'), ('CREATE')) AS p(perm)
      WHERE n.nspname = quote_ident(source_schema) AND NOT r.rolsuper AND has_schema_privilege(r.oid, n.oid, p.perm)
      ORDER BY r.rolname, p.perm::perm_type
    LOOP
      BEGIN
        cnt := cnt + 1;
        IF bDDLOnly THEN
          RAISE INFO '%', arec.schema_ddl;
        ELSE
          EXECUTE arec.schema_ddl;
        END IF;
  
      END;
    END LOOP;
    RAISE NOTICE 'SCHEMA PRIVS cloned: %', LPAD(cnt::text, 5, ' ');
  END IF; -- NO ACL BRANCH

  -- Issue#95 bypass if No ACL specified
  IF NOT bNoACL THEN
    -- MV: PRIVS: sequences
    action := 'PRIVS: Sequences';
    cnt := 0;
    FOR arec IN
      -- Issue#78 FIX: handle case-sensitive names with quote_ident() on t.relname
      SELECT 'GRANT ' || p.perm::perm_type || ' ON ' || quote_ident(dest_schema) || '.' || quote_ident(t.relname::text) || ' TO "' || r.rolname || '";' as seq_ddl
      FROM pg_catalog.pg_class AS t
      CROSS JOIN pg_catalog.pg_roles AS r
      CROSS JOIN (VALUES ('SELECT'), ('USAGE'), ('UPDATE')) AS p(perm)
      WHERE t.relnamespace::regnamespace::name = quote_ident(source_schema) AND t.relkind = 'S'  AND NOT r.rolsuper AND has_sequence_privilege(r.oid, t.oid, p.perm)
    LOOP
      BEGIN
        cnt := cnt + 1;
        -- IF bDebug THEN RAISE NOTICE 'DEBUG: ddl=%', arec.seq_ddl; END IF;
        IF bDDLOnly THEN
          RAISE INFO '%', arec.seq_ddl;
        ELSE
          EXECUTE arec.seq_ddl;
        END IF;
      END;
    END LOOP;
    RAISE NOTICE '  SEQ. PRIVS cloned: %', LPAD(cnt::text, 5, ' ');
  END IF; -- NO ACL BRANCH    

  -- Issue#95 bypass if No ACL specified
  IF NOT bNoACL THEN
    -- MV: PRIVS: functions
    action := 'PRIVS: Functions/Procedures';
    cnt := 0;

    -- Issue#61 FIX: use set_config for empty string
    -- SET search_path = '';
    SELECT set_config('search_path', '', false) into v_dummy;

    -- RAISE NOTICE ' source_schema=%  dest_schema=%',source_schema, dest_schema;
    FOR arec IN
      -- 2021-03-05 MJV FIX: issue#35: caused exception in some functions with parameters and gave privileges to other users that should not have gotten them.
      -- SELECT 'GRANT EXECUTE ON FUNCTION ' || quote_ident(dest_schema) || '.' || replace(regexp_replace(f.oid::regprocedure::text, '^((("[^"]*")|([^"][^.]*))\.)?', ''), source_schema, dest_schema) || ' TO "' || r.rolname || '";' as func_ddl
      -- FROM pg_catalog.pg_proc f CROSS JOIN pg_catalog.pg_roles AS r WHERE f.pronamespace::regnamespace::name = quote_ident(source_schema) AND NOT r.rolsuper AND has_function_privilege(r.oid, f.oid, 'EXECUTE')
      -- order by regexp_replace(f.oid::regprocedure::text, '^((("[^"]*")|([^"][^.]*))\.)?', '')

      -- 2021-03-05 MJV FIX: issue#37: defaults cause problems, use system function that returns args WITHOUT DEFAULTS
      -- COALESCE(r.routine_type, 'FUNCTION'): for aggregate functions, information_schema.routines contains NULL as routine_type value.
      -- Issue#78 FIX: handle case-sensitive names with quote_ident() on rp.routine_name
      SELECT 'GRANT ' || rp.privilege_type || ' ON ' || COALESCE(r.routine_type, 'FUNCTION') || ' ' || quote_ident(dest_schema) || '.' || quote_ident(rp.routine_name) || ' (' || pg_get_function_identity_arguments(p.oid) || ') TO ' || string_agg(distinct rp.grantee, ',') || ';' as func_dcl
      FROM information_schema.routine_privileges rp, information_schema.routines r, pg_proc p, pg_namespace n
      WHERE rp.routine_schema = quote_ident(source_schema)
        AND rp.is_grantable = 'YES'
        AND rp.routine_schema = r.routine_schema
        AND rp.routine_name = r.routine_name
        AND rp.routine_schema = n.nspname
        AND n.oid = p.pronamespace
        AND p.proname = r.routine_name
      GROUP BY rp.privilege_type, r.routine_type, rp.routine_name, pg_get_function_identity_arguments(p.oid)
    LOOP
      BEGIN
        cnt := cnt + 1;
        IF bDDLOnly THEN
          RAISE INFO '%', arec.func_dcl;
        ELSE
          EXECUTE arec.func_dcl;
        END IF;
      END;
    END LOOP;
    EXECUTE 'SET search_path = ' || quote_ident(source_schema) ;
    RAISE NOTICE '  FUNC PRIVS cloned: %', LPAD(cnt::text, 5, ' ');
  END IF; -- NO ACL BRANCH

  -- Issue#95 bypass if No ACL specified
  IF NOT bNoACL THEN
    -- MV: PRIVS: tables
    action := 'PRIVS: Tables';
    -- regular, partitioned, and foreign tables plus view and materialized view permissions. Ignored for now: implement foreign table defs.
    cnt := 0;
    FOR arec IN
      -- SELECT 'GRANT ' || p.perm::perm_type || CASE WHEN t.relkind in ('r', 'p', 'f') THEN ' ON TABLE ' WHEN t.relkind in ('v', 'm')  THEN ' ON ' END || quote_ident(dest_schema) || '.' || t.relname::text || ' TO "' || r.rolname || '";' as tbl_ddl,
      -- has_table_privilege(r.oid, t.oid, p.perm) AS granted, t.relkind
      -- FROM pg_catalog.pg_class AS t CROSS JOIN pg_catalog.pg_roles AS r CROSS JOIN (VALUES (TEXT 'SELECT'), ('INSERT'), ('UPDATE'), ('DELETE'), ('TRUNCATE'), ('REFERENCES'), ('TRIGGER')) AS p(perm)
      -- WHERE t.relnamespace::regnamespace::name = quote_ident(source_schema)  AND t.relkind in ('r', 'p', 'f', 'v', 'm')  AND NOT r.rolsuper AND has_table_privilege(r.oid, t.oid, p.perm) order by t.relname::text, t.relkind
      -- 2021-03-05  MJV FIX: Fixed Issue#36 for tables
      SELECT c.relkind, 'GRANT ' || tb.privilege_type || CASE WHEN c.relkind in ('r', 'p') THEN ' ON TABLE ' WHEN c.relkind in ('v', 'm')  THEN ' ON ' END ||
      -- Issue#78 FIX: handle case-sensitive names with quote_ident() on t.relname      
      -- Issue#108 FIX: enclose double-quote grantees with special characters
      -- quote_ident(dest_schema) || '.' || quote_ident(tb.table_name) || ' TO ' || string_agg(tb.grantee, ',') || ';' as tbl_dcl
      quote_ident(dest_schema) || '.' || quote_ident(tb.table_name) || ' TO ' || string_agg('"' || tb.grantee || '"', ',') || ';' as tbl_dcl
      FROM information_schema.table_privileges tb, pg_class c, pg_namespace n
      WHERE tb.table_schema = quote_ident(source_schema) AND tb.table_name = c.relname AND c.relkind in ('r', 'p', 'v', 'm')
        AND c.relnamespace = n.oid AND n.nspname = quote_ident(source_schema)
        GROUP BY c.relkind, tb.privilege_type, tb.table_schema, tb.table_name
    LOOP
      BEGIN
        cnt := cnt + 1;
        -- IF bDebug THEN RAISE NOTICE 'DEBUG: ddl=%', arec.tbl_dcl; END IF;
        -- Issue#46. Fixed reference to invalid record name (tbl_ddl --> tbl_dcl).
        IF arec.relkind = 'f' THEN
          RAISE WARNING 'Foreign tables are not currently implemented, so skipping privs for them. ddl=%', arec.tbl_dcl;
        ELSE
            IF bDDLOnly THEN
                RAISE INFO '%', arec.tbl_dcl;
            ELSE
                EXECUTE arec.tbl_dcl;
              END IF;
      END IF;
      END;
    END LOOP;
    RAISE NOTICE ' TABLE PRIVS cloned: %', LPAD(cnt::text, 5, ' ');
  END IF; -- NO ACL BRANCH

  -- LOOP for regular tables and populate them if specified
  -- Issue#75 moved from big table loop above to here.
  IF bData THEN
    r = clock_timestamp();
    -- IF bVerbose THEN RAISE NOTICE 'START: copy rows %',clock_timestamp() - t; END IF;  
    IF bVerbose THEN RAISE NOTICE 'Copying rows...'; END IF;  

    EXECUTE 'SET search_path = ' || quote_ident(dest_schema) ;
    action := 'Copy Rows';
    FOREACH tblelement IN ARRAY tblarray
    LOOP 
       s = clock_timestamp();
       IF bDebug THEN RAISE NOTICE 'DEBUG1: no UDTs %', tblelement; END IF;
       EXECUTE tblelement;       
       GET DIAGNOSTICS cnt = ROW_COUNT;  
       buffer = substring(tblelement, 13);
       SELECT POSITION(' OVERRIDING SYSTEM VALUE SELECT ' IN buffer) INTO cnt2; 
       IF cnt2 = 0 THEN
           SELECT POSITION(' SELECT ' IN buffer) INTO cnt2;
           buffer = substring(buffer,1, cnt2);       
       ELSE
           buffer = substring(buffer,1, cnt2);       
       END IF;
       SELECT RPAD(buffer, 35, ' ') INTO buffer;
       cnt2 := cast(extract(epoch from (clock_timestamp() - s)) as numeric(18,3));
       IF bVerbose THEN RAISE NOTICE 'Populated cloned table, %   Rows Copied: %    seconds: %', buffer, LPAD(cnt::text, 10, ' '), LPAD(cnt2::text, 5, ' '); END IF;
       tblscopied := tblscopied + 1;
    END LOOP;
    
    -- Issue#79 implementation
    -- Do same for tables with user-defined elements using copy to file method
    FOREACH tblelement IN ARRAY tblarray2
    LOOP 
       s = clock_timestamp();
       IF bDebug THEN RAISE NOTICE 'DEBUG2: UDTs %', tblelement; END IF;
       EXECUTE tblelement;       
       GET DIAGNOSTICS cnt = ROW_COUNT;  
       
       -- STATEMENT LOOKS LIKE THIS:
       -- INSERT INTO sample11.warehouses SELECT * FROM sample.warehouses;
       -- INSERT INTO sample11.person OVERRIDING SYSTEM VALUE SELECT * FROM sample.person;  
       -- COPY sample.address TO '/tmp/cloneschema.tmp' WITH DELIMITER AS ',';\
       buffer = TRIM(tblelement::text);
       -- RAISE NOTICE 'element=%', buffer;
       cnt1 = POSITION('INSERT INTO' IN buffer);
       cnt2 = POSITION('COPY ' IN buffer);
       IF cnt1 > 0 THEN
           buffer = substring(buffer, 12);
       ELSIF cnt2 > 0 THEN
           buffer = substring(buffer, 5);
       ELSE
           RAISE EXCEPTION 'Programming Error for parsing tblarray2.';
       END IF;

       -- RAISE NOTICE 'buffer1=%', buffer;
       cnt1 = POSITION(' OVERRIDING ' IN buffer);
       cnt2 = POSITION('SELECT * FROM ' IN buffer);
       cnt3 = POSITION(' FROM ' IN buffer);
       cnt4 = POSITION(' TO ' IN buffer);
       IF cnt1 > 0 THEN
           buffer = substring(buffer, 1, cnt1-2);
       ELSIF cnt2 > 0 THEN
           buffer = substring(buffer, 1, cnt2-2);
       ELSIF cnt3 > 0 THEN
           buffer = substring(buffer, 1, cnt3-1);           
       ELSIF cnt4 > 0 THEN
           -- skip the COPY TO statements
           continue;
       ELSE
           RAISE EXCEPTION 'Programming Error for parsing tblarray2.';
       END IF;
       -- RAISE NOTICE 'buffer2=%', buffer;
       
       SELECT RPAD(buffer, 35, ' ') INTO buffer;
       -- RAISE NOTICE 'buffer3=%', buffer;
       cnt2 := cast(extract(epoch from (clock_timestamp() - s)) as numeric(18,3));
       IF bVerbose THEN RAISE NOTICE 'Populated cloned table, %   Rows Copied: %    seconds: %', buffer, LPAD(cnt::text, 10, ' '), LPAD(cnt2::text, 5, ' '); END IF;
       tblscopied := tblscopied + 1;
    END LOOP;    
    
    -- Issue#101 
    -- Do same for tables with user-defined elements using direct method with text cast
    FOREACH tblelement IN ARRAY tblarray3
    LOOP 
       s = clock_timestamp();
       IF bDebug THEN RAISE NOTICE 'DEBUG3: UDTs %', tblelement; END IF;
       EXECUTE tblelement;       
       GET DIAGNOSTICS cnt = ROW_COUNT;  
       cnt2 = POSITION(' (' IN tblelement::text);
       IF cnt2 > 0 THEN
           buffer = substring(tblelement, 1, cnt2);
           buffer = substring(buffer, 6);
           SELECT RPAD(buffer, 35, ' ') INTO buffer;
           cnt2 := cast(extract(epoch from (clock_timestamp() - s)) as numeric(18,3));
           IF bVerbose THEN RAISE NOTICE 'Populated cloned table, %   Rows Copied: %    seconds: %', buffer, LPAD(cnt::text, 10, ' '), LPAD(cnt2::text, 5, ' '); END IF;
           tblscopied := tblscopied + 1;
       END IF;
    END LOOP;    
    
    -- Issue#98 MVs deferred until now
    FOREACH tblelement IN ARRAY mvarray
    LOOP 
       s = clock_timestamp();
       EXECUTE tblelement;       
       -- get diagnostics for MV creates or refreshes does not work, always returns 1
       GET DIAGNOSTICS cnt = ROW_COUNT;  
       buffer = substring(tblelement, 25);
       cnt2 = POSITION(' AS ' IN buffer);
       IF cnt2 > 0 THEN
         buffer = substring(buffer, 1, cnt2);
         SELECT RPAD(buffer, 36, ' ') INTO buffer;
         cnt2 := cast(extract(epoch from (clock_timestamp() - s)) as numeric(18,3));
         IF bVerbose THEN RAISE NOTICE 'Populated Mat. View,    %  Rows Inserted:        ?    seconds: %', buffer, LPAD(cnt2::text, 5, ' '); END IF;
         mvscopied := mvscopied + 1;
       END IF;
    END LOOP;    
    
    cnt := cast(extract(epoch from (clock_timestamp() - r)) as numeric(18,3));
    IF bVerbose THEN RAISE NOTICE 'Copy rows duration: % seconds',cnt; END IF;  
  END IF;
  RAISE NOTICE '      TABLES copied: %', LPAD(tblscopied::text, 5, ' ');
  RAISE NOTICE ' MATVIEWS refreshed: %', LPAD(mvscopied::text, 5, ' ');

  
  -- Issue#78 forces us to defer FKeys until the end since we previously did row copies before FKeys
  --  add FK constraint
  action := 'FK Constraints';
  cnt := 0;

  -- Issue#61 FIX: use set_config for empty string
  -- SET search_path = '';
  SELECT set_config('search_path', '', false) into v_dummy;

  FOR qry IN
    SELECT 'ALTER TABLE ' || quote_ident(dest_schema) || '.' || quote_ident(rn.relname)
                          || ' ADD CONSTRAINT ' || quote_ident(ct.conname) || ' ' || REPLACE(pg_get_constraintdef(ct.oid), 'REFERENCES ' || quote_ident(source_schema) || '.', 'REFERENCES ' 
                          || quote_ident(dest_schema) || '.') || ';'
    FROM pg_constraint ct
    JOIN pg_class rn ON rn.oid = ct.conrelid
    -- Issue#103 needed to addd this left join
    LEFT JOIN pg_inherits i ON (rn.oid = i.inhrelid)
    WHERE connamespace = src_oid
        AND rn.relkind = 'r'
        AND ct.contype = 'f'
        -- Issue#103 fix: needed to also add this null check
        AND i.inhrelid is null
  LOOP
    cnt := cnt + 1;
    IF bDDLOnly THEN
      RAISE INFO '%', qry;
    ELSE
      IF bDebug THEN RAISE NOTICE 'DEBUG: adding FKEY constraint: %', qry; END IF;
      EXECUTE qry;
    END IF;
  END LOOP;
  EXECUTE 'SET search_path = ' || quote_ident(source_schema) ;
  RAISE NOTICE '       FKEYS cloned: %', LPAD(cnt::text, 5, ' ');


  IF src_path_old = '' OR src_path_old = '""' THEN
    -- RAISE NOTICE 'Restoring old search_path to empty string';
    SELECT set_config('search_path', '', false) into v_dummy;
  ELSE
    -- RAISE NOTICE 'Restoring old search_path to:%', src_path_old;
    EXECUTE 'SET search_path = ' || src_path_old;
  END IF;
  SELECT setting INTO v_dummy FROM pg_settings WHERE name = 'search_path';
  IF bDebug THEN RAISE NOTICE 'DEBUG: setting search_path back to what it was: %', v_dummy; END IF;
  cnt := cast(extract(epoch from (clock_timestamp() - t)) as numeric(18,3));
  IF bVerbose THEN RAISE NOTICE 'clone_schema duration: % seconds',cnt; END IF;  

  EXCEPTION
     WHEN others THEN
     BEGIN
         GET STACKED DIAGNOSTICS v_diag1 = MESSAGE_TEXT, v_diag2 = PG_EXCEPTION_DETAIL, v_diag3 = PG_EXCEPTION_HINT, v_diag4 = RETURNED_SQLSTATE, v_diag5 = PG_CONTEXT, v_diag6 = PG_EXCEPTION_CONTEXT;
         v_ret := 'line=' || v_diag6 || '. '|| v_diag4 || '. ' || v_diag1;
         -- Issue#101: added version to exception output
         -- RAISE NOTICE 'v_diag1=%  v_diag2=%  v_diag3=%  v_diag4=%  v_diag5=%  v_diag6=%', v_diag1, v_diag2, v_diag3, v_diag4, v_diag5, v_diag6; 
         buffer2 = '';
         IF action = 'Copy Rows' AND v_diag4 = '42704' THEN
             -- Issue#105 Help user to fix the problem.
             buffer2 = 'It appears you have a USER-DEFINED column type mismatch.  Try running clone_schema with the FILECOPY option. ';
         END IF;
         IF lastsql <> '' THEN
             buffer = v_ret || E'\n'|| buffer2 || E'\n'|| lastsql;
         ELSE
             buffer = v_ret || E'\n'|| buffer2;
         END IF;
         RAISE EXCEPTION 'Version: %  Action: %  Diagnostics: %',v_version, action, buffer;

         IF src_path_old = '' THEN
           -- RAISE NOTICE 'setting old search_path to empty string';
           SELECT set_config('search_path', '', false);
         ELSE
           -- RAISE NOTICE 'setting old search_path to:%', src_path_old;
           EXECUTE 'SET search_path = ' || src_path_old;
         END IF;

         RETURN;
     END;

RETURN;
END;

$_$;


ALTER FUNCTION public.clone_schema(source_schema text, dest_schema text, VARIADIC arr public.cloneparms[]) OWNER TO postgres;

--
-- Name: delete_sessao_byfilme(text[], text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.delete_sessao_byfilme(schemas text[], filmeid text) RETURNS void
    LANGUAGE plpgsql
    AS $_$
DECLARE
    schema_name text;
BEGIN
    FOREACH schema_name IN ARRAY schemas LOOP
        EXECUTE format('DELETE FROM %I."Sessao" WHERE "Sessao"."idFilme" = $1', schema_name)
        USING filmeId;
    END LOOP;
END;
$_$;


ALTER FUNCTION public.delete_sessao_byfilme(schemas text[], filmeid text) OWNER TO postgres;

--
-- Name: drop_schema(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.drop_schema(schema_name text) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    EXECUTE format('DROP SCHEMA %I CASCADE;', schema_name);
END;
$$;


ALTER FUNCTION public.drop_schema(schema_name text) OWNER TO postgres;

--
-- Name: get_avaliacoes_global_byfilme(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_avaliacoes_global_byfilme(schemas text[]) RETURNS TABLE(nomefilme text, avaliacao numeric)
    LANGUAGE plpgsql
    AS $_$
DECLARE
    schema_name text;
    schema_count int := array_length(schemas, 1);
BEGIN
    FOR i IN 1..schema_count LOOP
        SELECT INTO schema_name schemas[i];

        RETURN QUERY EXECUTE format('
           SELECT
                f.nome AS nomeFilme,
                COALESCE(AVG(a."valor"), 0.0) AS avaliacao
            FROM
                %1$I."avaliacao" a
            INNER JOIN
                "public"."Filmes" f ON f.id = a."idFilme"
            GROUP BY
                f.nome
        ', schema_name);
    END LOOP;
END;
$_$;


ALTER FUNCTION public.get_avaliacoes_global_byfilme(schemas text[]) OWNER TO postgres;

--
-- Name: get_filmes_mais_visualizados(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_filmes_mais_visualizados(schemas text[]) RETURNS TABLE(cinemanome text, filmenome text, quantidadeticketsvendidos bigint)
    LANGUAGE plpgsql
    AS $_$
DECLARE
    schema_name text;
    schema_count int := array_length(schemas, 1);
BEGIN
    FOR i IN 1..schema_count LOOP
        SELECT INTO schema_name schemas[i];

        RETURN QUERY EXECUTE format('
            SELECT
                c.nome AS cinemaNome,
                f.nome AS filmeNome,
                COUNT(t.id) AS quantidadeTicketsVendidos
            FROM
                %1$I."Sessao" s
            INNER JOIN
                %1$I."Ticket" t ON s.id = t."idSessao"
            INNER JOIN
                public."Filmes" f ON s."idFilme" = f.id
            INNER JOIN
                public."Cinema" c ON s."idCinema" = c.id
            WHERE
                s."dtSessao" < NOW()
            GROUP BY
                f.nome, c.nome
            ORDER BY
                COUNT(t.id) DESC
            LIMIT 3
        ', schema_name)
        USING schema_name;
    END LOOP;
END;
$_$;


ALTER FUNCTION public.get_filmes_mais_visualizados(schemas text[]) OWNER TO postgres;

--
-- Name: get_filmes_mais_visualizados_semana(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_filmes_mais_visualizados_semana(schemas text[]) RETURNS TABLE(cinemanome text, filmenome text, quantidadeticketsvendidos bigint)
    LANGUAGE plpgsql
    AS $_$
DECLARE
    schema_name text;
    schema_count int := array_length(schemas, 1);
    start_of_week timestamp := date_trunc('day', current_date) - INTERVAL '7 days'; 
BEGIN
    FOR i IN 1..schema_count LOOP
        SELECT INTO schema_name schemas[i];

        RETURN QUERY EXECUTE format('
            SELECT
                c.nome AS cinemaNome,
                f.nome AS filmeNome,
                COUNT(t.id) AS quantidadeTicketsVendidos
            FROM
                %1$I."Sessao" s
            INNER JOIN
                %1$I."Ticket" t ON s.id = t."idSessao"
            INNER JOIN
                public."Filmes" f ON s."idFilme" = f.id
            INNER JOIN
                public."Cinema" c ON s."idCinema" = c.id
            WHERE
                s."dtSessao" >= %2$L
                AND s."dtSessao" < NOW()
            GROUP BY
                f.nome, c.nome
            ORDER BY
                COUNT(t.id) DESC
            LIMIT 3
        ', schema_name, start_of_week)
        USING schema_name;
    END LOOP;
END;
$_$;


ALTER FUNCTION public.get_filmes_mais_visualizados_semana(schemas text[]) OWNER TO postgres;

--
-- Name: get_insert_stmt_ddl(text, text, text, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_insert_stmt_ddl(source_schema text, target_schema text, atable text, btextcast boolean DEFAULT false) RETURNS text
    LANGUAGE plpgsql
    AS $$
  DECLARE
    -- the ddl we're building
    v_insert_ddl text := '';
    v_cols       text := '';
    v_cols_sel   text := '';
    v_cnt        int  := 0;
    v_colrec     record;
    v_schema     text;
  BEGIN
    FOR v_colrec IN
      SELECT c.column_name, c.data_type, c.udt_name, c.udt_schema, c.character_maximum_length, c.is_nullable, c.column_default, c.numeric_precision, c.numeric_scale, c.is_identity, c.identity_generation, c.is_generated 
      FROM information_schema.columns c WHERE (table_schema, table_name) = (source_schema, atable) ORDER BY ordinal_position
    LOOP
      IF v_colrec.udt_schema = 'public' THEN
        v_schema = 'public';
      ELSE
        v_schema = target_schema;
      END IF;
      
      v_cnt = v_cnt + 1;
      IF v_colrec.is_identity = 'YES' OR v_colrec.is_generated = 'ALWAYS' THEN
        -- skip
        continue;
      END IF;

      IF v_colrec.data_type = 'USER-DEFINED' THEN
        IF v_cols = '' THEN
          v_cols     = v_colrec.column_name;
          IF bTextCast THEN 
            -- v_cols_sel = v_colrec.column_name || '::text::' || v_schema || '.' || v_colrec.udt_name;
            IF v_schema = 'public' THEN
              v_cols_sel = v_colrec.column_name || '::' || v_schema || '.' || v_colrec.udt_name;
            ELSE
              v_cols_sel = v_colrec.column_name || '::text::' || v_colrec.udt_name;
            END IF;
          ELSE
            v_cols_sel = v_colrec.column_name || '::' || v_schema || '.' || v_colrec.udt_name;
          END IF;
        ELSE 
          v_cols     = v_cols     || ', ' || v_colrec.column_name;
          IF bTextCast THEN 
            -- v_cols_sel = v_cols_sel || ', ' || v_colrec.column_name || '::text::' || v_schema || '.' || v_colrec.udt_name;
            IF v_schema = 'public' THEN
              v_cols_sel = v_cols_sel || ', ' || v_colrec.column_name || '::' || v_schema || '.' || v_colrec.udt_name;
            ELSE
              v_cols_sel = v_cols_sel || ', ' || v_colrec.column_name || '::text::' || v_colrec.udt_name;
            END IF;
          ELSE
            v_cols_sel = v_cols_sel || ', ' || v_colrec.column_name || '::' || v_schema || '.' || v_colrec.udt_name;
          END IF;
        END IF;
      ELSE
        IF v_cols = '' THEN
          v_cols     = v_colrec.column_name;
          v_cols_sel = v_colrec.column_name;
        ELSE 
          v_cols     = v_cols     || ', ' || v_colrec.column_name;
          v_cols_sel = v_cols_sel || ', ' || v_colrec.column_name;
        END IF;
      END IF;
    END LOOP;

    -- put it all together and return the insert statement
    -- INSERT INTO clone1.address2 (id2, id3, addr) SELECT id2::text::clone1.udt_myint, id3::text::clone1.udt_myint, addr FROM sample.address;    
    v_insert_ddl = 'INSERT INTO ' || target_schema || '.' || atable || ' (' || v_cols || ') ' || 'SELECT ' || v_cols_sel || ' FROM ' || source_schema || '.' || atable || ';';
    RETURN v_insert_ddl;
  END;
$$;


ALTER FUNCTION public.get_insert_stmt_ddl(source_schema text, target_schema text, atable text, btextcast boolean) OWNER TO postgres;

--
-- Name: get_sessoes_byfilme(text[], text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_sessoes_byfilme(schemas text[], idfilme text) RETURNS TABLE(nomefilme text, idcinema text, nomecinema text, idsessao text, dtsessao text, horainicio text)
    LANGUAGE plpgsql
    AS $_$
DECLARE
    schema_name text;
    schema_count int := array_length(schemas, 1);
BEGIN
    FOR i IN 1..schema_count LOOP
        SELECT INTO schema_name schemas[i];

        RETURN QUERY EXECUTE format('
           SELECT
                f.nome AS nomeFilme,
				c.id as idCinema, c.nome as nomeCinema,
				s.id as idSessao, TO_CHAR(s."dtSessao", ''YYYY-MM-DD HH:MI:SS'') AS dtSessao, s."horaInicio"
            FROM
                %1$I."Sessao" s
            INNER JOIN
                "public"."Filmes" f ON f.id = s."idFilme"
			LEFT JOIN
				"public"."Cinema" c on c.id = s."idCinema"
			WHERE
				f.id = $1
        ', schema_name) USING idFilme;
    END LOOP;
END;
$_$;


ALTER FUNCTION public.get_sessoes_byfilme(schemas text[], idfilme text) OWNER TO postgres;

--
-- Name: get_table_ddl(character varying, character varying, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_table_ddl(in_schema character varying, in_table character varying, bfkeys boolean) RETURNS text
    LANGUAGE plpgsql
    AS $_$
  DECLARE
    -- the ddl we're building
    v_table_ddl text;

    -- data about the target table
    v_table_oid int;

    -- records for looping
    v_colrec record;
    v_constraintrec record;
    v_indexrec record;
    v_primary boolean := False;
    v_constraint_name text;
    v_src_path_old text := '';
    v_src_path_new text := '';
    v_dummy text;
    v_partbound text;
    v_pgversion int;
    v_parent     text := '';
    v_relopts text := '';
    v_tablespace text;
    v_partition_key text := '';
    v_temp       text;
    bPartitioned bool := False;
    bInheritance bool := False;
    bRelispartition bool;
    constraintarr text[] := '{}';
    constraintelement text;
    bSkip boolean;

  BEGIN
    SELECT c.oid, (
        SELECT setting
        FROM pg_settings
        WHERE name = 'server_version_num') INTO v_table_oid, v_pgversion
    FROM pg_catalog.pg_class c
        LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
    WHERE c.relkind IN ('r', 'p')
        AND c.relname = in_table
        AND n.nspname = in_schema;
    IF (v_table_oid IS NULL) THEN
      RAISE EXCEPTION 'table does not exist';
    END IF;

    -- get user-defined tablespaces if applicable
    SELECT TABLESPACE INTO v_temp
    FROM pg_tables
    WHERE schemaname = in_schema
        AND tablename = in_table
        AND TABLESPACE IS NOT NULL;
    -- Issue#99 Fix: simple coding error!
    -- IF v_tablespace IS NULL THEN
    IF v_temp IS NULL THEN
      v_tablespace := 'TABLESPACE pg_default';
    ELSE
      v_tablespace := 'TABLESPACE ' || v_temp;
    END IF;
    -- also see if there are any SET commands for this table, ie, autovacuum_enabled=off, fillfactor=70
    WITH relopts AS (
        SELECT unnest(c.reloptions) relopts
        FROM pg_class c, pg_namespace n
        WHERE n.nspname = in_schema
            AND n.oid = c.relnamespace
            AND c.relname = in_table
    )
    SELECT string_agg(r.relopts, ', ') AS relopts INTO v_temp
    FROM relopts r;
    IF v_temp IS NULL THEN
      v_relopts := '';
    ELSE
      v_relopts := ' WITH (' || v_temp || ')';
    END IF;

    -- Issue#61 FIX: set search_path = public before we do anything to force explicit schema qualification but dont forget to set it back before exiting...
    SELECT setting INTO v_src_path_old FROM pg_settings WHERE name = 'search_path';

    SELECT REPLACE(REPLACE(setting, '"$user"', '$user'), '$user', '"$user"') INTO v_src_path_old
    FROM pg_settings
    WHERE name = 'search_path';
    -- RAISE INFO 'DEBUG tableddl: saving old search_path: ***%***', v_src_path_old;
    EXECUTE 'SET search_path = "public"';
    SELECT setting INTO v_src_path_new FROM pg_settings WHERE name = 'search_path';

    -- grab the oid of the table; https://www.postgresql.org/docs/8.3/catalog-pg-class.html
    SELECT c.oid INTO v_table_oid
    FROM pg_catalog.pg_class c
        LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
    WHERE 1 = 1
        AND c.relkind = 'r'
        AND c.relname = in_table
        AND n.nspname = in_schema;

    IF (v_table_oid IS NULL) THEN
      -- Dont give up yet.  It might be a partitioned table
      SELECT c.oid INTO v_table_oid
      FROM pg_catalog.pg_class c
          LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
      WHERE 1 = 1
          AND c.relkind = 'p'
          AND c.relname = in_table
          AND n.nspname = in_schema;

      IF (v_table_oid IS NULL) THEN
        RAISE EXCEPTION 'table does not exist';
      END IF;
      bPartitioned := True;
    END IF;
    IF v_pgversion < 100000 THEN
      SELECT c2.relname parent INTO v_parent
      FROM pg_class c1, pg_namespace n, pg_inherits i, pg_class c2
      WHERE n.nspname = in_schema
          AND n.oid = c1.relnamespace
          AND c1.relname = in_table
          AND c1.oid = i.inhrelid
          AND i.inhparent = c2.oid
          AND c1.relkind = 'r';

      IF (v_parent IS NOT NULL) THEN
        bPartitioned := True;
        bInheritance  := True;
      END IF;
    ELSE
      SELECT c2.relname parent, c1.relispartition, pg_get_expr(c1.relpartbound, c1.oid, TRUE) INTO v_parent, bRelispartition, v_partbound
      FROM pg_class c1, pg_namespace n, pg_inherits i, pg_class c2
      WHERE n.nspname = in_schema
          AND n.oid = c1.relnamespace
          AND c1.relname = in_table
          AND c1.oid = i.inhrelid
          AND i.inhparent = c2.oid
          AND c1.relkind = 'r';

      IF (v_parent IS NOT NULL) THEN
        bPartitioned   := True;
        IF bRelispartition THEN
          bInheritance := False;
        ELSE
          bInheritance := True;
        END IF;
      END IF;
    END IF;
    -- RAISE NOTICE 'version=%  schema=%  parent=%  relopts=%  tablespace=%  partitioned=%  inherited=%  relispartition=%',v_pgversion, in_schema, v_parent, v_relopts, v_tablespace, bPartitioned, bInheritance,   bRelispartition;

    -- start the create definition
    v_table_ddl := 'CREATE TABLE ' || in_schema || '.' || in_table || ' (' || E'\n';

    -- define all of the columns in the table; https://stackoverflow.com/a/8153081/3068233
    FOR v_colrec IN
      SELECT c.column_name, c.data_type, c.udt_name, c.udt_schema, c.character_maximum_length, c.is_nullable, c.column_default, c.numeric_precision, c.numeric_scale, c.is_identity, c.identity_generation
      FROM information_schema.columns c
      WHERE (table_schema, table_name) = (in_schema, in_table)
      ORDER BY ordinal_position
    LOOP
      v_table_ddl := v_table_ddl || '  ' -- note: two char spacer to start, to indent the column
        || v_colrec.column_name || ' '
        -- FIX #82, FIX #100 as well by adding 'citext' to the list
        -- FIX #105 by overriding the previous fixes (#82, #100), which presumed "public" was always the schema for extensions.  It could be a custom schema. 
        -- so assume udt_schema for all USER-DEFINED datatypes
        -- || CASE WHEN v_colrec.udt_name in ('geometry', 'box2d', 'box2df', 'box3d', 'geography', 'geometry_dump', 'gidx', 'spheroid', 'valid_detail','citext')
        --        THEN v_colrec.udt_name 
           || CASE WHEN v_colrec.data_type = 'USER-DEFINED' 
               -- THEN in_schema || '.' || v_colrec.udt_name ELSE v_colrec.data_type END            
               THEN v_colrec.udt_schema || '.' || v_colrec.udt_name ELSE v_colrec.data_type END 
        || CASE WHEN v_colrec.is_identity = 'YES' 
               THEN 
                  CASE WHEN v_colrec.identity_generation = 'ALWAYS' 
                      THEN ' GENERATED ALWAYS AS IDENTITY' ELSE ' GENERATED BY DEFAULT AS IDENTITY' END ELSE '' END
        || CASE WHEN v_colrec.character_maximum_length IS NOT NULL 
               THEN ('(' || v_colrec.character_maximum_length || ')')
           WHEN v_colrec.numeric_precision > 0 AND v_colrec.numeric_scale > 0 
               THEN '(' || v_colrec.numeric_precision || ',' || v_colrec.numeric_scale || ')'
           ELSE '' END || ' '
        || CASE WHEN v_colrec.is_nullable = 'NO' 
               THEN 'NOT NULL' ELSE 'NULL' END
        || CASE WHEN v_colrec.column_default IS NOT null 
               THEN (' DEFAULT ' || v_colrec.column_default) ELSE '' END
        || ',' || E'\n';
    END LOOP;
    -- define all the constraints in the; https://www.postgresql.org/docs/9.1/catalog-pg-constraint.html && https://dba.stackexchange.com/a/214877/75296
    -- Issue#103: do not get foreign keys for partitions since they are defined on the parent and this will cause an "already exists" error otherwise
    --            Also conparentid is not in V10, so bypass since we do not have FKEYS in partitioned tables in V10
    IF v_pgversion < 110000 THEN
      FOR v_constraintrec IN
        SELECT
          con.conname as constraint_name,
          con.contype as constraint_type,
          CASE
            WHEN con.contype = 'p' THEN 1 -- primary key constraint
            WHEN con.contype = 'u' THEN 2 -- unique constraint
            WHEN con.contype = 'f' THEN 3 -- foreign key constraint
            WHEN con.contype = 'c' THEN 4
            ELSE 5
          END as type_rank,
          pg_get_constraintdef(con.oid) as constraint_definition
        FROM pg_catalog.pg_constraint con
            JOIN pg_catalog.pg_class rel ON rel.oid = con.conrelid
            JOIN pg_catalog.pg_namespace nsp ON nsp.oid = connamespace
        WHERE nsp.nspname = in_schema
            AND rel.relname = in_table
            ORDER BY type_rank
      LOOP
        -- Issue#85 fix
        -- constraintarr := constraintarr || v_constraintrec.constraint_name;
        constraintarr := constraintarr || v_constraintrec.constraint_name::text;
        IF v_constraintrec.type_rank = 1 THEN
            v_primary := True;
            v_constraint_name := v_constraintrec.constraint_name;
        END IF;
        IF NOT bfkeys AND v_constraintrec.constraint_type = 'f' THEN
            continue;
        END IF;
        v_table_ddl := v_table_ddl || '  ' -- note: two char spacer to start, to indent the column
          || 'CONSTRAINT' || ' '
          || v_constraintrec.constraint_name || ' '
          || v_constraintrec.constraint_definition
          || ',' || E'\n';
      END LOOP;
    ELSE
      FOR v_constraintrec IN
        SELECT
          con.conname as constraint_name,
          con.contype as constraint_type,
          CASE
            WHEN con.contype = 'p' THEN 1 -- primary key constraint
            WHEN con.contype = 'u' THEN 2 -- unique constraint
            WHEN con.contype = 'f' THEN 3 -- foreign key constraint
            WHEN con.contype = 'c' THEN 4
            ELSE 5
          END as type_rank,
          pg_get_constraintdef(con.oid) as constraint_definition
        FROM pg_catalog.pg_constraint con
            JOIN pg_catalog.pg_class rel ON rel.oid = con.conrelid
            JOIN pg_catalog.pg_namespace nsp ON nsp.oid = connamespace
        WHERE nsp.nspname = in_schema
            AND rel.relname = in_table
            -- Issue#103: do not get partitioned tables
            AND con.conparentid = 0
        ORDER BY type_rank
      LOOP
        -- Issue#85 fix
        -- constraintarr := constraintarr || v_constraintrec.constraint_name;
        constraintarr := constraintarr || v_constraintrec.constraint_name::text;
        IF v_constraintrec.type_rank = 1 THEN
            v_primary := True;
            v_constraint_name := v_constraintrec.constraint_name;
        END IF;
        IF NOT bfkeys AND v_constraintrec.constraint_type = 'f' THEN
            continue;
        END IF;
        v_table_ddl := v_table_ddl || '  ' -- note: two char spacer to start, to indent the column
          || 'CONSTRAINT' || ' '
          || v_constraintrec.constraint_name || ' '
          || v_constraintrec.constraint_definition
          || ',' || E'\n';
      END LOOP;
    END IF;
    
    -- drop the last comma before ending the create statement
    v_table_ddl = substr(v_table_ddl, 0, length(v_table_ddl) - 1) || E'\n';
    -- end the create table def but add inherits clause if valid
    IF bPartitioned and bInheritance THEN
      v_table_ddl := v_table_ddl || ') INHERITS (' || in_schema || '.' || v_parent || ') ' || v_relopts || ' ' || v_tablespace || ';' || E'\n';
    ELSIF v_pgversion >= 100000 AND bPartitioned and NOT bInheritance THEN
      -- See if this is a partitioned table (pg_class.relkind = 'p') and add the partitioned key
      SELECT pg_get_partkeydef (c1.oid) AS partition_key INTO v_partition_key
      FROM pg_class c1
          JOIN pg_namespace n ON (n.oid = c1.relnamespace)
          LEFT JOIN pg_partitioned_table p ON (c1.oid = p.partrelid)
      WHERE n.nspname = in_schema
          AND n.oid = c1.relnamespace
          AND c1.relname = in_table
          AND c1.relkind = 'p';
    END IF;
    IF v_partition_key IS NOT NULL AND v_partition_key <> '' THEN
      -- add partition clause
      -- NOTE:  cannot specify default tablespace for partitioned relations
      v_table_ddl := v_table_ddl || ') PARTITION BY ' || v_partition_key || ';' || E'\n';
    ELSIF bPartitioned AND not bInheritance THEN
      IF v_relopts <> '' THEN
        v_table_ddl := 'CREATE TABLE ' || in_schema || '.' || in_table || ' PARTITION OF ' || in_schema || '.' || v_parent || ' ' || v_partbound || v_relopts || ' ' || v_tablespace || '; ' || E'\n';
      ELSE
        v_table_ddl := 'CREATE TABLE ' || in_schema || '.' || in_table || ' PARTITION OF ' || in_schema || '.' || v_parent || ' ' || v_partbound || ' ' || v_tablespace || '; ' || E'\n';
      END IF;
    ELSIF bPartitioned and bInheritance THEN
      -- we already did this above
      v_table_ddl := v_table_ddl;
    ELSIF v_relopts <> '' THEN
      v_table_ddl := v_table_ddl || ') ' || v_relopts || ' ' || v_tablespace || ';' || E'\n';
    ELSE
      v_table_ddl := v_table_ddl || ') ' || v_tablespace || ';' || E'\n';
    END IF;
    -- suffix create statement with all of the indexes on the table
    FOR v_indexrec IN
      SELECT indexdef, indexname
      FROM pg_indexes
      WHERE (schemaname, tablename) = (in_schema, in_table)
    LOOP
      -- Issue#83 fix: loop through constraints and skip ones already defined
      bSkip = False;
      FOREACH constraintelement IN ARRAY constraintarr
      LOOP 
         IF constraintelement = v_indexrec.indexname THEN
             bSkip = True;
             EXIT;
         END IF;
      END LOOP;   
      if bSkip THEN CONTINUE; END IF;
      v_table_ddl := v_table_ddl
        || v_indexrec.indexdef
        || ';' || E'\n';
    END LOOP;

    -- reset search_path back to what it was
    IF v_src_path_old = '' THEN
      SELECT set_config('search_path', '', false) into v_dummy;
    ELSE
      EXECUTE 'SET search_path = ' || v_src_path_old;
    END IF;
    -- RAISE NOTICE 'DEBUG tableddl: reset search_path back to ***%***', v_src_path_old;

    -- return the ddl
    RETURN v_table_ddl;
  END;
$_$;


ALTER FUNCTION public.get_table_ddl(in_schema character varying, in_table character varying, bfkeys boolean) OWNER TO postgres;

--
-- Name: get_table_ddl_complex(text, text, text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_table_ddl_complex(src_schema text, dst_schema text, in_table text, sq_server_version_num integer) RETURNS text
    LANGUAGE plpgsql
    AS $$
  DECLARE
  v_table_ddl   text;
  v_buffer1     text;
  v_buffer2     text;

  BEGIN
      IF sq_server_version_num < 110000 THEN
      SELECT 'CREATE TABLE '
        || quote_ident(dst_schema)
        || '.'
        || pc.relname
        || E'(\n'
        || string_agg(
          pa.attname
            || ' '
            || pg_catalog.format_type(pa.atttypid, pa.atttypmod)
            || coalesce(
              ' DEFAULT '
                || (
                  SELECT pg_catalog.pg_get_expr(d.adbin, d.adrelid)
                  FROM pg_catalog.pg_attrdef d
                  WHERE d.adrelid = pa.attrelid
                    AND d.adnum = pa.attnum
                    AND pa.atthasdef
                ),
              ''
            )
            || ' '
            || CASE pa.attnotnull
              WHEN TRUE THEN 'NOT NULL'
              ELSE 'NULL'
              END,
          E',\n'
        )
        || coalesce(
          (
            SELECT
              E',\n'
              || string_agg(
                'CONSTRAINT '
                  || pc1.conname
                  || ' '
                  || pg_get_constraintdef(pc1.oid),
                E',\n'
                ORDER BY pc1.conindid
              )
            FROM pg_constraint pc1
            --Issue#103: do not return FKEYS for partitions since we assume it is implied by the one done on the parent table, otherwise error for trying to define it again.
            WHERE pc1.conrelid = pa.attrelid 
          ),
          ''
        )
      INTO v_buffer1  
      FROM pg_catalog.pg_attribute pa
        JOIN pg_catalog.pg_class pc ON pc.oid = pa.attrelid
          AND pc.relname = quote_ident(in_table)
        JOIN pg_catalog.pg_namespace pn ON pn.oid = pc.relnamespace
          AND pn.nspname = quote_ident(src_schema)
      WHERE pa.attnum > 0
        AND NOT pa.attisdropped
      GROUP BY pn.nspname, pc.relname, pa.attrelid;
      
      ELSE
      SELECT 'CREATE TABLE '
        || quote_ident(dst_schema)
        || '.'
        || pc.relname
        || E'(\n'
        || string_agg(
          pa.attname
            || ' '
            || pg_catalog.format_type(pa.atttypid, pa.atttypmod)
            || coalesce(
              ' DEFAULT '
                || (
                  SELECT pg_catalog.pg_get_expr(d.adbin, d.adrelid)
                  FROM pg_catalog.pg_attrdef d
                  WHERE d.adrelid = pa.attrelid
                    AND d.adnum = pa.attnum
                    AND pa.atthasdef
                ),
              ''
            )
            || ' '
            || CASE pa.attnotnull
              WHEN TRUE THEN 'NOT NULL'
              ELSE 'NULL'
              END,
          E',\n'
        )
        || coalesce(
          (
            SELECT
              E',\n'
              || string_agg(
                'CONSTRAINT '
                  || pc1.conname
                  || ' '
                  || pg_get_constraintdef(pc1.oid),
                E',\n'
                ORDER BY pc1.conindid
              )
            FROM pg_constraint pc1
            --Issue#103: do not return FKEYS for partitions since we assume it is implied by the one done on the parent table, otherwise error for trying to define it again.
            WHERE pc1.conrelid = pa.attrelid AND pc1.conparentid = 0 
          ),
          ''
        )
      INTO v_buffer1  
      FROM pg_catalog.pg_attribute pa
        JOIN pg_catalog.pg_class pc ON pc.oid = pa.attrelid
          AND pc.relname = quote_ident(in_table)
        JOIN pg_catalog.pg_namespace pn ON pn.oid = pc.relnamespace
          AND pn.nspname = quote_ident(src_schema)
      WHERE pa.attnum > 0
        AND NOT pa.attisdropped
      GROUP BY pn.nspname, pc.relname, pa.attrelid;
      END IF;

      -- append partition keyword to it
      SELECT pg_catalog.pg_get_partkeydef(c.oid::pg_catalog.oid) into v_buffer2
      FROM pg_catalog.pg_class c  LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
      WHERE c.relname = quote_ident(in_table) COLLATE pg_catalog.default AND n.nspname = quote_ident(src_schema) COLLATE pg_catalog.default;

      v_table_ddl := v_buffer1 || ') PARTITION BY ' || v_buffer2 || ';';
  
      RETURN v_table_ddl;
  END;
$$;


ALTER FUNCTION public.get_table_ddl_complex(src_schema text, dst_schema text, in_table text, sq_server_version_num integer) OWNER TO postgres;

--
-- Name: get_total_arrecadado_cinema(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_total_arrecadado_cinema(schemas text[]) RETURNS TABLE(nomecinema text, mesreferencia text, totalarrecadado numeric)
    LANGUAGE plpgsql
    AS $_$
DECLARE
    schema_name text;
    schema_count int := array_length(schemas, 1);
    start_date timestamp := NOW() - INTERVAL '1 year';
BEGIN
    FOR i IN 1..schema_count LOOP
        SELECT INTO schema_name schemas[i];

        RETURN QUERY EXECUTE format('
            SELECT
                c.nome AS nomeCinema,
                TO_CHAR(DATE_TRUNC(''month'', s."dtSessao"), ''YYYY-MM'') AS mesReferencia,
                SUM(s."vlEntrada") AS totalArrecadado
            FROM
                %1$I."Sessao" s
            LEFT JOIN
                public."Cinema" c ON s."idCinema" = c.id
            INNER JOIN
                %1$I."Ticket" t ON s.id = t."idSessao"
            WHERE
                c."id" = $1
                AND s."dtSessao" >= $2
                AND s."dtSessao" < NOW()
            GROUP BY
                nomeCinema, mesReferencia
        ', schema_name) 
        USING schemas[i], start_date;
    END LOOP;
END;
$_$;


ALTER FUNCTION public.get_total_arrecadado_cinema(schemas text[]) OWNER TO postgres;

--
-- Name: get_total_arrecadado_por_filme(text[], text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_total_arrecadado_por_filme(schemas text[], filmeid text) RETURNS TABLE(idfilme text, nomefilme text, totalarrecadado numeric)
    LANGUAGE plpgsql
    AS $_$
DECLARE
    schema_name text;
    schema_count int := array_length(schemas, 1);
BEGIN
    FOR i IN 1..schema_count LOOP
        SELECT INTO schema_name schemas[i];

        RETURN QUERY EXECUTE format('
            SELECT
                %1$L AS idFilme,
                f.nome AS nomeFilme,
                SUM(s."vlEntrada") AS totalArrecadado
            FROM
                %1$I."Sessao" s
            LEFT JOIN
                public."Filmes" f ON s."idFilme" = f.id
            INNER JOIN
                %1$I."Ticket" t ON s.id = t."idSessao"
            WHERE
                s."idFilme" = $1
                AND s."dtSessao" < NOW()
            GROUP BY
                f.id, f.nome
        ', schema_name) USING filmeId;
    END LOOP;
END;
$_$;


ALTER FUNCTION public.get_total_arrecadado_por_filme(schemas text[], filmeid text) OWNER TO postgres;

--
-- Name: get_total_arrecadado_tickets_semana_corrente(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_total_arrecadado_tickets_semana_corrente(schemas text[]) RETURNS TABLE(cinemanome text, totalarrecadado numeric)
    LANGUAGE plpgsql
    AS $_$
DECLARE
    schema_name text;
    schema_count int := array_length(schemas, 1);
    start_of_week timestamp := date_trunc('day', current_date) - INTERVAL '7 days'; 
BEGIN
    FOR i IN 1..schema_count LOOP
        SELECT INTO schema_name schemas[i];

        RETURN QUERY EXECUTE format('
            SELECT
                c.nome AS cinemaNome,
                COALESCE(SUM(s."vlEntrada"), 0.0) AS totalArrecadado
            FROM
                %1$I."Sessao" s
            INNER JOIN
                %1$I."Ticket" t ON s.id = t."idSessao"
            LEFT JOIN
                public."Cinema" c ON s."idCinema" = c.id
            WHERE
                s."dtSessao" >= %2$L
                AND s."dtSessao" < NOW()
            GROUP BY
                c.nome
        ', schema_name, start_of_week);
    END LOOP;
END;
$_$;


ALTER FUNCTION public.get_total_arrecadado_tickets_semana_corrente(schemas text[]) OWNER TO postgres;

--
-- Name: get_ultimas_vendas(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_ultimas_vendas(schemas text[]) RETURNS TABLE(nomereserva text, nomecinema text, valor numeric, datareserva text)
    LANGUAGE plpgsql
    AS $_$
DECLARE
    schema_name text;
    schema_count int := array_length(schemas, 1);
BEGIN
    FOR i IN 1..schema_count LOOP
        SELECT INTO schema_name schemas[i];

        RETURN QUERY EXECUTE format('
            SELECT
                t."nomeReserva" AS nomeReserva,
                c.nome AS nomeCinema,
               "s"."vlEntrada" AS valor,
				TO_CHAR(t."createdAt", ''YYYY-MM-DD HH:MI:SS'') AS dataReserva
            FROM
                %1$I."Sessao" s
            LEFT JOIN
                public."Cinema" c ON s."idCinema" = c.id
            INNER JOIN
                %1$I."Ticket" t ON s.id = t."idSessao"
        ', schema_name) LIMIT 5;
    END LOOP;
END;
$_$;


ALTER FUNCTION public.get_ultimas_vendas(schemas text[]) OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: Assento; Type: TABLE; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

CREATE TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Assento" (
    id text NOT NULL,
    numero integer NOT NULL,
    "idSessao" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    reservado boolean NOT NULL
);


ALTER TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Assento" OWNER TO postgres;

--
-- Name: Avaliacao; Type: TABLE; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

CREATE TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Avaliacao" (
    id text NOT NULL,
    "idFilme" text NOT NULL,
    valor integer NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Avaliacao" OWNER TO postgres;

--
-- Name: Cinema; Type: TABLE; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

CREATE TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Cinema" (
    id text NOT NULL,
    nome text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Cinema" OWNER TO postgres;

--
-- Name: Filmes; Type: TABLE; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

CREATE TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Filmes" (
    id text NOT NULL,
    nome text NOT NULL,
    sinopse text NOT NULL,
    "dtLancamento" timestamp(3) without time zone NOT NULL,
    "capaUrl" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    disponivel boolean DEFAULT true NOT NULL,
    "linkTrailer" text DEFAULT ''::text NOT NULL
);


ALTER TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Filmes" OWNER TO postgres;

--
-- Name: Sala; Type: TABLE; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

CREATE TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Sala" (
    id text NOT NULL,
    nome text NOT NULL,
    "idCinema" text NOT NULL,
    capacidade integer NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Sala" OWNER TO postgres;

--
-- Name: Sessao; Type: TABLE; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

CREATE TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Sessao" (
    id text NOT NULL,
    "idCinema" text NOT NULL,
    "idFilme" text NOT NULL,
    "vlEntrada" numeric(65,30) NOT NULL,
    "horaInicio" text NOT NULL,
    "idSala" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    "dtSessao" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Sessao" OWNER TO postgres;

--
-- Name: Ticket; Type: TABLE; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

CREATE TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Ticket" (
    id text NOT NULL,
    "cpfReserva" text NOT NULL,
    "nomeReserva" text NOT NULL,
    "idAssento" text NOT NULL,
    "idSessao" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Ticket" OWNER TO postgres;

--
-- Name: Usuario; Type: TABLE; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

CREATE TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario" (
    id integer NOT NULL,
    email text NOT NULL,
    nome text,
    senha text,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario" OWNER TO postgres;

--
-- Name: UsuarioCinema; Type: TABLE; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

CREATE TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema" (
    id integer NOT NULL,
    "isAdmin" boolean DEFAULT false NOT NULL,
    "idUsuario" integer NOT NULL,
    "idCinema" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updatedAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema" OWNER TO postgres;

--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

CREATE SEQUENCE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema_id_seq"
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema_id_seq" OWNER TO postgres;

--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE OWNED BY; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER SEQUENCE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema_id_seq" OWNED BY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema".id;


--
-- Name: Usuario_id_seq; Type: SEQUENCE; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

CREATE SEQUENCE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario_id_seq"
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario_id_seq" OWNER TO postgres;

--
-- Name: Usuario_id_seq; Type: SEQUENCE OWNED BY; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER SEQUENCE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario_id_seq" OWNED BY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario".id;


--
-- Name: Assento; Type: TABLE; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

CREATE TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Assento" (
    id text NOT NULL,
    numero integer NOT NULL,
    "idSessao" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    reservado boolean NOT NULL
);


ALTER TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Assento" OWNER TO postgres;

--
-- Name: Avaliacao; Type: TABLE; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

CREATE TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Avaliacao" (
    id text NOT NULL,
    "idFilme" text NOT NULL,
    valor integer NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Avaliacao" OWNER TO postgres;

--
-- Name: Cinema; Type: TABLE; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

CREATE TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Cinema" (
    id text NOT NULL,
    nome text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Cinema" OWNER TO postgres;

--
-- Name: Filmes; Type: TABLE; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

CREATE TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Filmes" (
    id text NOT NULL,
    nome text NOT NULL,
    sinopse text NOT NULL,
    "dtLancamento" timestamp(3) without time zone NOT NULL,
    "capaUrl" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    disponivel boolean DEFAULT true NOT NULL,
    "linkTrailer" text DEFAULT ''::text NOT NULL
);


ALTER TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Filmes" OWNER TO postgres;

--
-- Name: Sala; Type: TABLE; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

CREATE TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Sala" (
    id text NOT NULL,
    nome text NOT NULL,
    "idCinema" text NOT NULL,
    capacidade integer NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Sala" OWNER TO postgres;

--
-- Name: Sessao; Type: TABLE; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

CREATE TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Sessao" (
    id text NOT NULL,
    "idCinema" text NOT NULL,
    "idFilme" text NOT NULL,
    "vlEntrada" numeric(65,30) NOT NULL,
    "horaInicio" text NOT NULL,
    "idSala" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    "dtSessao" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Sessao" OWNER TO postgres;

--
-- Name: Ticket; Type: TABLE; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

CREATE TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Ticket" (
    id text NOT NULL,
    "cpfReserva" text NOT NULL,
    "nomeReserva" text NOT NULL,
    "idAssento" text NOT NULL,
    "idSessao" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Ticket" OWNER TO postgres;

--
-- Name: Usuario; Type: TABLE; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

CREATE TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario" (
    id integer NOT NULL,
    email text NOT NULL,
    nome text,
    senha text,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario" OWNER TO postgres;

--
-- Name: UsuarioCinema; Type: TABLE; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

CREATE TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema" (
    id integer NOT NULL,
    "isAdmin" boolean DEFAULT false NOT NULL,
    "idUsuario" integer NOT NULL,
    "idCinema" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updatedAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema" OWNER TO postgres;

--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

CREATE SEQUENCE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema_id_seq"
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema_id_seq" OWNER TO postgres;

--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE OWNED BY; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER SEQUENCE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema_id_seq" OWNED BY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema".id;


--
-- Name: Usuario_id_seq; Type: SEQUENCE; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

CREATE SEQUENCE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario_id_seq"
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario_id_seq" OWNER TO postgres;

--
-- Name: Usuario_id_seq; Type: SEQUENCE OWNED BY; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER SEQUENCE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario_id_seq" OWNED BY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario".id;


--
-- Name: Assento; Type: TABLE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Assento" (
    id text NOT NULL,
    numero integer NOT NULL,
    "idSessao" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    reservado boolean NOT NULL
);


ALTER TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Assento" OWNER TO postgres;

--
-- Name: Avaliacao; Type: TABLE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Avaliacao" (
    id text NOT NULL,
    "idFilme" text NOT NULL,
    valor integer NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Avaliacao" OWNER TO postgres;

--
-- Name: Cinema; Type: TABLE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Cinema" (
    id text NOT NULL,
    nome text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Cinema" OWNER TO postgres;

--
-- Name: Filmes; Type: TABLE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Filmes" (
    id text NOT NULL,
    nome text NOT NULL,
    sinopse text NOT NULL,
    "dtLancamento" timestamp(3) without time zone NOT NULL,
    "capaUrl" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    disponivel boolean DEFAULT true NOT NULL,
    "linkTrailer" text DEFAULT ''::text NOT NULL
);


ALTER TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Filmes" OWNER TO postgres;

--
-- Name: Sala; Type: TABLE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Sala" (
    id text NOT NULL,
    nome text NOT NULL,
    "idCinema" text NOT NULL,
    capacidade integer NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Sala" OWNER TO postgres;

--
-- Name: Sessao; Type: TABLE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Sessao" (
    id text NOT NULL,
    "idCinema" text NOT NULL,
    "idFilme" text NOT NULL,
    "vlEntrada" numeric(65,30) NOT NULL,
    "horaInicio" text NOT NULL,
    "idSala" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    "dtSessao" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Sessao" OWNER TO postgres;

--
-- Name: Ticket; Type: TABLE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Ticket" (
    id text NOT NULL,
    "cpfReserva" text NOT NULL,
    "nomeReserva" text NOT NULL,
    "idAssento" text NOT NULL,
    "idSessao" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Ticket" OWNER TO postgres;

--
-- Name: Usuario; Type: TABLE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario" (
    id integer NOT NULL,
    email text NOT NULL,
    nome text,
    senha text,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario" OWNER TO postgres;

--
-- Name: UsuarioCinema; Type: TABLE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema" (
    id integer NOT NULL,
    "isAdmin" boolean DEFAULT false NOT NULL,
    "idUsuario" integer NOT NULL,
    "idCinema" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updatedAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema" OWNER TO postgres;

--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE SEQUENCE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema_id_seq"
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema_id_seq" OWNER TO postgres;

--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE OWNED BY; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER SEQUENCE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema_id_seq" OWNED BY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema".id;


--
-- Name: Usuario_id_seq; Type: SEQUENCE; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE SEQUENCE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario_id_seq"
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario_id_seq" OWNER TO postgres;

--
-- Name: Usuario_id_seq; Type: SEQUENCE OWNED BY; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER SEQUENCE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario_id_seq" OWNED BY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario".id;


--
-- Name: Assento; Type: TABLE; Schema: defaultschema; Owner: postgres
--

CREATE TABLE defaultschema."Assento" (
    id text NOT NULL,
    numero integer NOT NULL,
    "idSessao" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    reservado boolean NOT NULL
);


ALTER TABLE defaultschema."Assento" OWNER TO postgres;

--
-- Name: Avaliacao; Type: TABLE; Schema: defaultschema; Owner: postgres
--

CREATE TABLE defaultschema."Avaliacao" (
    id text NOT NULL,
    "idFilme" text NOT NULL,
    valor integer NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE defaultschema."Avaliacao" OWNER TO postgres;

--
-- Name: Cinema; Type: TABLE; Schema: defaultschema; Owner: postgres
--

CREATE TABLE defaultschema."Cinema" (
    id text NOT NULL,
    nome text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE defaultschema."Cinema" OWNER TO postgres;

--
-- Name: Filmes; Type: TABLE; Schema: defaultschema; Owner: postgres
--

CREATE TABLE defaultschema."Filmes" (
    id text NOT NULL,
    nome text NOT NULL,
    sinopse text NOT NULL,
    "dtLancamento" timestamp(3) without time zone NOT NULL,
    "capaUrl" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    disponivel boolean DEFAULT true NOT NULL,
    "linkTrailer" text DEFAULT ''::text NOT NULL
);


ALTER TABLE defaultschema."Filmes" OWNER TO postgres;

--
-- Name: Sala; Type: TABLE; Schema: defaultschema; Owner: postgres
--

CREATE TABLE defaultschema."Sala" (
    id text NOT NULL,
    nome text NOT NULL,
    "idCinema" text NOT NULL,
    capacidade integer NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE defaultschema."Sala" OWNER TO postgres;

--
-- Name: Sessao; Type: TABLE; Schema: defaultschema; Owner: postgres
--

CREATE TABLE defaultschema."Sessao" (
    id text NOT NULL,
    "idCinema" text NOT NULL,
    "idFilme" text NOT NULL,
    "vlEntrada" numeric(65,30) NOT NULL,
    "horaInicio" text NOT NULL,
    "idSala" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    "dtSessao" timestamp(3) without time zone NOT NULL
);


ALTER TABLE defaultschema."Sessao" OWNER TO postgres;

--
-- Name: Ticket; Type: TABLE; Schema: defaultschema; Owner: postgres
--

CREATE TABLE defaultschema."Ticket" (
    id text NOT NULL,
    "cpfReserva" text NOT NULL,
    "nomeReserva" text NOT NULL,
    "idAssento" text NOT NULL,
    "idSessao" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE defaultschema."Ticket" OWNER TO postgres;

--
-- Name: Usuario; Type: TABLE; Schema: defaultschema; Owner: postgres
--

CREATE TABLE defaultschema."Usuario" (
    id integer NOT NULL,
    email text NOT NULL,
    nome text,
    senha text,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE defaultschema."Usuario" OWNER TO postgres;

--
-- Name: UsuarioCinema; Type: TABLE; Schema: defaultschema; Owner: postgres
--

CREATE TABLE defaultschema."UsuarioCinema" (
    id integer NOT NULL,
    "isAdmin" boolean DEFAULT false NOT NULL,
    "idUsuario" integer NOT NULL,
    "idCinema" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updatedAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE defaultschema."UsuarioCinema" OWNER TO postgres;

--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE; Schema: defaultschema; Owner: postgres
--

CREATE SEQUENCE defaultschema."UsuarioCinema_id_seq"
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE defaultschema."UsuarioCinema_id_seq" OWNER TO postgres;

--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE OWNED BY; Schema: defaultschema; Owner: postgres
--

ALTER SEQUENCE defaultschema."UsuarioCinema_id_seq" OWNED BY defaultschema."UsuarioCinema".id;


--
-- Name: Usuario_id_seq; Type: SEQUENCE; Schema: defaultschema; Owner: postgres
--

CREATE SEQUENCE defaultschema."Usuario_id_seq"
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE defaultschema."Usuario_id_seq" OWNER TO postgres;

--
-- Name: Usuario_id_seq; Type: SEQUENCE OWNED BY; Schema: defaultschema; Owner: postgres
--

ALTER SEQUENCE defaultschema."Usuario_id_seq" OWNED BY defaultschema."Usuario".id;


--
-- Name: Assento; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Assento" (
    id text NOT NULL,
    numero integer NOT NULL,
    "idSessao" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    reservado boolean NOT NULL
);


ALTER TABLE public."Assento" OWNER TO postgres;

--
-- Name: Avaliacao; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Avaliacao" (
    id text NOT NULL,
    "idFilme" text NOT NULL,
    valor integer NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE public."Avaliacao" OWNER TO postgres;

--
-- Name: Cinema; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Cinema" (
    id text NOT NULL,
    nome text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE public."Cinema" OWNER TO postgres;

--
-- Name: Filmes; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Filmes" (
    id text NOT NULL,
    nome text NOT NULL,
    sinopse text NOT NULL,
    "dtLancamento" timestamp(3) without time zone NOT NULL,
    "capaUrl" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    disponivel boolean DEFAULT true NOT NULL,
    "linkTrailer" text DEFAULT ''::text NOT NULL
);


ALTER TABLE public."Filmes" OWNER TO postgres;

--
-- Name: Sala; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Sala" (
    id text NOT NULL,
    nome text NOT NULL,
    "idCinema" text NOT NULL,
    capacidade integer NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE public."Sala" OWNER TO postgres;

--
-- Name: Sessao; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Sessao" (
    id text NOT NULL,
    "idCinema" text NOT NULL,
    "idFilme" text NOT NULL,
    "vlEntrada" numeric(65,30) NOT NULL,
    "horaInicio" text NOT NULL,
    "idSala" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL,
    "dtSessao" timestamp(3) without time zone NOT NULL
);


ALTER TABLE public."Sessao" OWNER TO postgres;

--
-- Name: Ticket; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Ticket" (
    id text NOT NULL,
    "cpfReserva" text NOT NULL,
    "nomeReserva" text NOT NULL,
    "idAssento" text NOT NULL,
    "idSessao" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE public."Ticket" OWNER TO postgres;

--
-- Name: Usuario; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Usuario" (
    id integer NOT NULL,
    email text NOT NULL,
    nome text,
    senha text,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updateAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE public."Usuario" OWNER TO postgres;

--
-- Name: UsuarioCinema; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."UsuarioCinema" (
    id integer NOT NULL,
    "isAdmin" boolean DEFAULT false NOT NULL,
    "idUsuario" integer NOT NULL,
    "idCinema" text NOT NULL,
    "createdAt" timestamp(3) without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "updatedAt" timestamp(3) without time zone NOT NULL
);


ALTER TABLE public."UsuarioCinema" OWNER TO postgres;

--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public."UsuarioCinema_id_seq"
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public."UsuarioCinema_id_seq" OWNER TO postgres;

--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public."UsuarioCinema_id_seq" OWNED BY public."UsuarioCinema".id;


--
-- Name: Usuario_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public."Usuario_id_seq"
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public."Usuario_id_seq" OWNER TO postgres;

--
-- Name: Usuario_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public."Usuario_id_seq" OWNED BY public."Usuario".id;


--
-- Name: Usuario id; Type: DEFAULT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario" ALTER COLUMN id SET DEFAULT nextval('"0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario_id_seq"'::regclass);


--
-- Name: UsuarioCinema id; Type: DEFAULT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema" ALTER COLUMN id SET DEFAULT nextval('"0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema_id_seq"'::regclass);


--
-- Name: Usuario id; Type: DEFAULT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario" ALTER COLUMN id SET DEFAULT nextval('"68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario_id_seq"'::regclass);


--
-- Name: UsuarioCinema id; Type: DEFAULT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema" ALTER COLUMN id SET DEFAULT nextval('"68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema_id_seq"'::regclass);


--
-- Name: Usuario id; Type: DEFAULT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario" ALTER COLUMN id SET DEFAULT nextval('"8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario_id_seq"'::regclass);


--
-- Name: UsuarioCinema id; Type: DEFAULT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema" ALTER COLUMN id SET DEFAULT nextval('"8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema_id_seq"'::regclass);


--
-- Name: Usuario id; Type: DEFAULT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Usuario" ALTER COLUMN id SET DEFAULT nextval('defaultschema."Usuario_id_seq"'::regclass);


--
-- Name: UsuarioCinema id; Type: DEFAULT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."UsuarioCinema" ALTER COLUMN id SET DEFAULT nextval('defaultschema."UsuarioCinema_id_seq"'::regclass);


--
-- Name: Usuario id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Usuario" ALTER COLUMN id SET DEFAULT nextval('public."Usuario_id_seq"'::regclass);


--
-- Name: UsuarioCinema id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UsuarioCinema" ALTER COLUMN id SET DEFAULT nextval('public."UsuarioCinema_id_seq"'::regclass);


--
-- Data for Name: Assento; Type: TABLE DATA; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

COPY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Assento" (id, numero, "idSessao", "createdAt", "updateAt", reservado) FROM stdin;
b3261f38-2bff-49e5-9a94-87448e78613f	1	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ee15f933-5578-495e-8789-7945d2be1e6c	2	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e26e32ef-7b5e-4cef-9f45-d773dc9b9bc0	4	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ef67e3a4-91cc-44de-8922-70d821ba88ad	5	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f36e86ba-cc44-470b-a4ba-90a4cf7941fc	6	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a0fa49cf-1c39-4b73-a0f8-500e96096364	7	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8e22db15-58a7-4b6b-a4ce-58b30a24b3f6	10	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c8bafc24-0388-47a5-bc1c-e08325430516	11	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4be13bc2-0b9c-4fc0-b02e-03ee8124e9b2	13	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
92d5d9d9-8eed-4615-a59b-90e332ec807e	15	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
78b887aa-498c-4a83-a82d-25c3b9863ad5	16	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
5568cb18-c962-4694-b48d-d01c70d83de7	19	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d812638e-f266-4724-a165-05e846cf3f6a	20	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b9bda1b5-e04a-4a67-9935-9c085854dc89	21	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3d03ed29-0389-44d2-a0c1-2b661f23bfc5	22	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3c0c8230-da1a-413d-8122-e48cefa98bcc	23	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b217fc2c-3164-4caf-a373-a86bb66ecb95	25	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
29347b69-dba0-41ce-80b6-647524cc387c	28	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
52d44225-62b2-448b-a899-0212a2cc9edb	29	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
870f8a7c-3cfc-4715-b23b-c7fd7d202f07	30	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cc182984-e696-45ca-8492-bcba94c4903f	31	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0b6498d8-b405-48d6-9e9f-3903ec219a33	32	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f1475c19-65bc-459f-8a66-1845c96c97dd	33	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0d088bf5-44de-40eb-92c8-d49cc7afd24f	34	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a85fe6e4-3654-41fe-b2e2-824ab4ddf473	35	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
91b4e0e3-e370-4fb2-ab1f-71452db0ebd3	38	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
30de9127-e97f-441a-9912-27dc089de3d0	39	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
18214d42-a172-487c-9269-0681d03befdd	40	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
5044e2d6-1b80-402d-a896-0abc0e038472	42	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
5f769db3-055b-4ea3-811c-b8c6200e7c6a	43	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
77e831b6-f51a-4a6b-810c-9ca35320618c	45	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
817c7557-1bf9-4e69-bc80-90824ef72c93	46	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d52f9c2b-ce81-4275-ab35-e172e5d2ec23	47	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
099df4fa-1e35-4d79-b0b0-193ec9854f96	48	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1ce800d6-d7fe-4b22-b5c7-3cec4ca0ed3b	49	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
bd1233e8-9db9-4225-9249-2cb71a7acd0e	50	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d5b8f199-5edf-462f-84d2-db68c032a97c	52	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4c425f55-c7eb-4ad0-9429-35c70c3f6e93	53	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8f45be45-709e-4368-aaee-1161f365f755	55	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
bea239f6-0301-44f0-a290-8b4b9a5fb1f7	56	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
832047a0-dd4c-405e-9a28-1c4873b547d0	58	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0e130cae-e622-4aa6-8413-3b39e3640fa0	3	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:52:50.961	t
d51e1b92-b196-4d6f-8f2c-2ccc669567eb	14	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:52:50.961	t
5e9dc46c-e98e-4207-84a1-ee513d89c47a	17	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:52:50.961	t
fb7bd3e6-63ff-430c-b312-59e8a4e7af6c	26	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:52:50.961	t
ea516ffc-c7b9-4125-a9f4-a7f4c3bc19b6	27	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:52:50.961	t
57d7c262-447d-47fa-80fe-87677b4f19ea	12	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:54:33.728	t
160649cc-ef29-4244-8000-716d9eee4595	24	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:54:33.728	t
081c7d12-27ec-44e0-a6c9-f5e921ef5df1	36	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:54:33.728	t
0cec4c2d-0eb4-4cd6-8f75-2c7e600cc539	8	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:55:00.019	t
7f5bacda-0914-4db2-bee4-50018441dc4e	9	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:55:00.019	t
dedcff33-ef86-452c-b064-10098eca15de	37	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-17 16:04:34.051	t
28955abc-cc4e-4baf-b2af-3160c22209fc	59	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e6ba0fc8-b1f7-4b3c-9e9d-2b60e979651c	60	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
43439c5c-bb86-4634-9437-6dbe8c9b4585	61	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3ea39ea2-b526-424c-9789-c6fc6c326c30	62	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e4c2b2ed-0d17-45fc-b7bb-795bf8ea60ce	63	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c0692394-e26a-44cd-a15d-cf855c6f8ad4	64	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
feb3a5d9-412d-4220-b2c4-c5a2ca8ebec7	65	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0abea857-1e58-427d-b067-6bae349046e4	66	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7ec0e321-2ac7-479a-b476-f25b1be64aea	67	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
df6cf828-1efc-4c23-98f4-42916be2752c	68	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
132b64d5-e7ad-4637-9d99-f3794e371177	69	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d16cc28b-0e3b-44d3-85dd-4cae3c868937	70	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4f2ed5a4-2772-4458-a823-e2c3458e9951	71	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
718e8711-59cb-4a76-862c-bfbcbc965c63	73	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1533ae2d-2cdb-45ff-aa44-7935aba6d9b7	74	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
00bc1541-83dc-4ce4-8015-bb44a9d96e3b	75	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8d9db1e1-21aa-41b9-aeb2-e0ba96031248	76	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3deedb2c-c917-46ee-bb6f-32e22101c2e5	77	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9d1221a5-8725-4bef-8a4e-64b8de6ef9b7	78	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a09a0d67-f280-405c-8b43-241fe4b2a575	80	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
77b2503a-04b7-4beb-94c8-eddd505d512c	1	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9271b5cc-4515-4fe4-9e32-23076bff4bc7	2	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
bdc30b6a-2aa9-43c0-97b8-74047c2116bd	3	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
5ed2ecf3-4f73-400e-a383-d296627e0ae4	4	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4469317b-72fb-41f7-bced-63cd00376994	5	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7f000c05-3a73-43b6-96ff-32a45fe86569	6	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
213c9e70-15ad-42cf-aa8e-09dc0c8188b2	7	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
87f21f04-a782-409a-b702-08464da4999e	8	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e3e11198-97f5-4685-8409-1f8dd5c287de	9	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4076b73a-db8e-43f3-b8f7-bee4cb8ba533	10	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
67116acd-72d3-425e-80a4-a84b02c78682	11	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fe87788c-ad16-4ff2-8f5e-d7ce06d395d8	12	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
369b5658-f069-4e9d-858e-d858da0aa88d	13	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d1fb8732-cd7d-45b1-9fce-b5f02c610055	14	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
84ed2946-b50c-4316-9dd0-846712cb8643	15	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8ea9ab9a-5849-4160-b48a-d32eb137c7f6	16	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fe35851f-36a3-404a-b783-a24b3297db16	17	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
48d491a1-9a8a-444b-9e0e-181dc9568a2e	18	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0d8f7047-da16-47f0-89ef-40be7a642208	19	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d89fd445-f02a-4216-b0fb-b04af03807b6	20	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
db55f2c1-e133-40f3-8a49-3a7ae2c97787	21	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4d0d96de-ea5e-4b5e-bb99-de4692da7bc5	22	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ec5b71fb-8216-4ce7-8887-8e2e4d7afa64	23	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b972512a-ca69-48e8-96db-cc7b1d0c1fd7	24	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ab9e8938-288d-4fd9-be7d-f311bdef7750	25	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ba2942bc-ef55-4858-a1df-4114c9821561	26	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2c98a5f5-8397-4896-a0e2-7e6d93f7cdc7	27	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1421a294-4526-4af3-9474-386f47e76764	28	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cc08efff-dc44-48a6-ace0-a885e09e0daa	29	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1faaa836-e449-4ec6-9a0b-400f8feb5ab9	30	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2ec399c4-adf3-4dc2-9870-7a7bf14d5170	31	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
31022629-28bc-4c7b-a56f-067a8ec57400	32	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
bd6af2eb-10be-469a-af1e-db1c5c9dcff1	33	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ef82f4b8-5659-49a0-acac-ee59fe413e5a	34	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
bd0e6b58-b0f1-4d08-80a7-8cdb53c88b82	35	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
001be9df-bd1f-4bd0-8822-adfa754ac875	36	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1a47b6af-ee84-4568-af3c-1b426e3021c0	37	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d3cfcdc9-6270-4b3c-a8d0-481705bf0b42	38	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0313c500-8dfe-489b-800c-859383088c86	39	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f70b42c9-d3f5-4820-beaf-08aecbe098be	40	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d222699b-2918-4211-868b-787fcf4f5b74	41	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c5881880-00c4-4aed-b1f0-31f607fb9317	42	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
568d408a-4cf1-44d6-85c1-3576a6880f0d	43	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b3815463-2b31-4950-9b6a-0357049f2223	44	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cc330c9a-80cd-4f2a-a7f0-5ebc9049ef9e	45	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
aa078575-a7ee-4376-92b3-595c8a0d5b51	46	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cb658b53-3ff8-4d8b-b2dc-b66a4547f53f	47	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
561dec6a-fc20-4679-8b1b-9412535534af	48	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f3c0ffb8-2a41-48b1-96b3-e1e3b355349b	49	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
38408dea-fdb6-42ef-b7f2-f22ec2257b4a	50	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e81b5a51-e5ee-4f53-915c-2df82c57ef8d	51	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
824eb962-9e81-4d6f-8e9e-f72136516b98	52	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8be995fb-1103-4383-b5eb-54b8df81790d	53	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0888fbdd-bbc9-4e34-95a9-87234670c5ba	54	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fdb3f064-8f7b-461c-900f-f952ffb0b65b	55	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9a6caa6f-9883-4398-b76d-9f6d3abae231	56	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a3a2de56-26d4-4acc-b62a-661b52f505ff	57	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e0b07e6d-5c9e-415a-8448-c3f9833af9a1	58	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f1c24764-74d4-4c20-b45c-4c1bfe46f0d2	59	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c3e272e6-ae67-4322-b993-3ec35ed6c4e5	60	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f1e735c3-3b43-418f-b5e6-b516856a2964	61	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
45cdee93-5cf1-4a49-8e2b-1ab393bd393c	62	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fc3e9547-bb3b-4dd7-b127-7154bc1ffbee	63	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
93f2f43d-4439-4c67-883f-4048ac35385d	64	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
5a0a47fa-0ce8-4391-a999-388b34f350d1	65	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
32327ffe-708c-4cd4-b384-804b46fcda83	66	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ed4af5e2-21d0-49d4-a519-79ba6584aedf	67	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3f9441ef-cbc3-4788-8e1b-84cdf8a2daa6	68	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
99308137-9fc1-4b9a-b347-13e344a24d9e	69	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2818534a-34cf-40ba-9e16-732558393e2a	70	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8da2f4af-d8d5-488d-b8d2-ae40a51781d3	71	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
922651f6-bb6f-48ca-b116-a034e4d09794	72	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
41a44915-473c-4d95-b305-915ff459f5c5	73	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
762f1db6-f7d4-47d9-aa2b-bb71483b6a7b	74	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
5bd98092-9182-4b62-8144-c521460c148b	75	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7e571aa8-4185-401b-a584-484fc0466802	76	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
92db5b87-06a1-425e-b4d7-f65bdfba93f6	77	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7bc5f980-1467-41a0-aab3-c64dbe7a9627	78	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
52865325-bd5b-49e9-a054-d8720b8f84cf	79	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8a9aeab9-685d-49d1-94cd-5f5a913ab38a	80	65faaa59-13e6-45ee-8129-8805a26eb568	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cbbab85a-184a-4e57-82cf-277792c5f007	1	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
064adcc6-304a-4394-a06a-83e093ddfef6	2	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8aee84fa-3b82-480b-8012-650a66655da9	3	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a96d74bd-4202-4410-aff9-a966f1a147b4	4	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
94d91dae-d34b-4b71-9f98-5ddd85092b60	5	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
059c5719-67d0-4147-b420-805f82d2ea82	6	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a37188fe-b117-49de-8fd4-04c10710b336	7	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4a73a380-506b-40af-ba13-6a010f4931ae	8	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a926325e-0b85-40ea-8e13-8d3516896dd2	9	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
451de603-b169-4013-8d75-bf44c70b5c08	10	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
03d47b58-2a27-4957-9910-32b603b08bc0	11	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2a6b26b4-eee8-4075-a9eb-ed15f29cf773	12	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d7b35d15-4e83-4ed5-abdc-b475040072e4	13	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0132a135-4369-4095-bf1d-a969fec6d655	14	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d07556ba-b06b-45aa-a778-b90aeeb15e1b	15	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
76660227-e2aa-4e14-be88-ecba45552189	16	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e7fff33d-ccf3-4b03-92be-020ba0e9355f	17	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ceee5794-8876-4ae7-9ee8-bf24f29a9bba	18	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b7dcacc2-ad84-4314-a57a-bcc7b69a502e	19	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ee21345a-cfb2-41be-9981-f630af9baf03	20	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cf1517d4-eadd-4731-bee6-220b59c2b2a3	21	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8a6cc762-4c08-4043-901d-0cd38a2fdc67	22	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d6f8a156-d91e-42e4-afe1-d1f503f9d3e4	23	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a39d134c-5153-4027-94d0-9e7813de4da2	24	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
57f3739a-35be-4f3e-86d1-f85cc21a6b7b	25	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
dcc98698-bf8e-49d5-add7-22dc353acfc0	26	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c92719b9-6c11-4649-9d61-8bf1dda1d657	27	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c44fbcfd-87b7-482c-828d-33c19981ccc0	28	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
829695ea-d062-447b-877a-81efe8e361df	29	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ba446f7b-bc12-4b71-8a53-3e043993b5fc	30	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c6694d65-4bbd-40ce-b478-1cb54ff22ba1	31	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a81e7374-513b-4ec1-a71d-1eebb5c06d09	32	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
714ed718-5192-4ac7-a174-9771216caea7	33	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ed64bafa-1946-469e-af6c-316b5372e86f	34	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c4f9cb65-1c02-4a2e-bc98-9fb21cee1554	35	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8843ad4e-e353-43d2-bbd8-76cfbe3c1b93	36	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ab370907-8c4c-46ba-924d-c529c21d7fdd	37	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b2d5008f-1da3-423f-bd6b-1b47dc4fa643	38	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ed80b089-9717-4d35-877a-37d33443c1b6	39	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3f6f5153-4edf-42f9-89c0-064cee3760d0	40	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
74c6e81b-ba78-4f80-bc92-b93fd1fc5a85	41	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9919d37e-ef47-490d-a6a2-13b4300cafed	42	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2a20f885-c29d-450b-9641-b4a5585c692b	43	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4a18a2a8-53cb-4fb2-a029-fa810500ac71	44	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f47a3a42-b578-405e-86cd-59e1dd9bdddb	45	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
22d3a52a-f525-4908-b275-a421aa484ace	46	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
6e58c28c-32bd-491d-bc21-f59c05ca0a68	47	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
5b303d0c-c23e-45b5-b609-0634f0e46191	48	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0a4aa25a-77fc-4d80-a3b2-3030eda4d4e8	49	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9f369b97-7c0e-4688-b0e5-c1559951b2ba	50	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
bfff43a0-60d1-4f0e-9117-78effb0fc9a2	51	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
622cc75a-1210-4b7d-90b8-16fd3b87d991	52	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4e800a08-d159-4e62-87e1-4d514a35f8c6	53	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e0196e4f-f05d-44ed-9963-adf8930fcef3	54	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fb9506bc-6d81-4ed2-a837-527b4735f5d1	55	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4b857213-8028-4658-bf4f-9a28b03b0b73	56	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e59389c6-aec8-47b7-afda-c9879f90f0d5	57	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2956fa76-ca95-4341-9332-0977b452c74e	58	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
89f07d79-2b97-401f-b07c-c04976f1e622	59	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
46278ee5-8c07-40ce-a645-79f99cfc08b7	60	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d36b8847-ca88-478d-8778-dd0bcb5ff4c6	61	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
26a9a5e4-728e-42b2-8da7-deef6a08d803	62	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8ba1b147-77f4-4655-8ea6-acf4ee87ff9e	63	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
35f616c7-6438-41fc-8314-77d92028cbd1	64	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
973cee73-e3ae-4fc3-ae71-f6d7eee34a47	65	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
da6f5b43-66d6-48d5-b732-375e3060658d	66	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
6450941c-3e3d-41c1-8d13-aac5dfca6175	67	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
58fca3d5-4e3d-4843-aef1-eeb92eb574d6	68	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f262e3e6-548c-49be-9835-40fadba751f3	69	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1c514f10-89ce-4d7d-b5e5-7cd3d4bcf142	70	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9f655c16-f5f3-4a31-b2ec-1dba43867b2e	71	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
02e9386e-b918-4187-816f-7db3bb1b0346	72	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8aaf3520-657d-45a6-8448-5abd85833de8	73	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a74f604f-bff4-4906-a402-3248acaa993f	74	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
beba0840-ff50-4c9a-b79a-269ad0ef0a2c	75	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
542f0384-7a51-4e74-915c-2d4f57d53e01	76	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cf5bbc85-6818-4aee-ab21-d2a704e0a2e4	77	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ba7c924e-241d-48cb-99e5-aa71cb1644da	78	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f7f1b46e-065d-40e9-b14a-1c685920d464	79	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7c604345-6064-4609-8ed0-3ed02d627b70	80	9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8f17a6a0-17fa-4994-b8e0-da7152951010	1	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
818150ab-8052-446e-9430-f72127464a6e	2	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ba184863-6008-44a4-ad4f-1fe3d4ea555e	3	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a8f2b82c-48c4-4456-95a3-e42307209d40	4	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
95d9ba04-962d-4a3e-aa8e-194690bed143	5	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
65717de3-f8a1-477b-b9c0-8ce63fcc2ab0	6	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
db9d11fb-310c-4c2d-8b5f-582071ae9afe	7	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7cb0ce16-291a-476f-a323-7a0acb778ec5	8	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fca077bc-abb3-442b-ae0a-2b8699747e00	9	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
6865aae3-58ce-4d0f-a925-71e37b990128	10	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a5baecfd-296d-4eae-91d0-066ee778f6e0	11	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3e81cff5-4383-42eb-8d9b-ad7d667bf4bb	12	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b9062c1a-46e7-4c28-9215-4e443c95c604	13	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7d60a827-b417-4b5f-9ba1-d588d7acf51b	14	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1bf9eba0-cdc3-4a7d-80a6-9f46cfc1dee6	15	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
91b703be-ea74-4e97-80f6-469a86f6d13f	16	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
24bb1d81-fc40-4355-8839-19baa7ce08d7	17	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1e01c00c-f6ad-45c4-a1ff-4efdb33cc980	18	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d3836661-4f49-4b98-b3a1-43f462bb945c	19	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
da98c12c-7f1e-432e-81d4-1481a4480c39	20	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
633f0a36-fc5c-48c8-94fc-9f08c14a9da2	21	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2792f261-dec7-4182-81f9-f9d178c237cc	22	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
80e708d1-98e0-44c3-9c6d-aac4acc7b1aa	23	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fac6d30e-9c99-4ec9-bcdf-73687be20517	24	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8e9f3b69-9114-41f1-9fa7-06eaedf3c54b	25	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f3f73c7a-cb83-4f8c-9f09-46c41c1aa4a3	26	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
18ba08d0-751e-4b2d-8e09-18cc9a6216cf	27	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ac8339bf-3922-4306-98f9-2b371449261c	28	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7c973b1f-2cc0-4da3-96a7-be9c4534c52a	29	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
127fa8b5-1bce-495c-9eaf-b4bab3956287	30	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fbbe8606-648b-4024-9fa0-ecae74bc6590	31	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7f4eff7a-e219-4da8-b42a-620a56ecd921	32	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
286244a1-6ca4-4d97-8239-e4f7caabe589	33	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
6bac5780-b1f8-49b3-971f-66c3a02f640d	34	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
06bdb252-cc73-4d88-8877-9780355ffb9c	35	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a9c210b8-d69a-4ec1-87c9-df4c96decabc	36	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c8092f96-afaf-46b8-93ff-9599c848f2b6	37	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
76d3df91-0a71-4454-97ef-c1791c2a3cdc	38	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
abcd163c-a265-482e-be59-fc8e72142599	39	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c36a0092-ce9e-441e-ba83-0989a217354a	40	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b05782ad-28d3-4714-ab9f-8805cdc92828	41	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e0874062-21d7-425a-bdfc-eac6480f0298	42	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1f48060d-aeb9-42f2-a06f-41ccd3661afd	43	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ee73c479-6a84-4c91-ac76-773c088d44c5	44	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
36a04346-da73-4958-8068-eda5bab35f5a	45	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c91c8172-7027-4898-a929-e840f43a1feb	46	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
730ed16e-c99a-4380-9ebb-5332ef8f5287	47	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
968ef67a-9ec2-4614-877d-f8b690b018f9	48	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cdcb0d17-4a04-4174-b3d6-7c213111dd58	49	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0b759ac5-09a9-40b4-a2a9-21d186f72d3c	50	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4503063e-f2e2-48dd-b9d3-abce86686b39	51	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
dc193779-199e-4720-bd60-1485babb7565	52	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
73343a5c-825b-4d57-ae90-966b8f9919a5	53	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f85ca396-2c72-477d-a314-a30438fa8476	54	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
117635dc-3280-4272-a162-5e4aba9f6539	55	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
bc9d4dbe-4ed7-450e-b4a7-8c5076132053	56	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9d0c1dc6-77d8-4d30-bb48-88a37bc5360f	57	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1aead45d-5abd-42a2-99cf-1beaf37c4bf6	58	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0dd174d5-afb7-497a-a63c-e0c6b1d66531	59	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fed81fcd-24dc-4a9e-98f6-6eb10d60a886	60	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4e2c537e-9d99-44fc-a7cc-049ba790ef9d	61	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e637cbd9-491e-4dea-89c5-d0c476ff5289	62	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2c9fb0a5-44a6-4ef0-adac-6bb9839b8e60	63	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c81488b1-0984-4b07-a899-faf46d37386f	64	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
61847384-abf5-42be-9a30-af9b21047fdf	65	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
791ef8d6-bdf8-4250-803d-793bddac3a17	66	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
48b9cb1d-6943-4b42-a3a9-c624e9291f94	67	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cabc1d67-2f0e-49a1-850f-85eab3af5527	68	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8a385995-c408-4972-9e13-737cd46ffc32	69	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
004e824f-e047-46e3-b6d8-42afa27cba45	70	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
db9e1b00-1970-41b4-9da0-4b28e9059087	71	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1db7fe02-d762-4bb9-baa3-a8ed674b001d	72	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
67fa4711-11e4-4a45-a455-c55d2e1d8dde	73	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
100ca51c-e798-45e6-8375-aef36f3ffadc	74	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
af8344ab-fd28-40d1-a741-20d3dbaa222b	75	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7c08ebcb-c382-4e61-940b-886603d70a6c	76	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b2c2c5c1-ae95-48b5-b47f-5fe845b1ef70	77	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
dde79ae4-d3cb-47fc-83a6-de597675c809	78	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
18853005-cb98-4699-b67e-5a6ce487bd36	79	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
98feace9-e28f-408e-afef-c459c21caa4f	80	f7560ff5-bc50-4920-9c73-3a43e0f39ff2	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b51352c3-8965-44b7-abac-e6fe4643aa02	1	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
dd67f573-3bef-4ace-aa3e-b1de102fb51d	2	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
701b9e5f-84d4-404a-a934-684aa4cf5ab9	3	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e6211ad3-b737-409a-8fe4-b577921a6ba6	4	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
eb5e9fe1-59cc-4699-a642-fead815a2186	5	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9f78882c-be6a-4c47-9633-e4789a940239	6	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a20472e9-5378-4988-89b9-76472c18f6d5	7	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
90629af3-c0af-40cf-b51a-f6db75a49c75	8	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4cf50e1d-b96b-4ccb-a9a5-5596a9472838	9	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7d0bcfcd-03d7-409e-8386-e2ada31d89bc	10	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d5b552b1-5964-45ac-a9bc-593d44dd6fee	11	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
80b2e480-7c85-47f9-89d1-a962c74db309	12	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c9699fff-6b2f-4ecc-8764-fc9464b7b319	13	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c26ec313-269c-44b7-bb70-3c99a432c4c8	14	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e678ba2b-cddb-411b-8fd7-bc269ea26c85	15	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7d79172d-f398-4ad9-bd51-5dc6f91e6c5d	16	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8bed8dcf-5387-43fa-82bc-fada067e2c50	17	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
5ca652a1-9900-4ff1-84a7-c82299f4300d	18	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
20c33589-3b0c-498a-860f-b1142679f83d	19	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
51b47c89-e304-4870-8c75-74c20ff7915c	20	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e7689309-47fd-48e5-b48d-fe42d6d9912f	21	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a8d86a95-48b6-4819-94de-327745744f70	22	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
6f772035-6311-4daf-9676-c30e34c67165	23	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2855b942-43fa-468e-855a-1843eac34d48	24	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a891009c-373f-421c-ae19-d2701bea6198	25	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
35fb2689-3c75-44e0-8a5a-939331df0a4f	26	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d2d0d832-56be-4267-86d9-687604a3ce61	27	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3cc7cdd6-707e-47da-896b-3e5b5beef216	28	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
86f05df3-9922-4999-b558-e65f58c76870	29	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f9343139-d5da-478c-9d64-b74d41da6c48	30	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
685e2e01-8475-4490-9718-83d456ab6545	31	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
aa2f0cd1-8270-4e12-b256-ab8194c90472	32	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
faded9d0-6261-4fcd-939d-cb4d805700cb	33	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cae8012b-ba06-45d8-a232-f3e0407e17f7	34	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9a6a7121-3d4f-477c-bada-343222cd05ea	35	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a38db23a-6459-4bb5-b44d-46637890dd76	36	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
513b28e2-6578-4be6-a3f8-a16324309c41	37	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
aa3b1675-4c00-47c9-a03d-279b73a7404a	38	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
32b944b3-aac9-43b3-a320-4329503bad86	39	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3897c1bd-dbe8-4272-bea9-29fb9ecb3e6c	40	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
82e12814-f0a1-45e7-bcc4-783c6f92e174	41	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b20be952-14af-4e9b-9eab-9bdafbc57256	42	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3d70cc77-3151-40fe-8f6b-03eacea4835e	43	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a1e86016-e1dc-4277-b9c5-299170f9929e	44	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
36c1ab68-4345-4fc6-badc-30179fb2879f	45	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
75c98690-420d-4ecd-93f7-dc14a8e51353	46	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0b900fc3-275c-4757-89ca-55d4065f7495	47	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0ef2bffd-2d90-40b1-9a09-ae4d66f49f90	48	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
23d7ad1d-0330-49b9-bc1f-a4023611284b	49	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fc7a5a1f-b6e5-4989-a3f3-c4b732e280c0	50	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
17ba499d-9560-4a6c-8030-2c802549b65e	51	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
6f39785a-4292-4acd-8bcc-45a3e6369a80	52	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
529a7deb-387f-46be-9133-640da6742358	53	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d3a53c14-2390-4ae2-9703-f49ab63b826a	54	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e8647632-a2aa-4df5-88e8-6cdc4b8343a4	55	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
dc5cc177-3692-4061-910d-c33100e3be1b	56	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7f034a3e-6233-4cf1-9351-8e946f5525c0	57	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a09737a5-44e5-4f38-a648-dd361302d54a	58	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c3b61ec7-17ee-4d59-924b-f182909690c9	59	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ffaa35d5-2b5e-4cf7-b5bc-4d36f3033f3a	60	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
5e5eeea0-8605-4a40-9645-da46efd50cc4	61	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
13227217-7838-42a8-8761-d70d02b7ac23	62	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
431ad139-d84e-41df-ad00-b4f8b39ab6db	63	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d11d364f-ace5-4ac7-acc5-57ae759b367c	64	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ee412707-ff2c-481f-89cd-5c357495675d	65	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b9208cbf-39a7-4240-9b9e-69eddadb1be4	66	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e6520b4e-efda-433f-aaaa-b977e5a7524e	67	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8b160249-5476-4337-9c7b-4d28712efe0c	68	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
59cfc170-424e-4daa-a74f-c84c027c0f83	69	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a426ae96-9f40-4433-9a9e-d0f0aea001bc	70	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4b85d003-7dec-4d25-9694-e69bb781f167	71	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2c67f879-7a9d-4696-989d-508a6cbe49b3	72	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
54966af8-4804-4f26-b945-16c33af58bbf	73	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
01a5072d-d263-4c9c-8385-8585b2e3e85b	74	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1bf144f6-7d93-4183-886b-fec018bc942e	75	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
46927801-1cf6-4b57-ac21-05b0bbf0145d	76	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8ac5e3ba-0e22-44a5-b8ca-1719732526e4	77	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0eb4288d-c0e8-4d64-b093-01ab8078ca8c	78	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9c82f143-b9d2-4fab-93d6-fa8ee5f1786d	79	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cf21295c-af34-44a3-9ca8-a6d7fefe05d9	80	e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
51f34646-dfed-4cbd-a49c-b5be83b8b65a	1	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e4eab724-5a2e-4266-894c-0a4225614c13	2	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3f0eae9f-75e9-4520-86b5-9fbe226b66ea	3	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
73cab09b-d173-4de5-877b-db1269c1e455	4	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fe465244-9a99-47a7-b0a7-3af79d4f768d	5	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b254e197-3ac5-4203-acf4-27ecb8bccf5b	6	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
645f621a-42f6-41ab-b88a-96560bfcd911	7	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
61f5a743-f780-4f31-9994-23eab9b291fc	8	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
06d74a6d-6c6a-4c0d-99f6-1416158345f2	9	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
36ec102f-0afa-43e7-b177-b4d869407022	10	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
119da14a-6263-44fa-9724-9a7420642d88	11	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
13f67000-2c40-4263-8c81-02dddf17582f	12	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
48f1014b-07d4-455a-b752-e617c4e3ec9e	13	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3d586f34-1f37-42c7-94ab-40b9e5198190	14	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
91f7a4b2-d2b6-4649-9c11-4a9ffb0803de	15	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c2d823c1-9771-4ec4-8a4b-393a8ab3e4d2	16	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1f85bfc0-74e0-47b6-9bf8-a82447186a49	17	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
aa2b2d48-034c-4dd7-8b26-9581ae8ad067	18	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
587dd5bb-748b-4967-95fd-a121df9af9f4	19	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
6f0dfb3e-3a18-458b-bc21-d22dd8d71a7e	20	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ce307cad-d8e1-4a52-9bc9-d6f64db3025f	21	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2abc5300-f12f-43ef-a3e6-7692c65628fb	22	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
eda97d84-8c17-4665-8076-2f608bb8c8c3	23	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
86d10e84-b42a-4363-8002-8ef5a2419693	24	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
88d01501-4fb1-4d53-959b-b9f8a207d205	25	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cb6ce9f9-b1ac-4ad4-9b27-4107a3bd41a4	26	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
63449b7d-17cd-463c-b396-a65d6d05f19a	27	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
998b580a-ce80-469a-a623-3d29701f952a	28	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d315917d-ab3e-413c-ac51-a0a85d8aa006	29	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3a3f8306-a08d-428a-84fe-d4656d765b24	30	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2f32f27f-59de-4600-872b-a632ef15bc95	31	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
bbf7037c-e0a1-4bc8-acc5-3457d204a3b1	32	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
812aa2da-ba7f-4c3e-8c17-3a9e5331037c	33	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a5a9838d-7b68-4fb0-a231-5add74841f41	34	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
889384a8-eaa7-421d-aa78-d5bfcd70d72e	35	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f6912306-60d7-456e-bdac-39fe1bf81587	36	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
92bafbc8-7f70-4cbf-bf08-0be5ea0b7e8a	37	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
97066f69-9614-4ebe-80c5-ea40fd4a1504	38	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
38b4914c-be9a-47c4-8347-00957e346004	39	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9605347b-9074-4398-916f-ff2cf445a7a6	40	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0ee2382a-3702-4f8a-8a93-ff3b4815c8e0	41	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
5062b433-eaec-4296-bed9-543aef80306e	42	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f8b51778-d2db-456c-a2f4-5fc175b70ce1	43	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3803e189-42ae-447b-bc2f-df3845e45907	44	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d5bfbb2e-a05c-4cf3-9e00-15eb1261c2b8	45	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cc5da048-63bc-4c36-8a42-6ee6dc4e36a5	46	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
8838c468-ca17-41dd-b256-157078bc4d55	47	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7ecc15b9-05c1-45d1-9fde-7e19a1d27664	48	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2e238d1a-73cd-4629-8a86-cb6f4c78828d	49	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a1fc8288-a6b1-495f-8324-a88eb2ebbdbc	50	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e54b6672-4bd8-450e-817e-1d815b375f20	51	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ed1c1dcd-e471-49fe-9271-5bb8a80290eb	52	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
6032516e-ba5b-480a-a82a-06303ac065bb	53	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
caf5edc9-baaa-459c-8d84-97faedf6de49	54	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3f9b5df7-4eb8-4ad8-bdfb-62aff621c368	55	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1fc08c92-c550-46a3-83d9-eed8404938e5	56	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
bd9e819c-acda-4267-8957-012da6273182	57	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ab4245fc-1b2f-4dbc-8282-272d72a0b7db	58	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ad23a6af-9b68-4723-9f4e-0426f7e4c387	59	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7e1e5afa-4ba1-4167-90a3-4ff0d5ef70d0	60	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
0614ecad-fc60-4c3c-9d6e-c98ea863480a	61	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
52e6959a-7dc5-409e-97aa-6055544f615e	62	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
54dd7e8d-2831-4be6-8719-ecfbe4317f32	63	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
df4ed639-ae10-449e-b5a6-8bca0bb57ab4	64	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
06ee5e82-28ea-43e1-bb60-2d4de08ad29a	65	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
24933b3c-5524-4bf8-b6a8-803e8234fa3e	66	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
72d77778-4731-4613-ab7d-29e2aebf6e21	67	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9425c374-2f50-4ac1-a350-3d7199ddc0a7	68	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
6a08053c-e0db-409c-bff8-699d7ec99053	69	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
947e2b1c-69a5-4a4a-99a5-d856a8482947	70	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2ac32caa-6567-4deb-81d1-645321d2ac5f	71	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
df42c562-2b18-4a2c-8253-11a05f7776c7	72	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
16ba5892-7c86-48d5-94e6-307f6862aef9	73	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
335ccc44-19f7-4322-8007-22dd6f98804c	74	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
970f815c-a30c-4e5a-9500-d5cccf6da267	75	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e2458ed1-5301-4119-8b50-361215cc4696	76	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e6a90787-36c2-4555-9472-96f9618ce92e	77	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
56255fc2-fc0d-4186-bf19-2ad95f6a5f7b	78	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
89ce107f-fa38-4ac2-80d1-0b67f1e8de92	79	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2959c939-7be7-47a1-8f6c-69ac0670b5ea	80	3e835407-40f2-487a-a130-d07ed9a5e87b	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
eb3eea55-ba47-4f65-b08f-3de6531d1018	1	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
48a1b86b-af0d-49d0-894b-31b2f31e19d2	2	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fe981bac-9f54-41be-b6aa-3ce4e54d0a5c	3	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1cacab5d-08ec-469f-b6f5-65de9b29b1fa	4	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
328c225e-e207-46c4-a97d-71131c0e54cb	5	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
e675dee5-2428-4fe6-86b3-9f18ec9b572b	6	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7929b664-d59c-4795-9fba-c13d84c7ee17	7	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1b1f0b55-9cd0-47ad-b50d-aa4cb16875fc	8	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
688e04b9-ddd1-4931-81da-4d3996a85714	9	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
501c17d0-dd0c-49c4-957e-a1c11c4ce277	10	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4a06ce56-76da-4099-93f4-69f77a4a7360	11	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9d050eaf-9a97-47ac-aeae-10430a4de1c2	12	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
81364364-f571-499c-af80-bb45b0f32e78	13	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
b00bc71f-66c7-4301-9129-fea759c8a38a	14	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
db40f242-6bfa-4522-90dc-5d337199214a	15	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c7581b2e-5ac1-4800-8913-15b353c66ef8	16	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
47970271-9fa9-4d0f-b66c-0cd5480961b1	17	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c074a5d1-83db-47f5-872d-1213cc8ec5bd	18	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
79fd5b41-5f92-4127-8059-6c911fa5a911	19	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
313d73a3-f592-4357-8692-6442ddb3825c	20	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
61804574-4569-4d62-a0f9-f2834b5a1388	21	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
f1ad4b28-432a-4804-8b33-a1fe602a7dc4	22	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7a7668f6-d406-46a5-8ea3-6f308d1bcac4	23	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9d471c11-054b-4e4f-99dd-51ed757a8fa9	24	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ae8d3d89-79c3-4615-97a2-f63b2ee521f9	25	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
6ca4b572-8a1a-44c0-9925-41b2b8bd1315	26	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ccabb3d3-6960-43e6-91d5-f8735fa2a1d7	27	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
337dee0d-524a-4451-b75c-839daed291c7	28	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1a5c7e6e-4b51-4de3-a939-e58fb7f64da4	29	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
703db559-3a02-4824-bab4-5882bd0d3c16	30	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
96344d33-c275-49bf-a919-a13e5a11f595	31	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1102f82d-34d2-4479-a5e4-c7c015185246	32	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
dbd34eeb-1741-4c35-bfb0-cfb4cb0d7943	33	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
94d5a9f5-ff3c-4728-a7cf-f9cca37214e2	34	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
bd53b83c-4642-4f62-b1a1-4284704a2800	35	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
71098ce5-1f7a-4862-b6a6-d925c67e0c2d	36	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9942bd89-3f80-463d-9c36-da7364968c35	37	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
69ab6cb8-778b-4fbd-a72e-b84603f002e4	38	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
93774733-96ee-4c71-bb95-b1c4c13451a6	39	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a7203687-e492-4293-8ffb-f6b33c9dc056	40	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
29f6d957-aa51-4508-b42b-cfc42b28dae1	41	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1567a1d8-6148-4745-b65e-9932c4725907	42	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9cc2feea-79a2-4395-9a42-74750bb8e440	43	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
516ae170-cb66-419a-8c71-dd1efd0edde7	44	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1fe20c38-f498-45f5-a083-ae97ab768749	45	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
25ad1085-a8db-48e7-a60d-a6f7098f7095	46	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
60478662-e0fe-4ee3-9709-767c2c4419c2	47	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
3bfa34e9-839b-4c3c-a9d5-f5979cdb3921	48	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
aaee136a-f4aa-4743-a927-3f3e594c55bb	49	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
4563f2db-be4f-4106-a108-b814f33b1ac0	50	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
63899da1-78d9-4697-adbe-6cce908ba816	51	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
dda09f92-4e50-4078-ab47-ce83313a3b3f	52	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c021cc49-dae0-41bc-8317-f0fc37d0773e	53	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d0457d76-27f8-45f3-b6a5-f65d11ebc755	54	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
fc3cdbe8-ce6d-4b99-bacd-ae34339d4e44	55	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ca041040-e4c3-4993-96a9-49c28a322c72	56	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
2e68335c-f404-45db-8fe0-74e00c089438	57	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
676e168d-2640-44d0-8eaa-28667f48f58b	58	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
71623e02-5b6b-4b2e-a661-de14b378e8b4	59	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d7e1b628-eb0f-4ba8-b15e-b0c1e3c31c83	60	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
5ba45ca1-b484-48bf-974c-773bb070af53	61	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1ae4cc4b-466e-4265-aca2-5146aa658582	62	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
d3d70d93-e020-4ca3-9162-375166b7cc0b	63	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
9a319404-dcce-49b0-8f7e-d4a7e1cd3334	64	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
edc8c131-28ff-4c11-913e-5c91dc5dfe3a	65	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
11be2cef-adc6-424e-861b-cb2f85c5064d	66	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
6402ab28-52b7-4552-ac28-af5e3df2c83f	67	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ad0beee6-e79c-4b1d-a637-e3230938c156	68	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7cb36eb5-ac80-41eb-a9ff-91a678d20e5e	69	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cdd37bf7-75d3-4c87-8766-82095b105814	70	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
39f379a1-4443-40d0-bae2-a45025c27713	71	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
5b862bfb-5f72-482d-a590-8e6e1787a3a6	72	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
c7d943f4-b906-4204-84a7-77d8c6462470	73	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
cbbe87f6-1953-4883-b2c7-513d2cbed58f	74	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
def65443-d3b5-4f35-a178-9fe8f3cdd2f0	75	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
ba572bfb-fac4-4b04-91eb-cba6f7a4c4f0	76	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
64756b9e-6ac3-4466-9987-1c9944282537	77	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a1b9fa87-d1ae-4fb4-8ffd-d5f94025de21	78	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
7552c1f6-a94a-43e0-b36c-1fd9f1661815	79	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
a25ed827-6764-46c8-bd19-83760ce6fab1	80	b2e76b1e-f81a-499a-8d06-c33d2825128f	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	f
1b0cd983-9a7e-46bb-b817-b8983e0f42e4	1	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
bda52b2f-a9ba-4806-8586-508163ab5085	2	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
31c7b01c-ed21-4210-bc3c-76dcb3cbbc72	3	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
9f05bc07-e726-47c0-b542-2fce1c0dbd1c	4	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
dc502b33-0a2b-49ad-b062-0db62978cebe	5	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8e81609d-3151-4942-93d0-ae9f47d4daec	6	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
73eb79e4-6c0b-47e3-87b4-e07c43623fa2	7	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
490a3d3a-0b54-4346-8287-3c912b3acd9d	8	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f5d360b8-ed7d-4e9d-9290-30ade6f47006	9	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b3435bf5-d107-450d-b78c-269f98405a0b	10	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fc3c407a-15f9-480c-a8fe-a2caa40bd261	11	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f71c0bf3-3eeb-41f9-9a86-fcc572280249	12	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
99928889-1325-41e0-8352-7a7ba4cda2cc	13	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
3a19d811-5e90-4384-be0d-9dcaa95ba5e0	14	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
cc1014b7-4c12-4e33-aeb1-a2380a8366fd	15	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7b5a4550-2dde-4426-b8e0-4191c9d77718	16	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4d4ceb1b-207a-4e0e-9a72-8a30e8560b48	17	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
cebdb44a-24bf-4da7-96f4-972e99eaa16d	18	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c007d7bd-0117-4eb4-9c50-2945c7bf6e62	19	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2efa2829-e1ed-4bb1-b110-7a6ba6c05191	20	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d7131d34-9bd9-4567-88c7-7bdd331c713b	21	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c27e00db-a1e7-42ca-b5ca-7e9fb54ed2bb	22	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a582887b-35d0-44c5-a20e-056af4a85a31	23	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
9744b92a-c29d-4454-a7d0-52dbf99ad1e3	24	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d34b0db8-4c14-4571-92d2-a3e7eb1712b2	25	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2cddc572-4693-449e-8bde-4f58f2ccc90a	26	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1ac2ecda-b4ab-45c5-9ec2-bc1f2285f017	27	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4b5d8c68-0e3a-4f27-90e0-eeb9c2facd24	28	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ec7aa55d-8809-4dc3-a90e-1ec30b740636	29	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c1821fbf-8c3a-4869-be21-ff9235499bc4	30	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
095a9d64-635e-4c85-b66e-bff31320c7ac	31	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
56bab22e-5ee6-4b2d-ac86-2b9b5ac6e620	32	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
9f088d11-c374-41bb-8507-b41b98c6fb48	33	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c7bef62e-d88a-4938-8c69-1b9ccdaa05bd	34	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e275c3c7-8194-4e01-8cd3-d200add4fde8	35	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fc466ef5-ffbb-499f-b05a-c80ec461fb81	36	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
6dee71da-dbef-4fe0-b708-af361b73d5c0	37	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b05e20e8-5473-4ba6-992f-d3f71903a941	38	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
32e790af-b4e0-4646-90ed-17f93d7d4d8a	39	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
cd5de9c6-b149-4e69-8a8f-7eb7fbf1b26c	40	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ffe3bbf2-af51-4682-b3e6-a97d99e73b9a	41	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
17f11482-8e4e-44b1-8c43-efb3a08f1c78	42	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
01100a5a-07f1-4f6d-9687-33cb6b295a59	43	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fc6d2c37-4a49-4c6d-b343-da740a562eb6	44	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d9c994a6-90fc-44bb-954a-b14e58a94758	45	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f28c5e3b-c52d-4a28-8936-497e4dcf572d	46	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
02ecccb3-882d-487b-8890-71647973a27f	47	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
5efe017c-78ac-45ac-9476-3b76f5cd9409	48	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f3888340-1a58-4ba8-bd73-5495d86f0ca1	49	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
cfd8e893-8598-40cf-b1c2-b3d7ac443ee6	50	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
031cd8e8-d40c-4945-965e-c0b4fd2a9c98	51	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2225d5de-0126-4f3c-9119-38cb25a1aac6	52	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
5fe0a28c-553b-49ae-af74-70468f5ce21a	53	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ef80f7ba-714a-4001-8c7b-05cffed1ea73	54	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1d014bc3-f305-4685-913e-2dbb5a484f83	55	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
522a2bef-a982-47de-8a52-7c426565b92c	56	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2485e6c0-3d00-4fc0-9caf-d9bf6a723c65	57	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f62eb588-7429-40b8-b338-02fc489d9b75	58	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
6cb311bc-1f49-41eb-9c84-892ea1f22ca9	59	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8bb3251c-deb5-440f-834a-c873f1058bfe	60	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a0179dd2-5294-45a2-8669-bf518a19bc1f	61	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ade39acf-8604-480f-a241-945d8a4226a1	62	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b4081602-e197-478b-81b1-cc0731207e5c	63	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
42a4de45-eff7-449a-bd19-0fc9ecfb1237	64	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
09fc751f-de7b-4628-bbc6-4fd3d6838aaf	65	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b864cc50-cdd6-4826-adc7-b973e80e29ff	66	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d88940b4-b4c2-474a-9ea2-02e009d52ce1	67	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a02f299b-823d-43d9-b1bd-60767162f11e	68	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8e4389b8-1f34-42b1-a097-87d4ba2306a7	69	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
52889141-bc14-4f52-8bc3-e4b761b4a767	70	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
567dcd92-edfd-4488-8baf-1b7cecaad46f	71	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e51502a0-9887-471c-aef8-6816b8da89de	72	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ce284cb9-4f82-41b5-a424-f2ff10866fa8	73	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
416a41ba-2f2a-4728-8f34-85de0df08e09	74	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
525e0432-ba97-4fe5-bc83-64d4fb85f388	75	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d925373d-8337-43a5-9ae0-4ed7ffc2a8e5	76	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
21c93caa-0850-4791-bf22-9ae1e22bdd29	77	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d448e539-b84d-4e7c-97f1-7125f1385cbf	78	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a569f3ef-4989-42f5-973a-8449dc6fdfa9	79	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a8688d29-17f7-45a2-a414-2ef025a3c39b	80	3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
864ca164-907b-4ae7-a3f1-c39e4d2383b1	1	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
300d529e-fad6-422e-abca-a2194a70d354	2	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4d64e24e-15da-431b-8442-b0f37fd709e3	3	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
201690a7-b008-4007-8f55-67189c3aa4db	4	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
02d6e423-7060-4c21-b7f6-6c42d31ff406	5	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d89135a4-0c52-4f3e-894a-2fe9b4505264	6	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
404db480-1cc5-41af-8086-9f105841024f	7	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7e986c80-9d60-408a-9a33-2e0e572e83d1	8	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
84d1276d-1f1b-4c23-9b5b-4a587338f7ed	9	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
98a6bc5a-a18d-40fc-844b-c99d2dc9d2c0	10	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
cabc9445-11e2-4177-b87b-f667f92466c4	11	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
57a65875-b864-4b94-bfc8-6ef93c5d67df	12	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
650300db-3c76-4d02-be5a-6a08e70beb77	13	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
dda38efd-dfd4-490e-ac4c-30a5abe042b2	14	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
50d81737-9a66-450e-82c7-f790b6f23e0e	15	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e8fdf9ea-d6aa-463c-9249-d2212fce5d68	16	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7236c517-3fa7-45e8-92ae-92f007478a38	17	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8c079635-5acd-4529-86e7-81a47ebe0b3b	18	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e3a74317-0d5f-4699-8e74-1842d8350edc	19	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b327a934-01e1-4ecf-9f43-5bc54fb88fea	20	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
04819ec3-8083-4af6-b1d1-ae5317052149	21	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2f8ba368-7a0b-48f1-8f52-ebf1faba39ee	22	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
aba9574c-d398-48f1-a690-ed65b0cd5d38	23	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4c1bada9-8bde-4350-9977-de41b7b97f75	24	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7d7b54be-5bef-4e2e-972b-5530241c44ba	25	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
387faee1-6afe-4847-b58e-95d67072f122	26	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
19a53637-f369-405b-8ff8-dbe323a22a98	27	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
6dcf315b-d468-459a-91d4-d260f4265ce8	28	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d25d8687-3d01-41cb-8b0c-21317c611ee9	29	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
aabc6040-d2a7-465c-be35-4085b71de322	30	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
9211f538-07f4-4ebf-ba6d-f62a34683d98	31	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ad2b8b6b-a4c8-4d85-963c-ac771e1e48cf	32	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e185f47f-f217-4c6b-87df-22309792a932	33	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8518b1cb-fb3a-467b-a70c-94569f953c48	34	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fb7f3296-8fdf-47e2-a730-81f358887ef0	35	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
51cf85b1-8949-4749-89f5-557891bf6375	36	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b3b6c139-03d9-4c7d-92d7-d428e88cc2ff	37	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f424024b-cec4-4c69-ae74-22b79ae24c75	38	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4d6eb349-0dfa-4f54-a847-a13f15d109df	39	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
6e7e8169-5267-47a6-a301-66d256cc8ab6	44	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
5a640971-f53b-4f27-9049-73f20b1335be	45	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8d5afb50-07ab-46d5-a8e7-3ba594169a42	46	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
3d799b9b-576c-46ee-8b87-839f630cf0db	47	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a028b2c1-d8fa-4216-9fef-08b3ea5bc4b1	50	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
18b35191-8057-4818-a9a5-0eafb0a83eb5	51	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
be341725-7638-410b-a200-4780838fd450	53	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c9d48fb6-296f-437b-b73d-72a17c6853f8	54	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e9d131c8-dc0c-4759-a501-91a2ea07c234	55	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
9e913f60-3bec-4426-98ac-e7a3c15515a6	56	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
64225347-1cdb-4fbb-a68a-fb92022871e0	49	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:57:27.309	t
a906e853-6147-4e3a-b945-b07869d61f68	48	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:57:51.061	t
92a52411-1c63-4f44-b7b7-7a66b0f6de24	57	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2b4c8d14-8649-430f-9211-a1f0cd9cc981	58	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1c254024-6941-43e8-b158-3dfcb33700f2	59	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
082d39d3-598e-47b7-a7cc-0857c0280ed2	61	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2b15df2c-ab56-44f4-940a-09ee4dd3ff31	62	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c9cbb481-c4a2-45d7-b1c7-92403fea273f	64	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
6e2f7476-a712-4e20-8cab-b97e61c923f3	65	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1e290270-2a61-4d91-b9ee-2f4f36aa48b0	67	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7b14d111-f775-49d4-bd96-e184aae0f757	73	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:23:14.139	t
881596a7-0cb0-49ce-af9b-3201efd455c9	69	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
59af9a40-5c61-4bfb-af39-de4a9fecbb5e	70	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
20034939-aa1f-4c42-9dc3-68d0f229f6ba	71	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
5b250823-f0e6-404c-bde9-1358e8a8cddf	2	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
1ffabde1-8c80-4453-9e40-25b549f0c707	3	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
db96e301-5b78-4b86-b3de-6878a5a8bdf5	75	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
03671e71-4586-4a5d-855f-2352402c547f	4	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
cea6f0b1-d21f-4072-ae39-252228faa83b	5	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
30280970-49fb-498b-b1a5-0cefd839063e	78	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
00367054-6f02-4950-948b-e652449fe132	6	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
1fe7e75f-365b-441d-8c5a-a7360778afa4	7	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
7df6240d-3f2f-4672-9f98-beb635720e6c	1	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
3abf1dec-ae0d-4753-a04f-d31973eac5ce	2	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a1f4f1ec-46a0-4ff7-9dc8-d15d815663fc	3	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e7350103-5372-41f5-b709-2e70601a5cef	4	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
3b980c10-8004-48ab-904b-04160ea573a6	5	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b1b535e4-d1e2-49aa-8d27-91bc9aac7c19	6	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
53cdedc6-7a6d-43b3-a7da-1406571e32aa	7	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
abf37651-0762-4829-80ba-419ec8ffa06b	8	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
aa80f558-bddc-4162-a829-4bd91d4f4ddc	9	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
64ebbc6e-444d-4a59-9a79-596c6bb21b1f	10	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
41258f92-dd05-4901-a378-c4e8b1faf44b	11	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a0bd0f7f-d232-4e25-9b48-aeabfb7195cf	12	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
22e45cf1-c08f-427f-a875-0688c4d78eb9	13	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d6b6c832-778c-4bf1-9612-0460b5ff0d7e	14	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
cf0afefc-62ab-4a76-91de-692482845860	15	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8ef8a26e-b3a0-4b9e-85d6-140b64a63ff2	16	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1a6122e9-9987-41ca-9193-733d00bfcf9e	17	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
79ef838c-6128-426d-a921-36237fd5c33b	18	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
20781537-0ddb-4639-9a79-44232b71aa9f	19	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
3ebe7d99-4bba-419a-888b-7276fe40a7b6	20	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
540a4b9a-b9c4-4349-9d19-696324a736f8	21	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
5ac62bbd-0f57-429f-aac8-9b0ba0496783	22	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
22034a97-10ea-42bb-94ea-f98e73f23b24	23	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
85a9da08-24b7-42dd-8304-0c5518f94e2a	24	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d4622e57-e557-4d30-8699-6258df841feb	25	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
dd477a86-bb99-4889-b0e5-d49893957ee3	26	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
81ebf322-afd7-4b96-8443-9aacc7dce83e	27	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b964998e-2b95-41cd-bcba-a73412026a00	28	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
675e7308-64fa-4ba5-884c-e5a51414b1f3	29	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
9fea4130-96a2-4345-9935-93914dacca60	30	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d791296c-8570-4d24-bda1-e6b58ce9b81f	31	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
28aa57dc-2c2b-4a4a-bb1a-5999753b76bc	32	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
aabd6a68-f636-442e-ae9f-a9cb1938d5f8	33	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f79905b9-963f-4d80-aa44-243232f73787	34	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f7844d0f-2e20-4dcc-9ead-9932bdbeb04c	63	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:57:27.309	t
0360134f-2a46-4dd5-aae0-50a2b0e2f0f8	60	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:57:51.061	t
04ea0a8f-0731-473b-bf40-81b41e46af58	72	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:57:51.061	t
797a79f6-114d-471d-a01a-2d1a466f41fa	35	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
3dd9f9b5-f738-45a7-a427-0aabb77909c6	36	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c3dd6ca1-50f0-4ca4-90ac-40b7a9112940	37	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
6d597c85-4195-4524-ae69-5cbff1222ecf	38	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
577d0af8-5b53-47eb-ae5c-16c4537e3d49	39	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fbe6cd3e-7ac2-4281-a972-acd4e915a066	40	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a0ec9110-822f-453d-92b1-6cba7e56e287	41	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4fd89a7f-5c49-4452-9411-c0924749b529	42	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
cce41494-db0c-475c-bdf6-2446866af28a	43	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2ff432b7-c2b9-40ef-a486-441d6f9116f4	44	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7b2d620c-c4a7-4a4c-a76b-4b2238d72f80	45	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a9b4b787-5be8-4ea6-aaf4-2445996b0fb4	46	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
25d715b6-8084-4910-bda8-2e851c80b595	47	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
054fb730-134b-46c5-8199-b13a5aaf40a2	48	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
152cb7e8-6ae6-4b3a-888a-f809efd0d4f2	49	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
dd12bef6-740a-47e5-b2d2-262bbe041a9f	50	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
43c0e23e-f0cb-4be5-b456-1291a9201d4b	51	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
422ffb72-b26c-47b8-a5db-01c74bf903fe	52	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7a7c639b-3785-49d0-b04f-646d1419b318	53	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
6ef9a074-60a4-46fb-aa02-01211ff23689	54	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
db11510d-1eac-41ba-8f9d-99ce1367ad37	55	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
9c782c73-ec52-400f-a286-6607981de07e	56	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
eaafa2ee-030f-4432-9f39-f9470ad665a4	57	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
3397e6b9-6780-426a-a1ce-1e14c3f95127	58	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b5cf001b-b379-49f9-bcdf-d86fd2273779	59	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
106ae979-5d5e-4edc-97e2-c6e8efd3d2fb	60	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
59f1e4bc-fb16-4955-8ccc-b4158e6cd53c	61	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4a1eb9c1-7988-439d-b4a9-beb0f9128d6f	62	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1611e6b9-5207-459a-b9ba-66ea284da653	63	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1f533d3b-b3c3-40cc-97ae-3c31a87c872a	64	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
268e5d0e-3e50-44d8-b321-7bfadf78921f	65	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e363d72b-99e7-4ea3-b670-bb34c4e2567e	66	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
986b3d7d-e7ab-4ee4-9d23-cd1655faa8df	67	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7df8effa-b9fc-43a1-b1a7-e0c1d346120b	68	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0faac73f-d83e-4be2-a073-5bd30f8ca94a	69	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
85bf9719-3672-4d9e-974a-974bcafc23a2	70	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
64c52286-c49c-4bc1-a013-fe549130e5fe	71	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
bd2739f2-860f-4346-998f-7273b8ee8aaa	72	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
82a0fa82-daa1-4185-b89b-a0cf5f869273	73	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0fbcaec0-09b0-44d4-9e96-baddbb0210c7	74	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
cd807d21-6d64-4ce8-8d5d-e7c4f43da9e0	75	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e249f406-6852-4e99-9e25-ebf1364150a2	76	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
207bc7f8-61d2-43e9-b0b5-e4eaacbebbad	77	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1074362b-c3b2-4873-b93b-b8a9008f1ae1	78	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b2ea64ab-5aac-4abd-acd7-07e519a69d43	79	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
3130f814-17fc-43e0-992a-6015548c073d	80	ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c9a6833a-1e3f-4502-8cd0-8ac1039d3742	1	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
763b1a23-db7d-440a-9c8f-bfb29dfc4378	2	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d645bbf8-1f3f-4efc-b52c-f9ee8bbd2ad7	3	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ebd2c2bd-623b-430f-afd7-e14317657e36	4	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
045a09c6-6b7c-400a-b972-2904cba2cb30	5	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b68dae0c-8eb1-4795-9137-065ea8ebaee5	6	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4fae210b-adbe-4bd1-9c46-06cbc9ce4086	7	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
5e46a69c-a552-4757-b438-22eb4aaebf8f	8	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4eb8553d-b10e-444d-8a3e-7d925922db56	9	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7d5f8f3a-79ba-42e8-9e1d-4ba83b3adc81	10	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b3c27143-9cf2-45ad-ad31-bed1ab3ff4af	11	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e15fb5e7-6e0f-4c8e-aae4-98216da425b7	12	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f099ac0c-0897-4251-bc86-a06761dd6f1f	13	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8f93943c-9902-4b8e-9a01-e9e911596b55	14	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fe1cc90c-58ef-4050-841a-e530752792c4	15	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f9195863-ce20-4c92-8e8c-1c4593b107f1	16	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
54b1fad3-e300-4d25-ad1d-e64df3f78f51	17	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
92744f4d-71c4-4cc4-ae33-29c2755413b3	18	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
71a866a8-b5bd-4ef6-9f51-b9d8bb2af072	19	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
edbe8263-7b7b-453a-9ca2-f014d5c4219c	20	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
829022ee-4d48-40f0-81fa-4f6b1fd6a8c5	21	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0c6e4792-e1b3-4d91-b23e-79041a123577	22	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c9487655-7ea1-4461-aa29-cc52e691b579	23	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
11bbb7d7-0212-4914-9792-0ea2fc37ef7b	24	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
171ddb2b-10a0-40e8-9a12-dd05af921454	25	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
30a14d74-4543-40ae-9b69-0ee5fe5892cc	26	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c9aa52a4-e475-4bd5-8f93-157a9be2d7ca	27	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
14ee3cc8-e43a-4935-b7ad-68c4995d37c0	28	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
869bcb16-2caa-4313-8fa7-6f3439425839	29	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c3585f77-82c2-4cea-b5f8-e819f72c2210	30	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
bf5e13cd-2724-4048-baa3-b00df774fb44	31	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2aff125c-cd7f-4990-9e1e-6132021f5984	32	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b4142731-cfd7-4296-86b7-4b97288b8d0d	33	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
623cda77-71bc-4c81-b4f2-001563e79d62	34	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ec455d56-a24c-4812-b6e7-c8775b938a0d	35	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4e816417-7ac6-4b40-8f4f-32b73d7eff47	36	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
65d0b27d-e65a-4145-91a5-377be2595645	37	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e15f6d71-cc07-45bc-aa67-72cf38c3ed17	38	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ca45e4ce-83aa-48a0-a285-aa365732c21e	39	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
64c785e4-c420-4b60-a566-fc25a292385a	40	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a2072d02-7710-4589-8e98-92d846947578	41	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
788897a4-4f35-4b9d-9f07-513412f4e025	42	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
12652941-cd1a-49b9-b6dc-56c63e4b0589	43	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4604b5bc-20ee-4750-9def-51161cfe9b1a	44	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c0bff120-2e60-4388-bb7c-915ccaf3afee	45	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c2ca57c9-9241-4daa-9d16-a5779e0d21e6	46	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
6381d52d-f47e-49b8-b789-1c4f819b7741	47	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
754cc80f-aece-4214-a5fb-6845657e0bb8	48	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
30b3e1e6-6290-4d8d-a086-0fa4691c7606	49	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8046730e-8f62-42a5-ab13-1e12e6656222	50	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8ca0aaf7-9e02-4da8-96e9-e0e7a06e886c	51	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
53f8fcef-8a29-4698-a8c9-c9e74a132691	52	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7a55460b-bcd4-48ba-b073-bb4cc01fbf6c	53	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b6878fca-5a59-4432-803c-ee5c8d6e618f	54	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1e87a602-7828-40a2-ba1a-53257b7995a8	55	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
3775754a-937c-4ae3-85c2-84a7a7ca36fd	56	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8f1b60d3-5f8b-41bb-b9d3-e2022cbf1a39	57	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
bde82f1c-86b9-47ce-bec1-c6d7c9a9a22c	58	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
15fa7ea8-b8c4-48d2-84ce-da0bc1a0da4b	59	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d4746933-c9e4-460f-9b48-97490de222bf	60	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0510fe03-743a-4ae1-a4c0-e8edfa0ae9dc	61	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ec8b35cb-2847-462f-9489-04b4029476f1	62	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f996f6ad-2503-4707-95b6-8de09912baeb	63	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
56c323c7-add1-48e8-884a-0897425fa32c	64	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
54947a8d-2c32-4ec4-858e-a17ea4de7919	65	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
860bccf5-f053-429a-8451-4c260a2f7968	66	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7534a9cd-67c4-464e-92b4-3149869b674b	67	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0dedc638-c327-49e4-aacf-90e50ce79237	68	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
abef9c89-abbc-49e0-8ca7-4226abadbc82	69	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
428c8c62-3e91-4af2-b8eb-df86579a41d7	70	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
65266472-9968-470c-9201-9186297361cb	71	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b0b25044-f15f-4a9b-9d9c-4a4db0ca3458	72	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1c547162-dee6-48c1-8a0c-1ddff63576d8	73	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b79b954e-1c6b-4294-bff1-cc67714eba80	74	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
91399460-360f-4df8-8e99-5a8b73ef54f3	75	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0e789ba2-ac20-4042-9542-fd28269dc3ef	76	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a3ebf063-f671-4ac5-b8aa-373c62ac46ef	77	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0a749529-19e3-42e1-a41e-a4deca82f4ee	78	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b43e1cee-7175-4ab4-a831-613c5ab6eaa6	79	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
35bd1763-b08f-4bbd-ab89-16ba9d8facb1	80	869d9cc9-18ed-4867-996a-67a8730b4c16	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
5a215f43-01a1-43ed-ba41-4ae52ebdb5c6	1	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ba278409-f4f3-4c54-bcbc-559d422a7060	2	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
5875cd49-b3ec-4e71-ab78-b618d58ec3b9	3	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
81172a59-1914-4850-a42f-f43bd9beaf19	4	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
564f2692-8c6f-44e5-9592-614f559a6b88	5	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
338d3202-d993-43a5-a5c9-cf0564f0e19d	6	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0601fe8c-4eeb-4d97-a2b6-9be703ac1d51	7	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1689e6c2-1e7a-425b-aeab-fea977ed7ae4	8	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f701c4d0-28c2-4e93-b1cf-b3847f925d2d	9	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4b4f8d20-c04b-47c2-af37-3bf22da4000f	10	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
41ab2c05-525f-4adc-b8d3-660065dbd6f4	11	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d37a0aae-5998-46bd-a17d-e368ce5af4b7	12	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
528dcaff-d96b-4705-a25e-eb480222ac23	13	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8d5ee5ea-3008-4cd3-98b3-5e74741e2336	14	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f37da507-2908-4da4-8c5a-aa7f21903515	15	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2d22e364-517a-44be-9692-8a51d291bfae	16	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
3eb31ed5-a69b-4b93-9dbc-ff3d30cd92df	17	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e8d2beac-96fb-4fb0-865a-8dd06f8d4197	18	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2b748221-67dd-4eff-9a81-b2964679b17f	19	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c0db2261-210d-406b-9ee2-edd6a91ad882	20	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
be32dc25-727b-4c1e-8e00-79a85a8618c0	21	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b90adeab-b854-437f-a2aa-187852e72574	22	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0268d864-1151-4ad2-a28e-d55079e3b499	23	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b43adb78-47d1-4ff5-9c1d-3d15b8b241fc	24	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
6f67304b-85ff-4465-a5c7-bcb77906ab7e	25	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
77ab6ed8-d78d-42fe-9435-4c893114287d	26	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
faff20de-0713-4e91-852b-d3d9f858f126	27	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f82f7ad8-2b3d-473c-8803-bf6473a3a77d	28	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
5eed5e89-e748-4af3-939a-c5e7ca80cb0d	29	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c186548c-388a-44bb-ab47-1fe8d54874b7	30	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8c1904be-88ce-45c2-9e3f-60f95737ffc9	31	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d7c67cd3-afed-4ee4-9ec3-6a71cd0b6a25	32	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c14a3b45-3bae-42ef-b757-a0b05bf66c59	33	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
68ea4e5c-b7eb-482b-bd53-6a101281f991	34	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
6a818d7d-7c6d-4044-ad2c-14b8a1391b5e	35	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
178ff7c8-cef0-4e9c-b0cb-34b331ca7fa8	36	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4a4b89ac-c649-484b-aa59-d845c36af286	37	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a686cfe2-acee-43bf-8ae9-7925464ed9d1	38	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fa190246-1a89-496c-aece-687d80d382c4	39	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
21f2c786-ec7d-4d88-9f82-e47b9c613ddc	40	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
240fa24f-49e1-4fa4-a23b-f2a97e2366b7	41	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b88757a8-9882-4688-8929-540b89726437	42	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c7b0053a-d873-4f71-8fcc-5332e20d3398	43	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
49b1954c-d5f6-45a2-a1f2-6303d8149a45	44	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
adc5442f-e7af-4288-b0d3-0b6659deea98	45	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4bc35a7d-0ced-4c5b-a6c1-c7d59ac7a3cb	46	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4c79bf79-16b5-40bc-bc4f-b088f90fde29	47	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a4a41492-e39a-4898-a8d8-4e911e02f789	48	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
194e691d-b92a-4100-9e97-bce9422d4b52	49	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a67b891b-2705-487e-8c1b-d53a3a104f23	50	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f1292bf4-1d9d-4426-bf78-bf321566d011	51	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
55802862-72f7-4f0e-afff-090389f7ccf0	52	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
68c462be-e8fc-4c80-bb3a-11ef81383d9a	53	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
53ba5f85-4fb1-4bfa-9dae-9d2a9d24fbc4	54	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
49b3ca76-f4b0-4a02-bd6c-caddfebb9c33	55	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
bb7a29bf-ffad-41cb-b850-31dae389da19	56	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
026d02bf-5e87-40d0-9eb0-2f61688b3e55	57	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
15088ae5-a616-457b-b2ef-3b54bc053498	58	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
71208317-11f6-48dd-b721-009fe9c90133	59	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b442a3b9-6dd3-4216-b518-b3bbdcd69dbd	60	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f797d0a2-c215-43c0-bccd-4003879ac7b0	61	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e1d87ea6-5a67-4cd5-a59a-ac196e87d48f	62	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2c92cceb-7b6d-480c-ad96-ad0501595293	63	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fb693c38-0575-4424-8b06-6024c9a9556b	64	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
6f497be8-b252-4133-afc8-69db2f7f2b24	65	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0de8604b-7ce5-4f5f-bc10-b1e20d7c8d47	66	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
66041149-0d2e-4ba3-967a-98d2cbd9cac7	67	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
38724f12-e5e4-47ef-96ae-78fb2b4fb702	68	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f7b05757-ce03-4a73-a226-c9ecd892e104	69	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
80d66988-8dd0-4c26-897e-dc136532c0ce	70	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
cef4a347-5322-4514-a682-f44c833f9a37	71	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7b219dea-2b8f-4684-b825-014e7e97ffaa	72	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e671cb0f-09dd-4561-b441-baf698eae1a9	73	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e40de57a-291a-4e0d-a3d5-d5ab43f6a085	74	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
9f7a6f49-c997-4a7c-9421-65eb2bc01b66	75	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
de8f21e7-8a5a-44d8-9210-db4843755f61	76	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7ccd6963-6422-4b23-80a2-83c0bb433586	77	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
5f48dc9f-ec18-476e-91cc-2902f1eae56a	78	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
32b85554-e043-4ceb-87e1-ffae28d5ab08	79	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c5dc8685-afc1-4424-96df-46c3167a2973	80	72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f337b332-f778-4118-9a46-dc9c1cb36616	1	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
31ee7af1-ea96-4878-b017-2c70fe6b78f1	2	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
5bafbd28-b067-4851-9980-d6caad1382a3	3	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
215b0207-76ce-49df-80be-edc4038f735a	4	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
067bc865-a665-4b58-8f87-f0ded1874b98	5	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0afb51e8-b7c6-489e-966a-dfe76404296c	6	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
febad343-dc24-4a63-a1a8-16b5d161742b	7	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ab659ef7-5485-4559-b447-9445abcbfc60	8	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a73e7b55-627b-4d24-98fb-5c4319e51f3a	9	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
18ecca92-bd4d-43c8-958d-3f393a23185b	10	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0face949-26de-4d4d-b650-5b75528da319	11	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
31edbac3-a15c-4f3a-a513-42fb10d964ca	12	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fc0655b0-ef8a-4af3-9a26-8fd0f8eff00a	13	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0d5e566b-fe4e-441e-bed3-bc40f3e402bf	14	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
bf6dccfb-8fb9-40d4-8d88-ed9d63b34021	15	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f341002d-528c-4255-a715-4974d1fd8d24	16	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b50292d3-279b-4976-899b-49519b0fdb6d	17	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
af16ab3d-472b-4083-8ccd-ff913a3e9ad0	18	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b0252fd6-a8eb-4724-800e-29ed0a3d5cb2	19	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a5b16d9e-4cdc-4023-b1c9-cf85502af788	20	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e7d559e3-6493-40f1-8d99-8961542e01ba	21	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
af2b1315-d491-4196-a2ef-1601d4ebec63	22	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
15ca271b-9362-4ab3-85ed-9ca33586bbb0	23	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b1e37c93-c730-42e7-8fcd-bc1073d1233f	24	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e566aa88-c517-45eb-8f1b-b16c80206d32	25	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f7ad58d8-caa9-4e61-a9af-008637b8bcb4	26	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e7323c4a-b49e-4087-a733-6e5e37302035	27	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fa5c6254-8802-4e96-a954-6a6004ce0a08	28	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
9c63e947-4639-43a8-a46a-580c9afb4b30	29	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f75b8ac4-5bd9-4c0d-b673-b727246bf05a	30	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
84714760-bca3-4ed5-b9a6-b1fca0ef5a6b	31	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
83ca952b-50d8-49d6-a667-07b21e895fe7	32	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
53f96e9e-0a2b-4646-a041-44eef415b3b0	33	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d5a0873e-fb3d-4bbc-8079-934bb595ef90	34	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d44c5c97-dc09-4976-85b2-af2b9e20636f	35	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
01000bc7-89bc-4364-903b-9148ec7b5a69	36	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
01508744-e631-4306-b9cb-d9b90e6f62a1	37	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
dba34e6b-26e3-41de-9d2f-0929dfdd0e29	38	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
53f06e1a-2280-4a2c-b7ba-ea77abaa55a8	39	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
beceaa88-c559-4239-bc3f-58535f5daffe	40	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
53d0a750-c33c-426a-b1c5-cae3c38925c1	41	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
94c99052-4524-4d53-9338-1c73c8143fb1	42	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ed6db915-b908-4773-82cc-99b3d718db4c	43	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
32c5c5a0-2405-4c23-8473-e28bf65ed0ea	44	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
caacdf03-1aba-40c5-8d3f-59238c834049	45	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a486694a-7624-4724-b76a-5d46e62f9909	46	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
69cee7fd-f82e-4ab8-b1aa-46d3ca1980c0	47	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4dce9162-1687-4526-bff2-90738d23a673	48	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e7b4a9f1-5b95-45bc-b8ea-eac291353f8f	49	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
69806c43-7a31-4da1-9edb-d5f8b009831a	50	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
efb734d7-2271-4bcb-b968-98a5b81f580c	51	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d7d8c92a-164f-4501-bfe3-a42ea1a14c6f	52	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2c1f9cfc-ff2a-4b4f-851d-4d0690b4c2b6	53	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d3547499-4b57-45bd-8062-62ee7cf4e78d	54	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
9a027ad8-274f-4c8b-bf89-7dfcc297a375	55	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7511640e-d0d7-4854-8a95-a759ccc21d72	56	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d3dca051-11ef-4648-a499-19f9cc9ece3f	57	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2f889581-e805-4222-8142-baf0d3921964	58	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c6e73582-e827-41db-8972-eb430def2eed	59	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0ae4708c-507c-41da-8677-f524e9314b2e	60	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e43daef9-ea96-4333-970f-42810858d333	61	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a3c46f42-2944-4744-8ca7-48d255c3beb5	62	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
82487067-da64-4e12-9c56-2d840f8a14e8	63	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
448a6bba-ae42-4b1d-b31a-359e28798400	64	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a7468b57-4083-4d7a-85e7-7417e22bfc73	65	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4cd59f68-8aa9-4311-b266-f2070494b2c0	66	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e4306b71-de89-4a7f-bf42-cc85c6bc2da1	67	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4be28701-2c16-4d73-99d3-61011a422745	68	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
5d91a0aa-855d-4210-b342-f05fb2ac1925	69	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
3c96bbd1-7ace-4bde-a24c-79dec1dc982a	70	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d0ddb001-0dbb-48c4-97e5-25338bcf1a24	71	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f934b0d4-504d-4fea-a1a3-88efcea50ac9	72	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1599db25-ed46-4b34-940c-1e513d9e9ef1	73	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f2028823-6f99-4b45-b28f-d59cfd467364	74	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
80f31304-7faf-424e-a654-d918c60613ec	75	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ee85fd04-0d47-4a77-a982-54b33492d1e6	76	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a688d1c7-f86d-4779-926c-78672676f5bf	77	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e9147a34-824d-4c55-b9db-7ff0ce99d96b	78	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
08b548bd-eb87-4470-aff5-aeb8f6dbeb50	79	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
06a357ed-ead4-4c03-90a4-308886b96830	80	19f072a5-7240-480e-8a97-e3133dc52aee	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
6fa1af2e-fd80-4d6f-9349-d6853d035d81	1	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ca5bfb30-1a3e-4f43-a795-e3899a09fb07	2	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
43949576-6e84-41b6-b16d-51ef77df9056	3	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ec244a86-4250-4135-96e9-10c3c9b015dc	4	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
747a6c65-8675-4fd3-8228-026a17e4c82e	5	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
7d420824-5c3d-4f1e-8d12-3a05cc793b50	6	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
714f742c-17f7-4c40-87c0-61fc7b665807	7	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
03ee8e33-0778-4a84-b8fd-2be4ceece475	8	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8e2a4bdd-ecc1-45c2-8a55-7bd92f1644ad	9	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
22b8964d-c188-4097-ada9-177530446479	10	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4e789405-1b68-4790-a701-0c18203292d2	11	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
cc00c202-bb3f-4ffe-8d44-df20e6406054	12	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2d521155-db7e-43dc-9191-9dee70056852	13	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1fb7ea11-5c7d-48ca-be79-b67cb2b4ebe4	14	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e1b2f9fe-0ab9-4602-9d22-a87fd888554d	15	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4d161719-fc41-4255-8755-1ce4743de24f	16	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
503473db-f419-4f9c-bcd5-dfd185af5e58	17	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a8396b88-2332-4562-8de2-9e41848bd819	18	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
bf946d13-8f9a-4d2e-9337-f0d24ba89ed7	19	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
32667b4c-0532-4eb9-ae96-3303d95d802c	20	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f619c88a-ab41-4576-8dd2-efa445e15d6a	21	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d84a2a6a-f40d-4a70-a7c7-2d889cc4eecd	22	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
727852d0-01ea-47eb-9a0e-51a3bd18061b	23	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
95223c4e-0736-49f7-8b2b-68d715d3421c	24	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
32361dbe-9914-4fc4-b9f6-e09c87e45c2f	25	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
eb63f9ca-de1e-4a22-b530-86ee7507dc47	26	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1edd8c80-99d1-4a06-95cc-dbbf5d4b5d20	27	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
e9570cda-a2f6-4327-90a4-414d13bf4b99	28	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
23f01692-153f-4d65-9de8-404d29496c7a	29	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b58723a1-ade9-4cee-a272-19ca72aade32	30	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0c334bcb-36d4-445e-9831-1f9e55cf90e2	31	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
44c67ae5-6329-4490-bf75-7eff19afcb77	32	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
30db4cc7-6187-42d9-b92d-feba25b5df54	33	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0f63ea6e-caa2-426c-9e39-7c6a3cd2e585	34	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fd8a8d9e-47a5-476b-8058-c6967997b2c0	35	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4773be73-4dab-4c70-961c-6e74da46a4d8	36	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1ab3ada4-195f-4a21-94e7-2c331c197998	37	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ea6e8e4d-48fe-474f-87ed-5a9c98a1ac8c	38	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f087d46c-cf45-4b51-88c9-665d66a19509	39	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
ca53d5e1-3e2d-4567-9b8a-44d1884a0628	40	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d9fb9096-0a67-48f9-896e-1cfceb65ccdb	41	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
9ff28af2-c219-47ef-aba2-91ae0b8912d2	42	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
59644b70-5f23-4d2e-9626-694d82f8fbe3	43	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d2d34c98-c5fd-4187-8f3d-b1647ba40903	44	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f65086e6-a524-4a9f-afe7-daaae75c3719	45	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a1be5a44-9c33-4079-92af-b8dcf314cb1d	46	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8bf59914-5907-4fbc-bb76-e542b4d914d1	47	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a9045e40-9511-491f-8827-56d1a215dda8	48	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b94713ea-1c22-4413-bfcd-4b701cb26e41	49	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
57ac9a13-334b-4b15-a054-dead1b9c7df6	50	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
32dedcbb-cfa8-4980-ab28-20137bdd1576	51	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
0325b097-765b-4ce9-97ca-99581a8b5729	52	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
3c358038-938c-4b2e-b13b-55533427d7de	53	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
cf7127bb-f9ac-4c78-8dfa-625f4d857209	54	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
99b9f351-348d-4e92-a8ce-e4d119013bc7	55	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
323b610b-4121-4ca4-b930-550330704bdb	56	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c9c8b6b6-8b41-458c-af4b-8297217790db	57	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2504f603-6ec0-45d7-a878-ed55742c1001	58	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8c636901-b6d2-4767-878c-593a3373d24e	59	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
f6a7756d-66e2-4f28-8022-d468718b6591	60	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
12a72bd8-1465-4613-ae3a-ecbf25650430	61	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
d6a9819f-e363-44b1-ad95-dd63e7b76210	62	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
32c95fb6-d213-4ce8-929a-e6e670714baf	63	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
2e6b3a0c-8e9b-4236-b912-b80ab26a6936	64	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
390661cf-33f2-4ef8-9c0f-6c7eed01c40f	65	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
873bf0cf-53e7-4888-a2f9-d4ea001f83a9	66	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
35e36c12-9096-4146-8a69-d8485991d872	67	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a0fc2acb-2f59-4f44-8e67-2c382489c758	68	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4a021073-aa6a-4d04-b953-9adcf49644ca	69	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
36109b5e-1570-4c94-9117-85aed9a6db4b	70	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fff8af05-b122-45aa-a7d5-443aa2cafe69	71	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
a00878f1-f64e-4448-93a2-285132d4b4f3	72	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
8af30463-6c3d-465c-8c7f-48829b1508c3	73	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
05a4cc85-dd1b-4a5f-b29f-cd95d1735909	74	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
c1e1fda6-f6c2-4e55-a9f5-1e950ff739d8	75	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
4b1b22aa-a2ea-41d3-bf9c-037f5b52167d	76	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
1420ee49-8ce8-4186-a033-d38de7443b8a	77	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
de951a12-e0dc-44a1-b50b-91763ce0c767	78	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
fd469cb4-9b83-48c3-bc61-365f5a814d43	79	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
9dcaf640-7fba-40bf-b0b4-af53eff95677	80	348d7f79-043c-47d9-8f4a-29fc2a31bb96	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	f
b8346885-ecfd-4ef4-a1b8-26cad23ab35c	41	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:52:11.505	t
b74db5e7-a577-4887-a5c9-32ffad10c4e1	44	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:52:11.505	t
8c9eedfb-e4fd-48ce-afcb-2bb2f4138bbd	51	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:52:11.505	t
4d43a32e-50b7-4a88-b70b-b7c581b56a44	54	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:52:11.505	t
3fde3798-a754-4b84-b30e-96f7375a5652	57	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:52:11.505	t
a6b0c63f-d813-4e24-80d9-3154547b6455	72	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 00:53:50.797	t
b057381c-b3f1-4794-a1aa-25f87603bfb4	40	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:56:37.149	t
357d7618-720b-41f7-8d46-ac7b699fa462	41	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:56:37.149	t
34f32da8-257f-4236-89c6-5ae009bbdd5c	42	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:56:37.149	t
1f8a38f0-a71c-4f81-a198-1e62dd0ea1c6	43	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:56:37.149	t
4138a8ee-4546-4741-b6ab-48c27ba35b6e	52	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:56:37.149	t
3a640480-900b-4f15-bdbc-287b2e0f76eb	80	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:56:55.298	t
3e1ad571-2843-440d-8b2c-32a61124293e	66	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:57:27.309	t
dbada7a3-6ebc-4b3b-b270-499292e77298	68	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:57:27.309	t
9558268f-c9fb-48c1-9ad3-c61ee15d6ff6	73	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:57:27.309	t
dd8b077b-14e1-425e-b615-f653618cd378	74	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:57:27.309	t
f80c2c98-fb6f-4feb-8282-70cc9947ae78	76	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:57:27.309	t
79536f88-2b48-4fb2-ab49-dc9c14bdead9	77	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:57:27.309	t
6a464cbc-6cb0-4149-acd8-c548fba19691	79	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:43:28.56	2023-12-16 00:57:27.309	t
a1dad0a3-c548-4252-87a5-9957f9bde63e	1	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:24:07.285	t
97708b62-dd0f-454d-a172-e80a153ad856	8	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
5376c6c6-0939-4e80-9e78-5e728be84f9e	9	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
440066ab-89ef-4b90-a544-5ae4736282ec	10	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
b447df80-339e-4f28-9011-5a900fe8b1e8	11	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
e3a785b8-46ac-4882-a1c2-7d587dabaccf	12	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
b8f5a04c-8296-4efa-af9c-cc1fa7af902d	13	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
917d6406-2f3d-4aa3-ac34-49510b74d00a	14	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
a257d395-b470-4e01-a95b-78fd6e126753	15	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
be066b6a-147d-4fc1-988b-696613841174	16	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
b0be4ac5-860b-4150-947d-db8396261240	17	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
f48206ef-6143-44f1-ae34-a7a6451d2600	18	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
2e2785a8-3b4e-45cf-95dc-11cee0c0f8d2	19	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
8792aee2-e875-4f17-bbff-ea7d7cd5aa74	20	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
aaafe8e9-15d2-4fde-83cb-5d2d015000fb	21	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
735879ab-176d-401d-9a2d-5989decc587f	22	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
ee422e69-7f20-41a3-89ba-ed73a3628fe3	23	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
a4df4e3e-a0e9-48b2-957c-6be1e757052c	24	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
dbf4f8a3-f166-48b6-8f0a-7f25ec900a6e	25	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
34beffbe-aac5-4175-9166-b0fbbe5dcca8	26	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
318cf592-85a8-4067-95e4-1caaa8a2449a	27	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
52c3fe29-1781-4b30-ac6a-d15040a4ccfc	28	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
3b7a6409-570b-43ab-9914-c84953db29b8	29	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
f42ea4bd-b3c4-45c7-9e1a-daa53e896976	30	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
2e8554a6-2d6f-48e2-b9f9-75817f16e284	31	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
e8750f59-5a53-4a6f-8d69-f67499112781	2	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
55d5dac1-c986-4daf-a185-76dfb75dd80d	32	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
7aa3b092-c8f6-4892-9cf1-7c1448ad611b	33	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
332dba9e-42fd-4b53-86ad-cfc3af89b3c7	34	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
5fda088a-7514-4670-a82c-d2d9b3fd58e2	35	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
6e9b1bf8-21fd-42c0-8e6d-2c721c0b5ac1	36	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
d409008b-693a-449a-bfd3-07006035a25f	37	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
91ae1756-bc6a-4a7e-a50b-5924c18eb1de	38	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
3b76b1ea-bab4-4c71-8d0c-ebacaaf54eb8	39	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
390f613c-da53-421e-bfe0-6fd7f1bb9dda	40	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
26825692-9ef6-4145-bad7-e322dafb2ae8	41	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
57b9d182-08fd-41d3-977e-4d7a9657a0c8	3	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
88d34ad5-9fb2-49ce-b276-8f3bed4dbcff	4	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
c599a0c1-f161-4524-b117-8bab0d04f89f	42	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
b52a7d77-f58e-47fb-926e-b2abb5823a12	43	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
9382dbef-4c42-4f93-b811-3b689da5f73c	44	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
2da21b99-9038-4898-a4d6-0ff27b1aa55e	5	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
273298c9-55a3-4697-b189-fb4525875149	6	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
091e3584-d632-4607-b0b3-70e433270bea	7	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
72f4ddc5-99de-4d5e-811a-8145f0cfa7ca	45	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
9d9c4106-7ebf-46aa-9941-2a20ed2ab963	46	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
557b694d-fd65-47b8-a3e6-e1da89d1680d	8	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
d75e6779-2847-44cf-88ca-f62c322fa16a	9	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
fbef48c4-af00-43ac-810c-480d2ced92c0	10	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
ebf13b1c-47cc-4824-a7bd-7e70f7702231	11	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
e17d2a03-46de-4e1b-b7fc-79005096a742	47	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
594fc5fd-dbce-4087-b718-c67aceb3a4e3	48	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
2a176a4b-58bb-47b2-848e-ea5d765fbd17	49	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
afd2e228-05a8-483e-ae32-def6561c7315	50	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
3d1edcd0-716e-402c-97e7-294ae633f0c4	51	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
132979db-675e-4a52-81a8-8a0b72fdf0dd	52	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
33efb098-6593-46ff-91a6-3949e94011e3	53	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
489a37df-d5ef-4d79-a710-c5d50746c38b	54	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
7c9068e0-1ca2-47c2-b803-b5658491aa54	55	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
437a0612-7f93-4e3c-86d2-7127a01d412d	56	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
0337d280-eb2a-4e8b-b221-8313f5fddcf2	57	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
a6e5644c-0bb8-4657-b38d-75553e57292d	58	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
72075cd1-531e-454b-a7a9-13060592ad13	59	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
5e77e84f-3445-43fc-807b-b66332778be6	60	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
19308418-6923-48d5-8c48-c23de4983603	61	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
c435fa80-2908-497f-bb09-007d97088e39	62	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
a9535395-6677-493f-b179-d133be5b8584	63	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
f66eb26f-dc5c-4263-9896-e24390c74c6d	64	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
5b4b91fc-25dd-48ec-8040-e022a9d9db36	65	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
ac7e8062-f3d0-4131-98a2-210c5913e44a	66	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
9a033387-2420-4e88-b32d-ecd629935a8c	68	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
e8292f3e-7efe-474c-b42c-7d6ed3c064bf	69	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
88c2e107-b002-4de4-a47e-be2b861899c6	70	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
a328d994-ace4-43e5-b75a-0ac0eb00e53a	71	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
8742640a-6d44-466a-b4f4-ffcb9275ff50	72	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
b796f0ad-b339-4e55-9bff-2dd4134a4f0a	73	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
1faa0b82-c841-4002-9e64-1f7022b7f16f	74	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
8a4cbaa1-4534-4bef-a865-50cecfbf09f7	75	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
02a3fac7-51fb-4438-aa2e-1a7bf6647a05	76	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
f4d33164-63a1-41f5-b332-b35137005af8	77	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
bb17336f-f742-4d90-bbaa-8658547f063a	78	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
ff0df21d-d251-43f8-972e-df3b6fcfaa57	79	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
fba11dba-cb83-44fc-9201-6938924d1875	80	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	f
86968c5c-2b9d-4b67-b9f8-2f360e0617f9	1	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-17 23:42:29.613	f
4c27b542-2f4f-4a28-b34a-bc782f746f82	67	fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	2023-12-17 20:48:44.004	2023-12-18 00:47:17.528	f
5ddd9c3f-0a8e-4745-8d6b-837343648b99	12	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
00bc7a7e-1c4b-452a-a213-50f9ad702b92	13	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
428b7cc8-98af-40de-a477-f36cc3580502	14	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
69d8e2ec-1707-4afc-b306-58ff7907bbbd	15	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
2a2ab462-4f5f-4604-b4e0-580124f59b45	16	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
fb9eb62c-d5a3-4351-bd41-8b1da6004f3b	17	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
b5f90c04-9221-4001-b812-45bd15a47252	18	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
18922031-fc2d-45e5-866b-2a7e7b5c0878	19	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
1c8de2eb-b1c4-4618-bc61-6c5b3752ee8f	20	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
7243e66d-aa06-4c21-9f50-938bc166896d	21	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
7a9cc857-8fbc-4499-9f6a-b5ccfca28caf	22	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
42b625d0-3f77-4736-9c7e-d78939252d48	23	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
5f2e475c-0780-45a2-b905-a3b5b546b327	24	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
be077769-16c0-4fc6-afd9-8ce9f36f5d1e	25	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
65168d79-340e-41d3-9840-7f3785d29ae9	26	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
e9f9413b-19c3-4f61-94da-c352e886638e	27	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
c93d112e-27dd-4cfb-b0da-8b6b06fb283e	28	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
ceb63395-a44a-4749-a16e-722b7bf1c7b5	29	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
9a83ca63-756d-4173-8e33-c263c130520e	30	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
446d1f40-fd91-42d9-bfce-06ef4b5d529b	31	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
afef54de-7cb5-468e-aab5-2eb2db2d5bb7	32	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
cf6cf68f-4465-4941-8f11-9391ad02fef8	33	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
8127fe60-5096-4d51-a7cb-760f253ccee9	34	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
dc984bc5-9bbd-42cd-8f2a-9d51c9206123	35	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
3f0feb37-8423-4516-bdb1-85849917426b	36	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
b79d1544-e89f-4380-b94a-b4b783b23e9e	37	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
811f123e-b557-401a-86b1-7512e8957f9e	38	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
9be8d25d-aed8-4a22-93d9-5658c6a4cfaa	39	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
0624603f-b028-4952-a707-c6758f346e20	40	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
f3f6188f-b1f5-4d65-ac68-f47eb30cd3c4	41	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
364f7098-dfad-4fad-abab-c98c458c88b8	42	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
fe3ea5a5-b45f-4755-a703-ab3074a3cd4a	43	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
ccf68954-584e-4360-9a4f-25800ee27ae4	44	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
733f476b-791d-4653-883b-95456bc227e5	45	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
aefa512f-63a0-4f6f-add2-448b4d80c38f	46	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
44e8d899-52fa-427d-a059-5f6c6e27ca96	47	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
7d22ecf0-2dd5-4def-be53-08ec57e4a08c	48	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
42d03fd9-b879-4eee-94e9-86a779c6e2f4	49	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
23fd74fa-df2c-4e11-88da-27bc2e542639	50	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	f
f03c3a43-40b1-45b4-a63e-97ade456164b	1	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:20.285	2023-12-16 01:39:49.941	t
4c1ed4dd-9836-453f-96c7-9ee61c164958	18	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 02:05:06.308	t
bf914e23-94f0-49e2-ae1c-3fd72158a9f3	16	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
9469886f-4c5f-4822-81cb-f2806abf1a46	17	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
e7fc4539-70e1-4b0e-925d-7ca95a8fb730	18	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
8c0a2ad5-8826-46b0-b230-c8a2cd1d34e5	19	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
1dde63de-d57b-48b9-98f0-f28bdba2420d	20	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
c8abac5e-0bf9-436e-9545-36ed14c7e595	21	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
0079c7ea-86fa-4985-8afc-e0b8a98a73c4	22	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
66ed19e8-a861-4858-9cb9-1cc7ec446a49	23	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
f43d86e5-50f7-40e5-86cd-d0ae0a479fc8	24	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
3811f46d-2b62-4468-8967-eddb84bb9b91	25	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
877c1c50-3acf-4422-8b31-49f817e62dfb	26	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
2b4b7e4d-9421-4361-8d3c-3adc8396e389	27	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
1a02b359-01c5-401f-bb45-50ac955d8940	28	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
09f9d95d-1910-4468-8657-01a0ba9a0139	29	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
5791fb47-47c2-4202-bc0d-158fc7ace5f7	30	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
ac05a64e-1e24-4aec-9357-0ea3b8d0e4bc	31	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
629b12b0-f3ce-47cd-8080-69d0184a8ca2	32	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
bb581571-66b3-4e90-8f98-a3d95dc67ef7	33	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
24dfc798-3f8d-43fe-9566-2b5334dbd255	34	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
e2a76a14-3660-46e8-b731-9e4c51282794	35	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
da32f160-aad6-4313-91bc-4eb621f867b1	36	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
7e0ff155-a658-4b3f-a6e8-426a3d825de3	37	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
065d9f10-2476-4da4-938d-ab22db26bdbf	38	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
35c3b044-b743-4c4c-96e8-5fbe8d473dc0	39	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
ba1c0a2c-5385-4e5b-897d-b83acfb3ad01	40	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
c9212e5a-52f0-47ce-ba2d-13120318ea55	41	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
48b2f084-178f-4cfe-a08b-c76a679f33d0	42	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
0fd5b9fb-6caf-48a8-99ed-589666357e3d	43	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
81a7223b-eb0c-4bb1-a29b-a2b39a3c8b7a	44	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
c206cf8f-852a-43bf-88c8-82ade7e1ce64	45	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
8c0012dd-c8e3-4798-9540-7646262efdb3	46	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
4987ce4d-45cd-4b74-8a71-ab753921ae76	47	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
f1e72e11-5147-40a9-bb99-d14f5f3ae320	48	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
396c2e16-f13c-4b70-b0de-9776656a46c3	49	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
43db3f3c-d1c1-4dbb-a7a7-f9659e4dcd60	50	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
72e50539-aad8-4d1b-a331-25a8ec135508	51	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
ca691bbf-4ac6-404a-9a87-498bca7cd160	52	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
f7cb3e66-5259-45e0-9f74-d91a72a95666	53	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
49e1153b-772b-4534-a836-9e7db313b762	54	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
b4db352f-f182-42be-b419-1bb8fa50ccd5	55	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
0f1ab49c-529b-4e42-abf3-db7984623965	56	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
2b61bb7a-8979-4ef6-9220-23deb93dd631	57	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
b8afb966-3732-492d-8246-f512d9a71a8d	58	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
6978a24d-82b7-46ee-b9f9-2dde8755d9aa	59	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
30ff70de-0491-4d85-9e34-0596617a9225	60	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
ed85974c-e5db-4687-bbac-b0e68e23b542	61	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
f0824fe0-89c1-4bc1-97e0-0f32b16ecd05	62	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
4fa18064-4593-4849-8b9a-161ff75a465d	63	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
17f6f7b8-859b-4982-b35b-6bb05bfe6ce5	64	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
386a474f-d7ed-4438-8bbc-f35d84fcb647	65	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
108e032f-f046-429d-bc6d-f7a32084fb4a	66	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
1322afd1-9fa6-4c12-b108-9bf5b178dcd9	67	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
04495cb1-d820-47c3-8c81-6e8c755c07db	68	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
78854262-c1a3-442a-9569-a81441fe4e48	69	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
60d0526e-0b50-4853-be38-374293bbaa58	70	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
a6680e61-7fd0-4207-900d-09a3e7496e55	71	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
12296885-d774-4358-9593-b416f5e820c1	72	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
55c70225-90e4-4ade-8f80-3483818d6181	73	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
ff463c23-1cf2-4770-948f-31a97bde4388	74	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
809b4fba-ab9e-4c38-99a5-091a0ba1bb82	75	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
b905743a-993e-4220-8da3-a9cd4a4d9901	76	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
b256137f-cda5-480d-8058-8aa9352734e8	77	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
2ab648e3-3035-4b20-a067-3c3868b12fa1	78	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
399c0e31-4f60-478c-bf01-ce77a35a206f	79	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
7c142844-b4c2-48ac-9e85-f89861db453c	80	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	f
c9e62d00-10b1-4b79-a4db-9957d1489c8e	79	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:42:52.578	2023-12-16 02:43:25.716	t
b7cc0835-6bfa-4c6c-b7e7-1c11fa70aa98	1	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
61c7c364-15dc-458b-a608-2a5fcc8efce8	2	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
0aa655ba-8b1d-434c-8238-6089b03926ed	3	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
a44cfe59-8c6b-41c7-9b49-c735a5e7c733	4	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
6d4b08cd-28ad-4d91-b6df-7f93b9847fd6	5	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
e72c74d8-7738-42d9-802a-a6e797392f7b	6	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
41d76e84-13ed-4c2a-ae18-bca2fb495832	7	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
acae76df-c187-43b3-b16f-0da32c5cf5e1	8	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
083a6156-dfee-4bdb-8063-f4674ec39bf0	9	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
a82f66b4-3843-42bf-a7fb-372aa717bdff	10	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
0d3836e2-41de-45ed-900a-f65dba7fc27a	11	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
967434dd-91d7-415c-b821-d141954c40f4	12	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
28d029a3-5160-4731-8993-71f1867ded2f	13	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
dd278682-ad7e-4c08-b0c2-faa443880cbc	14	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
cc4a21e3-1c6b-46b2-8e7e-60e3272bac24	15	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:03:50.343	2023-12-16 01:04:24.201	t
3794374b-1650-4d5a-99d4-c9de776ba85c	72	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 12:25:45.584	t
fb55307f-ec24-44a0-8007-d484f04dd5f9	49	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-17 17:36:51.631	t
fe3254cd-57c3-4840-b311-144c8542923a	3	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-17 18:49:13.743	t
28f410a0-6589-4552-9831-b3ccf2592c06	72	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-17 18:55:17.579	t
0ff0e4ec-266f-4b12-a7d5-4eb53fd9b26a	34	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-17 18:59:30.866	t
6a9b3a02-884c-4a7b-b947-2ae854b470d8	28	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-17 19:09:45.256	t
4e6378ca-0546-41aa-bab3-850a8eec8b3a	21	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
4d00e9a3-89ac-4c56-adc7-0b16863a49da	22	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
7c47b3f7-c0c5-4627-ba4b-97afc285a0ad	23	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
b7b410af-daf5-45bf-85db-fc30d3006df7	24	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
a7c701ba-6ddd-402f-afe1-521685c05d94	25	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
56de0aed-a974-4752-9469-2493ad76ff87	26	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
7ec4358e-193d-4e5b-9b91-8476f2f4be63	27	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
e0283e40-4366-459e-8c7c-0c32e81a3694	28	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
d7e219b3-27f7-4f36-a88f-28766adcdf5e	29	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
793a0edc-1b2e-4d0b-87e5-b6374d7a816f	30	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
6ed49c68-787a-4e21-bf68-6cf8b0a137bc	31	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
a562ea72-c957-4f20-8a24-8e3c38504ccc	32	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
fa4ca300-a31b-4e07-a2b4-e73b06285475	33	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
3715739a-fd88-4246-b3fe-8feb519707eb	34	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
141f97ae-4841-4618-9313-6bb533a661c9	35	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
bfd3dd16-c76a-4d21-9da8-8866eed6b27c	36	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
348fd09c-ead4-4dce-88b4-e27a00ffb0c1	37	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
c1fa73c8-c551-422c-86fd-c1340426bc65	38	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
d38c5086-c166-4325-a80b-616291379bdd	39	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
cec6ea11-ed8d-440b-998c-1179ca02089f	40	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
3a9c54f2-8f2c-470d-b4bc-f9d775af86ca	41	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
40048634-a3be-47bf-99e8-9676d5674408	42	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
7cb0c413-f3a9-43ad-b363-480e698862f7	43	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
411d8e0e-6e45-43fa-97c2-a12280d7a429	44	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
15527cd4-df03-4fbe-addf-ad43588c0726	45	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
dfcc4bc1-88bf-4714-a7a6-86202571952e	46	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
6711bfb0-c7da-42f5-9de1-6a16f0b19fb0	47	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
7e11e423-caf3-4790-be4a-03bff1092a35	48	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
6e895860-4153-4684-a84a-d3f4feef4a0f	49	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
dcf0caea-5f18-40a8-8aa6-cd9199e6bed1	50	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
c3c3493f-59b8-4dd2-8f43-4e9b2d7f66e9	51	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
dc1ef68d-4b8f-4e89-a3f4-3cb9a5039af2	52	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
d933a6ec-2cf0-4a43-8eb5-363141292b3c	53	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
425b1792-2fe7-4d50-ac5a-f1bc61e6de13	54	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
536a19f5-b476-4f68-b28f-2a062384dd70	55	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
391404d9-a41f-4683-82c9-2bea98064490	56	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
c9095002-78a5-429b-8a28-b9992b614daf	57	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
10b20a67-1c6f-45ba-89e8-d55c22020b26	58	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
fb5e8f95-0762-4e02-aa73-5c798a59d1ec	59	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
2c5c6ff4-edd6-4f78-8f4e-8bc7fc810043	60	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
08aa22e3-f495-4537-9dbe-bac9e796cd96	61	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
5e7b5bdd-d3bc-4931-9c0d-9159ad415a19	62	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
93d1a27f-43df-43db-a33e-a8f3c29873e2	63	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
2bda006b-6600-4d7c-806a-9b2dce3c33a2	64	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
265db278-cd2d-4214-a66e-cb162cd9dbf1	65	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
60725ab2-a435-400a-a2e1-27a9fc55f620	66	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
1248ec85-852d-4645-b8e6-8671d8cfc025	67	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
b47db3fa-462c-42a2-acce-0e97671997be	68	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
1b971e74-eea9-4716-960d-09040b026ef9	69	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
6ee2e545-a40b-4e51-916d-89b8a0aab5ed	70	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
95fdd7e4-8dd6-4be0-b818-84e71472017a	71	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
2db208c5-5b18-4850-8d60-5718675b0523	72	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
26cf3967-5e99-48fa-b172-0d7ab43a692c	73	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
843fdb35-a27f-4e22-97dc-5103e3801a01	74	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
5186254c-5294-4f67-ace1-410965a794dc	75	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
bd42880b-5fe2-4443-8743-e45b8423abc3	76	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
40136930-2303-48ce-a755-0fca181b053e	77	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
11af2892-49f9-4e2a-adb4-edbd7f6b5588	78	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
62ace01e-5e63-4fe0-be47-ff45f6d6a416	79	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
1610af81-2d85-4357-9a6e-9bfa231391cb	80	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	f
599bd1ca-c0d5-403d-8e0d-a5f7a28d0e10	1	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
ba7d0a29-748d-4525-ae27-a6ecc22a3a3b	2	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
80946491-2a8c-48ed-bd71-bd1bd30e7aff	3	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
763335bd-b30e-4816-98ba-85ec8ebb190b	4	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
90827b64-8b91-468b-ae81-2bb1dbda74aa	5	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
af130217-f55c-4d8d-8113-70b19f93b965	6	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
1a88806e-3b25-49a9-bc29-f26e749ede41	7	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
4e702c56-db50-4698-9667-0cee296e9df4	8	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
7f029eb6-9533-46df-9f3b-ea945d4623c0	9	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
aaac19b7-d022-4f09-9b5e-8a22e03f554d	10	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
c5c4079b-b0d7-4da4-8b47-46cf4ff008fe	11	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
58a6fffe-8fb2-4079-93c1-d057606f8083	12	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
df352a35-efb3-44c7-8736-f79fe4ce53cc	13	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
36c22258-fc77-45d1-9d31-0b8a7d1dcb5a	14	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
300d6f9c-056a-4d53-b42e-e0758ac3adff	15	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
b6ac89f3-5f00-436a-8f82-2198452e59fb	16	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
7a7d7939-5ea9-41ef-8d52-1a012d7a5064	17	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
2af8f0c5-a9be-4527-a6a0-dd88d3218932	18	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
8086faba-9bc4-471c-a02b-657b24fb1060	19	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
07849fc1-cdaf-4375-b67f-c298e4860169	20	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:09:13.773	2023-12-16 01:10:08.687	t
590e5db5-e573-4ba5-9105-1e1076a9aa0a	4	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ba5477fe-0027-4050-86d2-587d367b9a9b	5	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7491aa70-2533-445c-bf1a-79a3f2b76b36	6	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
862511eb-e150-4c32-aaf0-8b1e455245dd	7	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3dff9541-eecd-4a6f-ba55-a827f9b3d707	8	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e297003a-7340-4386-a305-f0e980c06ab8	9	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
468ebc77-6a61-4a83-8297-25b32d954abf	10	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9727c621-1bf9-4f5d-bc22-b76506d5a99c	11	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8e8f915c-687c-464c-baea-e0066e0c9c31	12	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
27d65b43-8a23-49f1-a21c-73aaaa83fdcf	13	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
197f7b5c-0be0-4d06-bd4f-ed79847f06ed	14	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e5381e39-7475-4b5d-97c1-e7d47af8013e	15	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8dd170d7-8f24-4880-80c0-171f8c0b27d1	16	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
51cdeeac-b828-4ae2-a343-a0e311d6e6fc	17	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9d766a64-7f3c-4468-a730-0af7430ad550	18	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4e52df63-43e2-4e66-8afa-1ea7df02143c	19	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
df8d59c4-a95d-4cfa-9927-3d25968336ed	20	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d45fdcf1-ab4d-4dea-8bbf-4b7f02afc182	21	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c6b0aa70-9d64-4194-bca7-bc1be41f6854	22	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9a875493-6d61-4c11-82df-43c8b2af04bd	23	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ed1010a6-f26f-417e-8b72-6524d2229522	24	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2563925e-b897-436f-b888-b24152fbd00d	25	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
320fd581-1ccd-4202-b876-7509f4621099	26	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d22b7cf1-1425-42a1-a7e6-eb32dc2ba367	27	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2358d035-47a2-4e57-b6ff-dbfc49e5e55e	28	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
79bf1afc-4d34-4550-ab5b-52fa4dae6fea	29	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b6dbd5d1-fd77-4d28-a330-0a913290e5e9	30	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3dcf83b5-b593-40f5-b5b7-eaf7570750e7	31	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
715d68b1-073c-41cf-81c8-f266b8189d66	32	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
31d06dbb-d07b-4f9c-aba9-b1bf004c80f6	33	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6a00ed40-a9a7-49a4-a060-238638b36de5	34	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ccc9574e-f95c-4f9c-b991-3c450d8e79d0	35	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9c445db0-7e65-4517-84b0-06fb2bc7b3f0	36	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7635f728-843d-4d8a-89ae-6184cb35a11a	37	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4b700061-941b-47b7-b792-da3c7c02e8a1	38	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9ee8db50-69cb-42e0-8126-80fbd3e9e0fc	39	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
19cf901f-cea9-4fec-86a2-94cb3993f623	40	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ba6ee5d2-bbb5-4f7f-b8d3-d0b0630d23d2	41	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
dc4d21fb-f5f6-40c3-a884-be08d776d93e	43	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3514e5f5-8e13-4e5c-9993-18afd29034c2	44	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
5d7d6bea-4206-4b0f-b9a4-ba0c0bcb1973	45	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
12d6d846-900c-4b22-916f-78c1b05c7378	46	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
5071344c-55a3-4d56-b172-d1d87101091e	47	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fdbb813d-7559-4132-8260-4a046d557f1a	48	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a1916d60-127f-4f25-a829-159811ba4f88	1	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-17 18:26:58.146	t
217e7db8-fc62-4206-967a-1bcb305beded	2	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-17 18:49:13.743	t
22f5da41-0e1a-417a-8e2c-fa2f3f760a1c	42	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-17 18:53:03.08	t
f7e43ec9-8348-4f0e-8e7a-af5db6770fbf	50	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
896cb4bf-c09a-4b9f-aeb5-690c486af643	51	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
87d0d6d2-d749-4529-be5d-d36a6e772979	52	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3e1a34f2-0c2d-4107-911a-6280db1a239a	53	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9ed54f8d-cc3d-4689-a9f1-6d1c48ac5133	54	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f442514c-d2ee-4baa-8b4f-1bc11eb7e379	55	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
34688917-b792-4a68-b0d3-afca3c1f1a0b	56	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8fc48a37-de45-4eb0-9e9c-69e6c7635d2e	57	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f539661d-74d2-4173-809a-d8a41b766975	58	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
eb38b3b8-9dc1-47f0-82e6-c737a9c217f6	59	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a37c91da-44f7-4f9d-a6c0-f4c80f490c31	60	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ad8008e8-3192-4a2f-b7ad-7dec30deea0e	61	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bcfadd3a-711a-4daf-b0cc-bcc85dead410	62	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7502799d-077d-40ce-8efa-b1c04d02c7a5	63	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3a2d8e9a-86b5-47d1-9980-8c73c542bcd9	64	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b317581a-aed0-44bf-ad08-5f5658fae739	65	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
18d8d39a-fda2-4cec-9e78-cfe49760411f	66	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
15d67672-07be-4162-87bb-60b9e5188c90	67	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
57bb6591-19c5-45d3-8c37-203cff4ad5a6	68	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4479d6b5-98b5-42db-ad9c-7e16e198a603	69	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e739cdc0-8c61-4d47-90f0-439349bb054f	70	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c5efd594-06c5-403b-9681-c3c02be85b14	71	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
acad2899-32f3-41ee-90c4-1fa5e03a0dbc	73	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fa486818-bdb4-46ab-b6f3-eebe634e99c9	74	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0ac382b6-d3aa-4afa-bfc2-d4e7637bca2f	75	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
dc291580-241f-4995-9b10-b43f31121830	76	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f6fb63df-30af-4270-8e68-ea8769d0d9fd	77	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ff03c19f-ea67-4301-b36e-2b5cf552abe9	78	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9e521014-ae9d-4f8b-8b79-cf617d44a7ef	79	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
95b46913-62f9-42b5-b0fb-a5eb82f01abd	80	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ead5802d-42df-4234-8a5f-a9fd102923d0	1	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
95e5b79c-0cb8-4e02-82a0-ee1899e367b8	2	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b3718bcc-fc22-4b5f-8418-36d4e0d47958	3	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3352fdb9-9f54-4672-9cc6-c3990bad518f	4	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f9fc6cdf-2e94-4f19-a267-d6d12aef6e89	5	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3ee417e4-a03a-4acf-9043-75e46b70e59e	6	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
dd915b2c-c94e-4132-b9d0-85a2539913c3	7	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9c416c40-542e-42c2-bbe3-cface826fd96	8	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6427fd1d-9b81-4b35-9ac0-d1b6b0a3c94b	9	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3662398f-c13b-47bd-9961-5ccb2d576aec	10	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ba465937-8a14-44fc-935a-82b032ac91a4	11	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2d90c43f-0c9f-452d-8344-cf4281d5511c	12	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9f219a8a-fe6b-4b98-8948-32471f285ae2	13	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0e630622-7daf-4bb4-b734-448ecabec614	14	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6603e280-f78e-494c-a8ef-da50efe86d42	15	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8d61a9e4-3628-42ea-aa6b-79ae3afd6362	16	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a985a360-457a-442b-8307-00312e852acb	17	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0d6f18d9-e624-4f44-976c-da81a4e24cf5	18	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
133845bb-9b59-4fa1-80fd-ceba00103004	19	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2e8ae2ef-5033-4e73-b4a0-1722aeee96ca	20	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
1877cc10-0d53-4c40-8f95-3abab8f605b5	21	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fc9d2ff7-dc18-470e-a833-a9079e329365	22	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9885e45f-6cd2-4680-b215-66b15adac79f	23	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a0b4e1ce-9d49-45bb-afda-3344a258b9e0	24	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ab87ed41-c07a-4f58-9626-3a99a135469e	25	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e884c958-c2f0-4cb1-8fab-0bb2d47cce17	26	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
443ad388-579f-4cec-a72a-ffe0dafac811	27	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9df4c098-2aa6-43c6-93a0-e240b573ba1c	28	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b9673d76-9696-4f1f-9164-92d33b207fde	29	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
13ff8be2-cc83-4783-806d-185a2aeb5399	30	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
40c5fd4b-af87-4519-888f-9e939eac780b	31	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
049f7c27-ec29-4f07-beee-ecdd0b7e26d3	32	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
dbd3981c-9b35-4327-87b0-2dea48d08094	33	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
469f155f-52c2-490a-94db-95fd7a2ae669	34	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
baedf78e-1337-420d-b503-c47360b3526e	35	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
74295fbe-47ca-4876-8ec1-84f9474c1867	36	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7d4e0b44-0d88-4bd6-8817-6be9d289c671	37	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d31c5348-0592-4a0a-aa13-f2a5fdab18fb	38	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
cef3d9c2-13c5-4b32-8c61-f318c3b322b7	39	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e3dbda36-440e-428b-96e9-14942c1624b7	40	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3f93849d-4f56-45d3-a8f7-00611eca3a9b	41	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
5d71eaaf-7065-4809-a83e-add33b2a5fea	42	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3e5b15c2-9acc-4020-aff8-a650f73e4309	43	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
be4a9578-8ea2-4620-b8ef-3bd31a029505	44	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d8bb61d6-94bc-4da8-8f21-7487fe422f32	45	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
63bb4bd9-328a-4464-845e-cbd52a14ee82	46	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6f667856-f801-42c1-baa8-28a6bde42dbc	47	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8cbeb2fc-9016-4aab-bc29-7ada2d278256	48	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
1d2052d8-cc91-42d3-ad30-d2d1cdec3763	49	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
12681752-51f5-4392-85e3-ea7e5da5baee	50	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9a3f9cc3-b392-43d1-9a18-35da812d4269	51	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
040c6fb7-7739-4fb7-ab42-04a46881f6d2	52	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
541e18ac-54ef-42d8-8f26-5dc190ba9478	53	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
64b781fd-dff3-47b6-87f4-6755d33b36fd	54	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
5184f58e-3405-405b-bc23-019f9292658f	55	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b6746674-f040-49cf-a3f9-122f32a5a3db	56	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7af3fd43-29f3-472f-ac82-c3cde62b8bcb	57	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6a99fa35-1ea3-4b28-ac73-316a6eee250c	58	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
dc31e02b-4921-4adb-8762-b8ac5cdeba50	59	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4126a944-4122-479d-a604-bae39077e71d	60	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
059e85be-30e8-498e-acc0-a866ccff1a1e	61	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b21ef24c-e616-44f7-b9d2-d8c1aec0048c	62	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
28652ab6-217c-4848-a004-22bf4b32ef68	63	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f856af06-6314-4b0f-b47d-d049a4c7c321	64	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
42350d94-678c-4732-9f7f-c499479e0a1e	65	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a113efe6-d9d0-4a8a-aefa-9138f8d40a85	66	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
79d75cf4-2b65-4e7c-9f82-7d30d86680f2	67	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
168838e9-34ab-4d5d-8974-51ca0a8195ee	68	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
199a0344-c046-4396-ae40-fe38db65416b	69	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8f9f9c85-3391-43e0-9a16-c5a4aff4cebc	70	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4e0e8481-5d52-4714-8fa9-c330bff54561	71	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e49e3aff-c210-4b42-b0df-136bdca0bab7	72	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4057fc9d-28bc-4b6b-87fe-6c66e04c9107	73	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b3a29bb0-ca9c-4323-85b8-e1fd9c277821	74	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
1150acad-1ec7-4d0f-9fd9-088420e278eb	75	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3ce0d743-6d34-4b37-a1e5-7b0f59c2e345	76	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
70f82565-844b-4aec-9918-b8bea3ce2509	77	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ed307071-f838-4da2-92c2-48ab32ee858c	78	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
799eda98-445c-4464-854b-f81ae616125a	79	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ea7a00a0-ccd2-463b-a609-69e152a2001f	80	78e67fa8-7544-443b-8565-d66e446a663c	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
637f3f39-e49b-45cd-89f7-07b4176db48c	1	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
174f0661-d8ff-4409-8667-f4a03c192b69	2	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
24537ecb-bb1c-42b2-a91c-b0a72f0e8854	3	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e3c6accd-f29d-4960-84dc-a38a620a3419	4	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bcc7a6bd-3c34-4665-8405-8eb80b2b66dc	5	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
5e21731a-ffdf-4128-9e02-c73eac478b17	6	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
009affcc-2cb0-430a-ac18-0b59bad69fe7	7	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c8386a23-5524-4a2e-a75b-59126f039abd	8	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7545799d-54a7-435a-b839-2ae32dd26352	9	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ff58a037-bb95-4114-b35b-3d7a63e51904	10	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
79716d39-3547-4c81-b241-59b77b4b705e	11	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8c9a2cf4-bf74-49f4-80cd-6c504556b81c	12	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
71b0272e-39f1-4975-8362-135d85c3862d	13	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
061a46df-54f5-4ce0-8e61-0276ce644545	14	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f17aef74-860c-4597-af96-0df209cb4cad	15	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fbd7c108-eb43-4032-9fd7-cbda447f7ecf	16	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b0c90885-4a3d-4354-a4b8-af6d934a9d1e	17	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2a0096c6-856a-4b35-9104-d69153da048a	18	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7216d24b-68ea-4102-892c-23b92637c2a9	19	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
75f820a5-70c7-445e-98d1-4d372d204dba	20	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fb41afe8-2e13-4906-b6a4-2b8a46b97e61	21	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
74042c3b-b1ab-4297-9443-5aad1bed9591	22	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a00082ec-eedf-4cba-b196-4fe445c96fe4	23	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
25ac9c0f-857e-4a4e-96bd-a892b4310e22	24	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7e1186dd-4b72-461d-94d8-6a4ea16f78c5	25	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0cf5b7ce-3ebf-4a11-9a28-5c8d1bada441	26	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
46df0525-47e8-41d1-9476-74cc30b15113	27	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7087bc7c-dd32-4896-8d6b-276f55f68415	28	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d5ac5a5e-5aa6-474d-a9af-dd3d9878fdef	29	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
df149b07-dc74-4950-96b8-9e2fc2d8dddb	30	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
abe152c0-b983-411a-8ae0-3a75fd77fefd	31	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
cc22bd38-3c5c-48ac-b217-b5038584ec7e	32	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
cec75c91-b9bd-4df9-98fc-f342df605ea1	33	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8e183a41-3928-4a15-9ea7-00ed0d9243a7	34	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
879673ff-77a8-4765-b411-0ca4ca049b75	35	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
87ac444f-b407-41f1-a75f-fecde004fe01	36	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f9fb5eb6-3c44-4bb2-ad2d-c1d5bc2779d2	37	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
04a7694f-dba4-4f86-a41b-7a0f961f93fa	38	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
cc38c4f9-f624-4594-9d0f-459be01006a2	39	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bd3c354f-838d-4885-92bc-8d14b4463796	40	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
13775ad8-2f4e-4f94-894b-2219ac8644f0	41	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b98c49a3-16eb-4ca6-8a39-c0c8ea4fa5f9	42	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9c3c3158-e804-4dbc-b1b8-37f93b5f18de	43	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bc17ef35-28de-41bd-b166-609b6a03a54e	44	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
416b8678-ff80-430f-9880-73e0f9a71e63	45	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
eb5ef22c-f022-4f54-8b26-fa5cb8ad04da	46	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
1ddb0d39-4d2a-47f1-adc2-f7b634e5c5d3	47	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b18453e0-a73f-4b05-ae02-4cb50d69c02a	48	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
227d6248-1ae3-4504-97b5-cb1939f64e6e	49	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b0175fcb-d063-4c30-a189-1896c5ebb5d0	50	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
796508ed-3310-4c8d-a0c8-761ce087682c	51	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8b02ab91-80d9-41ad-8f32-a1fbdcc86b56	52	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b7463eb0-d18f-4f81-828d-428057230016	53	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6488c3a3-935e-4b27-bb3d-c50626e0f8fd	54	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fdefd6c2-da8f-4913-ae79-0f314832b53e	55	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
beed925e-47a9-42fe-944f-5db5018e7a57	56	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
df5d0c24-bbb4-43a5-843c-08717084f6ce	57	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f3b5ac9e-db78-4917-97ff-1cff47ce4055	58	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8ea182f1-eb56-4f40-ab3a-d906400f2c6a	59	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ab7c8b9b-f115-40b8-898e-5e0ad2a8a131	60	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
20b4c7ac-4920-4359-b6dd-40c312d4b327	61	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
87f3628d-08de-47f5-8fb2-1dfbad56b390	62	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d798ed3d-40db-4e3f-a409-60b3865deb92	63	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
46d69ecc-e142-4d0f-8cac-7f8946617eb6	64	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
48855eed-67f6-47a4-8311-44fe99a72d6c	65	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
70742440-8104-4006-b5f5-199c4013d3db	66	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fc6599e7-b791-4fea-ae38-a3a1e86c9d20	67	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
79026e64-384f-41b6-95b6-7e56b5622efc	68	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ccaa8b67-8617-42e8-80af-1ea2422bc36a	69	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
18c3eae3-2b18-4d10-b442-cf45e67de252	70	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d764b542-eac2-4efb-8519-85f19095f992	71	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
831c6fb8-d0e7-463a-a07e-29de0acdf9a2	72	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f63ee81c-e212-4b7a-9b71-843d2d2968c0	73	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0b091f34-3102-4ad8-88b3-6421d6d14379	74	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
47e30237-a8c7-490f-9ab7-6902284f7f0a	75	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7072de85-deb1-42d3-a17b-daea3873a6af	76	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d986d4f5-fa3d-431b-a552-9435a7fa217f	77	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2d6e05b5-7463-42a4-b7f9-12889f183839	78	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8cfb3883-8591-40ce-b9f7-fd9ac39857d8	79	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6bfdbe11-9311-4b56-aec4-7308104aad67	80	9bcac518-fe9b-4180-acd7-b0c06b04aaaf	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
23441988-75d7-4ffc-822b-4dc71c2befb9	1	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
231dba21-8a59-4f47-82c6-262fa540dfb9	2	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6eb1d147-81b8-410c-a662-c7afca198c38	3	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d98df031-3869-4944-80c9-023b2adced10	4	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
92fce67a-adcc-4cd4-a7f2-c29b263e0538	5	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6063570d-d1f0-488d-89d2-e763080586b4	6	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e3824542-fa7a-4763-903f-0b0f1c54d999	7	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b92470be-c06d-4024-9d76-1474259365d7	8	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d985afef-2fa8-42db-b5e0-df932924393f	9	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2da1a406-fc13-40b6-93b1-bf9134d1b3c3	10	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d295bbbe-ff34-4a4c-b726-3cee0b358a43	11	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
34c96987-93e3-4a8c-88ac-dbe9c0891c93	12	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
418a7247-34a4-4e49-abba-319c449ae914	13	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4a03c89e-1492-4c00-bab0-29f1072f4b9d	14	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bd92dc5c-e826-494e-98a7-1f6c6d1fac6a	15	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d3a7be8a-9acc-45db-bf0a-627f9e571d49	16	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
855fe22f-cc5f-4db6-8c34-33ae3f159796	17	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4e7e0bee-f0be-4499-bab5-a1e82102bb38	18	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
95cb68bc-cec9-407c-a094-be68b2bfe051	19	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fa1fec84-2e93-4893-bcab-a7136d8df84b	20	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9642354a-8f08-4261-862b-a5babf02972c	21	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
5ff38ec8-bcb1-4b42-bd58-8d0ec32bdd1f	22	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0ccecba7-8a86-442c-b4dc-1c01d391525d	23	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b38e66e1-dbbc-4cf3-81f4-3b366cabc532	24	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7b5542b4-dded-4201-83c5-9d2981f3e7ba	25	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
910220a3-5844-424a-b472-940564f8d299	26	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ddae4d59-045f-4839-86bc-0ecc8678c77f	27	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b066860e-48e5-41b4-acef-dd427ee87fa0	28	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
58389496-89e5-4fe9-8b2e-72696106e34f	29	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e41b2bfb-035c-47a1-a712-eae9cab366fe	30	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
de6a8d50-ed2f-4b1f-a0a2-b847787e51bc	31	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a7f0b93d-0c72-411b-9b86-917fa1516cf1	32	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2919c5a2-f803-4924-bfd8-f89d4be70101	33	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fbefbab0-b44c-4917-888a-7c8956c52913	34	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0f9e75ce-6878-4442-9829-2bf06ebded3e	35	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e33c4c73-a659-4a9e-9c52-e70c584fd113	36	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
14e3d7d0-6f9f-4ff7-bee5-d954e3a2bae7	37	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
34fe236c-5589-4894-9c9b-25ec695e993a	38	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
5d9fae56-3875-4e9b-b8a2-b3e9308e7109	39	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d6e65358-150c-441e-b605-ec7897b6afa2	40	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6fb1e88b-6ab1-4670-8167-dda06a4cec6f	41	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4df80a6b-3e23-49ad-b543-42d0501c686e	42	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
608c9a3f-1798-467a-b651-83631af248eb	43	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a8fe6f4b-2f1a-422f-9a12-b4dc3368a5e4	44	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3505d70a-6b68-4912-84ae-deb3073d078d	45	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
22292431-45f1-4ac4-9bc8-8a120e578b4a	46	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bed6252c-e6a7-407c-a165-cbce50e1840d	47	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
67b1711f-cb03-4ddc-b12e-6821c7894f08	48	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c7bfb82f-237d-47aa-adee-854da897c7a9	49	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
124e28d7-c02d-420d-b3c0-9e1d39e2c76c	50	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6c8a8579-5969-4ecc-89e8-b435606cdb99	51	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9971de10-2761-4e74-b4e4-60be2aa2c2c3	52	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
1bce8cb4-f3fa-4393-98db-05ca85ee1011	53	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ff2c0946-6b33-4c99-9623-4dde925883b1	54	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
22268b05-8d68-4823-9ffd-400997e87039	55	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
58318c67-5471-41d9-8498-c659baa1d593	56	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
42006661-5ed0-4dd3-9299-ac37e932255d	57	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
76d931f5-b1ab-420f-8c39-bb8075cce01d	58	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
35de5a02-0d24-493f-ae12-1cec288f6b9c	59	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7b68225e-c4fb-425b-b099-98917bcfdc5b	60	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e2f6f4dc-24c4-4a62-8c21-fb70da5d894a	61	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
39c29f49-3134-4cf4-900f-60d2c59c9ce5	62	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
23f0bf7a-be46-436f-8fb0-0e32d25cd325	63	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f0c2b9ec-d66c-49f8-bb80-c4f3e90c15fa	64	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
501e79e9-2e2e-44b7-b6b3-6441579667ae	65	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7484775e-663c-40b3-ae0d-e058ad46ac7d	66	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a8aa3c78-4855-4526-bca4-b700ed771d91	67	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
16ef50b7-46b9-4066-87c8-2af82ccd1305	68	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f7be7074-10ba-4ffe-929b-2abd4758548d	69	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
df3259e9-41b7-48fc-a055-f014878510a6	70	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8d8eba69-52d6-41aa-b76d-5173af7d07ba	71	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
dc726e10-2b7d-4d4e-b536-a8ec7244c606	72	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
991ef0fb-e83a-49cb-8c61-95edaad009fa	73	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b1af66c3-a3ee-4e62-aab7-18828b752cf1	74	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c6203cb2-5704-4b51-a3e4-c8721bc93430	75	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
49646f4d-7792-4c10-b161-c2a5dad24ac1	76	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
775c336f-b6bf-4023-8cb7-ee4f6c3ab976	77	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
66f7b7e4-4284-47ea-913f-b04aca2ef86f	78	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
507e4701-1eb7-4bc6-b697-78fe97b992e0	79	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9118b6b1-60cb-49a7-865b-882923baa19e	80	2139c543-7189-45f0-9b45-1f4f34e78cac	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e3cf473e-7249-472b-8872-e5dfb4813631	1	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
35db7f13-0a65-406f-b021-6e8f4838dfb9	2	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8ee8e431-0a01-48e0-8343-cc42d02be4d5	3	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
35581aae-7c16-430d-819b-6aca0f16be89	4	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8ffc457d-3876-4a68-a451-f27fdd4729d1	5	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bab1815f-4ad6-40c3-a12d-41422be346ae	6	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b735294c-91d6-4bda-9e70-b4e9e7e5b40d	7	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
dda0409e-9ee4-4dd0-b900-6f5c575a623a	8	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b48e6085-4435-4af2-bc2c-d84b04513a18	9	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d4c9aa03-1933-4f50-b034-99bb40e45fcb	10	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0050b208-5708-4baa-b281-cfc86fb34605	11	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ccc6c907-f290-4555-a71b-b3bb61c7d223	12	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
61264b6f-b3b7-4012-9469-fd787985b49a	13	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6aaf6bd0-d4fa-41e9-af5a-9415459bc735	14	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ed49c981-b380-41c5-8cb7-59d8208a4d44	15	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
898123b7-efa0-4623-be7f-911dcc29dd59	16	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
793a8109-84ca-4e6c-bfbb-4847415e4f81	17	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
49ef94ad-5ad3-4c8d-bd64-4e5861fab056	18	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
48eeebc0-cb49-4866-9d70-d76341366828	19	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3234ea84-50b1-40ff-af3a-a1d5ba4c79c6	20	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6c69a6aa-3075-435e-8a01-09c590eee619	21	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
dcbb71ef-635e-4574-88e2-8a437180d9bf	22	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
75fc05d9-5513-48c2-952b-be8c03e72a0d	23	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2540592d-616f-40f1-a745-bce987d9064c	24	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
009bb1d4-7d81-4227-9794-93234479d043	25	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
95d6ec25-39c3-49f8-b821-a0addfd602e2	26	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
cccbd971-97f2-489e-ac16-afa72501dd88	27	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
23c9ee81-7f78-4fd1-898a-23dc1e0a6226	28	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6900797d-e03c-4870-9be7-045a47cbf9f0	29	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
24bc12c6-f19a-4963-87d8-910ed4a91167	30	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
25001a21-b13d-4098-9357-cd2dba2fe953	31	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9c1229ff-1ead-4967-99a9-1934f4d17cb7	32	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ac421e47-22d5-45df-aeaf-51ac7fd45383	33	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
58a4e80b-f840-46c8-aa48-dc9b2afab383	34	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
296b1587-0af8-4733-94b4-1c55f955fc5a	35	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
81f7b246-9fea-49f8-8239-9896fe0ddfce	36	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fb228671-b055-40ab-a4c8-b4e61aaded4c	37	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
02cbfa39-5ee1-46af-bc37-c2ad37caaf19	38	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
af662564-1ec1-4f64-a2c8-ae557e835bb5	39	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3e04f4ac-4005-4912-a0ab-06f7d850d6ec	40	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
14b986c0-0295-4ab5-86e6-6fa1d20f5afb	41	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
225882a3-7d1b-43ff-8fb1-f5f533da65c1	42	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
baf49f27-d6ee-47b7-be8e-10bcbcbab0cd	43	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
03837599-4dac-458b-98f3-dd397926f349	44	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
33ed5190-66b3-49be-99b2-3e205ea02147	45	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
978bcbee-d17d-4c41-bf0b-849d8c513133	46	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0651a817-760e-495f-8cf8-eb6d930eb8cb	47	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a2e7aebb-072d-4fa6-bcba-685252bd0feb	48	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
98636b7d-e987-4b7f-9b9e-31ee9a661d08	49	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
18be1d55-ffd0-498a-8d04-1d3e18f68369	50	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3e6c1fed-598b-4b41-ab72-4605995a48b2	51	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
5f35b8be-b5c9-4346-b864-d79ccf5e1781	52	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b6700d60-95e8-40c4-b877-38e88f49ead3	53	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
abacba0f-e639-45e3-9b08-159c3adf2943	54	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2dc45a33-95f3-47c3-93dd-ab596e89eb19	55	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d328c987-ba9c-43c9-95c6-bf6ddf579e71	56	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bcd668eb-4a36-4d2f-a528-4622527c9a27	57	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8ba1ef82-9173-4d7d-811d-c5578bc77223	58	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6b6fee5a-4a52-4876-9453-95ea188db873	59	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3b7fa7d4-792a-4c6e-b2b2-46a21a66770b	60	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9f5dff0c-7b4a-43a8-a752-a1b87c419f77	61	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f68c35da-e6cb-4d6f-8f81-95bc67aa0478	62	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c8ab01b7-3297-4dd5-a41d-0b15a480940b	63	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9c5e9c74-881e-456b-8ce8-fb1bff17ba58	64	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8e32e5cc-45de-4995-b747-e094fbeffefc	65	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
920b0bce-fa74-47f8-86fe-5bb8c7ac8125	66	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bd4fc139-400a-4601-a8ef-e47023ae8aa4	67	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e1ff96f9-9878-403a-9094-3ac9559066a1	68	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
55969b4d-4175-42a9-9d17-c616b5e16981	69	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
075a3e6f-b245-4001-93dc-4f5bbcf4fa26	70	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b9bf17ab-64c3-482d-8b5d-fddece8444a2	71	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
cd34341f-4b1f-45e5-86c9-5dd082401fa0	72	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
233a21b1-d649-48ec-a7d6-b0c36a56f359	74	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
29617cf6-8da6-4bea-b2e2-5d159560905e	75	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7fb36bbf-7f7c-4852-8c9a-8bc6a240b951	76	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ff9e5ce8-37e3-4450-8d85-f115325e8668	77	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fbadab28-bc45-42c5-9209-bd8264c181e4	78	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
881a9701-5d85-4b66-8065-5fc8a7dc99c0	79	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
27dca5ae-e65b-4343-853a-a7ea26a4b85c	80	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e9f57bcd-f91e-4064-ac41-e1c93186d23f	1	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3912ff70-ecc1-4b55-80ed-b5448e2075e1	2	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2945abb6-c331-460a-93ca-28cf2babb38e	3	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f1c99b94-b313-4018-bae7-6ae42d581c6a	4	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8171231d-76b8-4d97-a176-d08909cd43dd	5	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bdd5379c-3ce4-4496-ba31-28ccc1f81623	6	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9588446f-f7ab-4c6d-9d2f-ff547294c50b	7	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c27f19b0-b3ee-4bab-be04-3e0630a0e920	8	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
df93dee4-f462-4c7c-9423-ab1389406c7d	9	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d0a60a49-7110-49e5-99df-e813da03e351	10	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
33a463ee-f92b-4ced-a32e-9cd190568f89	11	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
10dcddd9-22ed-4444-896f-fa2832421ed1	12	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
df4cb23f-4481-49ff-a8b0-6f21fd952783	13	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
39151753-5c36-42d2-94cc-88cd2b597426	14	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b5c49a5b-05bd-485e-ad6e-d8460a40330f	15	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b834ab6b-a057-4b3e-b682-ab98a6efa66e	16	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3ce8c6d7-2287-423a-91a7-5fddc7ae3062	17	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bedd29c0-c216-46bb-8ea4-63734198a1d7	18	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
75974958-ec12-4611-bdbd-cfb97d7e85f2	19	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ea845364-aa40-4e63-80fa-dda2964d2dca	20	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
52be7639-b93b-4ed6-b920-e83b92e5ea13	21	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e81c7a32-96b6-427d-8443-faeeb2691514	22	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
af4a997d-64f7-4aa2-b78c-f96073ac9407	23	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
871f9bb5-7d1d-4a2f-9291-5a112ef8e07f	24	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
105cf473-643c-4c14-a047-554e10474e8b	25	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
30bcd413-78f8-4210-949a-02f1c1f7f053	26	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6bf0027b-bcea-43a9-a602-02acd5694c90	27	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6fc66273-fc23-4c07-945b-cb42fd8c08fe	28	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e4026f70-15f4-4791-b407-f2915661634e	29	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bdb0bb23-9597-4fe1-af6d-a8b505deecdf	30	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
40a88cdd-ad64-48e7-b15b-11f6e1d3e981	31	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0fe132e0-220d-45c2-b6c8-8644b6ac7ea1	32	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
42f22cb1-6867-4224-8b8b-07defe0b08cb	33	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7acaf815-0fe2-4070-8d78-d9dca067db2b	34	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a42654ce-1550-4599-b3b9-8ca7d9fde8ef	35	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7baa3e4c-f28f-439c-aa0b-0e2972942174	36	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ecb76e43-3927-4a9d-a9d8-a1f37b7e0525	37	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ae2f7e49-83a6-41ab-825d-471cd4fe4e35	38	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a3e14fe5-2314-4048-83cf-2b0880606dfe	39	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2c8f7ec2-03c4-403d-ae7d-726aaaaeb41a	40	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ce58f099-229b-4788-9faa-a602ef96bc20	41	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
80f23483-77fe-4b4e-9b3d-9d1043f57e5a	42	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4cfd0b23-d891-48d9-ac71-8b4f77a6e9a0	43	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ad27fb9a-8c70-4510-8e0b-4238a397af2d	44	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a2445f22-bfe5-4cce-bdd1-0e7907a4fce8	45	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d9d6af02-88ee-4814-8f76-f933ef5bae27	46	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
163dc4be-4ce9-44e8-8dec-43c743649f02	47	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4558eaf7-b717-4f3a-82f4-b49965de0b29	48	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c182645f-3a15-4802-810c-f77e928a96f1	49	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a60906c5-f342-4c62-9a66-639f31bee434	50	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
749da366-d13e-498d-ad2c-13777d44e8cb	51	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c4230e7c-e978-407e-b931-671811c57f08	52	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0a9c02d7-cce0-4a68-986a-1bbba7980c14	53	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
25f6bade-3332-4d20-833d-d91a47dba9fd	54	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d17cdcce-7253-45f1-8a9d-d92b7a7a7f73	55	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
682bde79-ea85-4ca6-b0de-7f9941dfe3e6	56	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e4890716-76c2-4910-b5a9-b2828873bf86	57	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
82f11129-5b6c-4280-a0a5-a1aad6894b20	58	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
5613a3c1-4dab-44a3-9e93-f7cde62b8c55	59	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ea6794a5-2229-465a-95bb-cf81badc646b	60	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
31b6de27-bb7d-45c0-9a00-b09c4f144e76	61	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2058304c-98dd-4c6c-9351-6007a8facb87	62	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c6aad564-a413-4353-bc5e-0d907897bf72	63	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d3eb2d71-5036-41fb-9710-78037b75ebb8	64	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c36fe681-79bf-4aed-800e-50ad997df9df	65	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
493bd1ad-980e-4d69-ac04-f05585c00c9b	66	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c5d84e39-2a6a-45d6-b313-0d4792277a3f	67	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a9ab0226-b1f6-4597-8a31-c88375caea7f	68	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ef09f149-cbb4-47b2-8c8e-8a822b2b30bf	69	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8c688ea5-95d2-42ac-9e2d-e6588ee829bd	70	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
00db425a-023a-4154-b4bb-1167a1a804f3	71	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a40e3a75-4abe-49a3-8bf1-fc4f8b6868e6	73	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b8b896fb-a3b7-4156-875a-49818b699857	74	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2bd3039f-6c29-437a-9491-94f9f91ce756	75	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
baaa6ef0-a35f-4b6e-bf54-5b25f45851e9	76	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
292fbc26-8689-42d0-8f92-3d71014c7290	77	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0c111fe1-9875-466d-af17-3a9c0ed004a2	78	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
997ecc61-0a20-42b8-9dc6-876b4862884c	79	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f3860bd8-67f4-480e-a9ac-6279e15f6d43	80	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
bc4e6426-abe1-4936-9ea3-6a46b73afa25	1	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
02558234-5d10-4902-95b3-5cc2196ff9e4	2	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
76806b71-064b-40da-9e84-80ef0a37638b	3	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6d86d754-d0df-47ae-9c86-fc8d19256df3	4	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ac3bd9fe-dd23-4609-991c-65da73e7e150	5	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
29804c51-fdcb-4501-99fb-1ee5eb565c99	6	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
33d1ebe8-ce15-4751-b50a-ab9c07e915d2	7	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f66d4215-2190-4c76-a193-788ce462c541	8	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
54d12404-f568-4b5d-803a-3d8000ba7de4	9	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f72f845b-52e7-40b6-b696-aaf8fa8cc2c6	10	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e111477b-819a-45ca-b118-dc6fc490030e	11	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
773b74fa-f975-438a-a86e-47f435942f37	12	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0f46ec20-d6c5-4960-96f9-fdf7d1ac7801	13	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6f367e28-c2af-461b-ab1b-d808fbf4e2e4	14	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7f682abf-bb09-453a-8162-432348102599	15	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
d32b2184-238e-401e-ae80-372d2e5090a8	16	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
6ec81311-6c06-4198-adb8-abe914bba2aa	17	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ddc87aec-4749-430c-955e-4d6009a1bf35	18	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
1374ee21-6e0a-4df9-af81-778f17637901	19	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
13ffd289-9636-4d59-9a77-7796be788d63	20	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
07bc6926-1609-4442-862a-64876a47a813	21	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
db8f0bf8-2609-4f58-b45f-7fb3c221acd5	22	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3c62d5ec-2174-4fcc-9b49-01018610a5c8	23	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
5e132b1e-b21b-4982-a518-1ff22d562271	24	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
efac554f-2871-4ddb-b903-a5344a122495	25	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f0e39d8f-30d2-478b-97de-eea6c0506e44	26	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
23456fa3-97a8-4d63-a3f9-047571633f21	27	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
cb0cc56e-7d72-4e92-a6e9-5c1366a52ed3	28	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
414dd4c8-0884-4532-afee-a721018075d4	29	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
42b2d085-0b7f-4ec2-a310-ce1a34e81697	30	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
16644857-a887-4294-a5fe-eef3498724dd	31	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
46b2692b-fa9c-4f15-a4b0-2b436b7e87b9	32	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b9c8bca9-7fac-4171-8db7-030f756140be	33	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
cff591ae-6b7c-4f32-8bbd-8eaa227e218b	35	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
3562d968-f913-448d-b994-dc03d7f53a9f	36	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
287418c3-5c78-496e-aa80-bf0c17420ce1	37	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9e3bf3a9-3485-4cb3-bc33-8041fc4d407b	38	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
93839d69-8ec5-43d7-825c-af56bac9cc0d	39	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9d4d8fa7-f6d7-4436-8043-6a7659fd3295	40	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
9c12b58b-c7e8-4dc2-a36c-d5c35f099ec2	41	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
23061bf8-8d4f-4045-a6f5-ab9289cd91ad	42	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ccae6da9-5109-4838-8c96-faae0450cb21	43	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fdf587a1-b344-4626-b9be-70228c5d2822	44	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
b724962e-28d9-4fac-a950-ac41fe2b2e6c	45	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
cfd61739-1cfd-41d2-b4fe-2fa7f9db7913	46	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
8d3b776d-72ff-4038-aa2d-d9d5b184ac6d	47	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
43a64b3d-31b6-4548-9955-1c6524d13fa3	48	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e415c758-8c23-408d-9378-00a13757dec2	49	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4bad6ad8-8947-4c9c-b794-6cd1f351a6d3	50	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
4df0cda6-3a32-42a3-a501-a85fbb73dc7c	51	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
c539c452-0db9-48f4-982b-f7b4b42c80e5	52	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a9e05a28-6393-4d1d-9a01-7f0508e026e1	53	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
409da3a3-9537-48e2-b626-f4e76f67238f	54	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
06a2b7e0-9193-4c22-95d0-71923dce5f57	55	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
35b6e5d2-2d00-4b99-b7e4-776ae4bf9732	56	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
38ea0a68-02ff-404b-85b7-798eac16b15f	57	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a881542b-edf2-44ea-964b-30ebca20142f	58	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
f2f5c769-9895-4b11-8b05-953d862b705b	59	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
241f7591-0ac2-4f85-9f0c-9c53070cd2d5	60	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
59e14907-30f9-4f04-9f95-427b22ff8b62	61	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
434c9b77-afd0-41dc-82d8-e8c80d748009	62	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e146156d-2339-4849-b6ff-884e8c205518	63	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
83418fa4-783b-4d09-aaaf-4eef9f788edc	64	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
840630d6-7090-4e14-84c0-79b47ebbbc79	65	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
429b3c37-ee10-4e2e-a619-cfe95d89f16a	66	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
19207ed9-b6a3-4e5a-b483-a33e694cca66	67	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e59564bd-d042-45d7-8077-fbc99032de51	68	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
cd8a0d9f-259f-40c8-b75f-b219304edccf	69	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
7a1b7213-7783-404a-9362-4f3b69bc71b5	70	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
2b0e3a0e-3703-40f1-aaf8-96c26f6d3561	71	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fc23349d-5afa-4914-887d-448cf95a6d32	72	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
0072d523-79f2-412b-b827-9fe11748239c	73	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
a3367302-b803-43ff-a497-578851c15de6	74	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
e3644bef-3a7d-4f4f-8b1f-d4461f4ee886	75	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
aa9df688-abdb-416d-b426-ebe8703161fa	76	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
fc5978cc-5c04-4459-af59-2aa95bba5716	77	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
dd7db8b0-0066-419d-8894-365167d41ac9	78	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
ef9a77c9-246d-494d-b340-fc4efd2cadc1	79	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
cae5b8dc-d34b-4d2a-aa11-7ec4b07bf9ae	80	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	f
35bf65f1-6738-4841-98aa-1d5cc107e04f	1	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
31f1369e-48b4-46d5-b38e-b9247859be28	2	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3de0f902-cb95-4ab5-9679-e884cbb98c93	3	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4a7b71b0-d617-41e9-a266-ea6d2fa8e60a	4	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
77cdd7f2-a668-4247-a85f-255e4bbdd141	5	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5a820e7d-42aa-4d3b-bb99-79585230b67c	6	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
7052eef4-b72c-4a1b-9d51-49b73fca264e	7	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
85a82c57-5d02-4221-868d-069b60307c90	8	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f43853ac-604c-475b-aa8f-8df5307c75b8	9	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9d46429b-c997-436f-acde-0bab3009b317	10	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4d58b2f1-21d9-4206-b6b1-568ce7efb03e	11	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
378549b9-97ac-4533-9880-74ec5e284d72	12	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0ae8496e-d33a-4f36-8c57-f2fcdb4e300d	13	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
05ed8c07-a40f-450e-b7a5-d7b9bf8da16d	14	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
6453a879-0e03-4c86-82b4-69f266ef017c	15	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
17a0d4d5-7f5f-4b9a-976b-9e916314a2f3	16	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
55a5ae41-37ea-4649-b6ac-312a78491274	17	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f5aa52af-7f8e-4341-b81f-61a1c4c55320	18	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
60b3b2e2-f9c1-4974-b060-a8443a158a42	19	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
60a26b7c-0485-43ca-92e9-bcc149b4d85f	20	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b327d380-2e13-4394-8d89-60e046cc804b	21	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
79dbae34-6d05-4577-bd0b-d3bb3b9a98c7	22	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5e71e269-4bd5-4fd2-9e99-ad89f93e1236	23	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0378350b-f0fd-479a-bcfc-d57faf0167aa	24	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c1f0428b-1352-4899-ac4a-132ea49149e8	25	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d1aa7ebe-359f-4d30-8fdc-f026b55c94fa	26	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e5f624d6-b6b5-445c-90e6-7f431ba605fd	27	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2895557f-efe7-4453-9dc9-d849e725da94	28	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
da1c3890-b51d-4efb-ac59-becb0afebf57	29	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
333dd2f3-37ef-4dc9-b24d-7ec6d702178e	30	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
47a473ad-792a-49de-930f-e94cd6b10a88	31	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
6f93e1da-a69c-4408-80c5-cec76baa5f65	32	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d30b63c3-b350-4e31-9e5a-05055bc60be7	33	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1addc206-28aa-40fa-b7a9-04baef4b1b41	34	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
859fe9b8-4ca7-433f-a921-2e66d2c89fd5	35	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d7baf303-31b6-4631-b66d-f2032ade911c	36	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2ba3d7d2-e1d6-4138-b3f6-3f2fd349466c	37	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
fa429279-a8e9-4309-8a00-d697165600c3	38	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
79efc13c-0c95-467c-9671-3e50c3677fc6	39	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
72523e2e-8cb7-4b77-970d-3588b5a9930b	40	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
37ffe3c8-fd03-4561-828e-cc8e428901a2	41	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
dc943c6a-9630-4d03-8c20-f807a2ce5341	42	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
79e8aff0-eddb-47dd-a053-ab0fb6238183	43	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2a2f12b0-002f-4764-8cca-7ebacb950110	44	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
902c58bf-1d3a-4f62-8313-dee8db0c2bc6	45	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
aa2f75ee-3aa8-4b25-ab9d-f29fa78bf737	46	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
caf77a4b-70c2-432d-a298-61ab1ad539b9	47	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
7ffa37ee-04ba-4e3b-a5a8-41d6b3fd5c32	48	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
788c43f4-e5ef-4c21-88b7-d7f8b66f6f93	49	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
37827674-b287-468e-8125-b15d0bf69819	50	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e21bd42b-f8ed-4a49-9e02-2d0ed5843d6a	51	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
30c40867-4d5d-4961-9edc-10490a8e4e18	52	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c8ac9727-86fd-4f47-9c92-35e8b7abc857	53	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f6e10039-f5f3-4ba0-8855-72b095517bf2	54	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
da50e625-f2c9-4d2f-a090-910426419e06	55	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
22c568f9-46d3-4f16-ad92-b66029ab319a	56	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
fc3b0590-1884-4b32-b87b-534e4cc4e971	57	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e235c8ec-dafa-4013-b23a-67d2059b08d6	58	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
99e3172d-ca1c-4310-a40c-80b4d32b4c81	59	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b112cc2a-533e-4d3c-8127-144c821f7b64	60	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c0e8e9eb-9cc6-499a-9548-3f6857540fe9	61	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
887cb6ac-14cf-4a12-9e30-b2bad6f0060e	62	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8e8d62bb-a371-4633-a673-ee4af9b09873	63	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b81f3a6a-2228-4c5e-8213-194a05871b26	64	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
73def96e-a35d-49dd-a23e-9f1d0b1cb908	65	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ccece32a-567f-401d-9ccc-eefc83dab3c3	66	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
cd2e7429-9f6f-4c20-b3c0-29d20ff32b7f	67	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9bab3b0d-9e03-4e92-abfd-fa4a85d72403	68	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
201d83dd-2df2-4e4f-85dc-4b5a3823578a	69	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5b01b536-0098-4d4f-ba6e-d2738a2e19a2	70	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
12b376b3-c301-40f7-8bba-b6957576dc74	71	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
71c05ae2-c26a-4c03-a44e-88c943f57eee	72	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5617ea9b-ccfa-462b-a2ff-b37b0763589e	73	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b5b54966-be69-47b1-91b1-b37c7e6d4c0a	74	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9dc13c93-d313-4c29-81ee-89717996baa5	75	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3e3adaa4-7928-4ff7-9f0a-ca605aaa86bc	76	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d53c916d-5990-4cf8-b242-16ef793620ca	77	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
85b23891-d390-46aa-9e43-a2253fcfe79b	78	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ee9adaf4-91ea-48b4-8faa-29bfb367769c	79	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c442f4cd-218c-47cf-a398-72b9fb050844	80	8778395b-f9a3-4af1-8b56-6fa7159c0aec	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
88b1fe32-81fd-4397-8fea-768afaeba636	1	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
10a914e4-ca45-4916-988b-d28dc5813562	2	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
cb1833f0-141f-4afc-b3e3-3491733782ee	3	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e2ceb1c2-53f3-415f-93e8-2dc46a45cc1c	4	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
adf75557-fc99-44f7-ba65-1136a5554fbd	5	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d30f1b0c-f64c-4972-bddb-9281e0584576	6	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
041bcc31-8595-4572-94c8-332664f45989	7	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
cd1bebee-6976-4f7b-a57e-1d274cd8c8b1	8	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
fc047560-76de-439b-ae81-9d172f4972f3	9	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9d19e69c-04c0-46f4-84c7-649af6dc91f7	10	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
26c23280-769a-45b9-96e7-f3383be473b6	11	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f6b25ee8-73ac-4ee6-93d5-e12c7171f472	12	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
80857729-82d9-4293-b1d0-a6acf882d4c7	13	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
46708490-d9d4-42cf-bfdc-d08c9f6fb917	14	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
18dcb9e6-79d8-4b5f-be59-eec40c6f8628	15	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
550cc5fa-9a01-4e43-81d1-28f295aa7a4a	16	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ed04fdf1-8009-4969-adba-a4701bbefe3f	17	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f2d84201-9027-4efb-bc13-5aec9d1c9f33	18	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2888de2c-af73-413a-aa2e-f83135543b34	19	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b9cc6222-59c4-41f6-ad05-c5a31b0ece57	20	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
01cd09ad-8ab7-4373-81ad-6df7810efcf5	21	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
56302f6c-4de5-4d60-a3e5-414f6a7961ab	22	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3f0c36b0-b7ea-4191-8eb7-d13bd56ed151	23	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
588a656b-1136-4a87-bb2b-a64cde7025e1	24	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f25f7a5e-eb55-4686-ba4d-f9ad40902fdf	25	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e0f21343-96ec-474e-b643-382e67ecab39	26	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
dea85faf-568f-46c7-9ea1-e2adf76507d2	27	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d80a3162-80ad-4340-ab53-610d25cab75d	28	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
dd5478e4-d737-4c5e-bc96-38862e6be44c	29	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
dc73f693-c9f1-4ee9-86f8-a102ec24ef8d	30	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ebcab4af-1f36-41ec-b1e0-b810983aa13d	31	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c9b8bd46-78b7-47ee-aa9a-1e85f4718372	32	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
26a1eb8a-ece5-46af-a59c-8274c4d6a521	33	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
60ec3e9e-5b12-426f-bb06-eb21bb9d088f	34	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0cf917ed-2dde-4e8f-89c8-d2e555132dbb	35	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
463b8383-9da0-42ca-b1c0-69550e3ec797	36	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
70d26aa2-2ca6-4c04-b529-804c344f810a	37	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2e726e31-f465-4edd-ac16-e9376561fea9	38	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
74e759a3-e3e9-4037-880e-6d889f5c562f	39	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9d110cd0-4cd2-4416-a0f8-f19478f097f5	40	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
dcea2182-f6c1-4bb7-8087-1e3507c3a35b	41	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
86905b8a-7637-4fef-b907-d11f367c03f2	42	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
97f732c2-6c94-40fe-be3b-9284b8e81092	43	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3a59697b-9a12-4411-a3cc-a0c2b9e2790e	44	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
68610e61-c4d3-4081-b707-48f882a3e5db	45	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e236f81f-63d7-4e24-a2a2-efccd4e2a5ae	46	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3cc6f85a-5d3b-42eb-8798-de7d59b456b3	47	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
7b3d39b6-a6dc-4005-9619-2030aff1a8ce	48	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9fbbea96-ba23-4fa5-86c8-02fa5f22cc55	49	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
bf778f1c-9789-4032-8b8e-ee8be140dab6	50	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a7236ae2-b951-429e-9ce0-a4bf9a1799ea	51	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ec835c20-7c89-4a11-858c-54613bdc8706	52	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
11f17c43-6fcd-433e-b8c1-c15ff41b4a28	53	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f8d5af3d-4946-43c4-8886-15f318838d30	54	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
79fdfd18-d8c8-4cf2-8b27-d6048edf900d	55	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
efbf321e-34e6-4558-ad0a-20844f746f63	56	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c1568792-e71b-4c08-98f1-8cc777818869	57	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
89c85dc7-8954-4133-8e63-7f282196c46e	58	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
305aa4ce-3bec-46c8-aa71-8eee319317d4	59	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8745a75f-2a60-4f31-a464-56ca133502ba	60	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
cec88f7e-338b-4b78-9e1c-3280b51240c2	61	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c34380fe-c36a-4f81-bfa6-fae40c2bbecc	62	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
82a76e17-1190-41f1-a10e-5164fb0a4302	63	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4fbf9c0f-7d42-4c5c-8d25-8b3798cf2e31	64	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3693d063-670f-4223-bbc1-e3a30d7144d1	65	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
cef9c39b-709e-47b7-b44d-d5b11973c069	66	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b41ef592-7232-492e-9aaa-cab9c873db52	67	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8afd0932-ec7e-4daa-bcfa-df5d89846d5a	68	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
bf504248-3376-47aa-b59b-581c17bf4d94	69	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0f0e9771-2761-4937-887e-59bf4e59d35f	70	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e769eae6-af9b-475a-b972-be5000945994	71	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
427f4c0d-e7ad-43f3-b355-387aecd34d49	72	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d5d74663-68c1-4a89-b248-ef0aea8232f9	73	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
88b0d8aa-b802-4189-9193-3e18f82bd126	74	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
7e0bd918-a306-45d2-814e-10517f3bd497	75	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a1e0798e-6448-4c08-be9a-43400246d6cf	76	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
422b9ade-85b0-4d22-91dd-185ef3641728	77	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1591a759-2edd-48fd-a0b5-cd7250ea29ce	78	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5ad9c73e-84e6-4b9a-b767-8fd6455ed10c	79	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
88735bad-9ee7-4429-a057-2de2cf33b7e5	80	66ece208-bfe3-4061-bbd0-d41820080945	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8a22488d-270d-49d0-a49a-e8d4e5020087	1	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
19abd582-2d98-4b59-b5a9-664ae1852b72	2	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f9dc7d3f-41d3-4b72-89d4-16174889cc59	3	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a915c0e9-3da1-4a69-a7b4-7a431bedc9be	4	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
65f2fdcf-07d1-4e9a-aab5-2b3f101056b3	5	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
6933fe15-58dc-4a1a-9386-5a007f3c760b	6	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d2006a5d-c565-460e-a244-9526de6e9a52	7	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8954c8a5-510b-4aa3-b368-04099b575f70	8	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
de0227fc-2416-4b7d-86ed-fb2fd12209cd	9	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
6ba395dc-0af2-4b86-8bbe-739c3e032559	10	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
db61d314-0969-404b-8076-b0d6dbd32c2c	11	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
366c07e2-7380-46e2-b023-44a7683fe16a	12	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
52a6b4e1-6fcb-4e47-8ca6-369e065cbfd6	13	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9c00d1e1-ca63-4850-bb31-861d43054a04	14	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
51c5ba8d-54bb-46b9-b603-5318b1fc011b	15	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e9f4335b-7967-434c-88f8-58fda2c0bf34	16	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f1ad7acf-e205-468b-a998-2d4767180220	17	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a5f20ed7-ab96-43a2-bb5e-d22cfcbf1919	18	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2b2148e3-e7af-4ab2-a207-512e4e2bd1c5	19	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
66d27899-a082-4f01-ba4b-8f173839fef7	20	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a8cce5df-e901-4151-812d-d6e7cdbeb08d	21	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
31d085ce-c20c-4158-bec2-56387964fe56	22	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
46f24768-72da-4590-8e4a-81c68c55ee00	23	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
727365ed-6534-49b5-a7d7-58416b52b1eb	24	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
87a429e4-a1bb-4169-b865-67dffd492799	25	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8610e7bd-d2a3-46e3-be7e-e2f8c66a113b	26	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e87faf57-3178-4f7a-999e-8d39a2e00284	27	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
6f1ed503-2677-4c2c-a85d-9ecefa55dbf3	28	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
fdb4b4c2-1fc2-4c29-a5c6-1609fbf7dd7a	29	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3edc7ec1-2d87-4a47-ac49-34ca3591c548	30	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9ca34aa5-5c39-4998-9234-d4ffcd061701	31	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2cca6115-e140-4eea-9f92-4c73148fc91b	32	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ca971e34-8b26-4d42-93a9-ef93419a052e	33	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
7b755f46-9470-439d-8d24-2c938c2f7e5b	34	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
51250714-0053-4a37-91b2-db67fc1c4e60	35	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
25daf3d7-2531-4243-86a6-cc1391b81aa2	36	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
61cc226a-13a3-45db-8da2-80aebcb30dc5	37	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b698ddfa-d538-402c-92f1-b1383fce961d	38	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
025dc7b4-9ad4-494b-bae1-b2fb98e78b27	39	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
23108c9e-fadb-4399-bac4-176c46d5ed12	40	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b0314b0b-ddfc-404a-b8bb-7064ad3aecc3	41	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4c1ab757-2846-4fe9-a1b1-a20147feb531	42	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9065fb66-d42d-4346-b2cc-d29a677205da	43	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
29d34fc7-38ef-4cc1-8ee7-5b31ec9f4ed6	44	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4980418a-030a-40ba-8de9-11ccac914925	45	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
aad32174-10d0-4794-8a9b-eec440d158cd	46	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
96f145fe-9965-4abc-b667-7031b383de5a	47	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ae41257a-abd5-415b-962f-ff910dc46886	48	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5f8436d0-8411-4ad0-bd95-5eb3ea7bee46	49	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e7edc5b7-9307-45d1-aba0-f249046b500e	50	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8d5119b1-dd0a-49b5-b0ce-27478d46575d	51	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c90e1707-fe2c-4acd-b46c-4a345128a6a5	52	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f4f1abee-b66d-4e36-9ddb-7286de1373e2	53	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
af69e7db-3b2c-436c-8d4d-c816679cb437	54	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
369185ba-e144-4c68-a878-a4d56f5849c3	55	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8a777e8a-30f9-4e77-93b8-6e4eb0a000ab	56	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2ccd0d96-6923-4e8a-8a83-9d5cb20ee45f	57	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a1af7f8d-03b1-4a10-b647-d5d91af25bbf	58	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c051cd10-5548-4a2e-ba86-22d5c3bd0653	59	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
220154fe-b59b-4274-838b-f6fde2fe5d12	60	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
6c9357a0-0d8b-41d5-8f4e-55370c6ed929	61	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
786c8105-5b1c-46e3-acb7-67b8df84fc2a	62	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4c1d73f9-703e-4c14-9c16-5adcb8e62a22	63	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e7d85940-959f-49c2-a05a-4e18ae941179	64	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
81b9c712-40da-4899-989c-4c33bbee9b43	65	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e057a288-f4cb-4fa2-8e07-1c864477ab47	66	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d2eda500-6e87-482d-ab7c-b0dbb18689cf	67	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
24ec7b89-24f7-47b4-8018-fcd3ca5ab533	68	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a3a62cbd-a231-417c-b230-3fa861402fff	69	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a624ece5-196c-4f17-b3ca-41b3963622a9	70	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ddfc3a3c-4010-4a7f-b83f-b5b27a3470b9	71	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5f44124b-ce5a-41bc-9357-08da72624947	72	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
71bff642-b7de-4dcd-85c0-f3a9534d78ae	73	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8d470da8-259e-4611-a86d-227fb52715e6	74	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8c6a306b-bb60-4b9e-b6a4-344a78fdf6f1	75	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
51ed5401-5aaa-452c-a956-a6717caebbd6	76	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
bb13cd63-4c8e-4587-a8d7-7987f10ba450	77	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
fb3f76e7-6b35-4d17-a473-498df438805e	78	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e3a750d6-ae6d-40b6-b953-092c2becb76b	79	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
149096d7-572e-4535-aa8d-6ea5c75384b4	80	d25a52b0-e28f-49b9-a275-bbd41f62a822	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5d52e6f5-76a3-4e8d-a5f1-24c5fabfa285	1	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0ede8a75-9309-40aa-8ac7-005f1ae39bef	2	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2e317744-f6d7-4380-864f-af1bda631879	3	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
62754a7e-9c5a-439b-a85a-d745d62c414a	4	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3a220889-9322-4580-b018-42edaeb52962	5	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
bc2b4335-22ea-4f70-a187-d2a5f15c1490	6	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c3f5e1f2-0e3f-4453-bc1b-064b7c2749b8	7	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4cab81c9-13d5-4f07-bc8d-c637f315d36f	8	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
891f17f5-70ac-4d96-8f01-f7d465020a2b	9	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2daaa79e-e2bb-4420-b57f-7fdbfd890087	10	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3ac79bd0-69ac-4fec-a0ed-64ee20e80fa4	11	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
180ab854-8634-4a41-935e-77c3af6220ff	12	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
16807740-3884-405d-bdf4-cb9555917094	13	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
52a2f4d1-1c67-4bef-939a-de5c21799844	14	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0c228398-fe91-4260-ad14-ea499f5206ef	15	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
46079e17-37ba-42c2-bd21-d402e7ed7a9a	16	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
38dffd94-2d6d-4ee1-9856-0ac6f0c29c53	17	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1173cbb6-30bf-4288-866f-dba0c144dda1	18	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8b834db7-1fc0-4201-af99-2296d15cf3b8	19	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
892700c7-0266-4356-9c67-948970d2c9b0	20	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
822e2a56-acea-46bb-8d26-9fc4592fccf0	21	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
60d0928b-6b35-4263-b326-23b9c25241df	22	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c24b0ac3-9705-4df3-ae2c-89f9849eb868	23	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a99115fe-2cdd-4fcc-86dd-171652cbdcd1	24	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
57d60d70-f25f-4bd6-a84a-603f9062327f	25	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c8a60b48-2db9-4af2-8477-164543e9bb9b	26	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4485834d-24cd-40a5-a864-478ff8d922f6	27	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
fe93756a-7f25-41fb-8dab-2a131bac0f18	28	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
122eae02-70ce-437b-8619-db8b58de164d	29	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
751e9538-6c0e-427f-a4dd-22a1be725621	30	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d2fce9e5-1030-419c-bc0e-46bb7f129a87	31	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a805807f-bb92-4f78-b7b5-7434d35f1f64	32	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1027aa6c-389e-4609-878c-0995cc758a2d	33	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3dc2c9fe-5b21-4f5d-a19e-7f027454a10b	34	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d17d058d-974b-47b9-aa4b-b0d4ae8164fb	35	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
62e4c1f6-ee5f-4a35-bd19-f04fb148df13	36	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
cfbed55c-3ed6-49e9-8d95-9d8594576dfe	37	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5fce4963-1722-4742-8777-eb59e5419cb8	38	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2fde10fa-6282-4d52-8469-1465679410df	39	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
bf052a7e-c8d6-4ce4-8a2b-50c28aa6d9c6	40	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2b8dbaa6-6e32-4812-afbe-ba9bfcd3a74d	41	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
43cddcbf-2b18-479d-8254-df1cd2094cb4	42	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4a2a22bd-d19d-41de-9466-560ca254ca2e	43	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3fbef36b-b14f-4c1a-8702-d6c4ce2c6773	44	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
62c44344-2e42-49fb-8c9d-dd2ad46c7042	45	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
fec4e974-2911-4b7c-ae6c-e186c2f34bb4	46	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0cdd38bd-92ef-40af-ab1b-bc280e8cdf1c	47	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
aeddf28e-e548-47e1-bbee-22a7c6b19e52	48	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
390e368c-b97d-4a40-a5ff-92b41fc7e363	49	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a24037ad-e338-45bb-ac36-dde4038e10ef	50	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b0873d7a-65c8-49f6-930f-fb7ab5e2982d	51	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
14275489-6dc9-4395-8e4e-47c9e3021184	52	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b97f489a-f80d-4edf-aa6b-73e3ec9aa9ab	53	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
19115436-7b40-42d5-95c1-d79a6e5d2e21	54	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
abc638f6-930f-4802-8b70-01235d068e07	55	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e554dffe-1c41-4336-a518-0c59885b1403	56	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f576ce63-ca34-4669-88f3-f8f26ae0beb7	57	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
70ca6999-b62d-4f39-b39f-751841025cba	58	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f7f9ecec-7c2c-4396-b20f-4268a77318e6	59	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c80380ee-730c-412e-bcbe-17a760855b67	60	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
016851f1-a12f-41a9-81fa-732c18633bf3	61	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
bc73b8f0-dcd9-4021-8d09-402686de4f42	62	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e0fc568d-5ea9-4987-b1e7-ca560c490a3a	63	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
bca04297-7f29-4056-b446-54fa9ae07b7d	64	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
18b6fd65-8cfb-40b8-a09c-6170e8a4efcb	65	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0b5a471b-9a1d-4c4a-a505-1692b5141204	66	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3757ad09-ca12-44ef-8c7f-624aceb3d274	67	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f5b19075-3e1c-41ef-9753-c49196d3303b	68	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
84c066ca-92bd-4c2a-9591-80c573a5dc0e	69	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1da40d9c-985f-4025-a829-98f33eda6f18	70	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
29d9d174-c800-41a8-8ea2-c134159f88b8	71	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
43bd1c0a-166f-4dd9-ba5a-385a68840588	72	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f3846820-1936-486a-9180-69b0bcae3c44	73	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
68034205-7439-4026-b672-8cea1998e944	74	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
451a5a58-c34a-40fa-8978-85d0d60276e6	75	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c3339223-d761-4794-b014-1db2ab04ac0b	76	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4d8e9a03-1bd4-4203-bc6f-97fa18a7cc11	77	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ef6bdaf2-1f65-4fb5-98a7-0ee4801edc5c	78	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3e54ec71-fc02-42cc-b359-5e7a9f2d55c6	79	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
829ef703-6a31-4615-8d56-f70d001bcd03	80	7491cee2-3f31-4f39-992f-019bc8d5711e	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f0851f72-9522-4df4-9931-7312ef1a22fe	1	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
dbe3c66d-9834-4b18-b31f-a0118e693137	2	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
db193457-8a52-4d78-aa86-2ce3c51348b9	3	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b4af4135-4f1f-4159-8720-33d7462c560c	4	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
de2bd63c-3526-4bef-9edb-e9d1c4719b0d	5	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
89e00c58-1b8d-4526-8a89-4a4315be177d	6	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
fb551875-76df-4f91-8de2-1ed5c5d04dfc	7	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b58d5d46-6817-485d-8cc9-2fcb4f18aa40	8	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5424efe0-0a28-46af-941e-3b690f1475ce	9	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c5898946-2dad-4f49-962f-512df6ff26eb	10	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
7808a77b-800d-48b9-b39c-a0f13acf1b5f	11	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
7fc972dd-220d-4496-8fbb-fb36abc37e0d	12	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1f3c6d0c-20a3-435c-9e1b-0024784da9a4	13	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8e5eec98-3214-4358-aca0-2bbed0c8aad7	14	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ec1d0786-3a06-4788-aec0-e0d042b4db83	15	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
47c7f8b1-bce6-4773-92f3-846a3317956c	16	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
aa1408c7-bc96-4d38-bcac-f6c65a5b6857	17	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2e6a69e3-8f09-46d3-bafa-069c841cae20	18	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
31bc4a67-055b-4780-ba61-be861b29a609	19	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d17ba36a-3e78-4f29-b2e6-2e18c73c05d0	20	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5de1a288-49cc-4208-9b3a-43da4ba08cd8	21	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c3cadb4d-2945-4228-80f8-5ad6f25bbdca	22	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
cbbf72f1-8a7e-4451-b036-27a08572cec5	23	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c955b125-f34c-4c97-841e-986c185d1d7d	24	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
cbfb90d4-333e-40a2-b497-05a4ff7c5a8a	25	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
96bb3ea8-5903-4621-bcae-7c51d3455453	26	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
dba43a5a-2c1b-4013-afcf-165e9601e591	27	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c7d62c6d-d16b-4291-aff5-fab08959eed3	28	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
fc01ac8e-cd4f-4ccc-bd97-7394e49495ee	29	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
684c09fe-1b0f-4dee-9212-1ea2c8fcdef0	30	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0ab75fbf-3b61-4870-b1ba-3d2eb80a4d33	31	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
72f82ea6-dbd4-4e9b-8af0-5b859e15d9dd	32	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e09494e0-ea36-46c9-8038-954537511b2d	33	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
6777c4a4-33c8-42f9-8a41-4f96bc1ddcd9	34	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
cdeac2fb-0a81-4a54-847c-384fdb9498c8	35	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
016a7798-f254-426e-bc35-176a8c951b3e	36	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d79af11d-9257-49ba-8666-9f35f01149a2	37	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0b67362f-9a80-4958-933b-abb9f19343ea	38	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
521b6ff0-ef49-426b-a94d-43fcdf53ab71	39	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4fda9095-a10c-4b7d-a121-c3903e34e53f	40	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
65308a68-790d-4f14-a986-c8254a708307	41	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4563fe06-db49-487a-887f-52cdd5bed85a	42	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
85141102-d958-4978-b01a-b49591d30350	43	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f1075461-c95e-4037-a7eb-9e9d78d2e513	44	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
83fbf4ac-a243-457e-b5c7-ce03bea91767	45	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8a72188b-da77-40aa-871b-fe757d76f184	46	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
29c96c90-f4e5-4faa-97be-c4bd92ed8ce1	47	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
524cd7a9-bd18-4a63-b38a-04aa876e91cf	48	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3fb038fc-f2ad-4a6e-b0fd-aec4b24cee3e	49	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
6098c136-4d50-4118-9101-0633526cd02b	50	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1eba056c-431c-48e7-942b-712129b65915	51	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
6f324bbc-c720-4351-9b35-a502e94e7122	52	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9b994b22-5ff3-46f7-8973-80aa38a22c11	53	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
57c83818-1e99-4fae-b944-25ff1b66e82f	54	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a8761225-9243-4eb7-a854-a1a9a7e63399	55	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
33ba8a74-4f1c-430a-a396-5d3bd484addd	56	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8ec15cf0-a581-484a-b860-ecee93b23e36	57	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
caca12a6-28c2-4a3c-b4d7-5af8e4ea5588	58	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1b7227cd-730d-4fea-b2a4-412b7848e175	59	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a598eec7-1315-4326-8e13-cebdbb4818c4	60	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f47f04fe-7137-4a61-b21a-6440da6c72c2	61	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2438ef3b-6b4f-499c-a89e-4b972d1c6e6f	62	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f1d2cd96-f79c-40c4-b2c4-a2e9b40f194f	63	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d6208895-d65a-4558-a15a-9de5ad534d98	64	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
fb1623d8-3538-45a4-b4fb-4c1d317fe5f2	65	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
75bf6548-fb0e-4324-ae22-830092caa447	66	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
190801b7-52d7-4021-a856-35c9ef9256aa	67	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
02a21533-6a63-474a-8ce3-fcfd29674e73	68	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
31f0509f-0626-4ee1-9d5b-ccd22b8a1a1f	69	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
11aa54d0-fa09-4f57-932a-3439ee66df37	70	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d8428a7b-edcf-444a-a6af-76337e694f5a	71	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5a3f1507-e781-4861-bdae-d9941c80e0b6	72	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
543b7eba-5d0e-478d-bb38-7e30cecfac90	73	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2e58f22c-8c2d-423a-88bf-bc70a10ea3f5	74	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b2ab96fe-22c7-4a0a-93b1-5ab15af2db50	75	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c4d3225d-b29f-46ab-9c92-7b7d1208fb65	76	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c0f2281b-a6a4-4968-8bb1-a881b44b25ca	77	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
66a13f13-c32d-4c02-a737-a0687dd3c84f	78	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
05b25999-a2df-4ef9-a7ef-8cc1615c8729	79	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
27d75665-34be-4028-9dcd-b5974fe972d5	80	7188314b-3a8d-4240-b0c8-93272ee24a14	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f23198e2-7f23-42a3-be33-23b26e8d090a	1	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0cdb6706-f4cc-4700-941b-16ac0864fdff	2	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
bf75ba59-ac66-48bf-96ae-b6318f258450	3	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b9c83bd2-219a-430e-9e89-4bb77d9d5a7e	4	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4b8c265b-35e1-42e8-91f4-d354f001a532	5	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
917f11dd-b6f7-40c8-9ed5-3998959f1e6c	6	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0ee9bf77-9d70-4f13-8387-f3cbf9fa21c8	7	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e459dd5d-da0c-4d95-899d-59fe3ab47ccc	8	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
7c037d11-2ace-4fb4-9142-b5a47029a558	9	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
32501330-f5bd-4baf-914e-09965b54183c	10	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c7b3b998-8468-448f-b958-36f27877eab2	11	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
98d7625d-eeac-42b0-a316-552527ecd4f5	12	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d54fd876-8bfa-4684-baac-4fac68b01513	13	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
70b6dc0e-6336-469e-9e69-91dff70d1051	14	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
195abb92-148f-43bc-9c73-d8fe6b09225e	15	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
577a131d-b3a9-4d29-b701-ed9c2f42dcd5	16	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e3c35404-88cf-4b30-9d3e-8184119301e5	17	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
280aa3c4-dbed-4ede-9f7d-04751ffd01f6	18	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d1029e36-cb57-48b1-9c51-9acb0f65a7c3	19	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
865e7c63-e250-437e-9a19-3d23264b9cd2	20	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
838f2f99-d04f-4ef0-a79e-009f22997f5b	21	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b06518f8-79cd-4e6b-943d-ec98ab4db563	22	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1161badb-3b9a-475d-9c94-6e041318dcb7	23	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2d63c5a1-8c6d-4b2d-81e2-874c423ea346	24	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9798788e-54dd-442a-8a45-a54278997685	25	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5b3b1e24-ac71-4428-9b9e-855eee082061	26	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
67692551-4c08-450e-8d39-ec90e47e1bfa	27	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
011e08f4-6804-48f3-884e-5882bb4679e4	28	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
44e04a94-b2f1-47d2-895e-4b02a6eab02e	29	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
bcd4d199-4b39-4fc4-9af5-0abf98ee6fa1	30	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
455a6750-6513-43c4-8749-660d6daad451	31	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c631e2da-d633-4908-8967-fb22aa924f17	32	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8c91e4e1-8d51-4d4d-be2b-3f4a7715c9ff	33	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
63516b04-c5ff-4d5c-95e2-1cc043fe816a	34	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8aeceda6-8cf5-40b2-9f91-10f50e0665c0	35	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9249645a-e0cb-4f5f-9cb7-84fed4ff2f7e	36	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a070c8aa-a0e9-4e72-91c5-872e0b69fb7f	37	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0a554619-ba77-49e6-8c9b-ea7244e4f30d	38	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
94f01a84-3870-4ae6-9694-6f2bdf4c88f3	39	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c84c088e-cbe9-488b-8704-51876f2fcbff	40	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ad32479a-45c9-4d02-b3d3-2f436ebedabb	41	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4a5e3f56-f52e-4cf9-a823-ae1789be915a	42	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
010467f6-a554-4a82-88b8-96c184c24223	43	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2012804c-9f68-43c5-bc50-dc08d211e253	44	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ff63b59f-74d8-43e7-a215-cd082faaaefa	45	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ee5c6923-521c-4f48-879c-7f9328c9e5d6	46	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
493ff34e-0a18-46a5-822a-407c08d07f1d	47	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
026e8f5f-248b-472e-9d08-9e463b1d78b0	48	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2762efc3-4c02-45fb-9b3b-1658e55a28ff	49	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1735db14-b3d6-42e9-bb3a-4cddc5519af7	50	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
62ca5089-fd71-487a-8748-b02cf0f21033	51	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d8d1bedb-e8ea-4bbb-8c11-42017c5e0a72	52	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
6921c601-7d16-475e-bf61-dcfe7b4dd849	53	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d99b2b9f-30fb-4600-8cbf-1bee5e726e7d	54	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
30ad68d6-34bb-4eee-be7e-3d4b0e2de417	55	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b068361e-8ab9-4c6f-8ac1-39164aef4991	56	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c618b0d7-f2c9-4164-b155-8223fbd1dc0e	57	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0b197c59-1589-48a7-bb12-600e9e2d80c2	58	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
bf465eb4-7212-4a3f-9cfb-a90352d81bbe	59	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4fca3b60-0749-440f-b06b-e46529efaf0b	60	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
08a78b64-c569-4028-9d6f-e6aa6f37e21c	61	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f4576862-2687-4ab5-99cb-10c79c5ff92e	62	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2250d9e5-9e8d-44cb-973e-bc3782b646af	63	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
6804e1ab-b75d-475e-acc4-3c90c69df993	64	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2d9ecaba-3f6c-4994-9db8-830cbab33248	65	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
aaa2565b-e087-45b9-91e4-a3f291ac9d6c	66	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4609f5ca-e380-4c96-89f3-b11eae4984e3	67	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
df3d6b66-f02c-4ae4-9100-a5b775563b49	68	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
14aaa56d-171f-43f0-a2d7-d89ee9009bef	69	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
6eb65f90-1662-4a6d-aa51-b63e476f8eed	70	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
af7297bd-b301-4833-92cf-2fbd9cce1179	71	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
61ae61aa-d9b6-44e6-a5a8-ca8daba812aa	72	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9e01c3b7-c69d-4bcf-bddf-c556270576ab	73	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
db3b16f0-8a0d-432b-bfb5-f0537940c1e0	74	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
17af47ec-489c-4a9d-9b37-00a93f9a86da	75	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d9f04324-cb67-48e1-a438-009c9b84d24a	76	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
ec0070fd-00eb-4c7e-ba61-acd1ee3e42e7	77	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0f136562-40aa-47d8-9fc0-8068995b6ce7	78	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
14ba01a9-2b9b-4b90-a01a-4c056b43e9a2	79	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
024279f2-48d8-42a0-bf06-878589daf194	80	76050df5-95d3-46fc-a782-287fd8a2ca09	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
78c6bf74-a953-4ab7-bd34-039d0bcd7103	1	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b3df08a9-ef28-4378-9cf6-9805588d0a28	2	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0f8a3ddd-3974-46a1-9606-a6fd363885e8	3	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4593dd1d-bfac-4dc7-b213-1682defcb637	4	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b18bdef9-41cc-4786-828f-cc3b8d5e3eeb	5	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
352ff0ca-1819-4b87-870a-acca71115ff7	6	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0bb57d94-2cda-419f-82a9-876384eb00e5	7	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
02846bda-5108-4db2-aede-7a4021d92fa5	8	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
002d63dc-c67b-4a9f-9ce1-eb645b12d06b	9	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
5e28c119-a271-43f0-8435-c1714987062e	10	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9b2b368c-7db9-4cdd-9f58-03ae7ae1eefb	11	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9ffdcac2-030f-4f43-8095-7fce22b9f964	12	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4e11d394-95e7-4c06-81c8-a8822d53e02c	13	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1029c791-bb5e-442a-8ac4-2d9896397e25	14	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
858fa2ca-99ba-4db7-96da-360cbe2fc9aa	15	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
14151350-c2fb-4274-a4e8-7e7ff7e8f4ce	16	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
70b2ef64-8734-4a0e-a441-798a09ee8d5d	17	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
45243f01-46cf-46e8-b1ac-0e056ae387a2	18	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f3e077c8-6860-47a0-924d-2f580bf0628f	19	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1555c385-8741-4ad1-a965-9d08e0e49d90	20	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8eeefd8b-feb5-4ab3-ae75-ff681fad4961	21	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
16ce2949-8509-413d-94eb-4f52a04dffc4	22	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b5e7c4af-4f8c-437d-ad8f-73ed88165ba0	23	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d75d3d05-5bda-4b0c-b693-8df5c9ac69bc	24	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c089afcd-b76f-4993-975d-f49ab05f01c7	25	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f67edd2f-7646-484b-8c04-f16ce0c83a49	26	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
eba9edb7-86d0-43f4-8ca5-0b2cabb6f62d	27	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4166d167-43a4-45f6-9f66-2e45d0667b97	28	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
84b86a38-c204-4b81-84e7-e0baaa636001	29	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
75929cdf-7cd2-4c23-9c13-8d60e3e573fe	30	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f78b2439-f8f0-4490-9047-5bb1a3211789	31	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
7bfd3a2f-22d8-49f1-b493-1536882ea949	32	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a05b2cba-2c6a-4b63-b8ed-b33d99a0be9a	33	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
597be3b6-9bc8-48d0-b401-01e312f93ba6	34	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
9a8c949c-7061-4293-82ab-1994138b2889	35	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
14a4d548-b225-440d-975e-111c41c4607a	36	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
abdbf261-7d99-4c2e-acc0-9b8bfff15f4c	37	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
04d1d8b9-6fec-49e0-bfcc-3db85fc7d427	38	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2274ac4a-db77-4164-bf77-2caad5f57a5e	39	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
19602f3b-810b-4731-8511-d2d77831dc21	40	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3956fabb-ed17-4312-8a56-0cfce3a81794	41	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
b4254324-7aa3-4e96-ac78-82f271dfe0fa	42	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f082097e-a99b-4b07-b211-8029f038a825	43	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
84a756a2-e4bd-4ce8-aebb-de3cae0911d4	44	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f4e6b920-dbaa-447f-8aab-a4a90f042bb7	45	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
bb16d700-345f-462b-91d0-7dfb5593a605	46	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
0c3823cf-02af-4323-b0ff-55e16f8bf8d6	47	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
07e95e67-22f0-4ae6-ba33-ba607ace4ed5	48	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a8c04339-58a7-4e2b-aaa1-bfcf463225d6	49	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2dd78285-a1c1-4144-b11d-797d490932f9	50	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f01500ea-832c-48a3-9bb8-e2484061947b	51	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
583f62ab-0cf5-4d77-838c-1a1edd20aac0	52	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
4836b2bd-9a04-45a1-b56d-470c115e2eb6	53	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f4f9aa68-827e-4e2c-84c6-ea6a82095bb3	54	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
98dc8906-6711-450d-97d7-26e1063e862a	55	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
34509399-114f-48b4-8ad8-2c5b55e8ebbd	56	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
7d7d9f3d-82e9-4b68-8c4d-0607487ded19	57	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2ef1ccda-7852-4ee1-8f22-83a1fe4e6e96	58	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
00f0df47-fbef-4830-a45c-7312fc5d19f7	59	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
d7bd3ae6-801a-4886-84a4-03c6d4791925	60	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
31fec7ac-dab5-492d-97c6-aa321ae3fac6	61	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1f56c2f1-0740-4167-87f2-d6d8b242c3f3	62	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1cd38486-2a2a-48e1-896d-380b4bb80c90	63	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c93880a8-2dbe-4f64-bc7a-392c1f7ae7e0	64	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3cc3f66b-a598-4e55-a277-526a872b553c	65	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c8237871-7db9-470d-885f-0f7cd24c0791	66	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
35d879fe-6b50-4a2c-9484-efdeb8d22c1e	67	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
bd2a476d-7668-4a09-b442-096b3caeeccf	68	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1b2e54d5-4a2e-4cfc-92c9-048aad4dc2fa	69	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
61b2b196-622c-40e4-b9cc-14546282e752	70	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
2b99e6e7-86e8-41a4-9601-694edce9fa94	71	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
539b5159-e9d9-4cb2-9c50-e4960580f85f	72	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
e84215d4-e827-4813-983b-b74d0a4c6448	73	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
c9b4b413-89e3-4e49-a1a8-059fabc06158	74	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
1b9148b1-b4ad-4a1f-a4ff-a07c89039c31	75	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
8d529f4c-f3cd-4f81-bf23-651e06d4d759	76	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
a7698d60-1502-40ee-83fd-31ad526f66f3	77	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
f5232d8f-91d0-4565-b261-1098afb79c15	78	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
3a203b6f-fcb1-456d-9808-545f41369719	79	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
7ec955e6-3a5e-4d22-9f25-8568232145c5	80	7adbafff-8b29-4432-a521-c9de87d843a1	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	f
90dc0724-1091-4d2f-9e6e-380ad97f1885	2	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
e41ea57a-380a-4bfa-8483-2dcec2a2aa98	3	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
836d3cf2-32b6-4e29-b7d6-b150b0c8bbb0	4	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
132a3fd1-ce29-4a4f-8399-4e1858f50576	5	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
3e4aaccd-44f9-4bd5-9559-c4a69126fad3	6	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
6a630df8-dc55-4a66-8009-147948e361cf	7	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
8bacf28e-820c-4635-9930-50c3cc004367	8	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
f56d6741-b112-4a5a-b697-45df9f5b01b0	9	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
ec6814ba-be99-4bc5-bf36-b94c15842ef8	10	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
5c6cc547-2d5e-4ce1-9842-a54405166b4f	11	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
41ae21c6-a14d-445f-8599-47ffcc44d56e	12	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
3efd2c1e-98e3-4b17-82ff-1be1bf50e322	13	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
46ee36ac-dce2-40fc-988f-8fe92a698ea6	14	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
4fd5b550-0144-4218-ac91-2ca8d2f4e9cc	15	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
c0b6e2a4-7f04-4496-8694-b4fa6d878ad9	16	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
78f6b083-95f1-452d-becf-9b58f448b198	17	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
ef87285b-da90-4c7f-9a8f-fc2a6a3d394e	18	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
62c595cf-7fee-4741-b85c-58173b35a173	19	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
c50e4eb2-89f3-46d2-84d8-91516607de52	20	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
f45cb984-8362-413d-973e-4bba1ec9cddf	21	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
01e1411b-d154-464b-aa6b-4921b33798f0	22	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
3ad77e48-ea52-4752-86ff-dc2699c5aac9	23	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
7df52782-a709-4cdc-a7df-2774e6058292	24	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
d44ccfb5-a469-4a65-9567-044d0ef929a9	25	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
e5a59ab5-f77d-4d7c-a338-124672f3513c	26	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
6cccb1a1-6c2e-469f-83d7-7c2cd32d81fd	29	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
fc12dbbe-c9fc-47ec-904b-ce5c3edbe450	30	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
249e07e1-1d79-4ebe-bc97-f8018c11054b	31	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
bde2a3ed-db5c-4cff-848c-441005753f61	27	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-17 19:09:45.256	t
cf3fc45d-cda6-4e70-a773-fb9d7c6065d3	32	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
ec2a6f99-938b-424d-9c5a-c1a0c78459cf	33	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
33f151e6-cbe3-4518-8245-e8ebe6b6311a	34	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
9956d636-4ee9-42f0-8331-a8d2e5f14fca	35	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
08c85820-af70-4555-9904-d4eab6019686	36	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
2ca14681-20ff-4ce8-a40b-3cc3f9c948ce	37	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
c2117591-db6f-42fb-b2e4-28440798d7a8	38	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
58d376b8-6934-4456-83b5-2cb55e9e6274	39	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
1adcd45c-c136-4675-aca8-3aa340ade5dc	40	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
111b22a8-456d-412b-81cd-b295bc5086c2	41	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
10d13932-d002-4351-9ecd-022421ef0de7	42	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
e281a0f6-e71b-4dca-9f02-df9c332c4189	43	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
5cc9c59f-38d9-4fa2-8053-2f54f727202a	44	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
5ec0e871-8b7f-40d6-a664-94309a5e0bfb	45	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
9720235e-cf94-4f4a-902a-183575b6255e	46	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
3795b0a3-2c8e-4542-ab62-d21462701990	47	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
b0c703af-7340-49f4-bfbb-a0b5727eedb3	48	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
98a38829-f3ab-44d1-9ac8-5ed9ac5fcef2	49	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
6c441db4-ce7e-448f-b7bc-cd7dd8936a11	50	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
4a16b69c-b268-44b3-845a-f20555f87d0f	51	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
3e78767c-8d67-4464-b004-e80f9217518a	52	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
b4cdc794-ed21-4153-b77c-cbd9cf785536	53	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
7d763adc-c40a-461d-aac9-bdf031c9c2b7	54	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
7a94b0c3-f4ee-4430-98bf-fe817cd8ac7e	55	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
16086fd8-21f5-405c-8783-c6295a9b49c6	56	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
266607bd-08f8-46eb-b9c1-ee92db2d98bf	58	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
dce2fca3-a9ed-4d03-ad40-8bcef26238b4	59	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
e801be06-443d-440c-8d1c-9cabda00ee51	60	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
463202a3-3b18-4ee8-a6f2-50d3e2fffea9	61	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
03a05f6c-a340-4677-b1fa-6629ffa70270	62	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
ac62dcd4-c56f-417f-a455-b730cf33002c	63	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
dab4f30d-9642-4c3b-b815-65f064c6adca	64	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
0a67471b-0f4c-48a3-8115-e8fa88ba558b	65	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
81e4bc8f-d692-487c-a429-5d4c8d2a8daf	66	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
e259c9c0-e05e-47ed-86cd-383a55caee0b	67	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
1284ecd5-b8c9-4e95-8b41-5c6ba079f8fe	68	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
6f891675-a4b6-4cc5-817f-e35811da16b3	69	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
0eada758-0530-481e-9975-6bbb0ab9b301	70	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
f97a474d-6c56-4d94-b093-4415f44a6a33	71	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
b84a59ac-a91a-448a-8d40-0f1635d5e506	72	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
b9c89027-4b4d-499c-ae48-70382a7c5120	73	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
9b41c207-9ec7-47ea-897a-eeca2bc8d96e	74	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
4f98ea3b-d3bc-4262-9852-0c6be5051fb5	75	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
2dad92f7-6216-44ec-979f-9300ea39431a	76	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
4611ce2b-1fa2-415c-9927-9c5d63700f37	77	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
7f5f4899-b60d-4c2d-94f6-1646bc21e22b	78	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
204867a6-3510-4f99-9aba-76bbd19828d1	79	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
a31890f2-488c-437d-89ec-06a61bfb29ff	80	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	f
64618c40-4a76-4762-91c7-c78dcbeab7d7	57	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:21:12.422	2023-12-18 00:46:32.827	t
\.


--
-- Data for Name: Avaliacao; Type: TABLE DATA; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

COPY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Avaliacao" (id, "idFilme", valor, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: Cinema; Type: TABLE DATA; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

COPY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Cinema" (id, nome, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: Filmes; Type: TABLE DATA; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

COPY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Filmes" (id, nome, sinopse, "dtLancamento", "capaUrl", "createdAt", "updateAt", disponivel, "linkTrailer") FROM stdin;
\.


--
-- Data for Name: Sala; Type: TABLE DATA; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

COPY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Sala" (id, nome, "idCinema", capacidade, "createdAt", "updateAt") FROM stdin;
7b1c950e-f84c-4940-bfa5-29bd0a551f82	Sala 1	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	80	2023-12-16 00:26:21.456	2023-12-16 00:26:21.456
5510f662-6acb-4c17-af4b-899bc3349ce6	Sala 4	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	50	2023-12-16 01:39:02.261	2023-12-16 01:39:02.261
\.


--
-- Data for Name: Sessao; Type: TABLE DATA; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

COPY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Sessao" (id, "idCinema", "idFilme", "vlEntrada", "horaInicio", "idSala", "createdAt", "updateAt", "dtSessao") FROM stdin;
b63a04eb-c7d1-445b-9093-e2b70c7b6274	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	25.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	2023-01-05 03:00:00
65faaa59-13e6-45ee-8129-8805a26eb568	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	25.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	2023-01-06 03:00:00
9e0f269b-1e0a-489e-b64c-2ecde5cd5c49	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	25.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	2023-01-07 03:00:00
f7560ff5-bc50-4920-9c73-3a43e0f39ff2	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	25.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	2023-01-08 03:00:00
e63d3cd4-1298-4fa7-9ab1-dc6f9e96977e	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	25.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	2023-01-09 03:00:00
3e835407-40f2-487a-a130-d07ed9a5e87b	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	25.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	2023-01-10 03:00:00
b2e76b1e-f81a-499a-8d06-c33d2825128f	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	25.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:42:52.578	2023-12-16 00:42:52.578	2023-01-11 03:00:00
3f856fb9-da3f-4730-9cdf-6f7fc9c1b653	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	02202da1-b071-433b-9b28-f6b4beab820c	18.000000000000000000000000000000	20:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	2023-02-02 03:00:00
4009a659-9f32-4731-8a7a-b038cf973b7e	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	02202da1-b071-433b-9b28-f6b4beab820c	18.000000000000000000000000000000	20:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	2023-02-03 03:00:00
ef58b92c-f67d-4c2a-b044-bbee4fcb6ade	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	02202da1-b071-433b-9b28-f6b4beab820c	18.000000000000000000000000000000	20:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	2023-02-04 03:00:00
869d9cc9-18ed-4867-996a-67a8730b4c16	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	02202da1-b071-433b-9b28-f6b4beab820c	18.000000000000000000000000000000	20:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	2023-02-05 03:00:00
72ba11fe-f44a-4b3c-a4c9-67fe08a67de3	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	02202da1-b071-433b-9b28-f6b4beab820c	18.000000000000000000000000000000	20:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	2023-02-06 03:00:00
19f072a5-7240-480e-8a97-e3133dc52aee	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	02202da1-b071-433b-9b28-f6b4beab820c	18.000000000000000000000000000000	20:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	2023-02-07 03:00:00
348d7f79-043c-47d9-8f4a-29fc2a31bb96	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	02202da1-b071-433b-9b28-f6b4beab820c	18.000000000000000000000000000000	20:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 00:43:28.56	2023-12-16 00:43:28.56	2023-02-08 03:00:00
d22f97d3-67cd-4688-bd47-7d9243db2672	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	60b26f45-58a1-418b-8884-818fdf137c0f	23.000000000000000000000000000000	18:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:03:50.343	2023-12-16 01:03:50.343	2023-04-07 03:00:00
b2e89703-9e10-4ab8-99bb-e90519a3d0e1	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	c5dd596f-c6d1-4752-8502-54e4890186e2	25.500000000000000000000000000000	20:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:09:13.773	2023-12-16 01:09:13.773	2023-10-21 03:00:00
0405a280-8a08-4a0d-a78f-00b93da6bbf3	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	b07d0f7a-9d03-4d02-be74-923108d72955	15.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	2023-12-09 03:00:00
78e67fa8-7544-443b-8565-d66e446a663c	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	b07d0f7a-9d03-4d02-be74-923108d72955	15.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	2023-12-10 03:00:00
9bcac518-fe9b-4180-acd7-b0c06b04aaaf	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	b07d0f7a-9d03-4d02-be74-923108d72955	15.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	2023-12-11 03:00:00
2139c543-7189-45f0-9b45-1f4f34e78cac	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	b07d0f7a-9d03-4d02-be74-923108d72955	15.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	2023-12-12 03:00:00
cfa591a8-ce9f-448d-be6d-67e4d2ee76de	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	b07d0f7a-9d03-4d02-be74-923108d72955	15.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	2023-12-13 03:00:00
b2ad7385-e2b5-45ac-b38a-cdccf12e281a	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	b07d0f7a-9d03-4d02-be74-923108d72955	15.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	2023-12-14 03:00:00
804aa4f3-7995-4e55-a5a3-9ba86debc62d	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	b07d0f7a-9d03-4d02-be74-923108d72955	15.000000000000000000000000000000	16:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:43.791	2023-12-16 01:20:43.791	2023-12-15 03:00:00
8778395b-f9a3-4af1-8b56-6fa7159c0aec	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	0853ee06-86a6-44f0-9941-f0bcac5eba25	25.000000000000000000000000000000	18:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	2023-12-11 03:00:00
66ece208-bfe3-4061-bbd0-d41820080945	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	0853ee06-86a6-44f0-9941-f0bcac5eba25	25.000000000000000000000000000000	18:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	2023-12-12 03:00:00
d25a52b0-e28f-49b9-a275-bbd41f62a822	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	0853ee06-86a6-44f0-9941-f0bcac5eba25	25.000000000000000000000000000000	18:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	2023-12-13 03:00:00
7491cee2-3f31-4f39-992f-019bc8d5711e	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	0853ee06-86a6-44f0-9941-f0bcac5eba25	25.000000000000000000000000000000	18:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	2023-12-14 03:00:00
7188314b-3a8d-4240-b0c8-93272ee24a14	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	0853ee06-86a6-44f0-9941-f0bcac5eba25	25.000000000000000000000000000000	18:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	2023-12-15 03:00:00
76050df5-95d3-46fc-a782-287fd8a2ca09	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	0853ee06-86a6-44f0-9941-f0bcac5eba25	25.000000000000000000000000000000	18:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	2023-12-16 03:00:00
7adbafff-8b29-4432-a521-c9de87d843a1	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	0853ee06-86a6-44f0-9941-f0bcac5eba25	25.000000000000000000000000000000	18:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:20:57.755	2023-12-16 01:20:57.755	2023-12-17 03:00:00
30b3b403-0718-4a55-8506-d6fdd336038d	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	9b05c043-9a7d-49de-88a5-ab31de1177ef	26.000000000000000000000000000000	22:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-16 01:21:12.422	2023-12-16 01:21:12.422	2023-12-09 03:00:00
88233011-6760-4c18-94b7-fb2c3323187d	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	d089e38a-d140-4f9d-9826-483c97c32fe3	24.000000000000000000000000000000	18:00	5510f662-6acb-4c17-af4b-899bc3349ce6	2023-12-16 01:39:20.285	2023-12-16 01:39:20.285	2023-12-16 01:39:06.967
fa07c73b-96c2-4ff1-aaeb-bc4c275872f6	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	25.000000000000000000000000000000	18:00	7b1c950e-f84c-4940-bfa5-29bd0a551f82	2023-12-17 20:48:44.004	2023-12-17 20:48:44.004	2023-12-20 03:00:00
\.


--
-- Data for Name: Ticket; Type: TABLE DATA; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

COPY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Ticket" (id, "cpfReserva", "nomeReserva", "idAssento", "idSessao", "createdAt", "updateAt") FROM stdin;
09c9146a-6b36-4ec5-b84e-f57f0a6dcc96	09438024576	João	b74db5e7-a577-4887-a5c9-32ffad10c4e1	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:52:11.505	2023-12-16 00:52:11.505
0eec6347-150e-41ee-a7ac-7af23db5b080	09438024576	João	3fde3798-a754-4b84-b30e-96f7375a5652	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:52:11.505	2023-12-16 00:52:11.505
282ce458-3875-454a-a923-a2c267bb99be	09438024576	João	4d43a32e-50b7-4a88-b70b-b7c581b56a44	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:52:11.505	2023-12-16 00:52:11.505
c744d3f6-f673-45d3-ba55-16fe4717280d	09438024576	João	b8346885-ecfd-4ef4-a1b8-26cad23ab35c	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:52:11.505	2023-12-16 00:52:11.505
00b509ff-07d4-4c21-a1a1-2972de9ad531	09438024576	João	8c9eedfb-e4fd-48ce-afcb-2bb2f4138bbd	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:52:11.505	2023-12-16 00:52:11.505
e577472a-e69c-48dc-884c-2bb7621b4706	12345678911	Joana	0e130cae-e622-4aa6-8413-3b39e3640fa0	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:52:50.961	2023-12-16 00:52:50.961
86841d92-1fda-4ac3-a35e-8c154829ebbc	12345678911	Joana	5e9dc46c-e98e-4207-84a1-ee513d89c47a	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:52:50.961	2023-12-16 00:52:50.961
0371ec05-1e84-4cac-8bb3-bfc4ed97aea0	12345678911	Joana	fb7bd3e6-63ff-430c-b312-59e8a4e7af6c	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:52:50.961	2023-12-16 00:52:50.961
1902c64c-0394-4ec9-a2fb-e974dcd881c8	12345678911	Joana	d51e1b92-b196-4d6f-8f2c-2ccc669567eb	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:52:50.961	2023-12-16 00:52:50.961
eff5f5f8-47f4-4f51-8afa-c4898ca8693d	12345678911	Joana	ea516ffc-c7b9-4125-a9f4-a7f4c3bc19b6	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:52:50.961	2023-12-16 00:52:50.961
96db80ea-2ac5-4534-a297-9e54ddb6e966	18462738490	Mário	a6b0c63f-d813-4e24-80d9-3154547b6455	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:53:50.797	2023-12-16 00:53:50.797
a465eec8-8ed2-4f30-b9a1-c3d656558905	92346412734	Amadeu Silva	57d7c262-447d-47fa-80fe-87677b4f19ea	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:54:33.728	2023-12-16 00:54:33.728
5489d392-dbfd-46c6-bc2b-f4b8f99e1c74	92346412734	Amadeu Silva	160649cc-ef29-4244-8000-716d9eee4595	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:54:33.728	2023-12-16 00:54:33.728
f76b0bf7-4031-465d-b26f-8bc4d4db428b	92346412734	Amadeu Silva	081c7d12-27ec-44e0-a6c9-f5e921ef5df1	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:54:33.728	2023-12-16 00:54:33.728
a2200c0b-6600-41a4-858a-4740eb9b1ab2	97654283791	Fabíola	0cec4c2d-0eb4-4cd6-8f75-2c7e600cc539	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:55:00.019	2023-12-16 00:55:00.019
e3bdf91a-eed5-413a-9449-aa23e4a9ec3a	97654283791	Fabíola	7f5bacda-0914-4db2-bee4-50018441dc4e	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 00:55:00.019	2023-12-16 00:55:00.019
8f92a102-f5f0-49b8-9c6d-f11a808eb33f	91293979123	Maria Josefina	1f8a38f0-a71c-4f81-a198-1e62dd0ea1c6	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:56:37.149	2023-12-16 00:56:37.149
1a9dc204-9d6a-4ccc-80eb-eb871d57e78f	91293979123	Maria Josefina	34f32da8-257f-4236-89c6-5ae009bbdd5c	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:56:37.149	2023-12-16 00:56:37.149
6a30c761-07e5-43d8-9ee8-2c305c8b2bab	91293979123	Maria Josefina	357d7618-720b-41f7-8d46-ac7b699fa462	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:56:37.149	2023-12-16 00:56:37.149
3fb37d43-5237-4189-b326-845512252374	91293979123	Maria Josefina	b057381c-b3f1-4794-a1aa-25f87603bfb4	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:56:37.149	2023-12-16 00:56:37.149
538cfaf0-f0f0-4b32-9e3f-a549984b29c6	91293979123	Maria Josefina	4138a8ee-4546-4741-b6ab-48c27ba35b6e	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:56:37.149	2023-12-16 00:56:37.149
1d4d9697-f8e4-47a4-93b6-2ebfbd4d5c44	19438274910	José Bonifácio	3a640480-900b-4f15-bdbc-287b2e0f76eb	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:56:55.298	2023-12-16 00:56:55.298
2cb87d47-dfff-4ae4-9fd1-5096fa8f84a1	12345678901	Jiramaia	9558268f-c9fb-48c1-9ad3-c61ee15d6ff6	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:57:27.309	2023-12-16 00:57:27.309
586b2eb4-2190-434f-8c75-7727af058fbf	12345678901	Jiramaia	64225347-1cdb-4fbb-a68a-fb92022871e0	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:57:27.309	2023-12-16 00:57:27.309
965c1df8-6dd9-40b5-bc29-0c8f65123e96	12345678901	Jiramaia	dd8b077b-14e1-425e-b615-f653618cd378	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:57:27.309	2023-12-16 00:57:27.309
7c5e8440-eae2-4a0a-81ae-e4e863f2467a	12345678901	Jiramaia	f7844d0f-2e20-4dcc-9ead-9932bdbeb04c	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:57:27.309	2023-12-16 00:57:27.309
ada8cb16-7c55-43fc-9f1f-fc95f5fd6622	12345678901	Jiramaia	f80c2c98-fb6f-4feb-8282-70cc9947ae78	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:57:27.309	2023-12-16 00:57:27.309
235f3897-12c1-428c-959c-44b05a910e73	12345678901	Jiramaia	79536f88-2b48-4fb2-ab49-dc9c14bdead9	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:57:27.309	2023-12-16 00:57:27.309
e2d72b2f-5873-4d25-9326-32a59d9d93c6	12345678901	Jiramaia	dbada7a3-6ebc-4b3b-b270-499292e77298	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:57:27.309	2023-12-16 00:57:27.309
a8ad7a4c-bcba-4e0f-9d17-6033c58841b9	12345678901	Jiramaia	6a464cbc-6cb0-4149-acd8-c548fba19691	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:57:27.309	2023-12-16 00:57:27.309
7fb44816-5121-4662-8ed4-045d0f79a30b	12345678901	Jiramaia	3e1ad571-2843-440d-8b2c-32a61124293e	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:57:27.309	2023-12-16 00:57:27.309
8214588b-c932-486c-b8d5-65f9a482e2a2	19237459601	Ícaro	04ea0a8f-0731-473b-bf40-81b41e46af58	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:57:51.061	2023-12-16 00:57:51.061
2546b645-ec97-432e-83a1-b47446a13536	19237459601	Ícaro	0360134f-2a46-4dd5-aae0-50a2b0e2f0f8	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:57:51.061	2023-12-16 00:57:51.061
15f89275-c42d-4def-ba7b-ad8ca2cbcb82	19237459601	Ícaro	a906e853-6147-4e3a-b945-b07869d61f68	4009a659-9f32-4731-8a7a-b038cf973b7e	2023-12-16 00:57:51.061	2023-12-16 00:57:51.061
1aec7d27-6518-472a-b5ab-53f072b451b6	12345623433	Josefina	64618c40-4a76-4762-91c7-c78dcbeab7d7	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-18 00:46:32.827	2023-12-18 00:46:32.827
1a3527fe-949b-47da-8f68-d634c0912629	31289283752	Jenifer	b7cc0835-6bfa-4c6c-b7e7-1c11fa70aa98	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
e0d6ff44-2929-4528-81c9-47d2fbef2443	31289283752	Jenifer	61c7c364-15dc-458b-a608-2a5fcc8efce8	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
a16c5ec0-9266-47f4-8095-b53e83bd5f19	31289283752	Jenifer	dd278682-ad7e-4c08-b0c2-faa443880cbc	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
7ab8d3b4-c656-4186-9a3f-8bb5357dd519	31289283752	Jenifer	28d029a3-5160-4731-8993-71f1867ded2f	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
d055620b-8a51-448e-9392-a4ecfd1c0bfc	31289283752	Jenifer	cc4a21e3-1c6b-46b2-8e7e-60e3272bac24	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
7f10a890-e93c-49fa-80ea-dae8bf54ffe1	31289283752	Jenifer	0aa655ba-8b1d-434c-8238-6089b03926ed	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
457580e3-7960-41b9-8b0e-ffcc2daeee0d	31289283752	Jenifer	a44cfe59-8c6b-41c7-9b49-c735a5e7c733	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
4ccf06b0-9caa-4704-9d5c-2a82c31f1f26	31289283752	Jenifer	6d4b08cd-28ad-4d91-b6df-7f93b9847fd6	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
4ef6a4a7-2cf7-4ab9-a4ab-a7cbd84a7616	31289283752	Jenifer	e72c74d8-7738-42d9-802a-a6e797392f7b	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
f9745e8d-ba9c-4293-94d8-38abb067db87	31289283752	Jenifer	41d76e84-13ed-4c2a-ae18-bca2fb495832	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
db59f27c-30e1-4b98-bb7e-bd8e66fa5f31	31289283752	Jenifer	acae76df-c187-43b3-b16f-0da32c5cf5e1	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
fd338b94-cc5c-4b6b-9b96-d39a5f2b31cc	31289283752	Jenifer	083a6156-dfee-4bdb-8063-f4674ec39bf0	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
386066a3-7366-4d3c-83c9-7d3d57424dcc	31289283752	Jenifer	a82f66b4-3843-42bf-a7fb-372aa717bdff	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
59780af2-4268-497f-991f-ec09027006e8	31289283752	Jenifer	0d3836e2-41de-45ed-900a-f65dba7fc27a	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
28c54600-a662-404d-9c67-21ef0d0b18be	31289283752	Jenifer	967434dd-91d7-415c-b821-d141954c40f4	d22f97d3-67cd-4688-bd47-7d9243db2672	2023-12-16 01:04:24.201	2023-12-16 01:04:24.201
570115fd-208f-4bc3-8bd4-cb738a601697	98765432109	Pablo	07849fc1-cdaf-4375-b67f-c298e4860169	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
dbde8a64-474a-4234-adc6-e4939aa77fc5	98765432109	Pablo	8086faba-9bc4-471c-a02b-657b24fb1060	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
e6364b03-5cfe-4d0a-b4b1-f0d00dfe2baf	98765432109	Pablo	1a88806e-3b25-49a9-bc29-f26e749ede41	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
672fa432-d948-4a4e-9d51-a4258ec14ca2	98765432109	Pablo	4e702c56-db50-4698-9667-0cee296e9df4	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
ed4275e5-a8bf-4466-971c-ba78ba998602	98765432109	Pablo	7f029eb6-9533-46df-9f3b-ea945d4623c0	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
7a8c9aeb-a8e6-457c-bd52-fe03fd39d1a0	98765432109	Pablo	aaac19b7-d022-4f09-9b5e-8a22e03f554d	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
5850f7a8-c4e5-4a37-9ad8-c91f4ed5f058	98765432109	Pablo	c5c4079b-b0d7-4da4-8b47-46cf4ff008fe	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
9051bc09-e97f-481e-9dd5-1dabeddc3d97	98765432109	Pablo	58a6fffe-8fb2-4079-93c1-d057606f8083	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
3678558b-3678-4b08-b587-1f897446e0b9	98765432109	Pablo	2af8f0c5-a9be-4527-a6a0-dd88d3218932	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
8d78a87d-3822-4f34-be5e-8d60c586b48e	98765432109	Pablo	af130217-f55c-4d8d-8113-70b19f93b965	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
24c8eecb-c754-403c-9e82-e78b4a9f4c4b	98765432109	Pablo	90827b64-8b91-468b-ae81-2bb1dbda74aa	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
510a7562-3ca5-4767-a000-31abff5d5bac	98765432109	Pablo	7a7d7939-5ea9-41ef-8d52-1a012d7a5064	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
8733aa32-4b91-40fa-9f47-3f68d70ed2cd	98765432109	Pablo	b6ac89f3-5f00-436a-8f82-2198452e59fb	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
3883283d-7398-43e0-9787-a5061f0cc484	98765432109	Pablo	763335bd-b30e-4816-98ba-85ec8ebb190b	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
1cf2a83b-a41e-4270-8c4c-a9a254c03d6c	98765432109	Pablo	80946491-2a8c-48ed-bd71-bd1bd30e7aff	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
d1ac2db2-8b9e-4f63-b07a-084a4cab8ad0	98765432109	Pablo	300d6f9c-056a-4d53-b42e-e0758ac3adff	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
0c3a5086-5d28-46c3-898c-cfc637937506	98765432109	Pablo	36c22258-fc77-45d1-9d31-0b8a7d1dcb5a	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
409bd4b4-fae1-46dc-a9b2-48e4bda9a193	98765432109	Pablo	ba7d0a29-748d-4525-ae27-a6ecc22a3a3b	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
9b5713f9-90bc-44ac-a0eb-5161c1a50f91	98765432109	Pablo	599bd1ca-c0d5-403d-8e0d-a5f7a28d0e10	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
92f7bcae-ec37-4535-9efa-ac1be25ac2fa	98765432109	Pablo	df352a35-efb3-44c7-8736-f79fe4ce53cc	b2e89703-9e10-4ab8-99bb-e90519a3d0e1	2023-12-16 01:10:08.687	2023-12-16 01:10:08.687
9ca3c0ee-7657-4902-a6d3-0a0b363ba7b0	90724356012	Eduarda	7b14d111-f775-49d4-bd96-e184aae0f757	cfa591a8-ce9f-448d-be6d-67e4d2ee76de	2023-12-16 01:23:14.139	2023-12-16 01:23:14.139
b63cad07-276b-4369-9f26-a3a3dbaaaf54	12345678901	Marcos Nascimento	a1dad0a3-c548-4252-87a5-9957f9bde63e	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-16 01:24:07.285	2023-12-16 01:24:07.285
621b0202-a713-4c11-bc0f-08b56095584e	12345678901	Fabíola Prado	f03c3a43-40b1-45b4-a63e-97ade456164b	88233011-6760-4c18-94b7-fb2c3323187d	2023-12-16 01:39:49.941	2023-12-16 01:39:49.941
eaf455ea-2a11-4c09-8854-a626e9a6f44d	12345678901	Jocimar	4c1ed4dd-9836-453f-96c7-9ee61c164958	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 02:05:06.308	2023-12-16 02:05:06.308
43cda232-4c81-4c1a-929d-526b6ac1561f	12345678901	Josias	c9e62d00-10b1-4b79-a4db-9957d1489c8e	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-16 02:43:25.716	2023-12-16 02:43:25.716
34bbc118-f260-4d59-93da-6ad158cad9ed	12345678909	Jonathan	3794374b-1650-4d5a-99d4-c9de776ba85c	b2ad7385-e2b5-45ac-b38a-cdccf12e281a	2023-12-16 12:25:45.584	2023-12-16 12:25:45.584
9f94b086-9b5c-439f-8feb-16f1bb835aa2	01844359000	Marcos Nascimento	dedcff33-ef86-452c-b064-10098eca15de	b63a04eb-c7d1-445b-9093-e2b70c7b6274	2023-12-17 16:04:34.051	2023-12-17 16:04:34.051
dbf40894-e87f-44ee-98e9-a258608ebd42	01844359000	Marcos	fb55307f-ec24-44a0-8007-d484f04dd5f9	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-17 17:36:51.631	2023-12-17 17:36:51.631
80aa2753-b85c-46b3-93f2-dcdc0cdc4c09	01844359000	Marcos	a1916d60-127f-4f25-a829-159811ba4f88	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-17 18:26:58.146	2023-12-17 18:26:58.146
2e30fbc7-e2cb-4516-9625-51fc9a07b353	76512345627	Marcos teste	217e7db8-fc62-4206-967a-1bcb305beded	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-17 18:49:13.743	2023-12-17 18:49:13.743
14203374-f803-4514-b013-e9a374e164fb	76512345627	Marcos teste	fe3254cd-57c3-4840-b311-144c8542923a	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-17 18:49:13.743	2023-12-17 18:49:13.743
c55f6ab3-a10f-4e9b-9a9a-74c861da797a	76512345627	Marcos teste	22f5da41-0e1a-417a-8e2c-fa2f3f760a1c	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-17 18:53:03.08	2023-12-17 18:53:03.08
d6134c38-87ba-45c0-a936-de1b2f5fe0d7	76512345627	Marcos teste	28f410a0-6589-4552-9831-b3ccf2592c06	0405a280-8a08-4a0d-a78f-00b93da6bbf3	2023-12-17 18:55:17.579	2023-12-17 18:55:17.579
9c9a9899-285f-48e1-8a84-69d1bbd53ce0	01844359000	Marcos	0ff0e4ec-266f-4b12-a7d5-4eb53fd9b26a	804aa4f3-7995-4e55-a5a3-9ba86debc62d	2023-12-17 18:59:30.866	2023-12-17 18:59:30.866
2f53f500-b1c9-4864-bf45-4f8ad0f9523d	12344321675	João	6a9b3a02-884c-4a7b-b947-2ae854b470d8	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-17 19:09:45.256	2023-12-17 19:09:45.256
fc31ffc6-a9b1-4d9c-bf7d-d58557974e88	12344321675	João	bde2a3ed-db5c-4cff-848c-441005753f61	30b3b403-0718-4a55-8506-d6fdd336038d	2023-12-17 19:09:45.256	2023-12-17 19:09:45.256
\.


--
-- Data for Name: Usuario; Type: TABLE DATA; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

COPY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario" (id, email, nome, senha, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: UsuarioCinema; Type: TABLE DATA; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

COPY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema" (id, "isAdmin", "idUsuario", "idCinema", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Assento; Type: TABLE DATA; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

COPY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Assento" (id, numero, "idSessao", "createdAt", "updateAt", reservado) FROM stdin;
0f7342c0-302c-44db-a6a3-f83d26bb0892	1	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:10:15.481	t
0775cad5-5aa4-449e-a271-8a8ce93e4b33	2	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:10:15.481	t
882c90be-ab65-4d94-b197-1fa8b31d19cc	3	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:10:15.481	t
3ed55d09-ec90-481c-ba71-2ae45ce86012	1	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:12:56.618	t
a72a46eb-456d-4ee4-b573-afe50f4afd94	5	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
314d1ebc-a869-48b1-86ac-d6bc63d245fd	6	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
97e5e5fe-cdd2-461d-8da9-bc997a4e313a	7	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
8ae459dc-478e-4cf6-9119-4e5adbbe1d0a	8	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
7f4550e9-007b-4a3a-bc1b-bedce85b5984	9	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
dc3849c7-f4a9-4847-a1e9-d3b08695b9b3	10	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
9afbe017-93eb-46e7-9622-6c1efc1290bc	11	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
8e6c09d0-9a13-4821-a7a3-f8ef18c3c564	12	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
4ef1e106-5626-4d6c-8e98-ec73715ef287	2	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:12:56.618	t
361e7612-19b2-4e4a-9400-b91ca663257f	3	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:12:56.618	t
0f425297-708f-4961-8bf8-610598fc5ee5	4	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:12:56.618	t
b028200b-3d57-496b-aca3-d707aab6522d	5	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:12:56.618	t
b7d3e2cd-9a50-41a4-a021-7f7b488fb6d6	17	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
e12080b3-6af5-48bd-b841-1b621ff0666d	18	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
4fc53b93-abde-4f73-b183-9b560692f12c	19	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
c64931ab-7e5a-471c-91f3-62139ae5d3c2	20	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
6dcf0878-0953-4359-990a-20b96dd7d773	21	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
108b7988-a09c-4f33-b0ef-21180fefb424	22	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
c288bf15-19cd-4879-b342-dfe3fcb6c360	23	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
8bba60fe-e97a-4adc-bf4e-ee857892b3d6	24	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
0b70848f-dbd5-4050-86e8-50fb35f09671	25	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
a47c8935-fabc-4b0c-bf51-3d973b9255da	26	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
6b555417-67f6-499b-9f9f-c5d70290a962	27	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
dfff38ac-005b-4ff6-bb96-2b931bb6bdef	28	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
82cbb494-ed65-4e18-ade8-df069cdfdbcc	29	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
c0f9b07a-7461-4e19-af24-23cb0d4b9f3a	30	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
cf497bca-8ee0-4bcf-96d3-bbd99aaa2850	31	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
0de94ae7-3449-4171-baee-b5be3be13786	32	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
e25077be-b146-4974-a763-8c8e9bb4b1dc	33	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
d5a790a7-770e-4df6-9140-8f837470356d	34	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
90ac5337-9e31-498a-b52a-c8e12bc872a4	35	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
6052686e-5024-465b-bb79-0bec254b74b0	36	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
b4bb8095-2200-4748-96b2-6d2a138e64ee	37	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
e316ba73-4f25-4d3c-af49-bbb702292b84	38	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
0712ca34-9ce9-4f60-b1e7-47e419935b36	39	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
4e583650-fe4f-4dbd-8f6b-116b9ed9bcc9	40	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
9a15c017-d9a1-4a5a-92ae-8e0651417b1f	41	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
9b081425-1a73-402d-ab9d-765af3cc980a	42	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
47b88b73-a3a0-4290-ac2c-ee192cfe5098	43	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
90953d3a-13db-44f3-bef9-030e1cde180e	44	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
037d05c0-c995-42b9-a39f-16b1515a216a	45	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
6c16bf48-dd2b-413b-bdf9-adc841287472	46	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
f3563d4b-e0e4-482e-8868-57fae6920f6a	47	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
6b193dba-bb5b-4b63-b8a3-817f1cd6008e	48	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
115dcfb7-2a18-4f84-9b73-116032075f5c	49	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
d22b0028-500f-453f-b265-aa37d9ed2809	50	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
a8113187-746b-4829-be57-40f1843c7a45	51	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
7cf8eb8a-b23d-4b31-8327-5771f595bbfc	52	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
d8eb07f9-b6c5-4d17-bb29-b1170bdd07c0	53	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
6bfa75e5-afda-4291-94b1-981849aa1870	54	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
65b249d5-d8b0-4f94-89ad-065aa03d8807	55	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
6a829cc5-51a4-4452-ae78-e865eb797fd7	56	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
a0dd2012-f6e0-4144-9d19-e3d2706be9e4	57	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
dff81781-4607-4b8b-b9a1-6b88219ae777	58	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
a9f8d0c6-53d6-4ebf-bc16-2e8067df60d4	59	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
b139940c-a6bb-4d10-8557-995a2d2fd8ac	60	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
bb52cacb-dc2f-445b-9ffe-7897944d081c	61	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
96a9ae01-5a5e-4831-b7a5-e97d1479b47b	62	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
7d32f553-6815-4092-9486-258ec3569417	63	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
32b27a6f-4aea-42c9-a4ac-ac1a8da51c56	64	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
c70d26c9-e61b-4259-b35c-0ce25c05ffb7	65	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
65893e05-9063-4be8-b8dd-652403a622f7	66	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
395a5c85-701f-49de-871e-7f1a590af3dc	67	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
e6559e68-2437-431f-9823-5a472cafc6d6	68	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
a771009d-3685-4457-b6c5-7dc29c456be6	69	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
41768c43-ac2a-48ba-a52d-c4c66ef36c12	70	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
21f3a10b-5c14-4fce-a81c-9c91e1c9ca9c	71	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
c06d841a-30ec-4609-8a81-9dde78780ba9	72	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
f8951d30-10ea-497d-ba36-f70c3f4c0c87	73	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
c2b0430a-cd36-442c-860e-5741022c0241	74	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
a9a6920a-e8e3-43b3-9ced-ab14e3e6fd9c	75	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
3ce90a7c-899d-4696-92a4-2bf3a7e7529f	76	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
6432c8f3-e099-4ec5-95d3-cc493421fe26	77	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
a0794c39-99f1-47f4-9eb6-88cec01d18ab	78	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
f13da1bf-7fa7-48e2-90c3-3314a23ea8a5	79	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
431ab547-ff34-4582-906e-4d7477e23bd8	80	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	f
66e80a4c-9e56-4b53-a8ac-456bf97578a2	1	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:10:51.999	t
19d7b5db-c75a-420d-bf1c-27a379c800be	13	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:10:51.999	t
9968902e-5eab-47af-8fce-2249ad656e1f	5	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:11:55.347	t
ebd751a1-ac5f-4cc7-ac71-aeaf1252745b	50	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c9031cfd-8b92-4d60-8ff0-0af185f611bc	6	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
af0dcc98-026d-4a67-981f-157443b50f18	7	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
17a20a91-9b69-4ef9-8dd8-a41bc5a612d9	8	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
4990d780-4f5a-4c75-9f25-4314db16aa9a	9	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
96b1921c-f4f7-401f-87bc-5a9b2ad909d3	10	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
1887216d-dfb1-401f-8de0-a148e574fb97	11	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
0a9e0ded-5eab-40fb-9744-79368cefb5c0	12	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
11779f65-ea46-4e1d-b046-30e41222cef4	53	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f2090eb8-f56c-4abc-820c-fca05cbb8c77	54	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8dda4414-f5c9-4abb-9046-11213922c59b	55	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3f8626e7-fe81-4469-8b94-e4d6d7d2e5d3	56	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d4b46776-7169-48eb-b9bc-53418f3f919f	18	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
fca307ca-3fae-454d-85b3-afb609f9557f	19	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
29a2126b-3838-4989-b09a-8d2125ca98e2	20	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
4b237074-a0b4-4cd0-b4f9-cbdb1ee17e16	21	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
d0dc075d-c8db-4b11-9fc5-d46d8ff4302f	22	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
fc6e44d0-36b6-446d-8ed8-0cfc74d02b6e	23	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
9310a348-e65c-48a1-836f-cb6f88ea3201	24	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
2de6d2c5-dcfe-4a5c-ba23-f7ce166b67c8	25	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
6b7d1f62-1e26-4c61-b63d-0ffcb7385902	26	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
1f2d61dd-93ac-4e9a-a630-867bd41b729b	27	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
2fc8b063-2622-4217-b060-5a9de5267c55	28	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
1cf061a8-8ad4-4ecf-bfde-177a83947b16	29	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
8765d718-81a0-4841-bd83-cf08618798f7	30	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
fd00fda1-2e50-4984-a0a2-e6f85b5f7313	31	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
67476896-6498-4929-b44e-9af429a14592	32	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
7daab841-8cd7-42c8-b6f2-8362ea13c872	33	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
65e75cc8-a49d-400e-b6e9-0efde536fc1e	34	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
5b0eb527-637b-4758-b644-924200f2f487	35	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
b4e014ba-3cde-44d5-b0fe-6bc39caeb4a0	36	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
93369f3c-82df-4c58-8f70-63d32ffbded3	37	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
3b560a94-0035-415a-9ad1-e6023632db49	38	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
5b272b4d-6202-4fdf-8783-11dfe698e650	39	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
baaf5d36-2d43-4ee7-8f6e-2e8369f2393c	40	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
11faee9b-c454-401f-b5a2-e8f7dce8297c	41	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
acc4c89c-0643-4be6-828c-906b6ffe2e91	42	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
4ee88615-cdb1-45c3-8d7a-93c59438a9ed	43	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
e0beb804-a79a-4f49-8520-88cec0740aea	44	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
0736276d-5064-4abd-b614-ce0062782545	45	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
88d3e92e-b960-423f-92af-cf58d479b2f3	46	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
38552d6f-a87e-4f63-b327-84bd2146b2de	47	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
3c1fd900-ace5-4587-bf63-be9ccacd7207	48	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
fadd9397-6621-482e-a47a-90fb0a3715ae	49	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
e526d2e4-ea3a-445b-9425-9542561fc7fe	50	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
5f3a8aad-fa73-4c20-95d5-552dea656c76	51	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
5f5198c0-b9a2-424c-8a30-1c457ac7a896	52	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
381a6561-b2b2-4af5-8003-be74276d4739	53	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
c6458ab7-683b-4dc7-909d-a7917ea9bfed	54	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
67de83ab-b316-4e6d-a7f4-5978e4d048a3	55	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
86e03449-6f68-4b1d-981b-c200883c1ef0	56	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
568bcb54-c5d2-437c-9407-9ebbeebb93ae	57	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
ce93890d-77a6-493e-a41b-3fced0fa0981	58	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
cf7197a4-9e64-4cb0-8404-30b1545913a3	59	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
3eaaf8ec-7fca-4b77-b95b-0aecdcccb1b8	60	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
84729285-b3ee-4522-a107-f24e4c110228	61	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
87d38140-cba0-41f6-8d88-629dc49b859b	62	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
10494a43-71f7-40d7-89ad-f13617ab7c23	63	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
47849072-8961-488b-90e0-7bd8f0652dfc	64	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
68577f08-e451-48a6-acf8-0cf77de7c2ce	65	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
6a0dc143-3dad-4dda-a2f5-43e651b106b9	66	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
e3520871-0f49-4350-8466-17eed3a344ff	67	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
7457bc4f-3cb2-4e96-a770-faa4e2439531	68	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
5e022d72-b16c-4f31-8ecf-3153cb493c00	69	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
d44e954d-6163-4e46-ab6a-db29c18358c7	70	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
35ca9b7a-4aa0-419b-9d04-6cb968421ba5	71	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
b7a61935-4dfa-4998-8cfa-108fb0eb96ad	72	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
d9f66c9e-fcb1-4314-9bf2-9f276b691ba6	73	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
ddb84a05-409e-4d0e-817b-06dbfc3a78e6	74	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
ac4e5416-2b46-44bb-9687-f71513173ad5	75	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
b316bc47-34e7-4230-b79b-9870134fbf0d	76	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
6cdb54e2-0137-458f-888f-445631483bc8	77	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
530f460e-fdb9-4308-a601-ba276ca4d899	78	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
12eebf35-1082-4878-9895-258f9460b768	79	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
4ab5154e-6d26-498d-92ff-3213f9176b6b	80	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	f
94b7857d-3eb9-48d3-a8cd-20ec0788659c	57	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
66c03dd2-3030-4765-908c-5011dccba773	58	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3af8d279-cb8b-40a7-ae35-4cf23e0f8836	59	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
bc98c7f5-73b0-44b2-8f3b-c1098e2db88d	9	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
798468ac-3f0b-45e6-87a7-64d731f38b82	10	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
09ec07f1-7bb6-4710-b424-67e1b2502aaf	11	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
6c5a900a-0863-4116-b3c6-c98705bc9386	12	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
15c0fc9b-7161-4e2b-90ee-f707de1bcbbb	61	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ce72157e-4972-43f7-a511-8b979960796e	62	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c5a18f82-b3b1-47a7-862c-b114ffca2e35	60	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-18 01:57:21.106	t
1be87679-4c8e-4ce0-a194-32576587a4b3	6	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:12:56.618	t
153a3f5a-6983-4f2d-b0a0-4104d9b53625	13	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:12:56.618	t
9e115fee-5dfa-4741-9b3f-366c77a76f4b	14	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:12:56.618	t
190c274f-d932-44af-97a0-a5af37f1adcb	15	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:12:56.618	t
50c8af80-c019-4a58-bcb9-3aa404d01ef1	16	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:12:56.618	t
5d1d8caf-232d-4b2f-be38-3cfc78b9dbd9	17	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:12:56.618	t
e7066c2e-ff6a-4a3d-945d-4a2a590d6a5e	63	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
89eb982d-4832-4363-8d70-f334e803d998	22	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
46992d11-b0bd-4198-8b9d-2f65189f4e3b	23	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
d39eb799-71da-4aae-bc32-e9b6853009ee	24	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
b7d7998a-bbed-4603-96b5-08d91e9c2ffd	25	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
21f3851f-1ab2-4305-a2e3-fd48ca3c7c3f	26	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
deee5520-4622-4d66-b2e5-1f190682a414	27	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
6b6eead3-0b38-4a7b-9c41-b1d133af3c27	28	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
3ffe6168-6f39-4a04-aca5-7039b6a447ed	29	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
fbd91068-1a14-41e2-9b4a-4698685480d1	30	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
daf3b570-59d7-4dc9-a666-19d36e30303a	31	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
afac7f16-58dd-4f82-80b4-f853ebe52541	32	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
e1723568-2507-4cf2-b87d-467c1e56a53c	33	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
8ac71be4-98c3-4da0-83ab-e244c33f9c94	34	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
db7498ee-441b-4f07-8a5b-ce938bf236bb	35	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
fbcf6b83-cdc9-48ed-b60a-052edd0ffa34	36	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
62e44a1f-ecdf-4c1c-9096-7903bc9981af	37	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
5440e177-1354-43d4-960e-a30baace4338	38	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
9a108514-3bfe-43d4-9c18-6f6ca97363b1	39	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
7229a6fd-659b-4dfa-b22b-aa120c6f7130	40	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
799c1933-3a53-4950-9eb7-ea447d3fc87b	41	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
d38e36cb-9b78-49c6-9a32-30f006838353	42	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
f2fd30a6-c3e5-42ac-9f08-83e71464fecb	43	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
b8d54a5e-dd81-4e31-b00d-e42d40a8064f	44	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
4193c08a-bed8-434c-a539-7c7720e63ee7	45	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
a47b5043-b19c-4e64-8228-8d16bdb327b5	46	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
90a43ea7-833e-4138-a46c-531f859f3984	47	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
ae24b206-6e73-4584-84c0-fda6937f213d	48	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
7ee60df8-537f-4442-bc0c-7ae4722850b3	49	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
2d1119e5-f6c4-43dd-ac35-55b3c397fb59	50	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
503826d1-81eb-4766-974e-ba2c1c07bc25	51	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
8b38b371-339e-4208-9400-e4a76bacbb8e	52	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
a07ff8d4-f0ed-40d5-a283-608c5ce73166	53	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
c8b17590-5c30-4eaa-8d4a-01fab8ae709d	54	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
fff0b366-3552-48b8-8307-bf3d1d1c71ff	55	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
93c03169-3d91-4aa5-b6a8-0e52eaad81f8	56	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
e3365418-ecbe-49b2-8292-e08c7dff5a0b	57	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
b27b7bd5-f3be-4c07-93b1-c69604cae2d6	58	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
4047f13d-37fa-4908-a4b6-3837a8d83a56	59	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
e2db9a58-0976-40eb-8499-bcb85d91568a	60	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
9bda8f80-0039-4b49-8590-ecb1b2e831c1	61	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
6c426608-8e79-4dc0-9c0e-933088052627	62	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
cf97d055-21df-4837-b988-90411e0ef034	63	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
80bddb96-cb02-4109-acfc-792065e54169	64	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
0b2c7e53-7f1a-4539-a2f7-20b1acad4b9e	65	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
b0ad00d2-e843-4f66-96cd-d6bdc1381bd5	66	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
4cd2bb71-df8a-4b80-94bc-2ed42dc6c563	67	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
ea63370d-9f02-44b4-8773-29f843d722cd	68	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
99ccdb82-cbbb-4532-9ca3-7f83f48985ed	69	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
e19f18af-67db-4a29-b463-a4790d48c886	70	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
3bf1faf8-9e8a-4a75-8604-8690a8ba5ab7	71	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
1c626404-69d2-4a01-9105-06c468f2fc6d	72	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
ba32a44c-6c9a-42b8-b6ff-61f071e17808	73	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
a7dea17e-d59b-4a27-ae64-e5f7ec8aedb2	74	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
55e9e8b4-75a2-4e3e-b77f-2fdcb664a4d4	75	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
862f7200-d96e-452a-b2d0-3326f5449d25	76	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
7c14412c-34a7-4792-bc52-02ed1cc0cf0f	77	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
0d2a83f3-10cd-4fe0-a3ac-3dc44b91ee57	78	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
ce355ca0-f91b-4078-964d-d09b67e14d3f	79	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
fa07e1ff-07e1-4451-b5e8-70b34b41819a	80	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	f
8f7c354c-f7db-4446-b3d9-4a0c1f2685c6	2	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
a41e0362-b946-4e45-9ac8-69dd282e5fb4	3	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
3e06c0a8-cfc4-4a93-8f8e-36119adbd318	4	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
6ece696d-05b2-4915-bd33-8df1d2b61663	5	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
5aad1ecf-9538-405e-81fe-f871c79dd7aa	6	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
635cabf0-7091-4017-8d18-9133485cc9a1	7	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
443e7ef5-5aa5-4462-a663-87b4bf09b360	8	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
4dcd5d67-44ea-4acc-9bc3-3f649cbfca31	9	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
1b0adb98-6bfa-4861-ab5b-efb2aa13cd9f	10	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
61787485-20c3-4005-afd0-501488cb05cb	11	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
ba4a478e-2022-4dc3-bf6a-a9e507e93d0a	12	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
d235156b-4d92-4663-9165-070c6d6fa0b4	14	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
da742624-32e3-401e-ae3e-eb7b5faa88bd	15	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
93adb848-708f-42e3-8cb4-a0405e5048c8	16	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
188fa5a9-792b-4e60-a82b-a3ec13c2cee9	17	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
fc74de8f-4bf7-4e75-aec6-1c6bd40d0cae	8	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
4b89ed98-97b5-4c31-ba97-1efd63d7a8e1	9	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
6c2425b1-714c-454c-ae17-3f20e3f3a4d3	10	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
543cc7bd-b80c-4bf6-8da1-b4afa1b026b5	11	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
adebc018-7fa8-44ce-b41f-73aa72cdee87	12	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
c0a0c588-b8bc-4179-bfc8-a7774d9b7519	18	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
1bfea4e5-5007-4224-b045-1b8c221322c1	19	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
6485030c-28bb-4836-a254-29b595fbac4d	20	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
1daa8edd-01f5-4b4a-b958-fd2fad6dbaee	21	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
83b946b5-9af2-464e-b468-50ba747883fd	22	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
2b9d2f94-1c6c-4d6e-a264-3c8c02fcf1aa	23	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
1c4d4eeb-a67c-47fb-901e-8c699cd98c89	24	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
59b42565-e262-4225-9572-b9a2466370d7	20	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
fbdac1fc-0e8c-4367-9a46-b86655604b72	21	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
a60866da-998a-4979-8f06-b910ec3cec43	22	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
d4c4d022-5859-4af7-bf6e-0800141195a5	23	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
51b675f1-a58b-4f55-b981-3a5ceb4d0fc5	24	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
b5cc3154-3777-4408-8e60-9bdf6d8d872f	25	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
18944d6c-355f-4de7-b204-2d7a2e9146d2	26	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
280edfe2-8372-4f9a-9dec-999b7f1243b8	27	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
c7051f8d-81c5-4938-a181-96539638ad1f	28	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
3f62b56f-3576-4bd9-b427-50d859b2a7c1	29	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
9ab776a8-bb6f-4544-8377-c2c728986af3	30	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
b0f00007-25c5-45f5-90d3-8248851d1542	31	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
ac1b5be3-6d97-4f1e-aa10-70f3dea3e3a1	32	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
85142e8c-4189-46c9-b9de-9a591949d4be	33	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
75b6255c-c9eb-4d6c-b629-c5c61508d317	34	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
20a3b8f2-ca03-462c-80d1-72a61b745d0a	35	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
41c8d595-0cfb-4a33-816c-91ecec56a8ba	36	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
bcc6fdb7-61b6-45fc-bb6f-4bafe3a026b4	37	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
374532f2-6314-47dd-aa9f-b946313c9e28	38	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
42d7c35f-1b12-4e03-bf1f-a644d189705d	39	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
453fd1ca-7496-4e69-8eae-8a9725e95ec3	40	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
ae5429d0-aa73-4af7-bd70-c5e0da782f5d	41	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
648271b5-559b-4273-abf5-fe32d4e3bb53	42	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
dd14f5b2-2012-4d3e-b5a9-240d28224d02	43	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
01a2334a-25fc-49cb-b9d5-da1e61c0fbec	44	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
12ce830d-7a20-4d3d-806b-ca8f49dcfa73	45	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
166b6383-89ee-49ce-a2e6-415dfc75fd46	46	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
e9dfb29c-d69e-47a6-85f0-d677e585fa5e	47	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
11e28b63-8173-4de3-8935-5a8a46d262b1	48	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
e918ee51-f77c-4629-9856-5755b1a2a784	49	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
7c67c72b-17c0-4d95-bc51-7cd5c1c80807	50	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
7c20095f-8447-436e-ae5c-52a2bed2096c	51	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
a763f64e-0050-4269-bd2a-f61fa5d82a50	52	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
93fc7601-491a-4902-b86a-0e3d21ed4995	53	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
73938235-b76d-4996-aa10-05f65db4d72b	54	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
0ae08faa-44d5-498b-a4b9-23ff049c267f	55	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
7cbdd10e-8b79-471e-be11-a54c4270610c	56	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
c7d42f4c-49f6-42e9-88dc-fcb18b10737c	57	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
d0570333-ae5e-4a81-9908-e19086d68d37	58	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
5f9d98bd-1fd8-4eca-b740-3572665d9d08	59	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
da22b48e-7acd-4b71-9e0a-b88e65ac0a1a	60	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
232a0243-7608-4c4f-b73c-e82798e380ce	61	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
cfee7e88-347b-47fe-ac5b-e047f11986e8	62	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
a203471b-a618-48e1-9bbf-e2789c93de2f	63	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
7ccaf21a-0427-4ae4-bd1b-85ca1fbc5236	64	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
e1158ca3-afa7-4f86-865e-0e866a949d8c	65	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
3ee888fc-a006-4bd8-8456-897098c4b107	66	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
1c5f44f5-9060-42e0-96c2-2b4a7276cb3a	67	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
b95ec2fb-558a-4544-9fec-ec484300a6c3	68	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
e8447836-cc2e-49cc-a5a4-c2ee300808e5	69	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
bfc8b6d6-78de-42fc-8a6a-6e80799024e5	70	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
736afa51-2871-422d-bfe6-fe35ef161271	71	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
72f06603-57ea-4f46-8703-fd2cc2ce2c03	72	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
3ebb9ffe-307b-4793-8da2-cd5abc3e2704	73	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
f46bb7a3-74b7-4a78-bc6c-9b0fdcb93ff8	74	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
b3b69549-fb4c-4be8-aa2a-ebfd618141d8	75	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
313f3f7a-2f59-43c4-b43f-594e957dbcb8	76	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
fc2c675f-2324-4b92-b996-77051f808dea	77	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
6546d87c-0dec-4689-a870-290ae76cb1b3	78	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
a7ddbfc8-88ce-4f21-a8b9-45595c86ed84	79	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
2d4b1571-e2dc-4edb-96e5-3d2038b3dc78	80	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	f
a80d7435-7765-4d08-b261-64a3f9c8c91e	1	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
43f0db00-f3b2-4807-9817-466ce12fb51c	2	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
b05beb8e-e0a8-44d2-a325-042f09e6e6f6	3	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
cacc7a2e-dc45-47f3-84e8-c0d9fd7abcf8	4	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
4f43175f-632d-49d4-bff2-5c54e7a7a5b1	5	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
a3be4251-5cd2-42bc-add6-bc1299a23ff4	6	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
aa1fcdb5-0bab-400c-9728-9396ca8d391f	7	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
acc87317-e4d9-4506-8c66-c93f97f2e70f	8	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
e0090926-209e-47c3-bf83-dc7f2b22dada	9	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
40ace9af-0de7-42cc-bf6b-d38146c536f1	10	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
8ceec60a-4720-466c-9950-9bb3eac14d18	11	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
8be5ef92-a332-4668-beac-7ce9962b4cb3	12	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
303cd0d7-d890-4934-86de-7af0d1244515	13	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
fb01fb70-46da-4eef-a3d6-f84ea6f9392e	14	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
d54bb7e7-ad02-4798-bc5b-f5bf03ca6dfd	15	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
43d1eac8-41f2-4d27-a292-1c115a4e5e05	16	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
efd20b07-a44b-4521-8f5c-73989f48b1ed	17	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
a5b07f3d-6010-4da2-811d-fdf392d6f656	18	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
b362c363-a331-4a60-93a6-a9fef12a1af0	19	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
bab05826-a3d1-4841-9daf-bd911f71ad30	20	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
e1927004-9d5a-4ff3-b81c-b79aad3f8678	21	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
dcf8541e-b124-4376-b661-616ea991899b	22	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
981a4de0-6770-4a40-a1d9-745a4668f006	23	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
109687db-6281-489f-beba-5294cf108ee9	24	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
ffd6b44f-7bce-4493-9dfd-77d6f754a21c	25	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
0337cd5a-63ad-413f-a3f0-a13ba1b23a6a	26	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
d24b4840-e62a-45c3-bb02-6c9c5ce7032b	27	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
ebb48ab3-e0f9-46c1-8bb5-2dfc7b781450	28	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
b512c4bb-ccf5-42e2-a96b-e2e0eb57016a	29	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
3af30717-9373-48f0-9905-b850fab5dedb	30	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
723a006c-4563-4300-83eb-926f7c2387dc	31	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
58389987-5b96-4641-bd03-e75457816b65	32	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
d951a059-8063-45f2-8a7c-a012937b1a3e	33	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
fd1c6966-cdd5-4255-9129-262f5e2c2d71	34	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
1e99a6df-27c8-45c2-9ffb-f030f3b8cbbf	35	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
3d634af0-30a5-4ebe-8e45-2863a69af0e3	36	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
8aa9e608-d9f8-4da2-b146-d1cc81982de6	37	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
5228d6fd-7e65-4fee-9b77-fa47b1b95384	26	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
e6b1dea9-6c27-4e4b-9a27-8e5683ee2aee	27	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
0fe15a11-2602-4b0d-a983-4b8070ac663f	28	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
50351cea-372b-47e6-804d-ea6b4636dae9	29	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
2951d260-1acd-4a13-a4cc-6db2e275a7b1	30	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
dd0d9bf3-bd6c-4428-a779-da6eb7f8423f	44	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
7af58f55-6e30-469a-9d5f-6e457ef7fa60	45	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
24ddee57-25c5-44b6-aa31-2a770c9458b2	46	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
30cb2418-b15a-43c4-b13b-ddacb8a80e7d	47	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
37ff7e83-a6a8-43a2-88e2-65fdc91e3f73	48	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
e292d1ab-42dd-433f-afb6-938163496f60	49	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
975ff6d0-f172-45f2-83e8-7ce8a37ca382	31	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
c5639df9-9116-427f-b964-697318cc6e30	32	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
373b5c74-fa2d-49ef-957b-a245bc77102f	34	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
40cbac70-83d8-4066-9c33-526382fe250c	35	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
9fcbc785-d3bf-47e8-a412-591188302194	55	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
a0d23fbd-70bf-418f-b4c7-7dc34d7456d5	56	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
3176f674-4736-467c-ae18-90547f04f5e4	57	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
e7c30e96-1a3c-4339-92af-9835ddd4584d	58	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
d7c2844d-63c4-4438-af3b-0edbdd13e4b9	59	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
93ef1cdd-aaf0-4fae-91c7-36ad69e15f4a	60	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
d09419ac-ea52-4dd5-9245-9f803c57fa42	61	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
8d810204-2834-43d4-9187-eccb322fa7fd	36	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
093d8ff7-3ad4-4389-bb27-19d9f521179f	38	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
3a8ff2c4-fcc7-4beb-8996-da1ea74954ff	33	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-17 21:25:40.32	t
1ccd15e9-3bc9-44b3-b084-9682fb617941	39	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
d2aa9f52-7c2a-4080-b9ee-a15529555222	40	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
f964c271-4884-4335-87b9-0d9a2c7ce56a	67	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
43f614d1-541d-4939-9261-f897a4e11e35	68	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
c8fe4f79-263f-4e73-8707-e2da8e4c8230	69	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
fd0c2c3e-a745-4cf5-8d7d-80040a40d999	70	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
15caa246-1b2a-49f0-a5c8-e9701415d977	71	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
05bb222b-a12f-4b41-946a-1b13c90380e2	72	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
550e6d05-a918-489e-9900-86a28f40d497	73	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
09f81352-c515-41db-aa5d-864057525256	74	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
38781f1d-bf63-4a09-bdbe-aef0c3a402ae	75	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
b859d984-04f5-4dc9-99fc-14e6c05ec7a0	76	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
0ec3f0aa-a45a-44bd-81aa-d6b953407051	77	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
779d2865-a410-4127-8d9d-d43dce2286d1	78	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
5444a3f1-a0ea-4917-bdfb-901008b572eb	79	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
6d651d72-89e6-475e-8556-f37995606f3e	80	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	f
04bb6cf6-47e6-4b03-8f99-187353eee259	41	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
fb9090ee-cabb-41ee-ac96-7d1f82e54203	42	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
75f453ce-ec20-4a84-a8db-1e8f818d8069	43	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
ea15d908-fac6-459a-b6ae-004006a2db8d	44	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
0cfc45c6-21d7-4a2d-bb6b-e059d8a441ac	7	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
8a10be1f-267e-4559-9c76-fbddf6e9e236	8	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
7fb93d19-07e1-4e32-acfd-8a5ba07d81aa	9	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
70926cae-4cc2-4dcd-8c64-239fd76a93c5	10	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
bc9cc1d8-a25f-4f3a-a2f8-09db11ecdb47	11	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
62cde638-32d2-4cb2-8c88-99ea1e45f26a	12	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
45b94140-e6a2-4b94-ab50-98a102ca6e6e	45	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
a5700626-57f5-40c7-87cb-c7fd6fad29cf	46	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
bfdf928d-719f-4076-8073-f4c0fce73f48	47	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
f51574cf-dc3c-468b-82ec-10af5779843a	48	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
d378be15-d5bc-403c-bc61-86660dfacff8	49	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
2e091ef4-2806-456e-9b26-04a720018254	50	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
e02a303e-2cfa-4998-86da-831d57878384	19	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
f5116abf-6ca1-451b-8d7a-29b3f3ec0362	20	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
d061380f-f388-4197-b1f5-71932d7b2e88	21	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
64c321b5-c67f-407b-a8cf-18cc58b80cac	22	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
b2356712-eebb-4d47-a603-633595c966e2	23	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
311e359b-273a-4138-8bd5-f12f7e2b6dc4	24	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
5114f705-cfe0-43ee-b803-dc40a15733c4	25	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
f474a303-54b9-4402-921c-fd9ebbb131ad	26	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
7ce768b0-9e98-46b0-861b-6e8f45381496	27	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
aef6e523-7c9c-4b28-b93c-12717febef72	28	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
91c014e2-5f1e-4609-ba8b-850c5d618924	29	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
a2e50ba2-0d83-4a5d-b1c1-0ee1ec1a9c35	30	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
7cf96e06-5fae-461b-805b-857f51de8e77	31	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
a30ee489-bea4-42f7-b9f1-df20bd3432f7	32	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
d91ae063-a686-4f34-a886-f248ae1a01ea	33	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
1b145a62-01e4-45af-aba6-323ad7823d94	34	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
b98b16b8-f6ee-4858-81c0-6b580d0b9be0	35	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
8a8981bc-4487-43d0-b571-2afad1002fd7	36	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
34602555-e105-44e3-a7d1-64125c7532f5	37	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
31707878-a190-4e72-9ec3-6358e87ee58e	38	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
a2fa2971-5524-4502-923f-2759aaeadb78	39	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
a5eb356d-dced-4f33-86cc-a617f80c4406	40	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
4ab4edb6-8625-48e3-a3eb-1943ba561df7	41	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
dd5c2672-faaa-4e57-a69c-7bd156e27498	42	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
804383ed-2c79-4b12-99e8-f14dc91570d2	1	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:06:50.074	t
ad0b4436-226e-44f0-9048-29aa08adb080	2	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:06:50.074	t
bcdb6aa7-050d-42ff-802d-b6b619a0b8dd	43	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
9088a255-dfe0-498c-bc25-a9e0fc30c428	44	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
af37bab3-a156-4e0a-9a8d-1153a3238ac0	45	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
2dcec3b9-f489-4aac-9fcb-b7eb0d515ba1	46	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
23bb39d2-7228-4cb7-aaeb-68617ae17645	47	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
9db28bef-9c43-40ad-9c0d-310636fe5eab	48	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
82bd48ff-c3f7-4566-be1d-85c2b48d8410	49	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
d5a1d1d2-d0b0-4046-b310-7e68df29b00a	50	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
9ff83f85-b7d2-4c15-a963-c140bc473b04	51	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
93583236-b4b7-45e2-8dbb-595d4c7d9a94	52	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
54b1d45a-70d6-4f72-87ee-4a5ef1da3d0b	53	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
ee6dd403-a424-4817-a84d-b9950eebad3b	54	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
1fba7973-ce1d-4998-87e2-aa3b632d43f6	55	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
73d506dd-e35e-4be9-9786-15dc7a00c04c	56	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
ec56f558-a4fc-4b99-b0c8-b70797c55ba0	57	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
f41231db-4ff0-4c16-8e74-9cebc17f4b47	58	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
94a80644-3cc3-42cb-a9e3-6b52137602bd	59	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
3db2d06c-03f1-48d8-ba32-b09f9e528d54	60	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
c6724c5d-3a99-436c-bed9-da236718d8d0	61	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
ac6ca093-834c-4c34-adad-597ab079827d	62	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
88b9dba8-05c2-4938-a089-c9df453aad9a	63	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
81c43c50-479e-4316-a077-23edcbf00a6d	64	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
9d2ae37d-b1a1-443e-a733-a1cc5dbc4e90	65	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
af68ab3e-7720-4f97-bcf6-7c99e4aa347c	66	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
a3c69810-c577-45c5-ae55-f586ca16d722	67	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
613195aa-d881-46de-9390-be1bb982aad3	68	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
1366b782-16db-4db1-b12b-d3abff83121f	69	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
d95fa1bf-6bb9-4d36-a03f-32517a6eb629	70	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
39db39a1-1be6-4b0f-82f5-c601b170e4b5	71	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
792728f9-f9a1-4e5b-84ca-7906390dce15	72	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
18cad03e-33bc-4c77-a6c5-95e7a5e397c2	73	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
bd19259e-eb97-4411-b810-5147f4c52715	74	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
4c51ae34-3cf1-4c7c-96ce-667a9762efc4	75	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
3141eb16-60c3-44aa-a12b-fa20b65411c9	76	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
8f1454c0-717c-4654-93a1-5c084fbbde83	77	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
5fa17299-ee83-4e52-8a21-a25b988360c6	78	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
67771ccc-8c53-41a7-bf5d-e2711c33e7a4	79	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
10d32a2e-b5fd-4b16-8952-5c5b7df0075e	80	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	f
826a7a5c-547d-49d1-8bfc-bc843fe247eb	51	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
f2dab222-42b1-4821-a23c-04fc45254e65	52	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
4b286841-7ebb-4b74-ad00-1bd7e5ad81ba	53	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
28e81849-9e44-421c-bb24-56407e547826	54	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
04cc9b32-4720-4ede-b734-d811e57fb291	55	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
988956dd-aa93-4b97-963d-bcbad4a62d47	56	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
02fd4240-92da-4c1b-9460-440988f1f515	57	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
0b2bbd87-e680-4e77-a180-c047cb1eaf32	8	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
6ca8af8b-7835-4aea-ac78-5d8569cea4de	9	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
2e4beb8f-77c1-4eb3-bbfa-9ea16840d748	10	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
ea2655d7-422c-496f-a458-59fe66528f7d	11	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
7899a0c9-bf21-4e97-969d-1bc2fd76ebaf	12	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
f1cbc231-0925-4b00-a2f8-c4bd32433983	58	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
d4306829-d532-4ffc-bc5a-a4ba28ac9707	59	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
ce111ed5-8544-484e-a4fb-10f00171be9d	60	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
4c7573ae-c894-4502-b1f6-a8fa2973b2b7	61	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
3ae490d8-26be-4042-a2da-9a5fef54d94b	62	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
c31caefa-149a-41e6-9058-ae6a34cbb1ab	63	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
a7a97922-6643-457d-b512-48b9b7f9da6a	64	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
8ead2544-9d1e-447b-861d-a3dfbd82200e	20	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
937f6e4d-0a91-4367-afa5-d1d00b0d788b	21	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
e1283842-94f9-4b73-836f-59bba92ae7b6	22	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
d558e7b6-370d-4106-9b65-69d2c36bbb7f	23	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
2cc2809b-e3ff-4ba4-b81d-a9280c694f49	24	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
aea36b49-dc37-4434-9940-6d9ade6447f7	25	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
03da8856-d724-4513-8165-59c41bc2f59c	26	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
8904571b-be21-476a-ac1e-0a7e8e763725	27	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
6610621c-43a4-433f-99bb-135660d67a60	28	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
ee5707ed-cd93-4323-8321-5c4d8b430a12	29	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
1ca0621a-97a7-45bf-8bb2-c7a2b652aef0	30	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
4884d95f-8f29-4e5c-a33d-e87eefc6b077	31	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
ec06b8bd-f70b-448b-afca-aa06fc0b78a9	32	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
ef6552aa-4296-4442-9b27-04919988880a	33	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
075b6909-91d0-49d3-ab29-e52e985694b4	34	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
1987dd36-873c-4612-9240-b649cc18f54e	35	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
017b8c87-b53b-4855-9a0a-3feba7d8ae68	36	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
b06fb5de-61a2-4366-be4d-21717c647674	37	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
260ba5de-996d-4d1d-a368-d78edcf451c9	38	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
40aa5650-7fab-4619-9989-b0a704ea3a54	39	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
3c725347-e307-444c-82a7-7cb119678695	40	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
ebf25034-7be7-433e-a434-d6f83e8b8ce7	41	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
91a1ad03-8f20-4f2d-bdf0-25ed154a3be1	42	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
057f2ab0-05cc-4233-813b-cf220722b17f	43	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
6cd1db48-9693-4df8-8be6-fe902dab38a4	44	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
6959a56a-eb1d-4e96-a988-713f4f668a42	45	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
7097b59b-068b-4b25-b037-1d8546626def	46	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
92812dc4-b08c-492c-84b8-cd54b9ec1c59	47	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
d820cb49-a04b-41f8-80ab-0a68b92bf6bc	48	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
a4cbf9ce-f553-42c7-a9d7-ac0ca22bbd4e	49	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
e3b1e128-3d18-4333-a889-47eba741a136	50	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
31d53a12-7b18-4fca-8070-9fd29159b913	51	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
fca94ba4-3063-47c5-8c5d-b334566ffa97	52	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
12fd352e-4780-4fc3-9bd4-bf94d6d0c36d	53	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
492d775d-b5a4-49a7-aa08-e4b3d89a913f	54	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
1d431c08-1442-400b-a6e0-d36306588a39	55	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
6e26bfdc-5e96-45bf-9e63-34072b725666	56	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
08adad17-b660-4ec7-ac1e-a705c03c71ad	57	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
3a9f878a-d68d-4098-9bc5-2ed1dab68f67	58	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
6d3efd1f-03de-448a-a397-67f67feddab7	59	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
374affaa-0d2c-477b-b7fb-b0b5e9a534d5	60	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
7c15bd46-e034-47f5-867b-c5290e76db16	61	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
94e1c915-e1c1-4ac1-9c6e-7dfa22db01b9	62	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
04849113-558e-4b85-9aff-06a49df7800f	63	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
6ecc6ece-0cef-42ab-81d5-1f7dba074ba0	64	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
60e0aff2-4e1b-4cc9-9623-69192a681f30	65	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
84b62f3b-1118-47dd-b861-bb0ee6db9039	66	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
a5d03545-05ac-4bd3-8cf0-a36df2b0e136	67	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
3902401a-72a3-430b-b69c-cdac94239e9c	68	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
6cdf7fa3-3a40-4995-bee2-f3009c5d1308	69	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
7dfd372b-1bac-45f7-bc1b-a93fd6c92d70	70	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
8bbe07d7-550c-4ff1-8fbe-73db569f6c52	71	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
9884d834-6c7e-4346-9c3d-2b40f2976d7a	72	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
725f0df2-90c0-4734-b296-7597cb187fa2	73	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
eee9b8be-5d04-4ac0-a12b-a43e539ef18d	74	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
03da2d87-36c1-462e-95ce-2018258b9c84	75	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
604cfeb0-e9b7-487f-b4c8-92bfec49cf69	76	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
114a577f-3d17-4638-81df-f9a607f8677d	77	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
ec1ea8d1-8701-42a2-b740-5f1994b4bffb	78	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
8b50c2e1-0815-47c6-8f59-6cee0ef0c8e8	79	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
c99c849a-09e2-47c2-973b-f14d26f7c6b5	80	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	f
4e2217a5-f349-441b-9d79-fc887256206e	65	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
f20750d2-98dd-4431-b169-d8b7edf715b8	66	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
0e98b5bb-89b0-4681-8acb-6aa24d2cf047	67	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
4f106e9a-1f96-4da8-9f07-9bfe253121bd	68	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
1d72c096-2ce8-4929-a505-c4c4bebe8248	69	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
87f21795-423d-42b8-85dd-b95158ac5e9c	6	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
ac9c9bd2-bcc9-49d8-8f91-ec589786cdbc	7	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
6e8a4eaf-3c8e-4e56-8013-39cf4d8f60f3	8	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
06a49438-ce52-44d3-935d-aca6b587b322	9	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
e18078ff-3f17-4400-9f2b-5f397743928b	10	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
ea0dc705-d12e-49d1-b68a-d5f8f8bac077	11	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
a9155dd1-7231-4921-aced-fa28e88fa36a	12	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
8d72bf47-a70e-428d-8563-dd9fd5f966ab	70	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
ee23a105-77a2-48ed-9b88-63688738a37b	71	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
71f64537-4e6e-4d65-839c-f38cd9274c7d	72	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
6b721b8b-580c-41e0-8418-88f1257bf2ab	73	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
ee87b7ac-8611-4b8d-89df-a8abe256ab96	74	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
04a5adc6-cb98-48b6-8cd6-8ab8368328dc	18	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
420d1bc3-423e-4eb5-a267-9cf7ac222fbc	19	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
58637539-0e8a-481f-91b6-d2226449c8a9	20	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
aca9d7d0-42b6-4064-8c6d-c4c7606885f7	21	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
c8977afc-1573-41cb-9507-544aa1867cd6	22	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
60f85d02-19a4-4417-bd0b-7a0598268721	23	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
5be9b0da-dd93-47ba-be0e-f04cf897b9aa	24	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
9de5566d-76c7-4152-a720-cb1f7003b327	25	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
9d05b4c7-640a-4642-8eea-d10434b3d95b	26	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
f6eaf27f-eee0-4586-b18d-9e15fd50bb0d	27	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
c3e4c001-7751-4389-8b84-da52df7c7ced	28	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
f7b54e77-a14f-43e7-9be4-34e4e8ed3685	29	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
b0faefb9-4cd0-408f-9cd7-6df2963f1912	30	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
d3d7b02d-7c9e-44e6-a181-60a02e25391c	31	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
4a726e90-eb8b-42ee-bb1f-719cdfcb7004	32	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
440b9078-52f2-463a-af06-6063264043c2	33	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
3e4b94d5-2ba0-496f-964a-aff14b82242b	34	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
569953d5-2421-486c-b97d-91139f001d50	35	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
e4743d7b-0b63-49cd-a159-132752424b69	36	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
3c1d8164-9142-40a6-bc84-3e84a95ee819	37	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
d431f61a-7bec-4cb7-9a7c-2c37022f5eae	38	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
25567b4f-f837-45da-931c-9a794127a5ed	39	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
75c63615-82b0-4ee3-8cd9-67faf1d1f9ca	40	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
f762736b-575e-477c-8c99-600005ea1238	41	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
b06b929b-d3eb-45ca-ba0d-677821b5b9b1	42	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
50c2cd59-88f3-4b4d-bb6d-2543f6c2c270	43	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
ac3ba4cb-8f7b-4b5b-bb1e-219192633994	44	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
5585f8cd-77ad-4417-845f-a56a1002cd2c	45	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
88d68d65-a892-4b10-9782-1a58a0bd6d4d	46	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
abc6156d-28ac-40bd-befd-b496b6def442	47	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
3969465a-0743-448b-9eaa-a463351d6063	48	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
ab0fc75e-c622-4ad2-ab72-a4b7ce42764f	49	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
7167d8a1-a3c1-447c-b9e9-c13f5c621b64	50	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
86271da4-997e-4020-967f-dc62aacdfd87	51	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
106d5827-dd73-4fc6-901f-114c8e6f6fd4	52	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
54eefcc5-aa54-44ab-add2-a8b207e4a0c0	53	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
afb72586-5709-4f17-8d7c-f6a89d4ef87f	54	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
20718f37-ed47-4495-8038-516f7b25c9d2	55	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
8c1d48b3-065a-465b-aa8a-07e00ba356c5	56	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
771178da-9ffc-449d-9f61-7a04ebb785a8	57	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
9058ee63-1b53-4430-bb60-f84482e4d6b3	58	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
a3322ea2-cef6-4eb3-b967-9313a80408d6	59	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
9739b250-e237-4cb4-adfd-8c6e9aa579e1	60	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
349898ec-babb-4766-994e-9efe3ca49068	61	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
8b7d0809-a31c-4ce8-b77e-0ea2895ff347	62	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
3b0b541a-0ff7-46e6-ad3b-78bae063496f	63	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
765dc000-d142-4963-8559-daadb05ebfdf	64	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
7449141c-aae8-4824-92f3-a2bda3596b4a	65	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
09210431-64ae-401a-ba02-01231ff17c41	66	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
11cfce89-f85b-4a63-8508-de53d34d39b4	67	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
57c5e2f6-bf10-4f16-a6de-56c53b324597	68	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
7fdeab69-a8d8-431d-a5e0-e972ce5f144b	69	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
8c6234b2-6d5c-439b-afa2-a2e65a350fd3	70	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
cfcf3a54-9f72-4aba-a1cd-1f8d013e23ac	71	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
f15a47ec-d9e0-4d03-bbff-4828ce18b652	72	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
90280f98-856a-4660-92d2-3ecd3e858c5c	73	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
6407e809-c726-4e84-b3bb-41b4b97e1320	74	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
6af48ef7-f945-4b01-8451-829dd71e5ec2	75	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
e01624e6-74b9-4d66-817d-acda93603bf3	76	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
d800d366-99f7-46c9-8d7d-e6eb5836d555	77	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
833bf177-c219-48c1-a0e8-b13be2699dfe	78	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
74dfb10c-8c79-4518-8316-8ab5fb29af85	79	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
59ee0d93-61cc-463e-8ae9-d6add7bf621a	80	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	f
a009d463-4d24-4dd3-ad49-3d5167ec6cce	7	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
c766150f-f4db-444e-a5bf-0b004817f4ba	8	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
f00fd6f8-68a7-4f7a-9573-18dc73344f69	9	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
d3abdfd0-3bad-4318-b59e-c311e2d49e28	10	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
84d1b65a-86c3-4a7b-a4cb-1c0ca6c0180e	11	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
4474e12a-4c85-4a9e-9cfd-473cff075588	12	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
e2d3b00d-dad5-426c-a378-f29ef721b051	18	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
49af16ba-aded-427a-9a2f-671862625d7d	19	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
20de9f1f-da37-4ad8-a300-86f9dfa66beb	20	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
0a5b2e33-502d-4ae5-8771-09d57b553650	21	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
bf012587-767e-4219-a6c8-9a4306c58676	22	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
9a164099-6c5f-460b-b9d8-f6303452ae3c	23	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
d3bd7c62-d880-4595-8e72-aacd3cd7b8ee	24	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
32bd20c7-8d4d-4ddd-9df4-0963e5217ed2	25	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
e92f4cca-da97-45ca-b969-3ebb0e97e20b	26	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
cc8be883-3feb-4e19-bb76-83735de2225e	27	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
507d88ae-d06b-4774-a812-68dcc54963ac	28	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
bbd816f9-ebd9-4356-8417-c0cf4f8aaa8e	29	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
58e46a5a-a241-4859-a588-62350248ae97	30	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
dce8b276-6026-4389-a21f-3aa0d6a8c545	31	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
38fe2bc6-f5e9-41a0-9f13-efa386a03bad	32	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
3bef91ba-77e2-4537-ab12-b980314edd5e	33	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
134e9db7-0887-4e4f-ba0b-ae9e9d4a086c	34	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
a6b2dd88-33d9-4b22-a156-f8f029566b0a	35	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
b2267dcb-2fc4-421b-b330-e49e1a70ac7f	36	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
531b045f-0f28-430d-9336-a446fb4d27d8	37	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
69f9ed7e-638b-4cef-8000-aa7dbda7cdfb	38	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
e5fcec68-82ca-4245-9b6f-c460e5ee6363	39	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
b17eaa94-2a63-41f0-8127-f5cc2bd8e12e	40	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
1e39c035-bc31-49f3-9be4-69b23ad72257	41	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
a4b2724f-af91-4c45-999f-a147c955d1ae	42	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
b2adb289-8ae7-4062-9678-ae04fbdc1da6	43	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
db992f1f-fa2e-4e61-9dbf-061ea8ff6c1f	44	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
0be35bf3-146e-4e52-a085-b65142358360	45	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
390b0995-b22d-4892-a8df-0b097ad91266	46	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
d697e13f-5d70-4f09-ba90-e73d81dee985	47	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
41d54a6d-d9fe-495e-a548-c0fdaa5b7500	48	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
369fa0af-4f92-4f2e-b789-09499e2fec4d	49	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
ef026fbe-765c-4a44-910a-7598a3988f43	50	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
c523cd13-7179-4ced-8184-5350799ab8ae	51	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
dac046f3-590a-4729-9255-85ea3860f8b5	52	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
bab59701-f4e6-4364-a28d-e212f88ef723	53	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
caa8becf-c9d3-45a8-bd09-0508f25c1a21	54	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
d6ca1677-4e92-4835-b31d-58fdd477a29f	55	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
03bf882c-62f1-47ae-82b3-7d012db90df6	56	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
d76f1760-2baf-4c32-9622-76f85990348a	57	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
cda59deb-75b5-4d4e-8ce1-78493cd2b66f	58	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
f7434220-9ce0-4f70-99ed-01a87d3ba7a1	59	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
19a6406c-a9bc-4c2f-abb3-5d724358792a	60	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
81b1c13f-0b13-4ca6-aa6e-ec1d11d6d871	61	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
0c463ab1-7811-4681-adbe-e79ad6c98628	62	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
9e4257bf-ab06-429e-a59f-b1756f4be22e	63	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
468cfa19-4749-4a3c-aca7-7d62e1c2d9f1	64	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
7a89a2d8-1d65-4cee-97e1-664987d7f543	65	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
026163ac-d544-4056-a77e-0865fa03177c	66	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
a1e7e5d5-4f52-40e8-b19a-f610bfe942c1	67	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
9d6845b5-a01b-41b4-9c6a-e9176b5a82b7	68	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
0d569b57-64ff-4349-9cf3-4fdfa53674a5	69	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
18ad183e-0f1e-4dbd-88fe-c1548053f35d	70	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
547b4b97-5755-4b53-90ae-d1fc4e133d7e	71	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
191bad6a-9e28-4e77-b6b2-3e5fa9b963e9	72	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
9188acbb-0ce9-47dc-9734-60bf17cd72d2	73	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
f2c014f3-c665-4270-8d2e-7c549dd25aa5	74	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
c42fa6e7-9030-462c-a825-b93802a4f21d	75	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
2b6ddca9-abea-4f37-8a64-0580ccf44107	76	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
90d52495-bddd-45b0-b059-1a4e67f8aecc	77	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
d2d08d9e-7a21-44f8-93e6-a38c71301ed8	78	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
2eac1788-0802-4881-835f-1af8a8c19edc	79	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
a847bef3-28b0-4ecc-84e1-074fdb07f9c0	80	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	f
7cd1a7ac-7114-4cde-bd37-cd0967a9c4b2	75	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
5cb14568-6f99-4db0-9866-02e8a24c5557	76	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
14ad4769-1976-40f2-b020-aabea6173050	77	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
d3f333da-69a4-4ed2-a9a2-694e5db289c7	78	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
d9cf83d9-cf41-47cf-9b3f-00644dd09af5	79	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
fe3d5656-0742-4e6b-88fd-9fd65a38fde7	80	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	f
5d00b4de-890e-4028-a546-706eb2e5256a	64	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a174db03-2c9c-4a4d-a268-30e874d808e2	8	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
7c8c7688-7dd6-4719-8585-7451a4bb501b	9	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
26605980-d05b-4d59-8884-c309511feef5	10	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
f5bf51d7-455d-4cfa-97b5-d65552a3878e	11	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
a22b5f1f-7779-4748-b79b-d1442e1422d9	12	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
e47b762e-1a3d-45cd-9dc7-73eb647ccbc5	1	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:43.377	t
2d41875f-23ee-48ae-b557-def538d077b3	13	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:43.377	t
0397dc6f-95a6-4885-ae6f-e9285cf300dd	25	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:43.377	t
68297e07-02f3-481a-bf74-3553e36d4252	37	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:19.942	2023-12-16 13:13:43.377	t
68b8cb54-fe43-4047-8577-50d6f7d3cbfe	65	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e999b1c5-3a5b-4b94-be44-7a76c4e053a1	66	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ec75872d-345f-48fc-b857-117b012aee97	67	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6e45f84c-e120-4c2c-bc01-1a618c3fee66	68	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
64152db4-118d-4b45-9fe7-47547ca98884	21	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
7e3d5949-beee-47e3-9f3f-c0dfffccf071	22	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
8f3cd452-f0ec-401e-a7f2-fdc17b02912e	23	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
2d928a30-4db4-4504-bd30-e91f76b107d4	24	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
a50ee098-f3a3-49a7-9199-0edc2de85743	25	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
27b9362e-8136-4271-94e1-5aba57f71a59	26	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
99a7d771-114b-4d06-9537-060a28555e19	27	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
a0ffb3d7-4eb2-4e78-a0ff-15398895ade8	28	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
d13032fc-0305-451c-8238-4b9c04bc3228	29	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
523ed7e2-86e8-427b-9f1e-94d66d5fd6f9	30	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
b7b391c0-9ade-4a53-a9e4-32c3ce7600e5	31	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
fbbd5454-7a44-4cfe-8ffc-e0eeefdc55d7	32	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
35113116-7150-4502-9f1c-145d4ced01e9	33	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
bd271d12-58d4-492c-97ec-e4ad51b2c1d1	34	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
651a6bbe-46b5-4895-a054-4fecee7a60c8	35	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
4822670d-bea1-41be-9605-5a39d1776365	36	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
a1dc1d83-63d7-462b-a55a-ac7d2d7a6062	37	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
e877a26b-28e1-42b7-9742-88340a7a7ebb	38	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
b9f59e61-39c5-4c78-ba46-7056b91ce24d	39	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
aafce174-2bb7-4806-80e5-f705fc347d3d	40	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
c722de12-f952-4a2d-888b-e144341a60bf	41	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
7bac24a5-8b70-4731-a19c-3c457d86cda0	42	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
1f1c9a45-ebe3-4669-858f-e604b67357e8	43	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
ab8cc2e7-372c-4aa8-9db7-dd23d62956eb	44	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
c5f15be8-0f80-4f6c-8abb-a71de4a9a1a1	45	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
b3fb8a9d-76f9-4be2-8040-0e5fc216f879	46	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
6def6cd8-2713-4e12-bca7-881bf27a87d4	47	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
c4732275-5b5d-41df-8d64-1fb1f7b60a4d	48	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
a52ed96a-1ad8-47eb-be15-67b7124c8972	49	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
411b0f54-f000-49d6-b4a9-53e000be7076	50	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
70080771-5cb0-4dab-868a-bab973ac3bf3	51	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
9104c6c9-7257-4622-a0ae-7ec2712804af	52	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
5bd08c9e-511f-4f97-9531-8956e67aad07	53	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
99a0c523-0762-4677-b6a3-6bfb98f0a8ea	54	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
53d183cd-0f24-41e9-983c-7641152ca599	55	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
8077b7c1-54e4-4d9a-b28e-1b2ab9f87ff2	56	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
486a3c82-f578-4c29-874c-4078c8fdd706	57	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
12ddc43a-7775-4dfa-9e15-6291b762b4ee	58	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
58e429a8-4575-476c-8c78-22f678872bdf	59	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
344fa6ad-c167-45ec-8409-05d4afd07d8b	60	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
17152698-64a2-4841-9aac-85c9d06616bb	61	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
c3ee8611-dfe5-424e-a730-47bc92ad2aae	62	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
4acbe62e-ee25-4cf2-b271-ba78f2e59c99	63	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
4bb47440-a5a6-422d-9dfe-fb46b97f4289	64	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
e0162aba-febf-4eac-888a-521ca6fdcba0	65	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
f5f46c50-7ef2-44da-9a51-69b5c1bd469e	66	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
e6d3a4ec-4a71-4598-86de-f673ad9cedf0	67	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
1615742b-c84f-4ceb-941d-d951d8760469	68	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
f725a3a7-ff78-42ac-a9bc-153b03f55631	69	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
48f4254d-ed5a-40aa-bad5-02fd83639e94	70	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
43c82669-fad0-4d18-8f87-43f55c821cd8	71	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
1eb148cd-71c1-4531-84f2-6171084895e1	72	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
444956a2-b9f5-4c1a-89ad-c57ee7e5449c	73	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
dac0d8a3-ecf0-43a6-9cc5-e6058beb1121	74	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
63868a73-6f6d-4c7d-8b50-e8b1c12aea70	75	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
2df38f72-3b12-499b-a32b-e94317c7eee7	76	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
e4122b5d-81b5-49d6-b6b3-200b6269aac1	77	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
4c90852d-d65c-452a-b87b-c7b40201381c	78	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
23eaa114-7b5f-4bc3-9c53-62930cc9addc	79	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
f606fb25-b0cc-4e17-adc6-f727ac1974d8	80	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	f
b570a59f-58f5-483f-be21-fceac1ea0dfb	1	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 13:03:13.284	t
ac9ef70d-1f8f-4145-8463-62b406790af7	2	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 13:03:13.284	t
9c14b96f-d79f-4611-a38b-ebb7b15e5705	3	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 13:03:13.284	t
488f4752-e829-4ed3-bc62-aca167d86c9b	4	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 13:03:13.284	t
611d2bb8-d232-447e-936d-b61466946c35	13	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 13:03:13.284	t
b0bf87ec-a27d-4c22-9a9d-95b5715570d2	14	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 13:03:13.284	t
5eaa4840-0d64-4b5f-8266-8aeea9a2c964	15	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 13:03:13.284	t
f392f361-a1e6-4f9a-ad46-bf85f7d808e0	16	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 12:59:37.936	2023-12-16 13:03:13.284	t
3e173a35-cbd8-4170-a4b3-990ac55030cd	1	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 13:03:53.675	t
9b1a6a34-a3a7-4085-87f8-fe965b5f2c89	2	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 13:03:53.675	t
90266e40-980e-4424-acca-3cadf527db93	3	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 13:03:53.675	t
18987e91-ade9-496b-a3f9-bc1019ae7e71	4	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 13:03:53.675	t
77dcf1f5-ee6f-4081-9329-798b43d9a255	5	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 13:03:53.675	t
907961d8-a636-49f9-9923-f8201940d9f0	13	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 13:03:53.675	t
17c9ed24-28fc-4ef9-9a61-1d2df28d8280	14	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 13:03:53.675	t
439d347c-6ffa-4d39-a4e5-d85421b625c8	15	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 13:03:53.675	t
d01d491c-7c1f-4d6c-b48b-21d79c31b736	16	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 13:03:53.675	t
e846c3f9-624b-4cd9-8191-110e4647c30e	17	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 12:59:58.137	2023-12-16 13:03:53.675	t
2de60cbf-2ea1-402e-bb87-884f3af5f489	1	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
c34ef804-d20e-4ce0-aa16-e0ce921f255a	2	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
2a073f70-0664-4a59-a2c8-5c4d5e5e8906	3	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
9963ef07-8bb8-471a-91eb-b321c1b1b346	4	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
946261b3-2599-4e58-9598-6f9f75ab2e34	5	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
38916e05-c3c5-4adb-b43b-79662b4fb8e8	6	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
ca1e0686-cac5-4b2d-9da4-40b5ab487bc9	7	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
87299a5e-0725-4f32-857b-a5c2324874b6	8	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
8bfdd61e-65ad-4323-92af-bd95460dbc44	13	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
c458313b-309b-4207-9e28-253473356171	14	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
d29b968c-06a5-48dc-ac93-602d0d3f9eb7	15	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
59fec709-620d-4fa0-9cdf-5a8b8398ee7c	16	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
edeead5c-2f42-4e13-b0d3-1e4f409b8b1e	17	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
e95aaa71-e002-4065-8071-27f649af24b8	18	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
2c3f267d-fe58-49d8-8e0c-150cd5dd8a3e	19	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
8355bd2d-ef54-45ef-a9e2-84c390e79365	20	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
17bb5fa3-dce8-4d6a-84ce-478d43a9a082	21	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:00:13.509	2023-12-16 13:04:34.385	t
534c3405-b01b-48b7-bf0d-ad96a3c6dbc3	1	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
dc5d795b-d896-43cd-bc03-5a64c266bd58	2	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
eb0ed4fc-daeb-4252-bf5e-e319fff061ce	3	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
de0a8570-ca7a-4d3a-aa4f-9577a1cd7136	4	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
e897474c-d56f-4358-97ba-a0da685d8364	5	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
b2b2d6f3-5b12-4b4b-9b96-98083944a6a7	6	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
b4c49157-9f8e-405c-b06c-bbbdbacfa5c3	7	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
dc303c16-5326-49ea-b249-1c76433f1bb3	13	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
b6d8ecf6-cec5-4434-a2bd-e335ffae522c	14	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
7750a9f4-10d0-4030-bb56-65a598a6c8cb	15	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
69cd280f-8b87-49da-93e4-035e4b43bf76	16	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
e5ca9866-5484-4fa1-ad60-f35e9ae17d44	17	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
bb2bc513-b383-4a1e-bcd9-5f73d8acc389	18	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
286ab23f-8007-42ac-b7e1-56d02b3e16f9	19	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:00:57.198	2023-12-16 13:05:42.629	t
e282df6c-1468-4c75-a808-4a7b0fe5bab2	38	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
bf6ebf90-9c4b-4177-8b97-8de77f48d8f6	39	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
4ccf84d4-d6c4-4b5d-880b-e023edab992b	40	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
b80c5ded-6925-4229-96c5-c5b94d29cdeb	41	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
3c2f5db0-3d8a-4118-a211-076ce43b68cc	42	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
5094a40f-a504-4a9a-8c77-bb0d62b49960	43	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
eb253a43-c244-4d45-b9e4-137a4ff0104f	50	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
39bc4b3c-328d-450f-8095-abe7134805e9	51	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
6cb2a0d8-19fa-493e-88b9-3060ac8b7220	52	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
0fd0e985-d930-4c94-9405-33d02a44c434	53	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
a797b3d5-35b9-4c9b-a21d-19d29b466355	54	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
ef907580-cfda-40f0-9fd1-80feff5dc473	62	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
c48c54a6-108f-411f-924e-584e539e6dee	63	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
5429ebce-87e0-4b2e-bc8f-7b93ff95eddf	64	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
04334099-6fdd-4f68-86f1-ae64a7cba350	65	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
8b1b2b55-54fd-48c6-9751-083df096badf	66	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:01:13.145	2023-12-16 13:06:17.802	t
38a54d91-12a7-487b-a189-571b451195d8	3	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:06:50.074	t
d6a03a69-85f1-4f04-b147-b7fd48c3975b	4	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:06:50.074	t
a2605c39-1bf2-4f25-9332-d8dbed58f9d5	5	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:06:50.074	t
53df4a6c-f999-4322-bbc1-ec599c66622f	6	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:06:50.074	t
3a371c99-6c62-4c2e-b130-76fb0d78ccb5	13	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:06:50.074	t
b40a6a28-9e78-4154-962f-b771fc6b40bd	14	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:06:50.074	t
68b1830c-52f5-4257-938c-96f3c0505f78	15	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:06:50.074	t
a6503b8c-03f9-4685-b97b-f55c98390993	16	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:06:50.074	t
e5b72f4c-92ff-40ba-9031-bd28830dbc59	17	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:06:50.074	t
25ad8a66-a381-4957-9662-3e7edff3df61	18	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:01:35.679	2023-12-16 13:06:50.074	t
bee9b5cc-5cdc-4d2d-b0fe-80139180b3f0	4	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
b1c00bf7-eedf-4ae7-bd8b-43beac745293	5	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
2121b7f6-7827-428d-9e7f-60ccc81f1ba6	6	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
602deaeb-07d3-4a93-b5bf-ae00ae34e034	7	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
e23260c4-5aa9-4464-9bcc-7cd3636bb426	8	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
00c54004-c2df-43d1-92dd-22f5032fd570	9	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
d5482c51-50de-4975-b7fc-bce5f5f09c14	10	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
a789d983-6450-47e6-a080-a6d4f9b03272	11	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
87ecab4b-73d8-420f-bb6f-f3e9bf09c8f2	12	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
7fcb773f-3cc4-4e59-bc48-93674393aa00	13	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
794d6b1d-1da3-4a44-9b8d-bf8d137c1556	14	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
15c07a1b-4bcc-4fdd-9c64-2c6c60f3bebb	15	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
5aa3e312-d34b-49c3-92e0-3d06b1a477e7	16	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
2dc66b29-7818-4685-b803-d9e111d486d6	17	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
320afcc9-cb4f-4590-8bf9-64d1c1866d0f	18	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
4cfa51e2-b6ef-48e8-83a3-0a2cab6c7710	19	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
a4f0c95f-1ce5-4001-a383-c203d460e175	20	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
fb3a30e6-dca1-43a7-9f0a-470d3970dcd8	21	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
1fe0abbe-2b9f-457b-9b05-e3be0ed0a13a	1	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
a88fdcf9-ae54-4cdb-8d4e-3ac233a3be6d	2	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
855f3e1e-c870-4c42-a066-f4802baef4f8	3	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
b54155ec-818a-4af3-af56-eeff8f248c8f	4	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
50c76e6a-ec94-4149-8a37-5175b793b4f5	5	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
8b346745-e90e-4303-8a6d-e5155ca5e0cf	6	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
347bdf3e-7ba1-4e29-a87a-f749d36f45ab	7	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
427ac96c-3643-44a1-8e3d-8f2f9b3c1401	13	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
b378a397-51bf-4137-a5c0-e8595c6dca46	14	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
797b4c7e-49b9-4c39-93e0-182129f62170	15	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
997be155-e211-4546-8b45-b81781a7d0d1	16	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
74f4a0d7-2d11-4872-a48a-b390bdf0ace3	17	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
ce08d286-7080-4fb4-a4cc-68b8587bc1a8	18	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
bb386255-ed95-44c3-b853-fa21c8651383	19	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:01:52.628	2023-12-16 13:07:13.642	t
41691fa2-46e2-45d9-8eae-bb02eb6cf7af	1	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:07:43.756	t
8c557937-6b78-41f0-8ef9-166da8957c82	2	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:07:43.756	t
3d56b484-fa0d-480b-934e-6815f1e43fdc	3	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:07:43.756	t
e605988f-0894-453a-af15-398a6bb35e4d	4	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:07:43.756	t
a1acef23-a8cd-4747-aafc-0bd46d0580f2	5	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:07:43.756	t
bbea5747-2396-47bd-9b27-157eeee01d4f	13	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:07:43.756	t
2324ff5a-c3fe-4e96-a828-2281f2b2e436	14	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:07:43.756	t
7e14fda2-2dd5-4d97-80e1-e21d2995dc76	15	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:07:43.756	t
a83a4d09-643f-439a-b43b-1d42e00b0e8b	16	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:07:43.756	t
f0dfd02c-8fe7-472d-8282-53e746128df5	17	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:02:18.61	2023-12-16 13:07:43.756	t
a2783dc0-f627-4945-972a-b28f5ca5313c	1	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
222b6ad7-c490-4a6a-8f43-2a41c4483479	2	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
a64d7a8c-a8cd-455e-bfe3-44f91d0ce47a	3	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
49a14d2f-d9a2-4b85-b3e6-863ec5d96c99	4	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
1f4a8ccd-9834-4bcc-889e-74eaca3acf67	5	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
d317faef-98f1-4a91-aa72-eab2976d03fc	6	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
5f526b6b-5785-4c42-9049-3285a2b0f4ad	7	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
57d30923-049c-4f43-9d87-f1d1d1e95b11	13	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
72bddb2d-4219-467e-aa8d-e1439ad33915	14	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
063d2e7e-cf1c-4048-bf82-27e722b1c189	15	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
1a1400fa-58ad-41cb-92aa-7f0a69a95dde	16	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
2e5dc174-009f-4912-8df9-dbba37551327	17	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
3514aecb-529c-4263-a8c2-2c234ee877be	18	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
d22c0820-2150-4835-a459-0dc3ca3b083b	19	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
71922c4b-cc7e-4d51-8298-4f946c3f32b7	20	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:02:48.33	2023-12-16 13:08:26.319	t
bc450737-6b3f-497c-a82c-d800e3d3efcf	41	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-17 20:31:17.397	t
748faf37-4080-41f7-993d-4fc632f6b29d	69	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
75b05b1e-6f44-40e2-bc45-b3c156bf2b2b	70	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
89f0ac8e-7f0d-427a-a1de-d28e2be5f3a0	71	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
678d5002-019f-431a-af0d-6a8d265ee103	72	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e19d020d-5e76-4445-89e8-2e3015e98845	73	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8ecbaed1-528d-40d7-ac45-b2e4c9229777	74	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
feb2eebc-f115-46ce-8bd2-6bc8af04af2a	75	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b66e9c23-45a6-4adf-990a-47c3eaca67b2	1	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6685ccdf-e460-4003-9189-e1525bd5e910	2	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c28b0e2b-abdd-4e3f-94fc-ac34e600ddc7	3	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
22d1f02f-ac7c-433e-8991-022f6e81f6f8	4	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
caf081fc-086b-43d8-bcfd-74671fc61737	5	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e4087ec6-f458-4079-b102-4885c0b1ab1a	6	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
69c86d8a-3c95-4bc3-baf2-d214a36d5d33	7	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e0c871aa-e7fd-44e2-a9c8-b3f817d1b722	8	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
268a1342-a4f0-4187-b27a-68c3788ee22a	9	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6c3e2a18-2d44-4bc2-bdb0-3debb8976c3f	10	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
927871b9-7072-4239-bb43-33490da2975f	11	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
189d8719-96a4-4eae-966e-26d27b781051	22	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
03090e5a-adc1-46f6-8372-e37821825896	23	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
45e2ac8d-0a7c-44f5-892c-5172e40b851f	24	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
431f50eb-71c5-4969-94c7-bf9c65d52fcd	25	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
df2a5aa4-a66f-4c56-bd89-4cad848bade4	26	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
10914883-0479-45b8-ac1d-1056f029198e	27	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
e41777ef-bb0f-4103-b003-bd8aeac2e8b6	28	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
351f9205-563f-4d78-8fa3-f14ab938b0fc	29	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
42adfbdb-454c-42db-a376-443ebfbb24e4	30	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
25593e27-1c14-48d7-bc2f-86b6c4d7e3bc	31	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
4446d325-3d7e-40f0-bd24-e8697ac3b3e7	32	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
ed544aa2-753f-4192-b02e-787527aa0118	33	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
76cb1b81-af36-4fe9-b540-d25f0854058c	34	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
322f6388-e1e7-4b99-b86e-6fdd4628c1e0	35	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
2877cdb8-1e4d-479b-a4af-2c92563a9fb7	36	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
a0457743-996d-4265-aae2-c5dd639ae3a7	37	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
721bba0b-3ac3-4836-b0f8-104ee7b6a886	38	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
6bd03485-a20c-4b0c-8326-ba29b4cbd131	39	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
4d928c59-a15d-4cc6-8270-7b0210a14542	40	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
e6130c95-9b5f-4a07-888d-426d113ad95c	42	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
3dee8075-2d01-431f-822c-0081e94dfae5	43	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
3dd5c4d5-bac1-455a-b6e0-f8403915f364	44	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
cc9c9f94-cf31-4608-8a8e-ad91ef5cb377	45	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
d153c398-41ee-4abb-9e1d-01e1423b5e70	46	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
29d7b9ec-bb29-4599-b791-a31395f4c215	47	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
ad0358f9-196e-4338-9d2d-3bf3c423e162	48	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
738a4879-fc80-4502-820b-ca9c087155d9	49	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
2b31849c-950f-43ac-8a4f-b9a1f5cd29cf	50	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
4ed2866d-1e4c-4945-9e6e-ea34ddb6e1a6	51	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
2b3b24e2-5b8f-4b7d-bda1-e7e066c520b6	52	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
c41da5b0-fe16-4711-8f85-668fd0795c7f	53	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
b83c740c-e45e-4c48-b41b-d0055e6f4f9d	54	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
8255c22e-8694-40cb-828a-ecc558b5fe8e	55	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
5441674f-2325-4128-9589-076f75218401	56	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
44b8cd16-e1a0-4b6d-b116-edfd96750091	57	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
22c93508-1f5a-4820-bace-e9d7d49499ce	58	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
9bd75f07-e045-4cbd-acba-ce1f14777d39	59	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
a885e63b-8a91-4d6f-b610-a660d64288e6	60	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
4d8d5ad7-f1ef-4148-80d4-509dd187db1a	61	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
70572f98-a955-45c9-a404-dc07c16bae05	62	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
55c9e9fb-79fb-40a5-98f9-5d7294ea1760	63	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
114578d2-eb69-48b6-af76-733e9899becf	64	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
e88f7aa7-32fa-492b-a7d4-2f5dee528db9	65	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
117c4b68-9e94-4f9c-8cfe-070059570bad	67	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
09bdb462-9ae5-4bca-b55f-36d4c33a9e92	68	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
cdcd79d9-19e7-4d56-bca5-b416a4d9b5ae	69	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
c9d63127-c2f4-4c10-af87-556c7e94b35a	70	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
5e33237a-6afa-47f8-afdd-0acddfb5a4a1	71	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
5280f13d-295a-42be-9bfb-cb7f20c66299	72	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
8a8e25e6-c93c-4b3b-ab2b-e15b8114440d	73	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
5d68f720-18f1-4c86-a01d-397eb8b3c4a0	74	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
b2a8262f-0883-4a84-bbc5-7ca3cb63fb6c	75	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
8c9dfa29-6ce5-4028-bebc-ddac5cf3c349	76	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
df0d244f-4a40-4bc1-8544-7caea7eb2cf5	77	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
fa723a63-f389-42c1-8600-8c6ada290e48	78	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
2b6d0332-e65d-47b0-a3ea-e3630bab2e1d	79	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
ade5b683-1220-4c3f-bb59-efcac17c7f96	66	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-17 20:33:40.299	t
33875d45-b6b3-4863-8aea-90123bb4316a	80	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	f
89d72b69-067a-44a6-91e8-a0a2eb791704	2	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
758bcfec-c85d-4f95-b11e-4d10d33eafbf	3	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
396d3807-e5cd-43bd-a327-4830753d5c11	4	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
3f3d56a9-04d1-4c8b-8500-b5d64988efec	5	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
1f36b01a-2532-4611-99cb-54fc3479b37d	6	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
d36240c0-2119-48a3-a88e-89143e8782ad	7	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
00e0bfae-1a4a-41cd-ae66-346354510321	8	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
3a39d73c-71b1-4553-93c0-3782b5906660	9	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
6964fa16-6286-478a-afb8-608e42c6f100	10	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
4bb4afc3-a618-4475-99f3-1dd5b8ec3b85	11	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
f01ca1bd-7043-457f-8aa3-561dabede68a	12	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
a582aaa0-06ff-48f9-af4a-9c33f0fcfa08	14	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
6013a1b5-a0d2-48f9-bbd5-20aa103bd7e9	15	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
ad22918c-2141-44d3-ad18-38f2d0bab5f4	16	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
824d4de5-bec5-479c-a1f7-1efd5e382575	17	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
d837c3a0-0018-41f6-bd79-8790969f000a	18	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
86309dec-e472-488d-ab1c-c73e1a882fd9	19	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
f4515e35-78be-4445-93b8-262700095dc4	20	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
d64ea6c9-9cde-4fd0-b32a-387f36a43b17	21	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
3d7f7ed7-b2ab-4c89-98f9-3115da0d9243	22	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
9c712efb-a06e-4075-ad09-201cc2070653	23	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
386327b8-4818-4499-b1e0-ac04ffcb7d7a	24	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
0eecfb05-cce8-4d91-a80a-7b00f307fac6	25	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
2055848a-1a2b-4176-8d81-e2fd2905c38b	26	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
8dbd1da6-aa68-4062-9330-fe0e651a83c5	27	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
aba32c37-fd4e-4296-8630-becd6e05336c	28	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
06fca525-543f-4e6a-b083-a134f7f5dce9	29	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
1934da4f-c1d7-4835-8f06-53eeb844967a	30	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
fb8ffe9c-a09a-4227-89a7-9fc46ea58699	31	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
e96185b1-0a79-4982-ad5b-ee0804859231	32	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
3dedd14c-42d4-45f8-a734-b0bca00cea91	33	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
3b32da16-675a-4736-b037-3986ae501c4b	34	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
7394f57d-ab6a-4b87-bc7d-a252f3846891	35	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
672e71c4-7711-42a5-86f7-ce00578a98fc	36	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
6628f1a2-13fd-4459-8127-6de53b29320a	37	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
36c64503-0191-48a3-9950-79995152772c	38	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
901226e3-6e78-40ff-a4d6-90b2f786eda7	39	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
64260d94-eaab-43cf-acc9-75194ed0205e	40	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
7134b7d6-9c4f-4098-afc3-39fd1cabb4dc	41	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
6a6113bd-0835-4805-b4e0-b992c9100781	42	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
f85365a1-e44c-45c8-ac53-510698f92055	43	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
15e7ecd8-b705-42e1-b40a-6ac9f26b1cc2	44	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
74ff218a-9758-420a-a45d-7d69369ca6a0	45	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
b32454e5-8391-4271-a7d9-752183d4da4f	46	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
67efeb01-5ac9-4977-9f56-349e00cb1eff	47	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
b4e5f873-b145-4e10-b12f-4ab59fdd0b9f	48	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
f084546e-2fd1-4b15-a2cb-d5e6fff96ac8	49	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
816eb567-18fc-4dfd-a47f-46f10d3b2070	50	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
4c3f5728-ef9d-4afd-8fa5-dd879395702d	51	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
f830d8b3-095d-48ff-8167-01a4c82170a5	52	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
b7392e24-bf60-44cf-9d12-215c481bca44	53	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
af914a3f-7bcf-4f60-89cc-1cabc8a7170e	54	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
b9db706e-aede-4798-bf0a-9699cd9d56d8	55	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
aaf8db3e-ad3f-4b66-8bfb-37e70e318605	56	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
1fcc722b-f132-473b-8019-ed06be0e5933	57	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
20e7c53b-a40c-4842-bb0c-556fd4fab6ef	58	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
1cbd4c4d-eaf7-4229-969f-bc33f622223a	59	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
73729e1f-e07c-4f18-b128-fdd70cc4f1bb	60	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
5afde958-d6bf-4032-ad7c-bf7ae1e34d90	61	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
b9cd979f-27b9-4760-ba3d-3b2b12ada5f3	62	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
b9685f44-20d6-448f-9564-dfd09519d4da	63	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
99e2eccd-d0ce-44be-8c6c-9d224b700477	64	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
fa411a55-4b37-43f4-99a9-1d78f9f63d0f	65	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
28e43fb8-051d-4f0d-9e91-3f28178191bf	66	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
e74d7408-8958-47bd-bdc9-52447f60a131	67	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
40069321-fcad-4157-a279-28797fdcc4ae	68	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
99f90c6f-8260-4142-9c5f-4c7f237c0452	69	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
79dbdfa8-0537-42a5-82d5-0cefb5846898	70	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
c09d60ff-4925-4ecd-b49d-30ab8a02ffc0	71	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
186e1855-6d3f-4249-9a82-681be3a29e8b	72	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
eb30b2e5-5334-4b32-a56a-54f6f6b2be87	73	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
c244248d-adf3-4bcc-9c44-0bb922dbb472	74	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
b0b50239-c0ca-4b2e-afc8-a596fd1228bf	75	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
68873660-d2b5-4966-9d6d-702b54b60f8e	76	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
daab97c2-bdb2-45c3-9d35-990c1d8a8687	77	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
0277a640-0d36-49d9-910e-d8f8b75aa84a	78	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
1a6d924e-9197-4c44-bb56-91c4590e82e3	79	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	f
da41d9e3-acf5-4805-9d46-56f397ceb5de	1	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
ac0beb61-a694-440f-bacd-2a618530cd56	2	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
dcf77959-13df-4b80-aa8a-d90def3df8a8	3	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
1e7d443e-2773-4673-988c-1fb082361cf4	4	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
7e3d696c-a89f-4a40-b3a6-221d84e4838a	6	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
7c47c05b-8156-4bc8-a439-4de0765da28d	7	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
e432db0a-64a4-47a2-84e7-624bf907b858	8	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
bf843d8a-d08d-487d-b67d-6637d6749186	9	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
aef85983-ed24-485c-9007-fc21f31a91d0	10	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
128d648a-0b8c-4c8e-81b7-271787b7a53c	11	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
51e4c062-2aac-4ba7-9cb3-977d2b8c5a47	12	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
09d4b877-a031-47ba-b493-8c66d43fc0fc	13	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
b3eadfde-385d-4e2a-b837-e0c82de6e1c1	14	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
c38c56d3-10d9-4f55-91e4-280ce4af2ede	15	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
5557e99f-1c4d-4e75-9a52-2f3b00992974	16	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
e0098268-4a35-42a4-b3c3-9e70e3acf1a2	17	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
71a83741-4ab2-45ed-a1e6-23a4b38b2c21	18	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
3f95dfa7-af35-4bb7-91da-b7ba3cd267cf	19	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
28496bc6-9bc9-4ed8-9082-edbe3132dfe4	20	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
260e9ee4-52ed-460c-a51f-2eafe6c36785	21	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
c8626672-4b98-4a8e-a33f-d4a5f9e20d98	22	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
0b669036-07ba-4cc1-8264-36652cd51410	23	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
6151600f-5e48-46e5-80f0-a3c57e61dfd6	24	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
7e95cab4-505c-42c0-ac60-47c53a5cfdbd	25	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
5dc4d255-2670-4d19-91d2-5aea8fec08e8	26	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
e6e86bd0-aa68-4354-b10f-48d1c3c66308	27	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
c00fdb15-4d83-4a87-9e7c-0eac315ab9d8	28	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
fbfc0f70-d711-4147-b9fd-085c4e56fc39	29	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
9ce6adde-c044-4026-9bd3-365304892018	30	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
9efc0873-639d-4e51-8e07-7c7234a2d892	31	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
2b28ccc7-531e-4e8c-abd1-acd2de3e44b8	32	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
f6eab3b9-f892-4c28-ab65-0e100348437d	33	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
4258e448-4eae-4b9f-816f-98c1c0f23ac1	34	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
3f4d79a9-83fb-4255-a8ff-109caa50be69	35	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
2c3c168d-9148-4b16-94fa-92372b8eeddd	80	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:09:17.169	2023-12-18 02:42:43.517	t
a0948832-be92-4b35-8e49-1ca5f52aaacb	36	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
02a8a817-fe30-4a9a-b080-c16a1e368a41	37	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
382aefe7-5a64-4712-8779-e6a6c48f095b	38	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
c3c8164d-fb6a-4ab1-b911-6177ac8f3b92	39	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
6e680258-3e7b-42af-bac0-6f9aefb84178	40	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
940dfe3a-553e-4bff-8564-ab660d7bc8f2	41	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
ad545124-7246-42cc-aec7-f482c4e1e762	42	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
f44dedc7-a978-47ec-a22f-25128848cc10	43	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
b186a4d4-d5ef-4aaa-9925-ee3534f9222f	44	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
3b6c900d-0b3f-4dea-88c7-1ef4d20c35eb	45	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
da2a28cd-6fb4-4ee8-8cb9-eec18db041e5	46	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
31125e1d-67da-4831-82ab-327286556919	47	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
9c7f631b-7712-44ef-b18a-0bd40e2298b9	48	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
36120963-65bf-4c76-a47a-14dae663dcb8	49	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
b422df32-cd6a-456c-a7c6-c71e1f0619a3	50	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
f9334972-13ae-4be4-9d6d-6487bfcecd21	51	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
bb4edb2e-0200-413e-ad8d-eb145b5d4409	52	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
3d40e197-bbc3-4ac8-86ae-7b8ea991402a	53	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
65221465-801d-4fd0-b387-a25aa0dfd4d4	54	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
c624d5d8-9139-495d-b6b1-7873dced4817	55	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
b752433b-44fc-45d1-8e70-d1aafcc34db7	56	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
6c630566-4ce5-40f5-8571-22e2ebb20dfa	57	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
df5db7bc-6659-4f53-97e7-63fe7a4bff78	58	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
4d2b3673-75c2-4ee3-b4f6-67e1fe7f92a6	59	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
8aef8c08-d72c-4a20-83ac-e8484bac9973	60	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
7a1dba33-73e6-4292-a4e0-0ea1ded8470a	61	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
5daffe10-4efe-47a8-8896-6920cbb161a4	62	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
c234e846-0d3b-4441-a579-c0a9e998b3d0	63	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
9d17c59d-44b0-4258-9431-747e91eb5ed8	64	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
ae1a8a3a-08b9-4049-8762-7f9629935697	65	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
44cec239-7553-408a-8a28-cec6d32270d5	66	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
b78b15e0-1985-4abe-8d87-514c08b0eba0	67	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
ac2341f9-189e-4094-93a3-6d901d14d4b8	68	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
46ffd33d-b07c-42c3-8701-ff436d116e97	69	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
c420d09d-659f-4f53-a53c-80c6cba20884	70	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
4ce6669a-dcb1-406d-a8e6-f2721b2445c7	71	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
0fd199af-b107-4235-9218-343447040bff	72	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
2d78d972-4472-41ac-8d35-0f233207f4be	73	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
8da7b5a8-e162-4fa8-a76f-1ef919879db8	74	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
05a735e7-9e32-45cb-acf3-a8abc33c8c82	75	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
3b9075d3-ddb4-4a1b-83b6-47348c2e15db	76	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
4e5f5a40-bc3c-4c6b-91c4-f857f0d3ef8a	77	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
1dfa70a5-293e-431d-9ef5-f312f08fd10d	78	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
b677ec03-c1c6-44c3-83a7-5df37803205c	79	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
6160702f-7fd3-4aa2-bd66-607596f271b5	80	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	f
32a7a3ac-dc05-437b-b123-7ce1defb099c	1	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7b097b93-c6aa-47ba-9048-9af72bc26f17	2	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
95c9f8c6-9196-4e35-b84b-b94f38e815e7	3	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b47ada9b-d7dc-4cb3-b1b5-7ecee66a35f0	4	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
dfd39334-bee0-482d-ab83-4f8303966a57	5	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9e8c5d86-752c-4ebd-9204-467637de37a0	6	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c16a70ff-5379-4be1-a2d1-9debfcf05f09	7	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5d001060-9cbd-44df-a66a-65ba42c869cd	8	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5099e4d4-4cbe-4564-8b9c-1429160c0784	9	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
163050e7-74bc-4d09-816d-37eb7803af66	10	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c5f6d791-01a4-4f29-a4e8-f530811a7d85	11	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5b16c421-26fc-4f23-b43d-27d960bcefb5	12	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2edfad94-a7c9-494c-89fd-69ca3c60f446	13	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
fe24da7b-d582-475c-b3c3-a20389126eff	14	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ddfdc5f3-efff-4d77-a61c-9703baf482e2	15	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4dab0667-f18b-41ba-a23c-787d02df88ff	16	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d40dee96-b65e-49b6-a2b0-0f8f7254f712	17	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
959866a2-de17-46c5-b098-67481a8ce834	18	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
dd0c60ab-98cb-49ca-8122-339916106fca	19	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a3f65f89-1037-4eed-bdc9-ba67426fef4d	20	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
bb5ecd7d-1b82-43cd-9bc9-c64ceda34ad2	21	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c788ddfc-5256-4a25-8486-e6c4b1db3523	22	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
17276545-5934-49e0-9503-ec2e58b4eae0	23	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f6c7c593-bd68-4558-bfcd-7ae5a6f46fd8	24	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c530eaf8-f99a-40e3-8b63-3395c6dc1e52	25	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b8cd0cd7-6861-40e2-8e6d-5c1db72b2f17	26	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3693fdc6-79b4-44af-971f-73d059164248	27	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3d0a9100-ad49-4418-be3a-c3be7d52d3b4	28	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1656a3ea-d10d-4dd3-8425-3925baf439d3	29	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
794fe591-c44f-4bcf-b981-bf2681ee6b1e	30	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9ecdd329-6903-4884-8c0e-b7bf92146453	31	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
fe350cba-b193-45fb-8d18-39287afec035	32	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d9ae5ee9-66ac-40e0-8f23-7025b5eb998a	33	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2a779219-fb0e-4203-9aca-cf0d801f20f5	34	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
27c68db3-2549-45b5-bed7-8ebb90784494	35	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ebe4d6dd-6662-471f-b109-aae140edd07f	36	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c71ff346-1499-4a4f-85fe-10a29d67413b	38	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
abd2818c-90c0-4322-a266-5e5faa9baa85	39	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
fd660308-0cf4-49da-acec-7813d3dbcc67	40	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f0573635-dea9-484f-b126-0d286ca5c805	41	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
85fd457f-d47e-4a52-9515-30e2f235ac86	42	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1e2f8e52-af7f-4791-a76b-319411634f4d	43	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
482deca1-7bc5-497e-8d25-006bc50da663	44	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e6fe3c8e-4084-45ad-913f-7f88ad63fa62	45	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a428a8fd-e39d-4a3a-a32c-7aac34c24877	46	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c72f0f3f-bb2d-40ac-82fc-9859eb391137	47	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3df3fc11-ef60-417c-9c75-89f8488e1ee0	48	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a3b0eb67-529c-49e5-8447-9177b39665ea	49	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4358e30c-acd7-4102-ad20-fb0b21e28856	12	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3908bfd0-947e-4f01-bf74-1db27f3e3cce	13	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9fd1dd60-b62c-4087-825f-35661506e401	14	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a3f4ce53-8a89-4e86-8ccb-bd7b4679e707	15	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1a061dba-dc9c-437b-aeb0-e00396681bef	16	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8519557e-4aaf-4aa5-92d4-dc7fabe11b75	17	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3f7e4ab5-5944-4c94-a314-b8532fa8d7eb	18	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3f9d7cb2-558d-4ef2-ad16-855874c02848	19	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c400b945-32da-41a7-8835-7d7b2d2f8638	20	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
cc5eb3dd-9dfe-4d27-884a-464c2be6939f	21	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
342b89f1-93e4-4ac7-a8f2-801911a61e06	22	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
039fc292-4844-4abe-b8fd-61e246f399a0	23	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b380f9a5-1ec3-4e34-8d04-166625bcfb20	24	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3df5d551-5fa8-4186-88cf-4db5230f999b	25	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e7b2459d-ca57-4d26-aa70-9b809bfa3a9b	26	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1870c233-4c5a-4c5e-991e-71a0ccb9bb7f	27	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
cb2a5a0f-763c-4b99-9d0e-edd0976a2f0d	28	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
97ad8d01-2266-46e3-8b3f-89e5e0f67449	29	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b9769513-86b1-4919-822a-034bf548afa8	30	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a5798f02-54ec-45b2-b27e-52b3f95a9110	31	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6bdf8253-ab47-49ff-b60c-e5bc16cd3cc4	32	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
bec13ad9-b11d-420f-aa8a-3d1548b30dab	33	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b8ea3dc6-c921-4a0b-a839-b24d0dcdc7a7	34	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
80f8c237-e89d-4620-b88f-d5d49d554c91	35	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c59ef4cc-9997-4bd0-b09e-2e056663a376	36	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
bb167d2f-b0ab-448e-b2aa-a8dbb872c969	37	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a1214b22-659d-4282-bb08-dfe26012431d	38	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e1dd16de-12d1-4f94-ac96-0892c9eeed20	39	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
80af4683-9d7b-4b16-9861-84585a66e65c	40	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
66ac756f-1092-4d07-a8a2-6cba7bc1a358	41	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
0c85f70b-282b-419a-bd40-a5e3a099d8df	42	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
fa68c264-e7bb-4e9c-9b3b-4786c98c2b7c	43	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6a90b4c0-1293-4323-a91f-9e26537fc819	44	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
932e2f0b-d8cc-43dc-aa2b-edd2db1a50dc	45	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
db09da47-8a2c-426b-99d1-f2ca104a900a	46	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8479784f-3505-42ad-88ad-2021c47cbbe7	47	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
18862793-96c9-4570-958c-854071296ec9	48	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a04ada47-41c6-4ff7-89f7-1fbe2e0eb377	49	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4e18423f-7c14-48dd-b9af-901f6330c7d9	50	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
09233fc7-2c6a-47a5-8d79-a6d16375b92c	51	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3bc9554e-dbf2-4d84-98a4-9b093c6ffaec	52	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6ac359a2-873c-4793-9305-93fa7fdff74e	53	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2e97e001-574c-4364-a60f-6b75c8f2cc75	54	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b4cc5e39-e9fc-44e5-bb80-548e3152276d	55	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5778c747-526f-48a7-8306-0f8783ac5f2d	56	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
908a7fe7-d869-4524-93c3-c71be17761f2	58	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
026b1e6e-e25b-479e-9fc1-2b466c4d0a1f	59	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
46e6fee8-f22a-4ab9-b8d9-bda7e9c07803	60	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b33d1cbe-4c53-4536-b9f8-7bb9730f405a	61	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
59ee474d-d737-4f24-b662-9950a2cf2d21	62	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
fd4df099-c557-4828-8e20-4561ca0b6e76	63	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
99dd61a2-e88d-4a1d-9633-8f3ee9c7378c	64	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f41aede0-e9ff-43e4-86cf-d67f3a6cb6d6	65	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5b9065f0-7860-4a17-8a37-cfa0625fbc99	66	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5aa771c1-acbe-4100-85a2-8cd354487f56	67	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1bff00a0-24ca-44cb-8ddb-8c8a7200fb6c	68	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
fb77f68b-684a-4123-8c76-bd5c795a325a	69	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4b5b4932-dbab-4a4f-b278-61babc04e76c	70	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a9e1bf2b-f341-4786-ae86-92ad3ada98cf	71	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
95876e28-309f-44f9-a9c4-0cabce91bdd6	72	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
84255287-d33e-4f62-9d7c-a2905ee9572d	73	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e96dc5f5-94ad-4e37-a2c0-7f4a2e78871e	74	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
64540064-1677-4b0f-a8e2-16921284fb33	75	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9a709e8b-601e-4f77-96c1-01be4674b756	1	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f8de8086-d603-4f5d-8ec8-564b839439c9	2	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a509f656-da5b-4433-a268-02f3d69142bc	3	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
043e9113-fffa-4be8-bd83-87324f937dbe	4	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b0111841-2a61-4c0c-8691-28a768078066	5	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ca6a333a-edb3-4980-b927-4327515691d0	6	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ff4d4297-773c-4731-ab25-93d1ab90a1d7	7	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8f21f96d-5854-4bdb-b922-ffcd13ca1f75	8	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
598196ab-efa4-4693-95df-a4afd778da24	9	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
992514b6-c74e-4497-a0b5-cf2b333de50b	10	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
40dad2b9-3de5-4c04-bf73-c6499fe25d72	11	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a7f099b1-2a87-4501-9baf-d31d7527cc6c	12	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
0580496e-b383-4f6e-88c4-584bb7fe5266	13	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
0ac4a803-f3ca-4a73-a10d-e248d9f09786	14	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
70e8d579-a0a4-46d3-b6e2-2c3d65c53c0f	15	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b5c1c056-854f-4b76-b3b6-e4c822aed3d1	16	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
955a9c79-4e79-45fb-9e10-fab0674e952b	17	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
82098883-7c2b-4598-b887-d5a661858fb3	18	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
177b447e-eb98-4dbd-a75e-fbc58fc5d300	19	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3e8ea890-5458-4e75-b479-9f5848ce00ad	20	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1d014991-a304-4118-96b6-6471967fce75	21	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
33dc7cf6-d1b0-46fe-9938-9858a3b417e1	22	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9bee64b0-600e-4a0d-910e-1db7bb49aab9	23	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8939e38b-3c67-4865-9880-fba9b0861748	24	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9cd287ca-d62c-4afa-aa13-084db5e2163d	25	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d68d280b-4132-42d0-8811-cb9f7cf6f08f	26	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2d668702-e136-44eb-84ed-4acdb422dae8	27	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5e26093f-5a89-4b2f-b454-8b7826e29f8d	28	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
72a74b4a-0d62-49bf-ae0f-433535ed0df4	29	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7af96dc7-7aea-4115-b3a9-5eb0e9eaf54f	30	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
18bdb466-34fa-4a45-8f4d-f7b57ed17124	31	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f1b30f41-6f29-451c-aaa5-4065ba4fc169	32	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a0b5c00e-9fcd-4391-8074-a3cc7f3f8933	33	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f79a9b88-d006-4fb9-aff5-e6dc416e438a	34	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
cb15306b-0de2-4ec7-9fde-86636ee243f1	35	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
18323ee2-0711-4336-b255-2ea16db2dc19	36	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
92b2bdc4-5499-4cfe-81fe-caeec2d110e4	37	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
214da504-de11-4fa2-974f-cd56cc16d41d	38	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6589436f-ac27-4ab4-aed1-60373b963146	39	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a5d3eaa6-9408-46b4-b156-fc6d397afad2	40	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
afdaaf1d-c6c8-4c40-a87a-9f125b26af63	41	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f5507989-04dd-4e02-9331-9ad9c008c101	42	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
fcfb890d-615c-4f99-8961-d128d0b03d0b	43	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2188b739-2926-4814-a6e2-72121a159c32	44	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d11b8a34-25e3-4235-b2f8-fbab4327fe50	45	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e6071d92-821e-4d5b-99fd-d226d30108c0	46	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
95094e55-c95c-4223-a48c-41165c6c1774	47	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4ade530d-9d29-4870-a084-ab2d6f9d1d70	48	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
407c00e7-5c5f-41ec-b8af-049e8162a44a	49	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
028ed0d2-11fb-44b0-a436-28488869acf1	50	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c984107a-85ed-4ae7-aa90-d4b291dc3f15	51	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
0926fdad-b851-4bbc-bece-47c8196b7dff	52	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
cccc55ee-e881-4a3c-8cc4-025f68d85d56	53	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b13ef527-ec27-4465-ba7c-c0680db303a3	54	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
bd3dbe4f-782c-45f6-9007-b20b7fcf5299	55	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
05d18e17-2efc-4c26-bae5-6401c24dd3c5	56	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c7cfc031-c4fa-4b03-b660-c5e8cf587dde	57	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b15c3c37-2609-4e66-b957-ac531ab51512	58	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b64641ae-1b12-4160-b86b-81cecea38e9d	59	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6e2ec07c-ab25-43de-89e9-c24ebff008fd	60	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
41c5ac74-3479-4421-b979-4d343a00b18c	61	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a0e861c4-2b79-4d05-bad5-8346d05ba784	62	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
61e30322-84cc-4018-8208-5004f88a833a	63	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7be95fa4-862a-42ab-bf07-67546b3cda5e	64	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
942e297a-bebd-4730-b660-7082d645ffb1	65	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
0f4650d9-e653-4bd6-a97d-64f259d94eb1	66	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e9b0fe00-1f60-42f2-aae8-bb59f4cdeade	67	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5cada34c-fcc0-46b3-8453-70d7f331ac2f	68	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c6157891-3f8d-4ecb-aade-78cfede396da	69	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
af70cfeb-ac58-48f2-81c5-e6015f538ea3	70	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
df89e697-f3ee-45c9-91d3-7e31af1b71bb	71	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2012c92d-295b-4a7e-a3c3-004e70bdab85	72	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
77acf1d5-d73c-43c0-9347-8f3e45f698cf	73	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1404d7ad-53d1-49b9-a4b6-5385395e3aa0	74	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
326e7b25-a10c-4ff2-b311-cbd61ee493d4	75	affc03aa-0d03-4ca0-9569-500f14480350	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d1b3adbf-fc63-4f09-8e7f-004c16637648	1	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f6a8e522-9fdc-402d-ab1a-59c0d33c23f6	2	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6897c2f7-a252-4a98-be04-21fd4dd4ac64	3	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f8624e91-8c8a-41e8-ab4b-d7bf92132bcd	4	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8fcf9e89-4cbf-4440-8578-adc0192f0bd9	5	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d48d1a96-f268-4d72-9309-56f07ed61f36	6	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1e9f3f1e-08ef-425b-a12a-12b94bb47fc9	7	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
500ed66c-4da6-4d9d-9841-1e6e116d04ab	8	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1332a943-60df-4a69-8ace-8c3f43f7702d	9	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5c948c6d-6ccd-4ccb-bcc5-c547106532d1	10	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7515c0c5-de68-40ad-9e3a-5ce9ecdebc04	11	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2b82c31d-540b-4063-9b9d-0b93ae81fb31	12	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a11b2c34-b13a-453e-b103-a1a48f4b4d08	13	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c9faaa89-0394-4a03-b35e-912b80d4402f	14	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8e1b643b-c0a1-4164-ae08-800e46b8e0c7	15	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1cf08eba-1e88-4474-8567-5a6b4953df76	16	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
48ef638b-29f2-420f-8964-5572f7e4adc9	17	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3c6cacf4-7596-4c3b-8211-7c994d59b60d	18	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
cc64b793-cb53-405c-9903-ba54800d7f47	19	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
0853da8d-f35c-41c5-b790-094999385bae	20	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
88029e8e-98ba-40b0-b3ad-631809b1b22c	21	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c9066a7d-b5aa-44ba-aed1-5a596302a109	22	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
691a79e0-34c7-4654-8c7f-76c99ff739b4	23	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
189cd6a0-1ae1-481f-8541-580374ada9dc	24	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f4694ac5-4fb9-4f19-85fa-464c1c959bd6	25	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
dc942331-887d-4e94-9c83-f6ca8711875a	26	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
86fc5068-33cf-41f2-825d-fd9cfcb7a2b9	27	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
12987371-c540-4ff4-905c-e9c1d5dfc6d7	28	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d3e5354d-1b55-41b2-b93f-16f0881ca44d	29	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6df26a6b-4d67-4535-90b6-fae31127d9c9	30	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6514540e-0d69-432f-b961-34d580b9d4b9	31	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
648c89ac-05f6-4d17-8163-030dc6970881	32	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f3351884-31f5-4e2a-87e5-d410a01436b7	33	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e4452355-4b76-4ceb-8e74-e9f31d8c21d6	34	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ad82bf72-9443-49dd-8745-78cc5e5469ee	35	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8bab367d-bcf3-4963-9834-f65a3b4a6c55	36	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
208911bf-7c2c-42f1-883f-f6d43e22f21b	37	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
141897ab-7d75-4dbd-90a2-5460e59ea0f4	38	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
228caa14-3ea5-4bf6-9272-673428dea5ef	39	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9e7887bc-9e26-48b2-9db8-6ad7784ddce8	40	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7a6b34bf-5293-4f83-a5ec-e09dc9bc1bc9	41	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
72c30744-aa8d-4cf9-af45-0aafa04862b2	42	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b480af62-f72c-4551-9c77-1c1226b6c5fa	43	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
64d2676f-7634-4817-a526-b65858474a6c	44	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b5bcd3bb-2248-49d2-9a29-c71809efa988	45	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3a92503a-5890-4ada-9952-9cb1569f9c10	46	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
cc7c0e2e-fdc7-45f1-bdd3-9efd9c8adaa7	47	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1a74bfb6-839f-4f02-b46e-43b449104935	48	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a53d78ab-bc92-4ea0-af67-6afa451b1fb5	49	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
098ce169-d7cc-4243-a2e5-c69791d51c89	50	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
28f7c743-4c1e-4262-be10-24b5a4cc7be8	51	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ec8c4c3a-72b2-43ca-b9fb-a30da3ef9211	52	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
256bb913-1eb2-4f5f-a9dd-9335e5c699c0	53	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c8601fa6-0694-4aba-a575-09b7319f1da7	54	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ab408953-70e8-4c31-9f45-a8a4f73e34d8	55	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
52ac4b3c-1c93-433e-a3e6-ecc013411f6d	56	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f900dc46-fd4d-4cab-abd2-dfd8541316f1	57	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3796eede-605e-4eae-a3d1-82f3c2be132d	58	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ab8a8f63-97ef-4f26-9daa-67bf2cdac544	59	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
46f52610-228e-42c6-8b9c-6374b6a4ce7e	60	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e856e425-f20b-49ec-bb07-d4d516f606e8	61	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
98d5f6e6-ddf6-4f0b-9d79-01e92a5caee9	62	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e7e0b8a0-2b9c-4553-94d3-908ff3afcf9a	63	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
772265cb-853f-4ef7-81d5-7bb712e5c5e5	64	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b43ce6e8-e04f-4c74-ba51-6ca0b6c2e76f	65	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4f738c25-8067-474f-a371-56147d6c0f9e	66	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
fde690fd-fa5b-42cc-8892-673ee1b85fbf	67	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
642e95fc-8246-4c73-b4eb-216de8719c24	68	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2692f6de-3982-4ace-8463-078db7fec7c2	69	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1244a80b-3bb2-4410-8c6b-73a25fc35b4f	70	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
0f7b3bcd-946e-4e55-9132-ec0542e65692	71	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
319ce80d-93fa-4f9a-a592-7291032c6a0e	72	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d456675b-bd6a-4d47-8840-6be76c6163be	73	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
bdd512eb-a94a-4b7c-bd51-c452729f570b	74	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
bf0884ba-78a5-46d9-a2e7-6fd56944beb3	75	adb6baa2-2900-4f62-b5e5-b4b3cb47214c	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b8aab545-2e3f-4a03-8adf-1686c6fe079b	1	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
27088885-3cf2-4f71-8630-ab027923408e	2	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
03f33330-c106-486e-a1ea-6d0e7fd07e1e	3	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
661924d5-5d6a-4dfe-8666-459a5eda9180	4	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
aa565add-13d9-49bc-81ea-dfba2ba7fbc2	5	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e7b2a017-8ba5-4270-9f09-0238fe0aee2f	6	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
745682c6-3b6d-4b4f-8baa-d45cdede6fe5	7	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8acf6c60-0dae-4a84-b770-7465381b675d	8	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
87675732-89d9-473c-9ea8-5b91e85a2b7f	9	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9a573c71-c149-435f-abff-555007942cba	10	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f5a00e2b-7e21-4196-8351-b8fa214e453d	11	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9553adc3-505c-4927-b016-44eac1686a0d	12	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
dceba56d-27d9-484f-b187-fd308df8f9dd	13	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b6a15be2-1c12-43a7-b155-d6f482f28b9d	14	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d6342be9-c5c0-4f4d-a59a-0b5d225cdd48	15	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2bd42fda-fa75-4e90-9b7c-56e13036681f	16	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d4a7ac46-4566-43a0-a59b-13ed6516f43e	17	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
30ad12e9-d271-4839-abbe-9b3dedd90ca2	18	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b4e72b11-f132-4abb-bd93-8a377db5846d	19	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d75f609f-867b-4e9c-b6ec-b4ba69562cab	20	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a75c859b-71f7-4c23-b9e9-4639d1364a71	21	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
0836dc72-5f14-4390-8091-db4d857488ea	22	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4789834b-1167-48ef-8aa2-18cc0a359512	23	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a8049d69-0b97-4596-897c-56d68660dd40	24	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e0ff5362-7483-4f70-8dff-df4eb79a2384	25	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8211f86b-314f-4de5-a6dd-33952917092e	26	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
07f2c9d3-a070-40e1-8316-566ebeb8ddde	27	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2a1d4771-07b6-4f18-a24c-f389fa4814bb	28	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
da2833c4-0cb8-443f-8915-3c8a38931b57	29	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
38e8f8ec-ddb1-43cd-aba5-2e1001b91171	30	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8a53c44a-42fd-471a-8934-54a050ef79a3	31	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d1fc184e-32fe-4c9f-9d1d-6af2a6a7fac7	32	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d907afd3-cfa1-44a2-bb00-c3a608b9452b	33	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4d84bef4-3117-46ce-89a9-0879576273ff	34	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e4789bef-0199-45e2-9a2e-67320444d7f9	35	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2c62a0eb-6198-4903-88b9-edf0223a6545	36	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6e73d440-fb36-4121-b306-4d4254f67c06	37	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
97cf0a63-9506-479f-80bd-73a2d504a950	38	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b8ae0bff-d758-4ef4-a5bd-44740fe0dd7e	39	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1ac1e695-ca0e-43be-99f2-e52dfddf1822	40	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7547f7e3-93d2-4d4e-935e-ebf39a39f2a8	41	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
0f3af9c7-02a7-4243-8e43-f16461907f95	42	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
09a5aaa5-5aa9-483b-a120-5182f20531fb	43	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9e342b1f-de48-4034-825b-d82cea8be6d7	44	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8952fc55-b4ae-4e63-bdab-ec32c303e705	45	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
705ba5b2-9117-472d-8f95-098f1f57488c	46	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ed123ecc-29e0-4e27-a6a7-eacd9c9b0144	47	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f6d9139b-281c-4201-b873-a96da3ed3c6c	48	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
0c440ee9-b7de-4618-a073-ed75379fcf69	49	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9ebe3ae3-b425-4975-980a-aacbf8cbbb26	50	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2b2932cb-abc6-4ef5-b1ea-0f525304678a	51	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
be51afe2-a54a-4148-b9ff-991fb400c381	52	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
75613e9a-61b6-4f84-9d4b-dfcb2f0d91bb	53	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
41e29556-419f-4f28-8e60-d3fa84fbbf85	54	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
95515fde-19fa-4b6f-ae09-518b9dab4788	55	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9602c03e-0a95-40b6-823d-b322cdba4c35	56	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ce7b6a00-7388-466c-8d0f-043433800df8	57	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2f4f5c55-2c72-4e8d-a6d1-76f6539ffb8d	58	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a0ed73e3-5ed8-4dd4-b7d4-b57e041e5d9c	59	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a6ee2812-9306-4efb-a6f6-ad5a52e70d38	60	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f952c088-e001-4be7-b0a6-313963533d06	61	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
47b80939-f43a-42bb-ace1-78ce0d3ad290	62	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1d36c4a9-d502-43fa-8a87-dd2e9ab8c9d2	63	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
0efb4707-72c1-44c7-b21f-153ea4dede81	64	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a0c88456-6546-4137-89d1-ede9fc4f19a5	65	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9ad0b673-6c14-44a3-a4d9-6fc4f721c8c8	66	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
216153cb-1a5a-499a-84b3-3f81eea96c2f	67	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3a387b78-9265-4e97-b11d-5a52a202aa60	68	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a4a70480-f5fb-4895-8da6-9e98ac614ff5	69	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6d930e36-d0c0-4dbf-b155-e893f04f9b64	70	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
595af7f5-8bff-4635-ac3b-1a7760141b7b	71	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1327bb06-ff5d-4c29-a73d-811c8b3c2bb0	72	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d68f184c-c703-4055-8dda-4eb126fffb30	73	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
bcea2e47-2f08-4de7-82e9-80bd30ec7f16	74	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
da6504b8-f10f-438a-9c44-a863197bf563	75	7cc3e0eb-1257-4b08-8085-656221f7964d	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8d09e5f9-8451-4694-a679-12c56accb57a	1	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
af9f1a57-805a-41f8-a7ed-2ede026bf96f	2	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c4407a94-65be-4dd6-936f-9892ea755796	3	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
72f584bc-9f7d-4cab-a04a-1f1246d18767	4	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f98deed9-e2be-4346-8dc8-341c23fe6513	5	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ac079842-7476-4147-a626-dbe80e9cc338	6	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
fa521d45-9b11-4530-a81a-da919f624314	7	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ee55e685-887f-42d0-839a-0f85bd180587	8	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
824dfaa1-e60f-480c-a158-731aea2c63e5	9	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6148ced5-86e0-467c-9ebc-9d8e5bb7fc3f	10	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
cb44014b-76a9-4098-8241-5111f3668c10	11	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
824063e0-6b46-4dd0-a9a6-42e14ac83c20	12	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7656da29-3675-4113-9080-00d78a45fcac	13	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
23ae7510-0859-41ac-90bb-704a26a6b4f7	14	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7e96bb18-534a-4f43-9b6e-0266e5ad4246	15	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1c8f6254-45db-49d2-9e2c-96b98f016be6	16	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
df55cb12-cd68-45e0-ba1a-68345833d993	17	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1837ce41-ebf8-4db0-9d14-b9de008f1999	18	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7f282254-638a-4348-8228-a215b602e97a	19	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
52d7667f-a759-4586-bf9e-1e45ba01a78a	20	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
da9a1579-c0d2-4d31-b510-e490f9fff12e	21	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b8113c38-3ba9-41a4-af20-55b39c9c05b6	22	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8970b240-8f8b-4ff7-b831-375cf7df902a	23	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
997a87ab-10a2-4155-87f0-81b9ec5ceddd	24	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5624d1bb-34f3-4ea2-bc0f-9530366f8409	25	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e16410b9-96d0-457e-a7b2-31ee0aa3744f	26	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
505836eb-c295-43fa-99cb-76ed8c79a707	27	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4406e720-7a80-4831-8689-81c716fec86f	28	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d7fb90c5-768a-401b-a41e-9a095158c0be	29	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4d296ff0-412d-471e-ae3e-da6384291d50	30	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
cb47f5b1-3fa3-48db-9aaf-67eb99d478ed	31	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
95e0c3ee-a62c-4436-8371-39e814f70122	32	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a692bc77-beab-43ba-95d5-88d01a0568d6	33	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f6155273-a820-4025-840b-55a934ed96f8	34	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
526b44ba-b85f-4a48-9ca5-3dba906a174b	35	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
75264147-e187-480c-812a-b5222b0e2c9b	36	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d37e8fdf-8bcd-4cc4-a078-c810a96024a3	37	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a9fc18f6-f140-4386-9ba6-94e50ff21bae	38	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ad3f1b45-b76f-4cb4-980f-72bbae62b6a0	39	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
9d752b00-2037-4f69-8d12-376816ef78b1	40	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
af6db70e-8488-4fcf-8546-9bb1f752c361	41	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c88d9d9c-41fd-43ac-9b22-0edc525b3f39	42	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
958857c8-9296-4d5e-949c-da542ec6bc57	43	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
170ba3ef-e27c-4188-bbd6-9e28abe57a5b	44	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
75a39877-b53f-405f-8e56-c62256a4a6d2	45	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1a37987b-9617-4406-b29f-74f85c27c267	46	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4032f4be-9676-4d47-b029-0517b89f232c	47	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
48f6d501-017a-45ff-b0e5-aba0073d893a	48	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ad4185c4-c017-4fda-9af9-08ecc2b3c80c	49	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6eac23e0-523a-48fc-9a95-a3262a322b1b	50	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d059aa84-2aef-4692-a805-bedbe1db0145	51	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6db37eb2-7afd-40ed-973b-4202bb888011	52	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
de086b2f-de4b-4b5a-b0b2-e23b2af7e0fa	53	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
093a6ef4-9332-49ad-a3c3-2255296ed338	54	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a80279c7-e451-41d5-976e-b5078662facc	55	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
20c2262c-52df-4125-8539-d7c857421274	56	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
397a9d4f-2c48-44f4-8bd4-dd45d12ca23c	57	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2db54bef-22aa-4e3d-baae-b7d984669f33	58	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a7f8c8d4-c671-4a44-9552-c54ce0a23dcf	59	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5fa1098d-5af2-468e-a708-ba8eab0af4e5	60	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
dd05e274-9c9a-4126-a358-fed445cc9071	61	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a91792dd-046e-492f-964e-d9b95a264785	62	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
dc5a6a1b-96c3-45ac-b24c-3845a3022226	63	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7b90f4d7-5142-4e22-94a9-509a22f87f77	64	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6ee43156-9e0f-4bc8-b010-1511a1799840	65	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
563c53eb-1c6d-402e-8881-94569c771887	66	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8ebcf39b-1b42-4b44-bb00-7838fcd9ca12	67	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
414ee48b-8652-4c6f-863b-217405744fba	68	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
837740ec-aea4-45da-a6ad-17b436b45169	69	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c044f6bc-679d-4759-9fd0-4066810ac3ac	70	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
42eb6830-d7ee-481a-94c0-6ab09729fe4d	71	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
eeae32f6-ba62-41e6-aef4-30689193f4d0	72	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7f10558f-303e-4acd-87a1-0151247c538f	73	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
be9b379f-51aa-4ce1-9e59-e55cc9d55fe8	74	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6a92d8dd-ed40-4f56-bedc-71ceb6748342	75	91e5794b-f3db-4847-8318-49112a5c6e8b	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ca776777-6ddd-47c4-9681-bca6834c9731	1	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b18aa2fe-0d57-430c-90f3-4b3c3463ffa1	2	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a7b957de-0f70-46ca-8543-2a244a7f3f68	3	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f8568b95-5cba-4a10-a197-e3fbd1af13c0	4	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f8c0653a-e801-4da8-b5ae-fef016166d4f	5	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
72ba334b-d269-432a-bc86-e1de415f022a	6	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d7e5191c-0c81-4934-ba9a-dad6761c95f9	7	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ceb17633-b320-4780-9342-5023a0da15d5	8	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6945d013-553c-4940-b480-07f4911cdf87	9	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5eca91b1-cfce-437f-b661-57e97c465ea4	10	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
033ca0ce-a577-45cc-8a5d-bf9167ec83e1	11	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6eff7c6d-1207-48cb-8d46-cabbb95230c7	12	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4a5d38ed-97d5-4e82-b01e-f9316c92f58c	13	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
98f4a191-4609-43eb-8963-8da1565ff7ed	14	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d5342d50-90db-40dc-98de-3f062867a38e	15	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
cf68a4b1-4d85-40dc-9dc5-499c991fa9ba	16	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
497476b2-4aee-4f3c-96c3-75e2bb1c9d26	17	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4263ce50-aec7-4f0d-baaf-2dc4f7288a15	18	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d9439c36-34a5-4831-993c-127088853b3b	19	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
347ca0a3-41ef-4ae9-b964-a6f9ff9dcc01	20	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3bfd2740-0e5c-453f-ad6e-e4141e9e1cac	21	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4505b663-cd88-4107-95c0-0b8a1cf698f4	22	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
0e035ce5-8682-4b0a-b94f-661b786d648c	23	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
1ee369b0-1dc9-4f57-8166-acb668e2b3be	24	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
583ed3fa-b0b1-4218-b46d-343122daa262	25	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ec410258-ec47-45a2-8380-277c3032de92	26	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
fd243dcb-7e8b-4fe0-aee5-9b24faa46364	27	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e5c5fa96-f6c1-4397-981d-11b1ea4c3e0c	28	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
608fb26c-418c-477e-a878-ae7a7ab8d16b	29	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
fef59b2e-c54b-423c-9ddd-e436eb7aa45b	30	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
58654f99-846e-4c73-be42-6ad437fb66a2	31	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7d2c4816-a78e-4712-acd7-c113c3fe8abc	32	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
041468bf-313d-425b-9505-fd72772121a5	33	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
7bf8a006-f240-4176-9e71-7890bf7b4a26	34	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8bfd1890-c858-4e6e-b8e1-fd4890d9e354	35	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5ea45534-d62f-4d35-a809-9a7833ee7c40	36	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4f459f65-3817-40e8-9207-59f74256a481	37	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
df075b51-a250-4a69-aabe-f4d88679b814	38	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
3a92580f-8273-4a53-8a4b-049351cf07af	39	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
db0ccb22-63b3-43a4-8af3-1b65ee82575e	40	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
6493f141-ce86-49c4-8007-7e082b01c781	41	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a8b63447-13cf-4280-bb15-c2412c0859da	42	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f56fe1b1-38fc-4253-be80-cdd78847730d	43	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
abf1e270-efd3-454c-a1ea-f2b83d7a8f59	44	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
a7e597f5-0b0b-4083-a3b3-cb66f212e0d1	45	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5fb89b67-7995-4402-955d-45369d0ee99c	46	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
5adfb4ad-9627-491a-a18e-7938230a5fef	47	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8e52cf42-4182-42af-91d1-ae5e93a76517	48	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8e2136f0-09e5-479e-ade6-d494ecfa447d	49	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c2f9a151-0765-473b-a71c-3f5395334e55	50	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
159a8602-f905-4843-ac45-5bc1dbaad77a	51	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4cfc25f5-8a1e-44d6-a2a1-6b460baaaf3e	52	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
bc061c55-00e7-4e67-a26c-ad6c7f8097e0	53	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
12c130fb-a779-480d-9ab5-fbc901302890	54	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4d0d02b1-0391-470c-94b2-7e49e6cf95e1	55	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
162fffa1-d30f-41a7-aa98-b84961134ba7	56	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
734dd6a7-dfcf-4818-bb90-9fab4a83b4f1	57	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d3b10236-117d-4f90-8097-ef0288ea0481	58	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c974c90e-c2ca-49ad-b644-b7e9adaafeeb	59	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
c414fa66-f2c9-4814-8af7-6f9228070e88	60	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d53219f9-2906-4763-a3ec-4b732f75314b	61	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
daac20c0-0c9e-47da-8083-c12d49002533	62	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
f28aee5a-3eb2-4346-83a1-53b0ea47e9db	63	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2b89a7f1-f26a-451c-845a-a789ed270d57	64	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
62917047-cfc3-4f35-b91c-1185da41a670	65	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
31be243b-324b-46c6-9b83-7cb6f61aed19	66	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2c0c5267-21de-4602-8137-882d0c552d8b	67	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8df87937-cce8-4b9a-8155-3dd180e43095	68	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
4f19a6a5-fde7-4a8c-bb46-471d2001410c	69	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
bd547754-a38e-4dbe-a887-63735b7907a7	70	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
ccc2c7c3-6b52-4bad-a9f0-74b0a4be3e33	71	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
e3e62220-b155-401f-a25c-e04f69074051	72	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
b2555ec3-0f4c-4b72-ac24-b4a0d0468366	73	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
8a680830-ddb3-470d-b77c-826550e93e80	74	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
d8065bd1-2633-4bbe-a153-5d400dad1f2e	75	2fe48812-72b0-4609-b873-582f5cc1f024	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	f
2a661f4d-ab86-4f25-9f23-be80a94a00a1	37	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-17 21:22:33.059	t
49b62e86-2754-40cd-a73c-57c08ddfee40	57	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:13:35.892	2023-12-17 21:25:08.86	t
caba43c7-8bf3-4a52-90b7-4b2ed720e307	51	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-18 01:57:21.106	t
43ee6065-31a9-44d8-8f8b-85144086d076	52	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:13:35.892	2023-12-18 01:57:21.106	t
\.


--
-- Data for Name: Avaliacao; Type: TABLE DATA; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

COPY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Avaliacao" (id, "idFilme", valor, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: Cinema; Type: TABLE DATA; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

COPY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Cinema" (id, nome, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: Filmes; Type: TABLE DATA; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

COPY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Filmes" (id, nome, sinopse, "dtLancamento", "capaUrl", "createdAt", "updateAt", disponivel, "linkTrailer") FROM stdin;
\.


--
-- Data for Name: Sala; Type: TABLE DATA; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

COPY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Sala" (id, nome, "idCinema", capacidade, "createdAt", "updateAt") FROM stdin;
e02aa91e-40eb-451a-8535-e9787d0be8af	Sala 1	68caa687-b8d8-46f0-b5a9-79bfc36eb456	80	2023-12-16 12:59:12.118	2023-12-16 12:59:12.118
00d4af88-74bc-46f5-aa73-e741e24c9c8a	Sala 2	68caa687-b8d8-46f0-b5a9-79bfc36eb456	75	2023-12-17 21:13:11.304	2023-12-17 21:13:11.304
\.


--
-- Data for Name: Sessao; Type: TABLE DATA; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

COPY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Sessao" (id, "idCinema", "idFilme", "vlEntrada", "horaInicio", "idSala", "createdAt", "updateAt", "dtSessao") FROM stdin;
91f573be-402c-445b-b446-e02ca5d7697e	68caa687-b8d8-46f0-b5a9-79bfc36eb456	07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	50.000000000000000000000000000000	20:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 12:59:37.936	2023-12-16 12:59:37.936	2023-01-05 03:00:00
47d9f816-767c-4c13-85e7-2044879b8ebc	68caa687-b8d8-46f0-b5a9-79bfc36eb456	02202da1-b071-433b-9b28-f6b4beab820c	53.000000000000000000000000000000	16:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 12:59:58.137	2023-12-16 12:59:58.137	2023-02-02 03:00:00
8ef82130-9c00-4e07-b7a6-ffec0b348aa4	68caa687-b8d8-46f0-b5a9-79bfc36eb456	0853ee06-86a6-44f0-9941-f0bcac5eba25	45.000000000000000000000000000000	18:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 13:00:13.509	2023-12-16 13:00:13.509	2023-03-10 03:00:00
28162b7b-25a4-416e-9212-c44c423ab286	68caa687-b8d8-46f0-b5a9-79bfc36eb456	9b05c043-9a7d-49de-88a5-ab31de1177ef	35.000000000000000000000000000000	22:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 13:00:57.198	2023-12-16 13:00:57.198	2023-05-11 03:00:00
33155ad3-dbfe-4250-a946-8a470aee3b57	68caa687-b8d8-46f0-b5a9-79bfc36eb456	5d801a30-4ced-4a9e-b218-e8822c90cef8	48.000000000000000000000000000000	22:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 13:01:13.145	2023-12-16 13:01:13.145	2023-06-07 03:00:00
80c24338-08ca-414e-9293-30eb7d4dfb0c	68caa687-b8d8-46f0-b5a9-79bfc36eb456	d089e38a-d140-4f9d-9826-483c97c32fe3	53.000000000000000000000000000000	20:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 13:01:35.679	2023-12-16 13:01:35.679	2023-07-06 03:00:00
7527ea80-e600-4866-a47b-050a20f44f9d	68caa687-b8d8-46f0-b5a9-79bfc36eb456	b07d0f7a-9d03-4d02-be74-923108d72955	38.000000000000000000000000000000	20:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 13:01:52.628	2023-12-16 13:01:52.628	2023-08-14 03:00:00
08570a59-c269-4607-9c09-d01f606bbfa3	68caa687-b8d8-46f0-b5a9-79bfc36eb456	60b26f45-58a1-418b-8884-818fdf137c0f	67.000000000000000000000000000000	20:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 13:02:18.61	2023-12-16 13:02:18.61	2023-10-24 03:00:00
882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	68caa687-b8d8-46f0-b5a9-79bfc36eb456	39fd46ca-4bc3-4676-a48b-4a4da4795afc	58.000000000000000000000000000000	18:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 13:02:31.931	2023-12-16 13:02:31.931	2023-09-07 03:00:00
c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	68caa687-b8d8-46f0-b5a9-79bfc36eb456	c5dd596f-c6d1-4752-8502-54e4890186e2	47.000000000000000000000000000000	20:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 13:02:48.33	2023-12-16 13:02:48.33	2023-11-08 03:00:00
265dcee3-70ad-4f68-869f-7b17df3efbe6	68caa687-b8d8-46f0-b5a9-79bfc36eb456	b07d0f7a-9d03-4d02-be74-923108d72955	18.000000000000000000000000000000	16:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 13:09:07.089	2023-12-16 13:09:07.089	2023-12-16 03:00:00
5259a389-0263-40ef-8a5d-7551babc3b45	68caa687-b8d8-46f0-b5a9-79bfc36eb456	0853ee06-86a6-44f0-9941-f0bcac5eba25	20.000000000000000000000000000000	18:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 13:09:17.169	2023-12-16 13:09:17.169	2023-12-16 13:09:08.449
e55a8050-03e6-4ee1-922e-5cac61544800	68caa687-b8d8-46f0-b5a9-79bfc36eb456	d089e38a-d140-4f9d-9826-483c97c32fe3	17.000000000000000000000000000000	20:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 13:09:31.434	2023-12-16 13:09:31.434	2023-12-16 13:09:22.211
bb98cdc3-72c1-438a-a288-8254088f5e80	68caa687-b8d8-46f0-b5a9-79bfc36eb456	07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	21.000000000000000000000000000000	16:00	e02aa91e-40eb-451a-8535-e9787d0be8af	2023-12-16 13:13:19.942	2023-12-16 13:13:19.942	2023-12-15 03:00:00
fc1a23bc-bec5-4bcb-9e92-98fa3b446595	68caa687-b8d8-46f0-b5a9-79bfc36eb456	d67b33c3-e334-45e8-9be9-4b8a052851a9	23.000000000000000000000000000000	22:00	00d4af88-74bc-46f5-aa73-e741e24c9c8a	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	2023-12-17 21:13:19.229
35143522-5df4-4334-ae65-278f67eacb9d	68caa687-b8d8-46f0-b5a9-79bfc36eb456	d67b33c3-e334-45e8-9be9-4b8a052851a9	23.000000000000000000000000000000	22:00	00d4af88-74bc-46f5-aa73-e741e24c9c8a	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	2023-12-18 21:13:19.229
affc03aa-0d03-4ca0-9569-500f14480350	68caa687-b8d8-46f0-b5a9-79bfc36eb456	d67b33c3-e334-45e8-9be9-4b8a052851a9	23.000000000000000000000000000000	22:00	00d4af88-74bc-46f5-aa73-e741e24c9c8a	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	2023-12-19 21:13:19.229
adb6baa2-2900-4f62-b5e5-b4b3cb47214c	68caa687-b8d8-46f0-b5a9-79bfc36eb456	d67b33c3-e334-45e8-9be9-4b8a052851a9	23.000000000000000000000000000000	22:00	00d4af88-74bc-46f5-aa73-e741e24c9c8a	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	2023-12-20 21:13:19.229
7cc3e0eb-1257-4b08-8085-656221f7964d	68caa687-b8d8-46f0-b5a9-79bfc36eb456	d67b33c3-e334-45e8-9be9-4b8a052851a9	23.000000000000000000000000000000	22:00	00d4af88-74bc-46f5-aa73-e741e24c9c8a	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	2023-12-21 21:13:19.229
91e5794b-f3db-4847-8318-49112a5c6e8b	68caa687-b8d8-46f0-b5a9-79bfc36eb456	d67b33c3-e334-45e8-9be9-4b8a052851a9	23.000000000000000000000000000000	22:00	00d4af88-74bc-46f5-aa73-e741e24c9c8a	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	2023-12-22 21:13:19.229
2fe48812-72b0-4609-b873-582f5cc1f024	68caa687-b8d8-46f0-b5a9-79bfc36eb456	d67b33c3-e334-45e8-9be9-4b8a052851a9	23.000000000000000000000000000000	22:00	00d4af88-74bc-46f5-aa73-e741e24c9c8a	2023-12-17 21:13:35.892	2023-12-17 21:13:35.892	2023-12-23 21:13:19.229
\.


--
-- Data for Name: Ticket; Type: TABLE DATA; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

COPY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Ticket" (id, "cpfReserva", "nomeReserva", "idAssento", "idSessao", "createdAt", "updateAt") FROM stdin;
dc4dad18-1412-4466-803e-9e79b335434d	12345678909	Jason	b570a59f-58f5-483f-be21-fceac1ea0dfb	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 13:03:13.284	2023-12-16 13:03:13.284
cd24cb61-def0-4db4-8504-05cdd10f9f27	12345678909	Jason	ac9ef70d-1f8f-4145-8463-62b406790af7	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 13:03:13.284	2023-12-16 13:03:13.284
fae374f8-6f7d-4285-bafe-6f8aac056056	12345678909	Jason	9c14b96f-d79f-4611-a38b-ebb7b15e5705	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 13:03:13.284	2023-12-16 13:03:13.284
9cf5a18c-a0d8-4e2a-98fb-7b3ce8b17f70	12345678909	Jason	488f4752-e829-4ed3-bc62-aca167d86c9b	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 13:03:13.284	2023-12-16 13:03:13.284
e7e17b43-a82e-430e-a5d2-f84fe8804aad	12345678909	Jason	f392f361-a1e6-4f9a-ad46-bf85f7d808e0	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 13:03:13.284	2023-12-16 13:03:13.284
85e4a77a-214b-4fef-bf66-241d03cbbff8	12345678909	Jason	5eaa4840-0d64-4b5f-8266-8aeea9a2c964	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 13:03:13.284	2023-12-16 13:03:13.284
054f6c16-1636-4942-851b-3270218b6334	12345678909	Jason	b0bf87ec-a27d-4c22-9a9d-95b5715570d2	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 13:03:13.284	2023-12-16 13:03:13.284
5a77a264-d910-4c63-af7b-2e0270eb0c3c	12345678909	Jason	611d2bb8-d232-447e-936d-b61466946c35	91f573be-402c-445b-b446-e02ca5d7697e	2023-12-16 13:03:13.284	2023-12-16 13:03:13.284
73ce172d-c9c8-4a33-93a5-166140ab58bf	12345678901	Marcos	3e173a35-cbd8-4170-a4b3-990ac55030cd	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 13:03:53.675	2023-12-16 13:03:53.675
b776d570-ac50-4839-8b14-4d29ab19e8b2	12345678901	Marcos	9b1a6a34-a3a7-4085-87f8-fe965b5f2c89	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 13:03:53.675	2023-12-16 13:03:53.675
05bff3a2-9e68-4b96-a27c-61dfbb9fdb8d	12345678901	Marcos	17c9ed24-28fc-4ef9-9a61-1d2df28d8280	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 13:03:53.675	2023-12-16 13:03:53.675
f7dd78bb-7a8f-47bf-b5bc-8e2dbbebfd2d	12345678901	Marcos	907961d8-a636-49f9-9923-f8201940d9f0	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 13:03:53.675	2023-12-16 13:03:53.675
33b6c67d-c479-44dd-bc20-6c6e6efa746c	12345678901	Marcos	439d347c-6ffa-4d39-a4e5-d85421b625c8	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 13:03:53.675	2023-12-16 13:03:53.675
c0b9dae9-4ae9-4167-9716-0d66b3d4094e	12345678901	Marcos	90266e40-980e-4424-acca-3cadf527db93	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 13:03:53.675	2023-12-16 13:03:53.675
01b9b158-27e5-406f-ac53-2dcf6e14e726	12345678901	Marcos	18987e91-ade9-496b-a3f9-bc1019ae7e71	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 13:03:53.675	2023-12-16 13:03:53.675
07823659-91f3-4e2f-b68e-d3248b4982b6	12345678901	Marcos	d01d491c-7c1f-4d6c-b48b-21d79c31b736	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 13:03:53.675	2023-12-16 13:03:53.675
40108e74-ea60-443d-acbd-cfa8a454fb28	12345678901	Marcos	e846c3f9-624b-4cd9-8191-110e4647c30e	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 13:03:53.675	2023-12-16 13:03:53.675
d2116b5e-508c-4e26-bdc9-964d8cc2eda4	12345678901	Marcos	77dcf1f5-ee6f-4081-9329-798b43d9a255	47d9f816-767c-4c13-85e7-2044879b8ebc	2023-12-16 13:03:53.675	2023-12-16 13:03:53.675
73c1cd87-4add-4e2d-a72b-fa6eb6e9b4d8	12345678909	Eduarda	2de60cbf-2ea1-402e-bb87-884f3af5f489	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
b3910e7e-3cd4-4bb2-a1a0-15885e27fd04	12345678909	Eduarda	8bfdd61e-65ad-4323-92af-bd95460dbc44	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
cbbfa236-d637-4215-aca0-914e6674d382	12345678909	Eduarda	c34ef804-d20e-4ce0-aa16-e0ce921f255a	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
a9135eef-03c2-4bc1-b0c4-95456ff6653a	12345678909	Eduarda	c458313b-309b-4207-9e28-253473356171	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
d77df02f-839d-45e0-9e47-e5b1f378ca58	12345678909	Eduarda	2a073f70-0664-4a59-a2c8-5c4d5e5e8906	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
59cac4ca-0bea-42f9-a86d-13acbc1e0c89	12345678909	Eduarda	d29b968c-06a5-48dc-ac93-602d0d3f9eb7	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
df4d63f8-d3f2-4a5f-b324-d0fee6b96d35	12345678909	Eduarda	59fec709-620d-4fa0-9cdf-5a8b8398ee7c	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
01211144-4a89-4800-93ce-f660444e1746	12345678909	Eduarda	9963ef07-8bb8-471a-91eb-b321c1b1b346	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
c274dfcb-737b-43d1-8377-471881773131	12345678909	Eduarda	946261b3-2599-4e58-9598-6f9f75ab2e34	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
ccd96c10-90e7-443e-9a5c-23401b0bf3f4	12345678909	Eduarda	edeead5c-2f42-4e13-b0d3-1e4f409b8b1e	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
8f733025-a1e6-4bda-8ab4-d5f469803eb8	12345678909	Eduarda	e95aaa71-e002-4065-8071-27f649af24b8	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
b58fd8be-db1e-40b1-bc94-f43df57fcc9a	12345678909	Eduarda	38916e05-c3c5-4adb-b43b-79662b4fb8e8	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
5491f642-6242-4a13-8847-d2feffaa80ae	12345678909	Eduarda	ca1e0686-cac5-4b2d-9da4-40b5ab487bc9	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
bb88dfdb-a12f-461d-8daa-ceb82e0f420a	12345678909	Eduarda	2c3f267d-fe58-49d8-8e0c-150cd5dd8a3e	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
397328a4-3a16-47aa-a1de-04e78b7bfa78	12345678909	Eduarda	87299a5e-0725-4f32-857b-a5c2324874b6	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
81d9aec2-0df1-498d-9d43-f2db952f46ef	12345678909	Eduarda	8355bd2d-ef54-45ef-a9e2-84c390e79365	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
e885e827-6f6c-46a7-bfca-91ce1e7b4a31	12345678909	Eduarda	17bb5fa3-dce8-4d6a-84ce-478d43a9a082	8ef82130-9c00-4e07-b7a6-ffec0b348aa4	2023-12-16 13:04:34.385	2023-12-16 13:04:34.385
d068561a-6137-4187-a95f-7475e7972370	98765432102	Jessica	534c3405-b01b-48b7-bf0d-ad96a3c6dbc3	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
76ebc550-a8fe-4dcd-adc5-ff58f3d0841a	98765432102	Jessica	dc303c16-5326-49ea-b249-1c76433f1bb3	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
9c1f49c9-0fdc-485d-9732-566bc5504a15	98765432102	Jessica	b6d8ecf6-cec5-4434-a2bd-e335ffae522c	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
a9c85b42-438b-447a-8324-1fe63a9237d8	98765432102	Jessica	dc5d795b-d896-43cd-bc03-5a64c266bd58	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
e97162bb-b31d-4e8c-abf0-54bd6fb0cdd6	98765432102	Jessica	eb0ed4fc-daeb-4252-bf5e-e319fff061ce	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
a4a82616-18db-4e17-87a4-4d65ef954a31	98765432102	Jessica	7750a9f4-10d0-4030-bb56-65a598a6c8cb	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
b6600562-5e9b-4d62-aa7a-a7db91d9d770	98765432102	Jessica	69cd280f-8b87-49da-93e4-035e4b43bf76	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
92d36ed5-13f6-431e-88d6-2d17d0438a29	98765432102	Jessica	de0a8570-ca7a-4d3a-aa4f-9577a1cd7136	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
42f85c75-8dcd-4163-9be2-d386f325f013	98765432102	Jessica	e897474c-d56f-4358-97ba-a0da685d8364	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
27147e72-19c2-4a3d-baa7-f20889884dc0	98765432102	Jessica	e5ca9866-5484-4fa1-ad60-f35e9ae17d44	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
934d5749-6cc8-48e3-9608-2e583f7e4433	98765432102	Jessica	bb2bc513-b383-4a1e-bcd9-5f73d8acc389	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
bfd8621f-c204-432d-86e4-43215e175d0c	98765432102	Jessica	b2b2d6f3-5b12-4b4b-9b96-98083944a6a7	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
87a3debf-4666-482a-b6fb-1ac2cb8966ce	98765432102	Jessica	b4c49157-9f8e-405c-b06c-bbbdbacfa5c3	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
588686af-c53d-4430-9e77-8a8c2fca388c	98765432102	Jessica	286ab23f-8007-42ac-b7e1-56d02b3e16f9	28162b7b-25a4-416e-9212-c44c423ab286	2023-12-16 13:05:42.629	2023-12-16 13:05:42.629
4b26d7ec-96c7-4d42-8b8f-820b86e827ab	98765433210	Juarez	b80c5ded-6925-4229-96c5-c5b94d29cdeb	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
a59bb2e4-9acb-4596-917d-023f33f0d60d	98765433210	Juarez	04334099-6fdd-4f68-86f1-ae64a7cba350	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
5ea0de64-581d-4fa7-a43e-54d7bff33e98	98765433210	Juarez	4ccf84d4-d6c4-4b5d-880b-e023edab992b	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
df9467fa-3b57-48eb-ad7c-d511e917b9cb	98765433210	Juarez	bf6ebf90-9c4b-4177-8b97-8de77f48d8f6	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
bdb2dc9a-4b14-42d4-b613-0d1328308532	98765433210	Juarez	e282df6c-1468-4c75-a808-4a7b0fe5bab2	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
821378cb-dc2b-47c3-b128-9615fab6aa39	98765433210	Juarez	eb253a43-c244-4d45-b9e4-137a4ff0104f	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
aa77e720-73ed-4fbe-9a36-176a238cb0d7	98765433210	Juarez	39bc4b3c-328d-450f-8095-abe7134805e9	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
11b6860a-dbc4-40ee-af28-3f9a90ce2087	98765433210	Juarez	c48c54a6-108f-411f-924e-584e539e6dee	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
b14bdc93-b8ce-4375-8002-a9cda3c768e3	98765433210	Juarez	5429ebce-87e0-4b2e-bc8f-7b93ff95eddf	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
05966f39-445c-409b-baf1-018157ff97b8	98765433210	Juarez	6cb2a0d8-19fa-493e-88b9-3060ac8b7220	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
73d4d32b-aa9f-4d59-ada3-965bf720c75d	98765433210	Juarez	0fd0e985-d930-4c94-9405-33d02a44c434	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
c8080326-61e4-42a1-9c9d-ba706eba9154	98765433210	Juarez	8b1b2b55-54fd-48c6-9751-083df096badf	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
079f9c63-2b65-4aac-893d-2bd159f663a3	98765433210	Juarez	a797b3d5-35b9-4c9b-a21d-19d29b466355	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
73da4236-3989-4837-8e39-6a201d48fa34	98765433210	Juarez	ef907580-cfda-40f0-9fd1-80feff5dc473	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
0a9950c9-94f7-4e9b-b48c-a46886a992b8	98765433210	Juarez	3c2f5db0-3d8a-4118-a211-076ce43b68cc	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
2d70f779-fdb4-445b-b531-354580574d41	98765433210	Juarez	5094a40f-a504-4a9a-8c77-bb0d62b49960	33155ad3-dbfe-4250-a946-8a470aee3b57	2023-12-16 13:06:17.802	2023-12-16 13:06:17.802
87fc15ea-1fdd-4ba4-ab85-825278b3985d	98765432122	Tauana	804383ed-2c79-4b12-99e8-f14dc91570d2	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:06:50.074	2023-12-16 13:06:50.074
f61d7a30-8a17-4762-9820-ee5d5d29af34	98765432122	Tauana	3a371c99-6c62-4c2e-b130-76fb0d78ccb5	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:06:50.074	2023-12-16 13:06:50.074
66e3711e-cc83-40b6-aa24-ee6f4a26324c	98765432122	Tauana	b40a6a28-9e78-4154-962f-b771fc6b40bd	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:06:50.074	2023-12-16 13:06:50.074
9eae9a34-0142-4e61-b832-c61bcb091391	98765432122	Tauana	ad0b4436-226e-44f0-9048-29aa08adb080	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:06:50.074	2023-12-16 13:06:50.074
6ebf1630-fa05-4d99-b1fb-62ad5acaba91	98765432122	Tauana	38a54d91-12a7-487b-a189-571b451195d8	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:06:50.074	2023-12-16 13:06:50.074
24c1c232-8ccc-4f93-a913-f48506b38372	98765432122	Tauana	68b1830c-52f5-4257-938c-96f3c0505f78	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:06:50.074	2023-12-16 13:06:50.074
d13eb731-db9a-48d5-b979-68fa9874a21f	98765432122	Tauana	a6503b8c-03f9-4685-b97b-f55c98390993	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:06:50.074	2023-12-16 13:06:50.074
4e913417-e656-475f-ad49-33d132c6d7fb	98765432122	Tauana	d6a03a69-85f1-4f04-b147-b7fd48c3975b	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:06:50.074	2023-12-16 13:06:50.074
51bd4ace-a3c1-4665-8153-88a404c5f2b0	98765432122	Tauana	a2605c39-1bf2-4f25-9332-d8dbed58f9d5	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:06:50.074	2023-12-16 13:06:50.074
3d2802de-ba12-4355-9ca5-910b363b576b	98765432122	Tauana	e5b72f4c-92ff-40ba-9031-bd28830dbc59	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:06:50.074	2023-12-16 13:06:50.074
f217e535-6aa7-4237-8ae0-272f07df80e6	98765432122	Tauana	25ad8a66-a381-4957-9662-3e7edff3df61	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:06:50.074	2023-12-16 13:06:50.074
85f1efac-b38a-49a8-85b5-628e5181f0ef	98765432122	Tauana	53df4a6c-f999-4322-bbc1-ec599c66622f	80c24338-08ca-414e-9293-30eb7d4dfb0c	2023-12-16 13:06:50.074	2023-12-16 13:06:50.074
f1206e39-0063-4530-a917-79bdb6221eeb	12345678909	Paulo Cesar	1fe0abbe-2b9f-457b-9b05-e3be0ed0a13a	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
6c209044-9935-48c9-a397-c993b09cc217	12345678909	Paulo Cesar	427ac96c-3643-44a1-8e3d-8f2f9b3c1401	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
b3fb9804-3f65-4206-954c-bde78845d983	12345678909	Paulo Cesar	b378a397-51bf-4137-a5c0-e8595c6dca46	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
f31fd117-83ae-48fe-88d5-814ac84b0759	12345678909	Paulo Cesar	a88fdcf9-ae54-4cdb-8d4e-3ac233a3be6d	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
d6a4f6ba-163b-4214-a394-972fe2e587b7	12345678909	Paulo Cesar	855f3e1e-c870-4c42-a066-f4802baef4f8	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
ed6ed13a-c87e-4da5-9980-ab1951458159	12345678909	Paulo Cesar	797b4c7e-49b9-4c39-93e0-182129f62170	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
19228317-9ce5-4822-ba95-b8aedb6dbaa5	12345678909	Paulo Cesar	997be155-e211-4546-8b45-b81781a7d0d1	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
75adb268-6046-4808-a782-4f6ddad341e3	12345678909	Paulo Cesar	b54155ec-818a-4af3-af56-eeff8f248c8f	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
ec6bf1bd-148f-43d5-a412-29a1e6742a7e	12345678909	Paulo Cesar	50c76e6a-ec94-4149-8a37-5175b793b4f5	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
4e28e6d6-3549-42ca-8e71-76a0a07baa51	12345678909	Paulo Cesar	74f4a0d7-2d11-4872-a48a-b390bdf0ace3	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
fd68f308-e90e-41b2-bcd2-02c5dd78c9c3	12345678909	Paulo Cesar	ce08d286-7080-4fb4-a4cc-68b8587bc1a8	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
11891549-0f51-42fc-903c-80062035cf67	12345678909	Paulo Cesar	8b346745-e90e-4303-8a6d-e5155ca5e0cf	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
074fadba-aedb-43f6-8197-a4c17ead8ccd	12345678909	Paulo Cesar	347bdf3e-7ba1-4e29-a87a-f749d36f45ab	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
812b096f-b1d5-455a-a9e7-8cf1a657f412	12345678909	Paulo Cesar	bb386255-ed95-44c3-b853-fa21c8651383	7527ea80-e600-4866-a47b-050a20f44f9d	2023-12-16 13:07:13.642	2023-12-16 13:07:13.642
0469b821-b4a6-4632-9105-5ef6dfe47e17	98765432129	Estevao Dornelles	41691fa2-46e2-45d9-8eae-bb02eb6cf7af	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:07:43.756	2023-12-16 13:07:43.756
92f0e1be-e2a3-4c33-bffb-aadc279a1726	98765432129	Estevao Dornelles	bbea5747-2396-47bd-9b27-157eeee01d4f	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:07:43.756	2023-12-16 13:07:43.756
1a67b2e2-7d37-4a99-9967-e8f6bfcc2599	98765432129	Estevao Dornelles	2324ff5a-c3fe-4e96-a828-2281f2b2e436	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:07:43.756	2023-12-16 13:07:43.756
922f202c-830d-4d5d-a65a-bae108ed0999	98765432129	Estevao Dornelles	8c557937-6b78-41f0-8ef9-166da8957c82	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:07:43.756	2023-12-16 13:07:43.756
8ae3afdd-879a-452c-8c39-ac8c1007e6b4	98765432129	Estevao Dornelles	3d56b484-fa0d-480b-934e-6815f1e43fdc	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:07:43.756	2023-12-16 13:07:43.756
534ce71b-937f-42a8-878d-7372df081e9b	98765432129	Estevao Dornelles	7e14fda2-2dd5-4d97-80e1-e21d2995dc76	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:07:43.756	2023-12-16 13:07:43.756
061e139f-e421-4da4-bbfc-5a1a4e9a68e2	98765432129	Estevao Dornelles	a83a4d09-643f-439a-b43b-1d42e00b0e8b	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:07:43.756	2023-12-16 13:07:43.756
da1e2378-c29f-4d5d-88f1-1d6d39159d2b	98765432129	Estevao Dornelles	e605988f-0894-453a-af15-398a6bb35e4d	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:07:43.756	2023-12-16 13:07:43.756
aadf6bb5-5108-4139-b31b-5e0cce0bb312	98765432129	Estevao Dornelles	a1acef23-a8cd-4747-aafc-0bd46d0580f2	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:07:43.756	2023-12-16 13:07:43.756
7dd41a82-4724-46b4-b42f-335f73440ebd	98765432129	Estevao Dornelles	f0dfd02c-8fe7-472d-8282-53e746128df5	08570a59-c269-4607-9c09-d01f606bbfa3	2023-12-16 13:07:43.756	2023-12-16 13:07:43.756
fcddf688-487e-427b-9c76-250fc9d354a9	98765432122	Ianael	a2783dc0-f627-4945-972a-b28f5ca5313c	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
3547ef13-bc54-4570-aec3-9c49ffb04b0c	98765432122	Ianael	57d30923-049c-4f43-9d87-f1d1d1e95b11	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
35b024ec-158a-4c15-b8dd-f3ee9bfcba4c	98765432122	Ianael	72bddb2d-4219-467e-aa8d-e1439ad33915	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
55e92d24-265d-4286-9e7f-dda069c0b8b4	98765432122	Ianael	222b6ad7-c490-4a6a-8f43-2a41c4483479	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
e5c9293a-7a21-404e-a7a5-bb3f2dd43ac9	98765432122	Ianael	a64d7a8c-a8cd-455e-bfe3-44f91d0ce47a	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
8482d7d1-0cc5-435b-8623-4ba1796b86ed	98765432122	Ianael	063d2e7e-cf1c-4048-bf82-27e722b1c189	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
fd490117-4f6f-4ee3-a119-38a770d105df	98765432122	Ianael	1a1400fa-58ad-41cb-92aa-7f0a69a95dde	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
50641d0a-382f-4184-af52-13574316cdb7	98765432122	Ianael	49a14d2f-d9a2-4b85-b3e6-863ec5d96c99	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
f30c58de-de50-4cdd-9879-59cf4c6466db	98765432122	Ianael	1f4a8ccd-9834-4bcc-889e-74eaca3acf67	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
319ad972-18a8-44ce-a793-1649bed87ad3	98765432122	Ianael	2e5dc174-009f-4912-8df9-dbba37551327	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
23de9bd1-d0e7-458c-b434-5038459fa9b4	98765432122	Ianael	3514aecb-529c-4263-a8c2-2c234ee877be	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
0a39b3d2-6f5f-47be-b4a3-fa9445dc80a3	98765432122	Ianael	d317faef-98f1-4a91-aa72-eab2976d03fc	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
a9bcc6f1-9190-4f49-98b7-3285c85a66f5	98765432122	Ianael	5f526b6b-5785-4c42-9049-3285a2b0f4ad	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
d6bcbb71-e933-4c14-9cfa-0239b9f9c4b8	98765432122	Ianael	d22c0820-2150-4835-a459-0dc3ca3b083b	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
f3c4db61-f34d-4c2e-aa80-95e12ac11de7	98765432122	Ianael	71922c4b-cc7e-4d51-8298-4f946c3f32b7	c5ee1200-2ed0-4d57-8017-da3b4ee3c1c6	2023-12-16 13:08:26.319	2023-12-16 13:08:26.319
a6fed36e-6855-4525-803d-13dc14b0ea93	12345678921	Messias	0f7342c0-302c-44db-a6a3-f83d26bb0892	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:10:15.481	2023-12-16 13:10:15.481
f13d0453-fe50-47ff-888b-4134b43429d1	12345678921	Messias	0775cad5-5aa4-449e-a271-8a8ce93e4b33	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:10:15.481	2023-12-16 13:10:15.481
8863de18-8c5a-4cd9-be51-ad19755d5302	12345678921	Messias	882c90be-ab65-4d94-b197-1fa8b31d19cc	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-16 13:10:15.481	2023-12-16 13:10:15.481
7ef2fe2c-f23a-4d92-a293-ca83ba346837	12345678909	Isadora	66e80a4c-9e56-4b53-a8ac-456bf97578a2	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:10:51.999	2023-12-16 13:10:51.999
8cc392a1-2873-4c2d-893b-fc815db1bafd	12345678909	Isadora	19d7b5db-c75a-420d-bf1c-27a379c800be	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-16 13:10:51.999	2023-12-16 13:10:51.999
81f7c647-3864-483e-be7b-9c12520f34b5	12345678911	Marcos Nascimento	9968902e-5eab-47af-8fce-2249ad656e1f	e55a8050-03e6-4ee1-922e-5cac61544800	2023-12-16 13:11:55.347	2023-12-16 13:11:55.347
2dd2b4c8-eaa2-487a-ad5e-0e195f8b9815	12345678878	Percival	3ed55d09-ec90-481c-ba71-2ae45ce86012	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:12:56.618	2023-12-16 13:12:56.618
ae85dd3b-0e4a-4d53-b0f8-e462006c8f57	12345678878	Percival	153a3f5a-6983-4f2d-b0a0-4104d9b53625	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:12:56.618	2023-12-16 13:12:56.618
bb3a5080-5b51-4ba6-af5c-fde0ef4db415	12345678878	Percival	4ef1e106-5626-4d6c-8e98-ec73715ef287	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:12:56.618	2023-12-16 13:12:56.618
fd6c3820-be15-449e-aab8-ecdef8c5f2f2	12345678878	Percival	9e115fee-5dfa-4741-9b3f-366c77a76f4b	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:12:56.618	2023-12-16 13:12:56.618
48a6696d-1d75-4703-9bba-c60e9fcc5690	12345678878	Percival	190c274f-d932-44af-97a0-a5af37f1adcb	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:12:56.618	2023-12-16 13:12:56.618
832ea012-3947-416e-b8bc-f2fa2856e1d8	12345678878	Percival	361e7612-19b2-4e4a-9400-b91ca663257f	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:12:56.618	2023-12-16 13:12:56.618
a8df3617-4bc5-4245-9e4d-11034250dbb8	12345678878	Percival	0f425297-708f-4961-8bf8-610598fc5ee5	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:12:56.618	2023-12-16 13:12:56.618
3f788fc4-4578-40d6-969f-b99c1f5ef7de	12345678878	Percival	50c8af80-c019-4a58-bcb9-3aa404d01ef1	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:12:56.618	2023-12-16 13:12:56.618
d473ed13-2c73-41e3-af02-a73bbae9cc43	12345678878	Percival	5d1d8caf-232d-4b2f-be38-3cfc78b9dbd9	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:12:56.618	2023-12-16 13:12:56.618
38b0b1d8-489b-4efd-977b-da57f007937d	12345678878	Percival	b028200b-3d57-496b-aca3-d707aab6522d	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:12:56.618	2023-12-16 13:12:56.618
f482c063-3754-45a2-bb14-34ef691be46e	12345678878	Percival	1be87679-4c8e-4ce0-a194-32576587a4b3	882f09ff-c6fd-453e-b3e3-9c25c2b1dea1	2023-12-16 13:12:56.618	2023-12-16 13:12:56.618
9edd7b8e-b91e-4ba1-b285-a84c21e0181d	98745621232	Joana	e47b762e-1a3d-45cd-9dc7-73eb647ccbc5	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:43.377	2023-12-16 13:13:43.377
08976e6e-4486-48b1-af5c-c8491f5fee47	98745621232	Joana	2d41875f-23ee-48ae-b557-def538d077b3	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:43.377	2023-12-16 13:13:43.377
7fa26c1e-6537-4fbf-818b-1774a4c092d0	98745621232	Joana	0397dc6f-95a6-4885-ae6f-e9285cf300dd	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:43.377	2023-12-16 13:13:43.377
ea9bfb0c-9e4d-4bbc-b7a2-0bfb0d2f573a	98745621232	Joana	68297e07-02f3-481a-bf74-3553e36d4252	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-16 13:13:43.377	2023-12-16 13:13:43.377
13102064-f71f-40f6-9b39-719f931fa9bc	01844359000	Joana	bc450737-6b3f-497c-a82c-d800e3d3efcf	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-17 20:31:17.397	2023-12-17 20:31:17.397
f6fa346f-777b-44e9-8b3c-498267ff107c	01844359020	Maria	ade5b683-1220-4c3f-bb59-efcac17c7f96	265dcee3-70ad-4f68-869f-7b17df3efbe6	2023-12-17 20:33:40.299	2023-12-17 20:33:40.299
0d50d6d4-b237-4231-bb7f-62973eb7335b	01844359000	Joana	2a661f4d-ab86-4f25-9f23-be80a94a00a1	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-17 21:22:33.059	2023-12-17 21:22:33.059
3dc56798-9cf0-4010-82ae-015ea1e04e5b	96523476512	Joaquim	49b62e86-2754-40cd-a73c-57c08ddfee40	35143522-5df4-4334-ae65-278f67eacb9d	2023-12-17 21:25:08.86	2023-12-17 21:25:08.86
6b8d7fa5-713d-4d83-bd57-1761ca8defd4	96523476512	Joaquim	3a8ff2c4-fcc7-4beb-8996-da1ea74954ff	bb98cdc3-72c1-438a-a288-8254088f5e80	2023-12-17 21:25:40.32	2023-12-17 21:25:40.32
b05f5f20-d0f5-49d6-b138-a5555097654e	01844359000	Joana	c5a18f82-b3b1-47a7-862c-b114ffca2e35	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-18 01:57:21.106	2023-12-18 01:57:21.106
53bcf822-01f1-4f8e-b9fe-d493c1ae5e9f	01844359000	Joana	43ee6065-31a9-44d8-8f8b-85144086d076	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-18 01:57:21.106	2023-12-18 01:57:21.106
15e549e5-278c-4c7d-8213-79cf3883a440	01844359000	Joana	caba43c7-8bf3-4a52-90b7-4b2ed720e307	fc1a23bc-bec5-4bcb-9e92-98fa3b446595	2023-12-18 01:57:21.106	2023-12-18 01:57:21.106
44794282-2721-4d30-a2c0-c387c05bf3d8	01844359000	Jiramaya	2c3c168d-9148-4b16-94fa-92372b8eeddd	5259a389-0263-40ef-8a5d-7551babc3b45	2023-12-18 02:42:43.517	2023-12-18 02:42:43.517
\.


--
-- Data for Name: Usuario; Type: TABLE DATA; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

COPY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario" (id, email, nome, senha, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: UsuarioCinema; Type: TABLE DATA; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

COPY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema" (id, "isAdmin", "idUsuario", "idCinema", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Assento; Type: TABLE DATA; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

COPY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Assento" (id, numero, "idSessao", "createdAt", "updateAt", reservado) FROM stdin;
60336277-ff4c-4661-8e14-be34bfddc57c	2	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3b20b627-a17c-429d-acef-4a64cce5065d	3	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0ecbc7f4-3c5a-4961-a1d6-d3170865fd55	4	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
15ad58ea-3578-4b48-9dbf-59daf10781cc	5	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
44ec1eae-3033-46aa-81b4-359cb1f7a2ef	6	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d47ba84a-4b1e-43e8-95a7-f628ff523c9c	7	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ee0356bb-8b7c-4df8-9d74-6a12d07a0bb0	8	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
2c9616d6-1dde-4098-a470-07a75c109ef7	9	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ae087c03-5961-4f80-8b27-1383b2798b1d	10	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e3474c3a-8d84-4b59-a7f9-cf2559d5e259	11	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1d1317c8-4c8e-4c38-b48d-2e7d4f112b1d	12	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
bd35ee30-49d9-48ff-a83e-6e7b61b1550e	13	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6c980db0-4058-497b-9d92-2949ce184c74	14	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f49bf80b-5fb9-4ca3-ac8d-9c4cb04db968	15	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
429d6807-5347-47ed-811f-e063a135bac0	16	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f8121ea6-d1f6-4db1-9873-44e11d687529	17	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7ddc2933-12ff-46de-9203-d26155a85ea0	18	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
37395e77-ebc2-45c8-a163-31ee7cd92c69	19	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0303be6b-6ed6-45cf-b239-ef5340346226	20	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e0deab00-5278-4ca4-b24e-95d855a74126	21	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f58b0019-8186-4182-acd3-4b7ab807e156	22	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
73199ab7-8a9f-4eea-923e-fd882e86b3d3	23	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
94e5d6c1-9761-4af2-bc64-560b2ab8d20d	24	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
12be4ee5-6b2a-4d0d-81c8-93cf5a29e473	25	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4b81df54-1a8f-4497-8fa4-6fc278038e7a	26	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
80a25db6-ae16-462f-8c26-a4fc842e1090	27	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c751a9d1-12ff-4278-8df6-a494d1ac242f	28	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0507b6ed-1470-4578-b81d-e7b985328933	29	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
31afe627-31a3-4ce8-842d-f094dff7e4ed	30	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
14ec2072-b404-4b01-9a83-abe5a636d4d5	31	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b8c26e44-bedf-4cf7-96b8-55b04318dc18	32	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4b1716df-504f-4c17-aab2-46b10bffa913	33	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e61e64ae-6aec-43b2-8da8-5ad55b96579f	34	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
bdf6d6a5-ff7c-4d01-bfae-a676e49aa88f	35	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3707ed88-6eb7-4454-8f99-9c79f3bdd794	36	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
96439afb-f0e3-44d3-bf1c-b229ae7cd638	37	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9a3484eb-d090-42f9-bbad-d9f2c17d3ae1	38	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8d3dd11f-9bf0-4f4b-81db-bdeb77800241	39	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c931aa9c-f4d4-4927-8b3a-8bafc7c17776	40	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
656e3bc7-facc-460d-ab9f-922f66d8eb93	41	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e27543ea-285c-4d90-b5b5-3cc36a1f0f15	42	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
94f74375-6509-4a04-8ec8-6b4ba01eb789	43	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4577e47c-5f60-4c5f-8ddc-e61c3c9de833	44	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
872142bf-17a4-4e46-80c3-209504ce89a5	45	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3db5fbdd-c6a1-446c-bae0-f4f825aa304c	46	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d55ca291-34f8-4d0a-bdd5-cbffaf402c66	47	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
67dabf40-b45b-4a9f-9d19-595002e819a0	48	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ec72347b-b2f0-4424-99bb-bc7b35095fe3	49	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e49c0a49-9c50-4622-9e79-3232bedb1ba8	50	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
736ddb3d-a5f1-4139-a1d7-bbcf1b2015ee	51	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
73b2a726-efa5-4a07-af76-20186586edf0	52	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7b7ba967-0177-4506-a021-b59f703f0ccf	53	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
680fe3f4-20aa-4f7a-8454-6a53fb6bd2d1	54	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c2c848e5-ab40-4d5f-86d3-e582fe509e27	55	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
071d867b-6485-47e1-87f2-92d5b93abcc3	56	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3b16f9ae-6766-4c83-b6c1-20e0e6caf398	57	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
34e93c01-b1f5-49df-a9fd-c0deb143f83a	58	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
05254d36-14ef-4e6c-822d-ff5ccfe6211a	59	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f2a2e19e-6432-4b3f-8920-c4d1da43767a	60	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
13fbee71-6307-4549-a13a-16728198d5af	61	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a9b6d21b-716e-411b-b769-5095aceecba7	62	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
60cc5b01-3d36-470f-9978-36973ad9fe15	63	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e4136184-4e53-4a98-a6db-4c97662d90b5	64	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3511f873-91b8-4147-8940-792b9497e5d2	65	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
16f0c396-6c94-446f-8223-4578e7650e03	66	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5a307502-2e5a-498a-bc9d-9c28b138996c	67	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a9a4f6a0-2968-439e-b2e2-5738c19b5497	68	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
42006f3d-a2cc-4ce7-8120-89f785ea4349	69	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
2235addf-0592-4822-b7d4-78178231400c	1	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1fc739ef-8f67-42d2-b4cd-9591408e4954	2	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c4f62d93-734d-4607-96ec-ec10f08451d2	3	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7d0aea57-a0b0-45af-baf6-902a8fc36793	4	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
eabbae1b-b7c0-4945-8d32-1afe462b41b1	5	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
cfa23fcc-c690-4f2e-8fec-cc01ce48c59d	6	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
813f210d-47bc-4def-bdc1-e26b5ff47378	7	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7eb6d9f1-9bb1-46a1-8ac9-4fe14b7a7246	8	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
40eb4331-3d0d-4eb1-9b2e-537203c2b104	9	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
bcca5014-44af-4758-88d3-7a3411449101	10	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
41067ed9-5db0-41b9-a870-5b2de6a6a494	11	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c539f943-802a-486c-876a-f622bde402c7	12	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
fa8d162c-32cf-4607-95d2-55c969583d51	13	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
68d5df94-254d-4837-8453-0e8ad5b7aa06	14	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0071a361-98d9-4f5f-9b33-b252ce1160ec	15	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6112ba9d-89dc-4fc5-b417-d6db4f0d34e3	16	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8fff760b-7d18-40bc-9a2f-bf818bf94c18	17	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
46d83121-3ed4-4b20-b712-70afd7bc3299	18	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
58ae468c-f5b4-4d60-94be-08d663b069b5	19	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
de95cb97-e852-4495-80fa-43ca29bc1e16	20	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3e939564-ea10-46cc-893a-b26505299347	21	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9070c972-5dd0-4478-a038-0bed9aed4995	22	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ef16b920-2fa5-471f-8d69-c4d29abdac08	23	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6aad7296-7a92-40c3-98a3-f2e39de1b56f	24	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8bb59b9d-6fef-4576-b6e7-1e0b87208db3	25	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6b9d294a-fb49-4fbf-b846-e679f59d0ecf	26	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
046561a3-4a20-4ec7-9ca1-94fe96ebdd21	27	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
eae7092a-97c2-4ef8-a7f9-7f8699df81ca	29	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
102e24d4-b9c5-44e5-9087-8fcf9764c765	30	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3ea2c2a2-306b-49aa-a0b5-ff76236b0a4a	31	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
76336982-bd80-4338-b642-7984f579c3b6	32	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b62d539d-00f9-4995-a89a-2c7c0ff2ab0f	33	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
aca9e3ed-aeb8-4366-ba33-2e1f0de61775	34	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1acfe0ec-ad99-40bc-b9a3-ed635b55f368	35	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e398bbb9-20e2-477c-9f86-e142523c47f5	36	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b7766495-334f-4016-96ac-85dade007db6	37	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e0e4e30b-e190-4640-978c-37fdfcada9e4	38	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4f530ded-3fcc-43d1-9d27-b80123f7bc5e	39	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
14293802-406f-4e74-a681-ccda07962071	40	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0f885b50-bfca-423c-84a7-f3cfcc0d03d9	41	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9a851602-fa8a-4acb-8137-873ae7a4b046	42	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9e621abb-cf65-4d92-8bd0-e53ac2669cd4	43	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
37a0a828-c65c-4c5e-bd08-8ddc2f0203f1	44	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9b73fe95-4a18-4b7f-9596-c1de5cf61806	45	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3e577e13-6651-4b9c-bd3a-96070264e555	46	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e35bd1e8-d68a-474c-9c37-2ca483e11e11	28	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 02:11:03.741	t
f9c885b9-8fd7-4db8-b92d-7a4589927392	47	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6fc66a90-3a92-43bf-8c58-8e609f5434e7	48	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a16efb9b-6cb0-449e-974a-2f9f802f0534	49	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c881263b-d9f0-42fa-a470-226bc4d5784d	50	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c4bc16af-f32d-408d-80e2-abea2b79060e	51	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a67d7cdf-08e4-44ee-9355-bd47757451d8	52	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5f03c2f9-f591-4fca-89f2-03c12197b147	53	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6aeeacdc-0264-48e6-9238-2681bc873ade	54	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
cfb1c322-1ba3-4c7c-a420-6258e7bb2116	55	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f3d5c2a5-4800-425b-80a1-96f2549ceeff	56	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1ae8c08a-ee77-4462-8594-5fe3c18ce833	57	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6aca0884-2b1b-4616-9f9f-57637a181793	58	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ef952ee5-c480-466a-9ed4-cd7b86768ff3	59	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f83f00dc-8232-476d-a367-4d3f8e24caff	60	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
582bc67a-3a59-4343-8831-ca41fcc8a5d1	61	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
88153df0-899f-49ef-8172-c36375e114ac	62	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
58e84768-de6f-489b-a21e-d32120dc65e1	63	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
006eff00-f9c8-46a3-af44-b75bf3c8446f	64	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ee1d8f80-7d13-401d-ae89-8b2cfc86ae1f	65	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ade8f061-fa76-416a-a164-9d8ccd7d8260	66	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9255fb1b-9b41-4a97-81b6-6106ad5f7ef9	67	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
aa484e31-8614-4eed-8082-229d2a1a0d54	68	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7222c631-f113-41f2-bcab-9324cdac185e	69	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
47f3f887-3c5f-41c4-9c36-2e471d2d615b	70	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
be84e50d-5c92-4c00-9960-b3d21f5c526d	1	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
23f43f04-4ac2-41a2-9963-dfbd25268160	2	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
67be00da-6fb7-4fa8-a442-fb2dd4305ea6	3	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
84717d52-1774-4477-8b32-90eaec8443ba	4	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7e40726f-0104-4641-b8d3-0207356064c2	5	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f4468090-07ae-467f-8b24-610b7c49a2f9	6	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3f839fb6-b40b-4242-8407-594540a60ec8	7	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
af253086-cbb2-4d0c-8ccf-920f15dfc066	8	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ceec33ec-fdb4-4b05-b07d-ad0b85b91f7a	9	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5a93d04f-b064-4358-a1e8-35e09766e17d	10	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f67496f5-2a36-42d7-85e5-44f7ad404b00	11	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
839dff5a-648d-4c60-be2c-be426bb9a70e	12	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c473c2a9-c7b5-4bcb-9c98-0277cec6ad59	13	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d847b63a-c55b-49f5-b384-80e8797ab567	14	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e9ac6b4d-1214-4c08-966a-2fc3aab15e18	15	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ba932d20-cf48-4448-be20-463ac2bb2a78	16	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a8471d04-49d0-43c1-8361-479781effc71	17	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9e47a85e-a206-40b4-a744-5dab52e572b1	18	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c730e121-2b1a-4a87-9632-02036a901673	19	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f98a55cd-56fa-4aee-a661-730517fdf6f9	20	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
39700196-7fef-4bf0-984b-0d657ae30c43	21	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7b79813b-6ae7-45e6-a2c5-b0df9a1ac59e	22	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c743b9d8-7849-4b32-b9be-2333ac70611f	23	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6e80812a-f8e1-4b27-b40a-5252d3d73f83	24	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0a3ae6c3-35ff-4b72-a364-59334804b686	25	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8b2aff09-2fc3-4720-a832-3ef8a9ffcc1e	26	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
bfb6d28b-b43d-44e5-8ca1-1da80cb1760c	27	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9cdbf951-d986-4b83-bfbb-db62ddb669ca	28	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e46c5683-cd49-4a4e-82de-1f6099190d46	29	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0a2391d6-4593-41dd-95be-365ec421ebaf	30	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5f3a6403-9887-47a6-8229-dd4a80a50a98	31	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7af91230-d3ba-4f7d-9829-46d5c5360e33	32	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
95b9b656-823a-4e4d-995f-cbb11db9b91c	33	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5c09a617-e595-436d-b74b-42587e427cfb	34	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1bda4d92-7108-4b5a-a693-06b1c528f83c	35	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
99b8a7de-2835-4e63-a3ef-46547d4dd73f	36	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
314f1630-1943-4de3-bcbc-34b2eed1c86b	37	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7f253e3d-b5d7-497a-b51f-a281e5bc87d3	38	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
11ca5fa6-dd1b-4e0a-ab1f-b9fc7218e493	39	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9c49f19b-9c73-45cf-bffd-71887f4d47f6	40	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
22f13e3d-64c7-448f-bd87-82e167ff1b60	41	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b49eb6d1-80ed-4ce8-9df9-20ecdc5ad4a6	42	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
cd61e7d8-a835-4a49-9aef-2dbd388a6401	43	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0196878f-01b6-4862-95ac-aec0eeb815f1	44	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7fd0cac3-e5e7-4576-9946-7a851e155aea	45	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7fd77a98-62f5-49dd-95f3-5bfa3b3ecb4c	46	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5cfe374a-5777-4f5d-af0f-37737eb7ef0a	47	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b0edfd79-1ca1-412c-ae6e-8e5d35826f4d	48	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d9a106f7-e739-4972-9fcc-c354b08c39a0	49	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4e68ea2f-7531-4cc7-a982-cf87f77032f4	50	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
98b8e0c7-4e8f-40c7-9ba1-8d52034c9e4e	51	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8154d5c9-233b-4b31-afeb-640828340dd2	52	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5d5dc6d5-c87b-4478-bf49-36c73c824eec	53	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7c20c2b6-3543-4264-be08-2d7b3ac593c3	54	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3ee6676f-6625-4ce3-bdf5-0c24175acd1c	55	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4800d8ec-07e6-4710-a73a-675f3eeab8b1	56	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5296d717-56ce-4d07-ac9f-2585ea185ddd	57	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6e1aee75-14e5-4eab-b259-1f0814bb27aa	58	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
80e8eaa5-ba34-48ee-be8c-82eab745c99f	59	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a3944fd5-ea43-4316-8b49-7302fc9ec03d	60	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d29e4577-1294-407b-897e-6b70b0d2b48d	61	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c9e2fa12-9a89-49ec-9e6d-b68e9777a981	62	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
edb7d24c-4283-40e1-a61d-a0d444c96f8a	63	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
162677eb-8178-4c10-a4bd-0bd8a0fabbff	64	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8560a74b-8e5e-45e6-9dd1-9e117c1044df	65	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
38a6716e-86c6-4313-b09f-5fa736949aee	66	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6dde711e-257c-4c1f-bd09-7d18d91a3d8e	67	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0ff8aa0d-9b42-4954-af5e-cfa23bf33451	68	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a88014e7-1124-4cd9-8c39-de2af58a39c5	69	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
92233cb5-52ef-4e66-9804-b32796a0cb97	70	d2b5c637-1110-4ec9-b1d9-4217c98fc348	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a41b2b92-43c1-4954-b9ff-53c82c2fc6a4	1	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9f750ad7-0cad-4617-a528-e6cb3dff1ab7	2	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
de6703be-4fc6-40ee-8dbf-d9362d7ec363	3	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
385d9aea-a8bf-471f-a08e-c468fe99a523	4	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ef7da0e4-9c7a-497b-ac7c-b32cf1450b37	5	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9e32f3ce-113a-4e82-a693-a77d655787c6	6	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
47d8dbcd-09e7-40db-9290-2396680ea796	7	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9950b27b-62fb-4f1c-838b-16282107465b	8	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
71a3c65a-c51a-48b1-8735-7492e1ffeb25	9	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f9773dde-19c6-40c0-ae36-d39fbd8517a0	10	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
dda79247-394b-418d-bc44-1c2fb777bb04	11	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
860e7b61-0642-4553-bb79-641d4c9e315a	12	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
252dabbb-764e-4334-982c-d634996da0b5	13	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
55912bfd-df08-4890-921f-ca3659959b1c	14	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0419d8b1-f3f9-46eb-a266-432e4edbe3ce	15	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
36f5cb6e-2378-4e69-8d91-7cfe5cd11af4	16	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
861be6bf-da65-45f3-b1cb-220c4a3efc2c	17	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7422bb63-2d7b-4eea-8c9c-47ff6cdb677e	18	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
91b81e34-7bd3-4ad4-82dc-820ba567cb65	19	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
27e59bb4-0842-43b3-9e30-3f99f8f746f1	20	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
82b196f1-0d39-4211-bf0a-876b8fed57d3	21	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
53a539df-3dbf-456f-bbb9-887f67eb8aeb	22	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1fbda56b-7fba-46ef-846c-3edf71705ecd	23	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d7d92350-4342-4651-a976-a703d16cede3	24	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
235f22e6-1c81-458e-902a-6694ddff19ef	25	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
fdaa890a-147d-4e86-95a1-4296e161f1fb	26	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
469b999b-45f3-475a-b401-02ae5071e582	27	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b11e37c0-2a33-40d9-85f2-19b62b9edd44	28	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d00ca964-f684-406c-8091-ad4ce1915f6d	29	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4ec245dd-1f2a-49ba-a803-d3b2f909556b	30	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
75bedcae-a769-4753-a15c-3b331f5e82fb	31	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
dd534fd9-c896-4d50-8b6f-55814d5b709d	32	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
76960787-9874-47ab-9983-0896c91efee1	33	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1743e76f-bc7b-47e2-a027-be0cb0bd6290	34	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e6576162-f079-4a3f-8649-9f26ae5b42f2	35	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0955074c-7334-4554-9068-3b6c639069c1	36	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e44542a2-fde8-46c3-b874-0a4970a0679a	37	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e192fb83-8da1-4f92-9a59-adfa7a1c1f69	38	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ce626e05-2358-49ab-a63a-96e8fb56e628	39	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
67680bad-c40a-4bcf-b965-40aec12c0bb1	40	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
2f5028cf-d767-4873-88bf-b7d874b4cb29	41	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0b68c779-5f5b-41a1-8d72-6b586c819575	42	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
024955b8-80d9-4aac-8437-12b9d9c740a1	43	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
59ed3eeb-5faa-45ae-8db6-cbd74d44f092	44	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ebe1b753-d4b8-4d56-8ca1-db5958dc3a18	45	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4faeeac4-7ffa-4964-b905-bc6b5fc2dcb4	46	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
58051804-3af8-4f4a-8751-5bc7f7707c0a	47	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d0f9d85b-dde1-407b-b971-d7ca9ad2bd54	48	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ccb433ed-5639-4b69-ae0a-cef11c420c2d	49	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8b6ec6ce-f6b7-48d3-9b3f-5ad50924e22e	50	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0b76aa8a-1373-40c2-b0e5-c6de928b1530	51	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5295ed02-91d4-4ba4-9d58-b0ab444b3d60	52	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
dc9d6d67-eb3b-4be9-a4a4-e49b0b54184f	53	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
386a7cc8-670b-43a2-8303-99a83dafb045	54	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f6730fd7-0a68-4c82-ae59-178f149f34e3	55	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9b6eeda2-3b3b-42ed-a19e-e442a0cc2d56	56	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d9db5074-19e6-45a4-bb50-731933eaf59b	57	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0b275c24-7a41-47b6-8cbb-96bb62e4fadc	58	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
16076773-b0fb-4247-a52f-26d56dc93c22	59	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8629989d-eb90-4c75-ac07-02ba8ccc10a2	60	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
95bb8e5c-afbb-4486-9cc9-918384035aff	61	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
24f09633-611e-46e1-ab72-0b01ef0c04ec	62	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
da10e7bb-807b-459d-934c-d39bc951e32a	63	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
81bc7443-922b-409f-ba74-824d159a2e12	64	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
bf3a962e-5c13-4311-ba4a-55027cf5f855	65	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e92d8ebe-e152-4bb3-893a-c4ace0cea6a2	66	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
bc7102d8-df56-4394-8f76-cdf22460c13d	67	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a0b7edc8-f3e5-4b1d-ae9d-f0b51c42b613	68	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
513b0520-2c91-4961-aef3-94df12063f61	69	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e4dcaa97-4629-47f8-b93b-6bb6912ba006	70	4407ad92-b304-48af-933d-2fda9118965e	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f591a610-5391-4019-b352-a3c5563fe09d	1	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3eb1f0de-fc79-4a58-a7bc-5f2fcc1e7c55	2	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6e411ecf-a54d-4f4b-8eb4-bf6b2fd7be0c	3	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
feebb445-a6c3-4944-9ad2-49a0fec50cb7	4	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3a84223e-ee7e-4014-ab5b-ef43902e2db6	5	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
861c5907-756c-465d-9f43-d251cd525825	6	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b8a1c709-4ca8-4a26-868f-f4b61229fac2	7	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
210bea21-76ed-4bbf-b561-bf626288ed96	8	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ee470722-b310-4eb8-b425-c16fb21c90e0	9	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
cef1eeae-62b3-4b13-b75e-8750a7fbeccf	10	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1e09662d-a132-4a29-91ae-70038e84c247	11	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
19e629b1-7f1f-4ac7-9ea5-20a42e52e372	12	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
fb44d4ef-895e-4888-ac0e-062505b5ca0e	13	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
673dc210-bd68-477e-bfea-c1d8a8470550	14	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d5e340ea-3720-4b76-a624-d39ffd11d32a	15	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
bd335e11-454e-4d5b-b64e-fbeaeb6358b3	16	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
effe4635-cb8a-4bc0-bf3b-a624c33c8dde	17	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4e66cb36-76a0-467e-a0b2-1c4d1c2a8251	18	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b3754317-9b63-4425-8fba-653a692caadb	19	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
dfb54a76-dcd8-410c-8145-1a579657639c	20	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b530728c-47c5-4657-997f-a4bdacff9d3d	21	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b2a34f6a-6e92-404e-b6bd-d836d8f9a051	22	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7d8c2fd1-3840-464a-a827-197f9f11de82	23	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f9da2446-721b-494d-864d-d68ff1053624	24	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
54bca39a-f798-4ce7-981c-1d625a3ab033	25	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1f350c07-d340-4120-b757-b8bcc0262606	26	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
314841e1-886a-421f-97ce-ae56b7fa9816	27	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
364280f0-0a06-4372-8fcb-9e14f5ab2c0e	28	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3925d72a-c9c6-47d2-b33a-1af3fc14edc4	29	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
835d0bff-e77d-4cd7-ba5f-79ad8ed01dbd	30	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
88c81a09-ef5a-467c-a83b-a1836cbca9ac	31	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4cd5056f-5827-442e-8025-80407e344baa	32	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
904ca237-0d7b-496c-a88b-84ac8b0d3cf7	33	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c61e26f2-6d53-4534-8348-2a83f234b873	34	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0380f991-fa66-47a4-9eb8-01e486e3d4b4	35	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
86b98f9d-87b8-4bea-8ce0-ada41272765c	36	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4561d42e-2b8b-40dc-98da-715b79748009	37	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
95178b50-5717-467c-86ed-67ca6472b4f1	38	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4f6a891a-c4ac-4e3c-a42e-bb19e51665ec	39	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
33e0fa12-1db3-43d8-a225-b1fb29b2fb63	40	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ac37ff58-4e69-4302-8698-0c454a56b26f	41	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6003baea-d7e4-4615-8812-c76d37260ee9	42	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1b9ba9db-eb26-40fb-b77d-a964aa620065	43	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
50500fc0-78cc-4f9c-b63a-2fcb923b5aef	44	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b49f8508-e388-48a6-a6c6-37613db2f743	45	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
434f961e-1b9e-40aa-a3eb-b210f112fa44	46	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d5d445b8-4244-4710-99fa-704bacbb89fc	47	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1a6d0400-e826-4de1-86ef-8ae1ee87ecd7	48	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8c927d7d-46b7-46a3-a4da-044e6b822854	49	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9fed8307-7b80-4144-a6a0-9fc65eef1867	50	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
db842e0e-f433-40f2-bfa4-2dfd7cb2bf0a	51	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
35f2debf-d865-4518-ae54-18d40ee1ae47	52	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e2831a41-bc16-497e-917c-a466310d6119	53	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
03c6ebf4-5c2f-49fb-95b3-0e0b94328e17	54	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
731fbe10-b81b-4eb9-bc6c-f333ded5c285	55	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
34d0cb67-5237-4c58-9b53-60786086bba7	56	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c884546d-340b-45e1-b1f4-dd4c6729e144	57	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5680a3d7-ba00-40c1-ae35-c3856ced2d02	58	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8f366c5b-7bc8-4274-88ce-a0b4c7bc3441	59	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a6ccabc3-06ac-4a57-a9f5-2812714f7cae	60	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a789ad71-6c34-4fed-912b-7cf841ca7b68	61	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6a3091f6-9407-4769-9a02-5363c304c188	62	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3f39676a-c511-46f7-91fb-1dccc31eaed0	63	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d566a75c-f706-4504-a528-833c18094ddc	64	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
da63253f-170d-4a02-ab32-a370ed172657	65	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3423b802-5e38-468f-b523-471b14194b3f	66	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
294693f6-d73a-437a-a45a-9776cb525f08	67	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e2dbe4e1-d17d-431c-981e-b97133da7d38	68	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1b12dda3-6192-40d4-acc6-f7695fca4e3c	69	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
05136566-e74e-4f9b-81ad-af32a1feeed1	70	c1374acb-d3f7-4654-8dbb-c23be902a608	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
33cff140-e621-4f0a-8e53-b4766573deb9	1	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
2d63a992-7984-404a-8c24-3001d74a9bd4	2	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
70102f13-3c15-4ba2-be4d-4c5876211fa0	3	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b38c3f49-ee58-48bb-9e5e-9e07888c7fb2	4	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
dc5df797-df39-4540-b629-4a961bff5859	5	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
055bf9b0-3281-4d02-9d24-3b99db9a83d3	6	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a68ffa16-3fed-4185-855e-273715038ffa	7	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
eb70b926-f63b-44dc-b7f5-5bbe20110910	8	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5eff7621-2994-4409-b189-58f754b8f2a1	9	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
73ba5ad0-0e25-47d2-9694-fada8be956ee	10	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ae24d5d0-9316-4046-99f3-34b5ff66e219	11	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
2a0de0ac-ce29-4435-9e20-a43d108b81fa	12	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
016f6ccc-ef60-41f1-8e01-3b656181bb37	13	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9e6e7e52-14ed-4555-b6c0-5cca6a7e3ba7	14	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
63515947-842c-45e8-921f-b603d784506a	15	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c0b84594-74fb-4d0b-87d5-c0f27cbeef5c	16	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f93d18f3-2413-4222-b2c0-2de1cf03e787	17	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5ee870c0-3186-4744-8131-82211b5aee5d	18	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
301d8779-8047-4702-a247-ba15a4fa74ae	19	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
31ea9c90-df4f-4883-9ed8-96df0422e52b	20	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ad2fb48c-2b33-477f-9b20-c136cf43ed41	21	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f61fe26c-a568-42a1-a889-a4225c320b45	22	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
87f71ccf-d9ce-4dfd-a609-e9bc1a10ef6a	23	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5a2fee68-d563-4cee-9dee-e5f741548bb2	24	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
cc4b8172-1a15-42fb-94c8-c46cfb03b772	25	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
54915ab1-fddf-45ea-81c8-e88e91335de6	26	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c8bf5723-7146-4f53-b292-609759337d48	27	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
bf828c1c-c513-4693-9f50-c9838ef08e6f	28	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
7a7778b0-4431-477a-92f7-75aa5d9ba400	29	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6038f2dc-3276-452c-be69-28f26223d0cc	30	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
bdae1b53-1954-4b62-87af-5bc2d6aec395	31	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ac8a4513-0a64-4364-82f9-6fffffa55ffe	32	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
bb84f42d-b840-4d99-ad01-bde389510b8b	33	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ca329a22-5ed9-4be5-b207-42769ba71943	34	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4bdcee24-42ff-49ee-82ed-829bcf166780	35	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
04a24ced-d1bb-4dfd-8d63-7fa8a2799b1b	36	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ea7e7711-9c1f-4a59-ab96-a3508dbd24e9	37	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0d82b094-f944-4435-8e1a-a7c4f4f0e8b4	38	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e44b0c81-ba92-4dd3-b599-9e0c73726f04	39	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
649ce4a9-66c0-4b30-8c91-06797c112f6b	40	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
aaba6a67-1375-4031-962c-64ba5601b8d1	41	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f1caa25e-eec7-400c-819f-16f4f6f864db	42	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
14a3bbca-c83d-4cd3-b1b5-77fae59d4670	43	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f7deb33f-1096-4c77-bb6b-0a68ff7d78cc	44	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1113efeb-d7f2-4ba5-b043-6e75266dd58f	45	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e472b1cd-3a61-4520-9f08-f47aaa23c625	46	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
54e41fe7-79f3-4245-8556-c5763f795cdf	47	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
60a13397-c022-49b9-80d6-87790984acfb	48	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0919680a-cf9d-49bc-903d-e24e40a3d4c6	49	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f628aa8b-59ef-4465-aacf-00294c704bd1	50	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
299bf437-5364-4a1a-8c5f-43f386a1c5f0	51	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b78dec4d-31db-4445-b89c-9b90c623607f	52	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f4c512c4-1e7f-44e0-ba17-e9620e645e87	53	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b5c140e8-bdb0-426f-9170-f0d42fe80822	54	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b4094c57-fac8-48e9-a613-ab25981a6e42	55	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
75fba8eb-ed8c-4b56-8ba4-055278349f7d	56	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
06f3541f-1900-4920-9d71-5d9cf2c90947	57	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5b8bfec2-b949-4325-a8dd-c1b3a0817d9c	58	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d960db64-ee2c-45ce-b32d-868456895ae1	59	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9937ad6b-51d4-42fb-8be4-8e22eb43c348	60	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
9106a115-2350-4364-9764-a79104e06993	61	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
052f5508-b437-460c-b540-1faf43b1ff41	62	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
95e8cd93-bc99-47e1-9752-87277eb0885d	63	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
33299d8e-c0ff-4f15-8bf5-7deff92f5606	64	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5cfd324e-a121-46d0-81f8-9bb42c068807	65	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
823bfe60-3943-4a56-abc8-1c29bc57f887	66	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a28b14fc-77d8-41bf-9fc2-491464c0156c	67	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
617a57df-dc1d-42da-b514-1899b342bb89	68	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
61134907-65a1-4c9e-afef-b72ced080411	69	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4e9438ee-9dbe-4298-86c2-2834014455e1	70	b3930fe7-a744-4ec0-900e-f3661b68e69b	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1ac4a313-018a-4049-8781-d26398ba5e8f	1	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
dbce1cd4-e219-4aef-90f2-e1ff992ef9c5	2	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4c93ed85-a180-4bd6-8868-cc575bb119a1	3	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f7042619-854b-4c66-b4e6-4d713e1b96cf	4	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d56f266f-3f23-428c-a9a3-2c53cfb45991	5	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5f9ff554-094b-4111-aad2-6eaa3018ca8b	6	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f2ecac27-2ab3-436b-8040-6d8f12e578f4	7	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
dc14c33e-a285-4ffe-aa0f-806f35d73426	8	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
40856295-555e-462c-88f0-dbad75335d2f	9	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
970d03ae-abb8-46a5-8ef6-00fb01b847e0	10	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5c9b4762-f1dd-4cfb-ad60-a3478b482288	11	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3f414d63-bb02-4883-950f-d8085aa177ed	12	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b139099e-167d-4585-922e-a547a2a5709e	13	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
72e70230-e367-4e03-9261-3b9ab023f600	14	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
abe09cc6-605e-43f6-8974-568e23af055e	15	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
cfcedc54-adb9-409b-bc75-3ad3d3d3abe3	16	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4ff02797-2200-4fe3-ab8d-a100ed570439	17	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
99b7ef2d-e938-4ad7-b3b3-6dd262fb8981	18	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
c50000a7-68e9-4113-b414-15f0f863f4c9	19	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b82f55b0-6c99-4b0a-a4a8-9c796a8354b0	20	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b7649b80-5053-48ce-9182-30d561978d03	21	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0aa36172-d80e-4fbe-8496-ddb6d313a8da	22	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
208f41a4-5354-41f5-8aa9-82139605a1d6	23	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8588abc0-2eae-48b2-8441-5fea29a37074	24	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0d23fe66-bbb7-4d1f-b860-1c2dbe87309c	25	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
3d904a3c-749c-4d50-bca7-e53f1a917e68	26	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
bbaa1f35-80d4-42e4-98fb-003c1a00144f	27	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f668fcee-d2d9-4698-8eac-06ddb16f2731	28	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
121b174f-493d-4a17-b601-d21dfc241782	29	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
44eab080-b414-4c54-bbb6-60869d2c2244	30	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1ce882b2-39d8-4dfa-ac1a-ac09d483a555	31	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
df552a76-a7fd-4f8e-b3e2-3d8c515f0b23	32	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4024f27e-5174-4c7a-869c-5399b54b7cbb	33	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
5849d864-2e64-4f80-8491-3a05bc9aa7e0	34	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
597a6a0a-d432-4b75-bae3-e47febe947e3	35	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6ec06a58-49a3-417a-8fd6-34b3bfbde923	36	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6da2f101-9de2-4ebc-9b5d-937c2c35b168	37	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0f1fa33a-ce5d-49ed-82f7-e767a5a084c7	38	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e4358f67-c7b2-40ba-a56b-317967e81880	39	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
0d5ad081-8880-47cb-8995-72c2397e5dc3	40	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
51536de3-130b-46ef-beaf-4f903f573c06	41	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
66bcca1b-0984-4b3f-9d14-7502211a24d8	42	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e4e7e46d-5586-44a4-99e9-f1b15a92dd4d	43	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
2a48e3d1-2bc4-49a7-a51a-dd1e4de624d4	44	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8c5045cb-0432-4799-ae3e-87beb86bf38e	45	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e40613a1-909e-42ce-ab54-1a069dff56a9	46	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
45cb140b-920d-45da-85e4-e7a61f3adf55	47	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
47f638f0-bb08-4786-a6fe-e7e5f3fea47c	48	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
00691c95-498f-454f-83a7-5995135a162c	49	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
82632645-b4ba-4e56-b44d-29eb65e85c5c	50	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
699b74dc-8e96-4e39-bdbf-cc8230dcd8c6	51	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
dda3b31f-bcad-4ad6-bf10-4c9e22a5018e	52	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
39b13fc4-2963-4463-9018-3330ee845a8c	53	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
4376c064-8719-4c77-9a43-c533fb2e18b4	54	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ae84e110-6da7-41da-8b6c-3905d8e3c26b	55	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
1e6b509c-fb33-4f1d-a007-214ed516eefd	56	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
45d93b03-c344-411e-ae12-2ecff0c314b9	57	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8594fc54-4249-4888-8ca0-2691a9f5e182	58	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
ee530f2e-3bc9-4e3f-a7e0-429ce9d7c247	59	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
b47610bc-e685-4183-a3f2-56ed8ea0c2f2	60	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
d5974125-57e8-4674-8bfe-02db1e13051d	61	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f9950de3-9209-4ec1-bae0-ce80f29c290f	62	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
8df6c396-67d5-435f-8ecd-5309f3d7350c	63	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
976196b3-2ba4-4c49-bbfb-9043712c4e1b	64	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
bbb4c25a-7152-44dc-8728-da10864aef7c	65	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
def4c2e8-6c08-4cc5-b269-1f20b62f7b5e	66	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
a416afcd-89ad-4da0-b983-639e5a4c7eae	67	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
6961286a-21a8-476d-b4b6-a8af095100d7	68	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
e2cc06d8-8a0e-4e9d-99e2-ff56c0de8609	69	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
f1606ad0-1e22-45f9-9733-e619c1d40a31	70	ede44602-5517-469f-be49-69970b0d72ba	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	f
dcff22ff-6c39-428f-97fa-0cb5f0f868e4	1	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 00:42:35.126	f
67ca81b1-b1b0-49ef-b77e-9ef9a241dd03	70	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 00:03:28.323	2023-12-18 02:09:37.936	t
\.


--
-- Data for Name: Avaliacao; Type: TABLE DATA; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

COPY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Avaliacao" (id, "idFilme", valor, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: Cinema; Type: TABLE DATA; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

COPY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Cinema" (id, nome, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: Filmes; Type: TABLE DATA; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

COPY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Filmes" (id, nome, sinopse, "dtLancamento", "capaUrl", "createdAt", "updateAt", disponivel, "linkTrailer") FROM stdin;
\.


--
-- Data for Name: Sala; Type: TABLE DATA; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

COPY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Sala" (id, nome, "idCinema", capacidade, "createdAt", "updateAt") FROM stdin;
b445afaa-41ef-4f44-8d11-729df03d47bf	Sala 1	8afa6a12-cf3d-42bd-98b5-4961debd9bf3	70	2023-12-18 00:02:57.502	2023-12-18 00:02:57.502
\.


--
-- Data for Name: Sessao; Type: TABLE DATA; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

COPY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Sessao" (id, "idCinema", "idFilme", "vlEntrada", "horaInicio", "idSala", "createdAt", "updateAt", "dtSessao") FROM stdin;
2884ab6b-19f9-432a-b3e4-030ebfff0a18	8afa6a12-cf3d-42bd-98b5-4961debd9bf3	d67b33c3-e334-45e8-9be9-4b8a052851a9	25.000000000000000000000000000000	18:00	b445afaa-41ef-4f44-8d11-729df03d47bf	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	2023-12-27 03:00:00
35d7f81a-062e-419f-a6db-06a3eaa279d5	8afa6a12-cf3d-42bd-98b5-4961debd9bf3	d67b33c3-e334-45e8-9be9-4b8a052851a9	25.000000000000000000000000000000	18:00	b445afaa-41ef-4f44-8d11-729df03d47bf	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	2023-12-28 03:00:00
d2b5c637-1110-4ec9-b1d9-4217c98fc348	8afa6a12-cf3d-42bd-98b5-4961debd9bf3	d67b33c3-e334-45e8-9be9-4b8a052851a9	25.000000000000000000000000000000	18:00	b445afaa-41ef-4f44-8d11-729df03d47bf	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	2023-12-29 03:00:00
4407ad92-b304-48af-933d-2fda9118965e	8afa6a12-cf3d-42bd-98b5-4961debd9bf3	d67b33c3-e334-45e8-9be9-4b8a052851a9	25.000000000000000000000000000000	18:00	b445afaa-41ef-4f44-8d11-729df03d47bf	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	2023-12-30 03:00:00
c1374acb-d3f7-4654-8dbb-c23be902a608	8afa6a12-cf3d-42bd-98b5-4961debd9bf3	d67b33c3-e334-45e8-9be9-4b8a052851a9	25.000000000000000000000000000000	18:00	b445afaa-41ef-4f44-8d11-729df03d47bf	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	2023-12-31 03:00:00
b3930fe7-a744-4ec0-900e-f3661b68e69b	8afa6a12-cf3d-42bd-98b5-4961debd9bf3	d67b33c3-e334-45e8-9be9-4b8a052851a9	25.000000000000000000000000000000	18:00	b445afaa-41ef-4f44-8d11-729df03d47bf	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	2024-01-01 03:00:00
ede44602-5517-469f-be49-69970b0d72ba	8afa6a12-cf3d-42bd-98b5-4961debd9bf3	d67b33c3-e334-45e8-9be9-4b8a052851a9	25.000000000000000000000000000000	18:00	b445afaa-41ef-4f44-8d11-729df03d47bf	2023-12-18 00:03:28.323	2023-12-18 00:03:28.323	2024-01-02 03:00:00
\.


--
-- Data for Name: Ticket; Type: TABLE DATA; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

COPY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Ticket" (id, "cpfReserva", "nomeReserva", "idAssento", "idSessao", "createdAt", "updateAt") FROM stdin;
d95ed6f2-75c2-46c7-9300-182e72fd605c	12345678901	Fabiola	67ca81b1-b1b0-49ef-b77e-9ef9a241dd03	2884ab6b-19f9-432a-b3e4-030ebfff0a18	2023-12-18 02:09:37.936	2023-12-18 02:09:37.936
0fc05fcd-8bc3-4f56-875a-37fbafe1edb4	12312312312	Maria	e35bd1e8-d68a-474c-9c37-2ca483e11e11	35d7f81a-062e-419f-a6db-06a3eaa279d5	2023-12-18 02:11:03.741	2023-12-18 02:11:03.741
\.


--
-- Data for Name: Usuario; Type: TABLE DATA; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

COPY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario" (id, email, nome, senha, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: UsuarioCinema; Type: TABLE DATA; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

COPY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema" (id, "isAdmin", "idUsuario", "idCinema", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Assento; Type: TABLE DATA; Schema: defaultschema; Owner: postgres
--

COPY defaultschema."Assento" (id, numero, "idSessao", "createdAt", "updateAt", reservado) FROM stdin;
\.


--
-- Data for Name: Avaliacao; Type: TABLE DATA; Schema: defaultschema; Owner: postgres
--

COPY defaultschema."Avaliacao" (id, "idFilme", valor, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: Cinema; Type: TABLE DATA; Schema: defaultschema; Owner: postgres
--

COPY defaultschema."Cinema" (id, nome, "createdAt", "updateAt") FROM stdin;
92ea6680-9481-4c5a-a994-1cb678a21900	mCine - Passo Fundo	2023-12-06 16:47:00.927	2023-12-06 16:47:00.927
24304e50-e8be-422e-8b98-d7ca278400f7	mCine - Santa Maria	2023-12-06 16:48:59.888	2023-12-06 16:48:59.888
e7135d23-bdb1-4b42-b2fa-1855bd610f2f	mCine - Pelotas	2023-12-06 17:10:53.086	2023-12-06 17:10:53.086
\.


--
-- Data for Name: Filmes; Type: TABLE DATA; Schema: defaultschema; Owner: postgres
--

COPY defaultschema."Filmes" (id, nome, sinopse, "dtLancamento", "capaUrl", "createdAt", "updateAt", disponivel, "linkTrailer") FROM stdin;
92480b2f-f8c0-465f-8d8d-996d92a24bd0	Harry Potter - A Pedra Filosofa	SInopse do FIlme	2002-12-31 23:00:00	CapaURL	2023-12-06 14:42:36.925	2023-12-06 16:07:53.644	t	
991cbbd1-da67-4877-9605-26d998e4891a	Harry Potter - A Pedra Filosofal	SInopse do FIlme	2002-12-31 23:00:00	CapaURL	2023-12-06 16:14:57.714	2023-12-06 16:14:57.714	t	
\.


--
-- Data for Name: Sala; Type: TABLE DATA; Schema: defaultschema; Owner: postgres
--

COPY defaultschema."Sala" (id, nome, "idCinema", capacidade, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: Sessao; Type: TABLE DATA; Schema: defaultschema; Owner: postgres
--

COPY defaultschema."Sessao" (id, "idCinema", "idFilme", "vlEntrada", "horaInicio", "idSala", "createdAt", "updateAt", "dtSessao") FROM stdin;
\.


--
-- Data for Name: Ticket; Type: TABLE DATA; Schema: defaultschema; Owner: postgres
--

COPY defaultschema."Ticket" (id, "cpfReserva", "nomeReserva", "idAssento", "idSessao", "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: Usuario; Type: TABLE DATA; Schema: defaultschema; Owner: postgres
--

COPY defaultschema."Usuario" (id, email, nome, senha, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: UsuarioCinema; Type: TABLE DATA; Schema: defaultschema; Owner: postgres
--

COPY defaultschema."UsuarioCinema" (id, "isAdmin", "idUsuario", "idCinema", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Assento; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Assento" (id, numero, "idSessao", "createdAt", "updateAt", reservado) FROM stdin;
\.


--
-- Data for Name: Avaliacao; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Avaliacao" (id, "idFilme", valor, "createdAt", "updateAt") FROM stdin;
93198318-4ee2-42d5-a21b-0bbfad899fde	02202da1-b071-433b-9b28-f6b4beab820c	3	2023-12-16 18:50:11.446	2023-12-16 18:50:11.446
1a5b5b9b-1111-4a9d-920c-0bd2aaac3025	02202da1-b071-433b-9b28-f6b4beab820c	3	2023-12-16 18:56:11.23	2023-12-16 18:56:11.23
85d7d623-76e4-4790-a58d-312877705e06	02202da1-b071-433b-9b28-f6b4beab820c	2	2023-12-16 19:00:32.045	2023-12-16 19:00:32.045
c5f53725-8103-4310-885a-d7855ce21476	02202da1-b071-433b-9b28-f6b4beab820c	4	2023-12-16 19:00:38.51	2023-12-16 19:00:38.51
cb481d8e-c590-4136-9a92-ffe6e964d7b7	0853ee06-86a6-44f0-9941-f0bcac5eba25	5	2023-12-16 19:02:47.726	2023-12-16 19:02:47.726
f8e16a94-a75a-4c75-aa79-27aec75b8b10	02202da1-b071-433b-9b28-f6b4beab820c	5	2023-12-16 19:02:53.791	2023-12-16 19:02:53.791
5a82cd98-73d2-4f28-a6a0-6f10f0b494e7	07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	5	2023-12-16 19:03:00.062	2023-12-16 19:03:00.062
6e796a0b-8716-44ff-9500-f4459c125a57	0853ee06-86a6-44f0-9941-f0bcac5eba25	4	2023-12-16 19:03:03.742	2023-12-16 19:03:03.742
8f7a7518-0c39-4669-859a-8cbf58cf4981	c0d8322b-6ddc-4ae5-a772-5bd1911867fc	4	2023-12-16 19:03:18.852	2023-12-16 19:03:18.852
2c2a72f0-eec2-4b67-89b0-9b7af3b90902	9b05c043-9a7d-49de-88a5-ab31de1177ef	4	2023-12-16 19:03:21.665	2023-12-16 19:03:21.665
05c5b8f4-48c6-4b97-bf7a-755a87263539	5d801a30-4ced-4a9e-b218-e8822c90cef8	4	2023-12-16 21:59:44.937	2023-12-16 21:59:44.937
88ef21bc-0d5b-40fb-ba03-ed2abe1039eb	b07d0f7a-9d03-4d02-be74-923108d72955	4	2023-12-17 13:18:46.258	2023-12-17 13:18:46.258
0ee91532-0920-4303-b307-bb6799337bd7	0853ee06-86a6-44f0-9941-f0bcac5eba25	4	2023-12-17 13:18:49.982	2023-12-17 13:18:49.982
55d0b449-82d7-4866-9755-ec46ad2aa8a2	d089e38a-d140-4f9d-9826-483c97c32fe3	4	2023-12-17 13:18:51.346	2023-12-17 13:18:51.346
d5083a2c-309a-40a0-9f65-233efb2c662e	c0d8322b-6ddc-4ae5-a772-5bd1911867fc	4	2023-12-17 13:18:55.179	2023-12-17 13:18:55.179
8ed486f0-be0e-433e-8357-5a41a85769cd	0853ee06-86a6-44f0-9941-f0bcac5eba25	5	2023-12-17 13:46:58.217	2023-12-17 13:46:58.217
2f1de3eb-07ab-4db1-89d3-a595b1a7ccc1	b07d0f7a-9d03-4d02-be74-923108d72955	1	2023-12-17 13:47:13.657	2023-12-17 13:47:13.657
ac8e9291-4b36-43c7-aefa-cc229023db4f	07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	4	2023-12-17 14:17:23.122	2023-12-17 14:17:23.122
e4983063-f1fc-4545-a9da-963d588f8c7c	9b05c043-9a7d-49de-88a5-ab31de1177ef	5	2023-12-17 15:00:09.814	2023-12-17 15:00:09.814
1898998a-0b59-48f5-a0c2-d485a86743f4	b07d0f7a-9d03-4d02-be74-923108d72955	5	2023-12-17 15:00:12.91	2023-12-17 15:00:12.91
a28b4000-69b3-49d2-bab7-1c44f89acca5	d67b33c3-e334-45e8-9be9-4b8a052851a9	5	2023-12-18 00:22:53.218	2023-12-18 00:22:53.218
2ed69abb-0287-4467-9c8e-5ba2276f3b8e	d67b33c3-e334-45e8-9be9-4b8a052851a9	5	2023-12-18 00:22:58.163	2023-12-18 00:22:58.163
\.


--
-- Data for Name: Cinema; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Cinema" (id, nome, "createdAt", "updateAt") FROM stdin;
68caa687-b8d8-46f0-b5a9-79bfc36eb456	Cinema Florianópolis	2023-12-16 12:53:15.988	2023-12-18 02:05:23.452
8afa6a12-cf3d-42bd-98b5-4961debd9bf3	Cinema Teste	2023-12-17 23:45:11.381	2023-12-18 02:05:35.099
0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	Cinema Santa Maria	2023-12-16 00:05:18.521	2023-12-18 02:05:56.856
root	Painel Administrativo	2023-12-09 22:01:21.888	2023-12-18 02:06:26.69
\.


--
-- Data for Name: Filmes; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Filmes" (id, nome, sinopse, "dtLancamento", "capaUrl", "createdAt", "updateAt", disponivel, "linkTrailer") FROM stdin;
07c24fc8-c4e0-4ee1-8d0e-cc7bb0b63d2f	O som da liberade	Um ex-agente federal embarca em uma perigosa missão para salvar uma menina dos cruéis traficantes de crianças. Com o tempo se esgotando, ele viaja pelas profundezas da selva colombiana, colocando sua vida em risco para libertá-la.	2023-03-07 03:00:00	https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702686910/vcjizyl21pzrpks9gorg.jpg	2023-12-16 00:35:15.985	2023-12-16 00:35:15.985	t	
02202da1-b071-433b-9b28-f6b4beab820c	A noite das bruxas	O detetive belga Hercule Poirot investiga um assassinato enquanto participa de uma sessão espírita de Halloween em um palazzo assombrado em Veneza, Itália.	2023-12-01 03:00:00	https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702686983/px3xexcbabjwq9jxjzmt.jpg	2023-12-16 00:36:26.008	2023-12-16 00:36:26.008	t	
0853ee06-86a6-44f0-9941-f0bcac5eba25	Peter Pan	Wendy e seus irmãos são levados para o mundo mágico da Terra do Nunca com o herói de suas histórias, Peter Pan.	2023-08-12 03:00:00	https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702687055/t05oov2no39qjcfylukk.jpg	2023-12-16 00:37:38.276	2023-12-16 00:37:38.276	t	
5d801a30-4ced-4a9e-b218-e8822c90cef8	Harry Potter - E a Pedra Filosofal	Teste	2023-12-10 20:44:07.748	https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702241061/qrxrf1xvokulkwqmvu2e.jpg	2023-12-10 20:46:20.712	2023-12-10 20:48:42.071	t	
d089e38a-d140-4f9d-9826-483c97c32fe3	Truque de Mestre	Um grupo de ilusionistas encanta o público com suas mágicas e também rouba bancos em outro continente, distribuindo a quantia para os próprios espectadores. O agente do FBI Dylan Hobbs está determinado a capturá-los e conta com a ajuda de Alma Vargas, uma detetive da Interpol, e também de Thaddeus Bradley, um veterano desmistificador de mágicos que insiste que os assaltos são realizados a partir de disfarces e jogos envolvendo vídeos.	2023-09-02 03:00:00	https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702687330/wusu3cjovbdqxow9tvbq.jpg	2023-12-16 00:42:12.884	2023-12-16 00:42:12.884	t	
d67b33c3-e334-45e8-9be9-4b8a052851a9	O Último Reino: Os sete reis devem morrer	A história do drama histórico se passa após a morte do Rei Edward. A paz, anteriormente estabelecida pelo monarca, é colocada em risco quando seus dois únicos herdeiros, Aethelstan e Aelfweard, entram em uma disputa acirrada pelo controle do reino e da coroa	2023-12-20 03:00:00	https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702847529/kqoc9z6akwsgg5mk6egt.webp	2023-12-17 21:12:14.147	2023-12-17 21:36:59.47	t	https://www.youtube.com/watch?v=yBg1qSZSNDA
9b05c043-9a7d-49de-88a5-ab31de1177ef	O Protetor - Capítulo Final	Sentindo-se em casa no sul da Itália, o ex-agente Robert McCall logo descobre que seus novos amigos estão sob o controle dos chefes do crime local.	2023-08-17 03:00:00	https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702859246/fhjnlq5rrns6vzxyqeep.jpg	2023-12-16 00:40:48.118	2023-12-18 00:27:28.903	t	https://www.youtube.com/watch?v=4dvYznqqqPo
c5dd596f-c6d1-4752-8502-54e4890186e2	Harry Potter - E a câmara Secreta	teste2	2023-12-02 03:00:00	https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702241513/tj0v6xt8pcr53miiqsba.webp	2023-12-10 20:51:58.422	2023-12-11 16:54:51.177	t	
b07d0f7a-9d03-4d02-be74-923108d72955	Oppenheimer	O físico J. Robert Oppenheimer trabalha com uma equipe de cientistas durante o Projeto Manhattan, levando ao desenvolvimento da bomba atômica.	2023-12-16 00:31:33.537	https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702686775/ex44ryiwuntbc0abldqp.jpg	2023-12-16 00:32:58.468	2023-12-16 00:32:58.468	t	
60b26f45-58a1-418b-8884-818fdf137c0f	O senhor dos anéis - A sociedade do anel	Em uma terra fantástica e única, um hobbit recebe de presente de seu tio um anel mágico e maligno que precisa ser destruído antes que caia nas mãos do mal. Para isso, o hobbit Frodo tem um caminho árduo pela frente, onde encontra perigo, medo e seres bizarros. Ao seu lado para o cumprimento desta jornada, ele aos poucos pode contar com outros hobbits, um elfo, um anão, dois humanos e um mago, totalizando nove seres que formam a Sociedade do Anel.	2023-10-13 03:00:00	https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702686805/mmy4xpsyw1fgq1iyxlkb.jpg	2023-12-16 00:33:31.009	2023-12-16 00:33:31.009	t	
39fd46ca-4bc3-4676-a48b-4a4da4795afc	O senhor dos anéis - As duas torres	Após a captura de Merry e Pippy pelos orcs, a Sociedade do Anel é dissolvida. Frodo e Sam seguem sua jornada rumo à Montanha da Perdição para destruir o anel e descobrem que estão sendo perseguidos pelo misterioso Gollum. Enquanto isso, Aragorn, o elfo e arqueiro Legolas e o anão Gimli partem para resgatar os hobbits sequestrados e chegam ao reino de Rohan, onde o rei Theoden foi vítima de uma maldição mortal de Saruman.	2023-05-04 03:00:00	https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702686834/rcrq9outicurkaip57ix.jpg	2023-12-16 00:33:56.889	2023-12-16 00:33:56.889	t	
\.


--
-- Data for Name: Sala; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Sala" (id, nome, "idCinema", capacidade, "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: Sessao; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Sessao" (id, "idCinema", "idFilme", "vlEntrada", "horaInicio", "idSala", "createdAt", "updateAt", "dtSessao") FROM stdin;
\.


--
-- Data for Name: Ticket; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Ticket" (id, "cpfReserva", "nomeReserva", "idAssento", "idSessao", "createdAt", "updateAt") FROM stdin;
\.


--
-- Data for Name: Usuario; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Usuario" (id, email, nome, senha, "createdAt", "updateAt") FROM stdin;
3	marcos@mail.com	marcos@mail.com	$2b$10$Ys1TDeHDUdkbp/9ayo3IFeUzL7hWQ86Xbr3Ojupe1MIMayKVbfHpK	2023-12-09 22:03:00.239	2023-12-09 23:23:23.295
1	admin@admin.com	admin	$2b$10$Uq83G/1wZXNE99.Q0Dka/..wd9KyFsFADN6QUojbqOfQuDWqPoVH.	2023-12-10 00:31:39.355	2023-12-10 00:31:39.355
9	comum@comum.com	Comum	$2b$10$aAcxh/SVy5joAvOV/6N2sufoM8Qla6cbXxwqzMawv/oPzob01QbYC	2023-12-10 14:06:41.821	2023-12-10 14:06:41.821
\.


--
-- Data for Name: UsuarioCinema; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."UsuarioCinema" (id, "isAdmin", "idUsuario", "idCinema", "createdAt", "updatedAt") FROM stdin;
82	t	1	68caa687-b8d8-46f0-b5a9-79bfc36eb456	2023-12-18 02:05:23.452	2023-12-18 02:05:23.452
83	f	9	68caa687-b8d8-46f0-b5a9-79bfc36eb456	2023-12-18 02:05:23.452	2023-12-18 02:05:23.452
84	t	1	8afa6a12-cf3d-42bd-98b5-4961debd9bf3	2023-12-18 02:05:35.099	2023-12-18 02:05:35.099
85	f	9	8afa6a12-cf3d-42bd-98b5-4961debd9bf3	2023-12-18 02:05:35.099	2023-12-18 02:05:35.099
90	t	1	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	2023-12-18 02:05:56.856	2023-12-18 02:05:56.856
91	f	9	0d943f9b-5d00-4bca-a6cc-169dfa31ea7a	2023-12-18 02:05:56.856	2023-12-18 02:05:56.856
94	t	1	root	2023-12-18 02:06:26.69	2023-12-18 02:06:26.69
95	f	9	root	2023-12-18 02:06:26.69	2023-12-18 02:06:26.69
\.


--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE SET; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

SELECT pg_catalog.setval('"0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema_id_seq"', 1, true);


--
-- Name: Usuario_id_seq; Type: SEQUENCE SET; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

SELECT pg_catalog.setval('"0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario_id_seq"', 1, true);


--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE SET; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

SELECT pg_catalog.setval('"68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema_id_seq"', 1, true);


--
-- Name: Usuario_id_seq; Type: SEQUENCE SET; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

SELECT pg_catalog.setval('"68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario_id_seq"', 1, true);


--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE SET; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

SELECT pg_catalog.setval('"8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema_id_seq"', 1, true);


--
-- Name: Usuario_id_seq; Type: SEQUENCE SET; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

SELECT pg_catalog.setval('"8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario_id_seq"', 1, true);


--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE SET; Schema: defaultschema; Owner: postgres
--

SELECT pg_catalog.setval('defaultschema."UsuarioCinema_id_seq"', 1, false);


--
-- Name: Usuario_id_seq; Type: SEQUENCE SET; Schema: defaultschema; Owner: postgres
--

SELECT pg_catalog.setval('defaultschema."Usuario_id_seq"', 1, false);


--
-- Name: UsuarioCinema_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public."UsuarioCinema_id_seq"', 95, true);


--
-- Name: Usuario_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public."Usuario_id_seq"', 11, true);


--
-- Name: Assento Assento_pkey; Type: CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Assento"
    ADD CONSTRAINT "Assento_pkey" PRIMARY KEY (id);


--
-- Name: Avaliacao Avaliacao_pkey; Type: CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Avaliacao"
    ADD CONSTRAINT "Avaliacao_pkey" PRIMARY KEY (id);


--
-- Name: Cinema Cinema_pkey; Type: CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Cinema"
    ADD CONSTRAINT "Cinema_pkey" PRIMARY KEY (id);


--
-- Name: Filmes Filmes_pkey; Type: CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Filmes"
    ADD CONSTRAINT "Filmes_pkey" PRIMARY KEY (id);


--
-- Name: Sala Sala_pkey; Type: CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Sala"
    ADD CONSTRAINT "Sala_pkey" PRIMARY KEY (id);


--
-- Name: Sessao Sessao_pkey; Type: CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Sessao"
    ADD CONSTRAINT "Sessao_pkey" PRIMARY KEY (id);


--
-- Name: Ticket Ticket_pkey; Type: CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Ticket"
    ADD CONSTRAINT "Ticket_pkey" PRIMARY KEY (id);


--
-- Name: UsuarioCinema UsuarioCinema_pkey; Type: CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_pkey" PRIMARY KEY (id);


--
-- Name: Usuario Usuario_pkey; Type: CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario"
    ADD CONSTRAINT "Usuario_pkey" PRIMARY KEY (id);


--
-- Name: Assento Assento_pkey; Type: CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Assento"
    ADD CONSTRAINT "Assento_pkey" PRIMARY KEY (id);


--
-- Name: Avaliacao Avaliacao_pkey; Type: CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Avaliacao"
    ADD CONSTRAINT "Avaliacao_pkey" PRIMARY KEY (id);


--
-- Name: Cinema Cinema_pkey; Type: CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Cinema"
    ADD CONSTRAINT "Cinema_pkey" PRIMARY KEY (id);


--
-- Name: Filmes Filmes_pkey; Type: CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Filmes"
    ADD CONSTRAINT "Filmes_pkey" PRIMARY KEY (id);


--
-- Name: Sala Sala_pkey; Type: CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Sala"
    ADD CONSTRAINT "Sala_pkey" PRIMARY KEY (id);


--
-- Name: Sessao Sessao_pkey; Type: CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Sessao"
    ADD CONSTRAINT "Sessao_pkey" PRIMARY KEY (id);


--
-- Name: Ticket Ticket_pkey; Type: CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Ticket"
    ADD CONSTRAINT "Ticket_pkey" PRIMARY KEY (id);


--
-- Name: UsuarioCinema UsuarioCinema_pkey; Type: CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_pkey" PRIMARY KEY (id);


--
-- Name: Usuario Usuario_pkey; Type: CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario"
    ADD CONSTRAINT "Usuario_pkey" PRIMARY KEY (id);


--
-- Name: Assento Assento_pkey; Type: CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Assento"
    ADD CONSTRAINT "Assento_pkey" PRIMARY KEY (id);


--
-- Name: Avaliacao Avaliacao_pkey; Type: CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Avaliacao"
    ADD CONSTRAINT "Avaliacao_pkey" PRIMARY KEY (id);


--
-- Name: Cinema Cinema_pkey; Type: CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Cinema"
    ADD CONSTRAINT "Cinema_pkey" PRIMARY KEY (id);


--
-- Name: Filmes Filmes_pkey; Type: CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Filmes"
    ADD CONSTRAINT "Filmes_pkey" PRIMARY KEY (id);


--
-- Name: Sala Sala_pkey; Type: CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Sala"
    ADD CONSTRAINT "Sala_pkey" PRIMARY KEY (id);


--
-- Name: Sessao Sessao_pkey; Type: CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Sessao"
    ADD CONSTRAINT "Sessao_pkey" PRIMARY KEY (id);


--
-- Name: Ticket Ticket_pkey; Type: CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Ticket"
    ADD CONSTRAINT "Ticket_pkey" PRIMARY KEY (id);


--
-- Name: UsuarioCinema UsuarioCinema_pkey; Type: CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_pkey" PRIMARY KEY (id);


--
-- Name: Usuario Usuario_pkey; Type: CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario"
    ADD CONSTRAINT "Usuario_pkey" PRIMARY KEY (id);


--
-- Name: Assento Assento_pkey; Type: CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Assento"
    ADD CONSTRAINT "Assento_pkey" PRIMARY KEY (id);


--
-- Name: Avaliacao Avaliacao_pkey; Type: CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Avaliacao"
    ADD CONSTRAINT "Avaliacao_pkey" PRIMARY KEY (id);


--
-- Name: Cinema Cinema_pkey; Type: CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Cinema"
    ADD CONSTRAINT "Cinema_pkey" PRIMARY KEY (id);


--
-- Name: Filmes Filmes_pkey; Type: CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Filmes"
    ADD CONSTRAINT "Filmes_pkey" PRIMARY KEY (id);


--
-- Name: Sala Sala_pkey; Type: CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Sala"
    ADD CONSTRAINT "Sala_pkey" PRIMARY KEY (id);


--
-- Name: Sessao Sessao_pkey; Type: CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Sessao"
    ADD CONSTRAINT "Sessao_pkey" PRIMARY KEY (id);


--
-- Name: Ticket Ticket_pkey; Type: CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Ticket"
    ADD CONSTRAINT "Ticket_pkey" PRIMARY KEY (id);


--
-- Name: UsuarioCinema UsuarioCinema_pkey; Type: CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_pkey" PRIMARY KEY (id);


--
-- Name: Usuario Usuario_pkey; Type: CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Usuario"
    ADD CONSTRAINT "Usuario_pkey" PRIMARY KEY (id);


--
-- Name: Assento Assento_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Assento"
    ADD CONSTRAINT "Assento_pkey" PRIMARY KEY (id);


--
-- Name: Avaliacao Avaliacao_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Avaliacao"
    ADD CONSTRAINT "Avaliacao_pkey" PRIMARY KEY (id);


--
-- Name: Cinema Cinema_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Cinema"
    ADD CONSTRAINT "Cinema_pkey" PRIMARY KEY (id);


--
-- Name: Filmes Filmes_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Filmes"
    ADD CONSTRAINT "Filmes_pkey" PRIMARY KEY (id);


--
-- Name: Sala Sala_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Sala"
    ADD CONSTRAINT "Sala_pkey" PRIMARY KEY (id);


--
-- Name: Sessao Sessao_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Sessao"
    ADD CONSTRAINT "Sessao_pkey" PRIMARY KEY (id);


--
-- Name: Ticket Ticket_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Ticket"
    ADD CONSTRAINT "Ticket_pkey" PRIMARY KEY (id);


--
-- Name: UsuarioCinema UsuarioCinema_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_pkey" PRIMARY KEY (id);


--
-- Name: Usuario Usuario_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Usuario"
    ADD CONSTRAINT "Usuario_pkey" PRIMARY KEY (id);


--
-- Name: Usuario_email_key; Type: INDEX; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

CREATE UNIQUE INDEX "Usuario_email_key" ON "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario" USING btree (email);


--
-- Name: Usuario_email_key; Type: INDEX; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

CREATE UNIQUE INDEX "Usuario_email_key" ON "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario" USING btree (email);


--
-- Name: Usuario_email_idx; Type: INDEX; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

CREATE UNIQUE INDEX "Usuario_email_idx" ON "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario" USING btree (email);


--
-- Name: Usuario_email_key; Type: INDEX; Schema: defaultschema; Owner: postgres
--

CREATE UNIQUE INDEX "Usuario_email_key" ON defaultschema."Usuario" USING btree (email);


--
-- Name: Usuario_email_key; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX "Usuario_email_key" ON public."Usuario" USING btree (email);


--
-- Name: Assento Assento_idSessao_fkey; Type: FK CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Assento"
    ADD CONSTRAINT "Assento_idSessao_fkey" FOREIGN KEY ("idSessao") REFERENCES "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Sessao"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Sessao Sessao_idSala_fkey; Type: FK CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Sessao"
    ADD CONSTRAINT "Sessao_idSala_fkey" FOREIGN KEY ("idSala") REFERENCES "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Sala"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Ticket Ticket_idAssento_fkey; Type: FK CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Ticket"
    ADD CONSTRAINT "Ticket_idAssento_fkey" FOREIGN KEY ("idAssento") REFERENCES "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Assento"(id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: Ticket Ticket_idSessao_fkey; Type: FK CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Ticket"
    ADD CONSTRAINT "Ticket_idSessao_fkey" FOREIGN KEY ("idSessao") REFERENCES "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Sessao"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: UsuarioCinema UsuarioCinema_idCinema_fkey; Type: FK CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_idCinema_fkey" FOREIGN KEY ("idCinema") REFERENCES "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Cinema"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: UsuarioCinema UsuarioCinema_idUsuario_fkey; Type: FK CONSTRAINT; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

ALTER TABLE ONLY "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_idUsuario_fkey" FOREIGN KEY ("idUsuario") REFERENCES "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Assento Assento_idSessao_fkey; Type: FK CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Assento"
    ADD CONSTRAINT "Assento_idSessao_fkey" FOREIGN KEY ("idSessao") REFERENCES "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Sessao"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Sessao Sessao_idSala_fkey; Type: FK CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Sessao"
    ADD CONSTRAINT "Sessao_idSala_fkey" FOREIGN KEY ("idSala") REFERENCES "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Sala"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Ticket Ticket_idAssento_fkey; Type: FK CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Ticket"
    ADD CONSTRAINT "Ticket_idAssento_fkey" FOREIGN KEY ("idAssento") REFERENCES "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Assento"(id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: Ticket Ticket_idSessao_fkey; Type: FK CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Ticket"
    ADD CONSTRAINT "Ticket_idSessao_fkey" FOREIGN KEY ("idSessao") REFERENCES "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Sessao"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: UsuarioCinema UsuarioCinema_idCinema_fkey; Type: FK CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_idCinema_fkey" FOREIGN KEY ("idCinema") REFERENCES "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Cinema"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: UsuarioCinema UsuarioCinema_idUsuario_fkey; Type: FK CONSTRAINT; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

ALTER TABLE ONLY "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_idUsuario_fkey" FOREIGN KEY ("idUsuario") REFERENCES "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Assento Assento_idSessao_fkey; Type: FK CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Assento"
    ADD CONSTRAINT "Assento_idSessao_fkey" FOREIGN KEY ("idSessao") REFERENCES "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Sessao"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Sessao Sessao_idSala_fkey; Type: FK CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Sessao"
    ADD CONSTRAINT "Sessao_idSala_fkey" FOREIGN KEY ("idSala") REFERENCES "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Sala"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Ticket Ticket_idAssento_fkey; Type: FK CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Ticket"
    ADD CONSTRAINT "Ticket_idAssento_fkey" FOREIGN KEY ("idAssento") REFERENCES "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Assento"(id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: Ticket Ticket_idSessao_fkey; Type: FK CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Ticket"
    ADD CONSTRAINT "Ticket_idSessao_fkey" FOREIGN KEY ("idSessao") REFERENCES "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Sessao"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: UsuarioCinema UsuarioCinema_idCinema_fkey; Type: FK CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_idCinema_fkey" FOREIGN KEY ("idCinema") REFERENCES "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Cinema"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: UsuarioCinema UsuarioCinema_idUsuario_fkey; Type: FK CONSTRAINT; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

ALTER TABLE ONLY "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_idUsuario_fkey" FOREIGN KEY ("idUsuario") REFERENCES "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Assento Assento_idSessao_fkey; Type: FK CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Assento"
    ADD CONSTRAINT "Assento_idSessao_fkey" FOREIGN KEY ("idSessao") REFERENCES defaultschema."Sessao"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Sessao Sessao_idSala_fkey; Type: FK CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Sessao"
    ADD CONSTRAINT "Sessao_idSala_fkey" FOREIGN KEY ("idSala") REFERENCES defaultschema."Sala"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Ticket Ticket_idAssento_fkey; Type: FK CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Ticket"
    ADD CONSTRAINT "Ticket_idAssento_fkey" FOREIGN KEY ("idAssento") REFERENCES defaultschema."Assento"(id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: Ticket Ticket_idSessao_fkey; Type: FK CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."Ticket"
    ADD CONSTRAINT "Ticket_idSessao_fkey" FOREIGN KEY ("idSessao") REFERENCES defaultschema."Sessao"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: UsuarioCinema UsuarioCinema_idCinema_fkey; Type: FK CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_idCinema_fkey" FOREIGN KEY ("idCinema") REFERENCES defaultschema."Cinema"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: UsuarioCinema UsuarioCinema_idUsuario_fkey; Type: FK CONSTRAINT; Schema: defaultschema; Owner: postgres
--

ALTER TABLE ONLY defaultschema."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_idUsuario_fkey" FOREIGN KEY ("idUsuario") REFERENCES defaultschema."Usuario"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Assento Assento_idSessao_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Assento"
    ADD CONSTRAINT "Assento_idSessao_fkey" FOREIGN KEY ("idSessao") REFERENCES public."Sessao"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Sessao Sessao_idSala_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Sessao"
    ADD CONSTRAINT "Sessao_idSala_fkey" FOREIGN KEY ("idSala") REFERENCES public."Sala"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: Ticket Ticket_idAssento_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Ticket"
    ADD CONSTRAINT "Ticket_idAssento_fkey" FOREIGN KEY ("idAssento") REFERENCES public."Assento"(id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: Ticket Ticket_idSessao_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Ticket"
    ADD CONSTRAINT "Ticket_idSessao_fkey" FOREIGN KEY ("idSessao") REFERENCES public."Sessao"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: UsuarioCinema UsuarioCinema_idCinema_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_idCinema_fkey" FOREIGN KEY ("idCinema") REFERENCES public."Cinema"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: UsuarioCinema UsuarioCinema_idUsuario_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UsuarioCinema"
    ADD CONSTRAINT "UsuarioCinema_idUsuario_fkey" FOREIGN KEY ("idUsuario") REFERENCES public."Usuario"(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: SCHEMA "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"; Type: ACL; Schema: -; Owner: postgres
--

GRANT USAGE ON SCHEMA "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a" TO pg_read_all_data;
GRANT USAGE ON SCHEMA "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a" TO pg_write_all_data;


--
-- Name: SCHEMA "68caa687-b8d8-46f0-b5a9-79bfc36eb456"; Type: ACL; Schema: -; Owner: postgres
--

GRANT USAGE ON SCHEMA "68caa687-b8d8-46f0-b5a9-79bfc36eb456" TO pg_read_all_data;
GRANT USAGE ON SCHEMA "68caa687-b8d8-46f0-b5a9-79bfc36eb456" TO pg_write_all_data;


--
-- Name: SCHEMA "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"; Type: ACL; Schema: -; Owner: postgres
--

GRANT USAGE ON SCHEMA "8afa6a12-cf3d-42bd-98b5-4961debd9bf3" TO pg_read_all_data;
GRANT USAGE ON SCHEMA "8afa6a12-cf3d-42bd-98b5-4961debd9bf3" TO pg_write_all_data;


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE USAGE ON SCHEMA public FROM PUBLIC;


--
-- Name: SEQUENCE "UsuarioCinema_id_seq"; Type: ACL; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

GRANT SELECT ON SEQUENCE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema_id_seq" TO pg_read_all_data;
GRANT UPDATE ON SEQUENCE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."UsuarioCinema_id_seq" TO pg_write_all_data;


--
-- Name: SEQUENCE "Usuario_id_seq"; Type: ACL; Schema: 0d943f9b-5d00-4bca-a6cc-169dfa31ea7a; Owner: postgres
--

GRANT SELECT ON SEQUENCE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario_id_seq" TO pg_read_all_data;
GRANT UPDATE ON SEQUENCE "0d943f9b-5d00-4bca-a6cc-169dfa31ea7a"."Usuario_id_seq" TO pg_write_all_data;


--
-- Name: SEQUENCE "UsuarioCinema_id_seq"; Type: ACL; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

GRANT SELECT ON SEQUENCE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema_id_seq" TO pg_read_all_data;
GRANT UPDATE ON SEQUENCE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."UsuarioCinema_id_seq" TO pg_write_all_data;


--
-- Name: SEQUENCE "Usuario_id_seq"; Type: ACL; Schema: 68caa687-b8d8-46f0-b5a9-79bfc36eb456; Owner: postgres
--

GRANT SELECT ON SEQUENCE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario_id_seq" TO pg_read_all_data;
GRANT UPDATE ON SEQUENCE "68caa687-b8d8-46f0-b5a9-79bfc36eb456"."Usuario_id_seq" TO pg_write_all_data;


--
-- Name: SEQUENCE "UsuarioCinema_id_seq"; Type: ACL; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

GRANT SELECT ON SEQUENCE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema_id_seq" TO pg_read_all_data;
GRANT UPDATE ON SEQUENCE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."UsuarioCinema_id_seq" TO pg_write_all_data;


--
-- Name: SEQUENCE "Usuario_id_seq"; Type: ACL; Schema: 8afa6a12-cf3d-42bd-98b5-4961debd9bf3; Owner: postgres
--

GRANT SELECT ON SEQUENCE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario_id_seq" TO pg_read_all_data;
GRANT UPDATE ON SEQUENCE "8afa6a12-cf3d-42bd-98b5-4961debd9bf3"."Usuario_id_seq" TO pg_write_all_data;


--
-- PostgreSQL database dump complete
--

