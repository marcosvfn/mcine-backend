CREATE OR REPLACE FUNCTION drop_schema(schema_name text) RETURNS void AS
$$
BEGIN
    EXECUTE format('DROP SCHEMA %I CASCADE;', schema_name);
END;
$$
LANGUAGE plpgsql;
