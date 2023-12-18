CREATE OR REPLACE FUNCTION delete_sessao_byfilme(schemas text[], filmeId text) RETURNS VOID AS
$$
DECLARE
    schema_name text;
BEGIN
    FOREACH schema_name IN ARRAY schemas LOOP
        EXECUTE format('DELETE FROM %I."Sessao" WHERE "Sessao"."idFilme" = $1', schema_name)
        USING filmeId;
    END LOOP;
END;
$$
LANGUAGE 'plpgsql';
