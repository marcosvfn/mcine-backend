CREATE OR REPLACE FUNCTION get_total_arrecadado_por_filme(schemas text[], filmeId text) RETURNS TABLE (
    idFilme text,
    nomeFilme text,
    totalArrecadado decimal
) AS
$$
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
$$
LANGUAGE 'plpgsql';
