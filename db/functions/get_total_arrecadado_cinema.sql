CREATE OR REPLACE FUNCTION get_total_arrecadado_cinema(schemas text[]) RETURNS TABLE (
    nomeCinema text,
    mesReferencia text,
    totalArrecadado decimal
) AS
$$
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
$$
LANGUAGE 'plpgsql';