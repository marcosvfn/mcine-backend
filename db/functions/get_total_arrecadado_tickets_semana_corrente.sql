CREATE OR REPLACE FUNCTION get_total_arrecadado_tickets_semana_corrente(schemas text[]) RETURNS TABLE (
    cinemaNome text,
    totalArrecadado numeric
) AS
$$
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
$$
LANGUAGE 'plpgsql';
