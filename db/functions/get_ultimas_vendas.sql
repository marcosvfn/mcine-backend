CREATE OR REPLACE FUNCTION get_ultimas_vendas(schemas text[]) RETURNS TABLE (
    nomeReserva text,
    nomeCinema text,
    valor decimal,
	dataReserva text
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
$$
LANGUAGE 'plpgsql';
