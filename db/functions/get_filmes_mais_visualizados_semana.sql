CREATE OR REPLACE FUNCTION get_filmes_mais_visualizados_semana(schemas text[]) RETURNS TABLE (
    cinemaNome text,
    filmeNome text,
    quantidadeTicketsVendidos bigint
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
                t."createdAt" >= %2$L
                AND t."createdAt" < NOW()
            GROUP BY
                f.nome, c.nome
            ORDER BY
                COUNT(t.id) DESC
            LIMIT 3
        ', schema_name, start_of_week)
        USING schema_name;
    END LOOP;
END;
$$
LANGUAGE 'plpgsql';
