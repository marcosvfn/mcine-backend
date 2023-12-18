CREATE OR REPLACE FUNCTION get_filmes_mais_visualizados(schemas text[]) RETURNS TABLE (
    cinemaNome text,
    filmeNome text,
    quantidadeTicketsVendidos bigint
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
$$
LANGUAGE 'plpgsql';
