CREATE OR REPLACE FUNCTION get_sessoes_byfilme(schemas text[], idFilme text) RETURNS TABLE (
    nomeFilme text,
    idCinema text,
	nomeCinema text,
	idSessao text,
	dtSessao text,
	horaInicio text
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
$$
LANGUAGE 'plpgsql';