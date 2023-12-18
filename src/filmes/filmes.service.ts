import { ConflictException, HttpException, Injectable } from "@nestjs/common";
import {
  CreateFilmeDto,
  FilmesMaisVisualizadosResponse,
} from "./dto/filmes.dto";
import { PrismaClient } from "@prisma/client";
import { CinemaService } from "src/cinema/cinema.service";
import { AvaliacaoService } from "src/avaliacao/avaliacao.service";

@Injectable()
export class FilmesService {
  constructor(
    private prisma: PrismaClient,
    private cinemaService: CinemaService,
    private avaliacoesService: AvaliacaoService,
  ) {}

  async getAll() {
    try {
      const filmes = await this.prisma.filmes.findMany();
      const result = await Promise.all(
        filmes.map(async (filme) => {
          const avaliacao = await this.avaliacoesService.getAvaliacao(filme.id);
          return {
            ...filme,
            avaliacao: avaliacao.nota,
          };
        }),
      );

      return result;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getOne(id: string) {
    try {
      const filme = await this.prisma.filmes.findUnique({
        where: {
          id,
        },
      });
      const avaliacao = await this.avaliacoesService.getAvaliacao(filme.id);

      return {
        ...filme,
        avaliacao: avaliacao.nota,
      };
    } catch (error) {
      console.log("[FILME_GETONE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async create(dto: CreateFilmeDto) {
    try {
      const filme = await this.prisma.filmes.findFirst({
        where: {
          nome: dto.nome,
        },
      });

      if (filme) return new ConflictException("Filme já cadastrado");

      const newFilme = await this.prisma.filmes.create({
        data: dto,
      });

      return newFilme;
    } catch (error) {
      console.log("[FILME_CREATE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async edit(id: string, dto: CreateFilmeDto) {
    try {
      const filme = await this.prisma.filmes.findUnique({
        where: {
          id,
        },
      });

      if (!filme) return new ConflictException("Filme não encontrado");

      const updatedFilme = await this.prisma.filmes.update({
        where: {
          id,
        },
        data: dto,
      });

      return updatedFilme;
    } catch (error) {
      console.log("[FILME_EDIT]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async delete(id: string) {
    try {
      if (!id) return new ConflictException("Id do Filme é necessário");

      this.prisma.$transaction(async (transactionClient) => {
        const filme = await transactionClient.filmes.deleteMany({
          where: {
            id,
          },
        });

        // Delete cascade nos outros schemas via raw
        const tenantList = (await this.cinemaService.getAll()).map((item) =>
          item.id !== "root" ? item.id : null,
        );

        const nonNullList = tenantList.filter((item) => item);

        if (nonNullList.length) {
          await transactionClient.$executeRaw`SELECT delete_sessao_byfilme(${nonNullList}, ${id})`;
        }

        return filme;
      });
    } catch (error) {
      console.log("[FILME_DELETE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async getFilmesDisponiveis() {
    try {
      const filmes = await this.prisma.filmes.findMany({
        where: {
          disponivel: true,
        },
      });

      const result = await Promise.all(
        filmes.map(async (filme) => {
          const avaliacao = await this.avaliacoesService.getAvaliacao(filme.id);
          return {
            ...filme,
            avaliacao: avaliacao.nota,
          };
        }),
      );
      return result;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getNomeFilmeExternal(idFilme: string) {
    const queryResult = await this.prisma
      .$queryRaw`SELECT nome FROM "public"."Filmes" WHERE id = ${idFilme}`;
    return queryResult[0].nome;
  }

  async getCapaFilmeExternal(idFilme: string) {
    try {
      const queryResult = await this.prisma
        .$queryRaw`SELECT f."capaUrl" FROM "public"."Filmes" f WHERE f.id = ${idFilme}`;

      return queryResult[0].capaUrl;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getTotalArrecadadoByFilme() {
    try {
      const filmes = await this.getAll();
      const idFilmes = filmes.map((filme) => filme.id);

      const cinemaList = await this.prisma.cinema.findMany();

      const tenantIds = cinemaList
        .map((cinema) => cinema.id)
        .filter((id) => id !== "root");

      const qryResult = await Promise.all(
        idFilmes.map(async (idFilme) => {
          const query = await this.prisma.$queryRaw`
              SELECT * FROM get_total_arrecadado_por_filme(${tenantIds}, ${idFilme})
            `;
          return query[0];
        }),
      );

      return qryResult;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getFilmesMaisVisualizados() {
    try {
      const cinemaList = await this.prisma.cinema.findMany();

      const tenantIds = cinemaList
        .map((cinema) => cinema.id)
        .filter((id) => id !== "root");

      const qryResult: FilmesMaisVisualizadosResponse[] = await this.prisma
        .$queryRaw`SELECT * FROM get_filmes_mais_visualizados(${tenantIds})`;

      const formattedResponse = qryResult.map((item) => ({
        nomeCinema: item.cinemanome,
        nomeFilme: item.filmenome,
        visualizacoes: Number(item.quantidadeticketsvendidos),
      }));

      return this.agruparPorCinema(formattedResponse);
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getFilmesMaisVisualizadosSemana() {
    try {
      const cinemaList = await this.prisma.cinema.findMany();

      const tenantIds = cinemaList
        .map((cinema) => cinema.id)
        .filter((id) => id !== "root");

      const qryResult: FilmesMaisVisualizadosResponse[] = await this.prisma
        .$queryRaw`SELECT * FROM get_filmes_mais_visualizados_semana(${tenantIds})`;

      const formattedResponse = qryResult.map((item) => ({
        nomeCinema: item.cinemanome,
        nomeFilme: item.filmenome,
        visualizacoes: Number(item.quantidadeticketsvendidos),
      }));

      return this.agruparPorCinema(formattedResponse);
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async agruparPorCinema(
    resultados: {
      nomeCinema: string;
      nomeFilme: string;
      visualizacoes: number;
    }[],
  ) {
    const cinemas = {};

    resultados.forEach((item) => {
      if (!cinemas[item.nomeCinema]) {
        cinemas[item.nomeCinema] = {
          nomeCinema: item.nomeCinema,
          ranking: [],
        };
      }

      cinemas[item.nomeCinema].ranking.push({
        nomeFilme: item.nomeFilme,
        visualizacoes: item.visualizacoes,
      });
    });

    return Object.values(cinemas);
  }

  async getCinemasSessoesDisponiveisByFilme(idFilme: string) {
    try {
      const cinemaList = await this.prisma.cinema.findMany();

      const tenantIds = cinemaList
        .map((cinema) => cinema.id)
        .filter((id) => id !== "root");

      const qryResult: SessoesDisponiveisReturn[] = await this.prisma
        .$queryRaw`SELECT * FROM get_sessoes_byfilme(${tenantIds}, ${idFilme})`;

      const cinemasAgrupados = {};

      qryResult.forEach(async (item) => {
        const { nomecinema } = item;

        if (!cinemasAgrupados[nomecinema]) {
          cinemasAgrupados[nomecinema] = [];
        }

        cinemasAgrupados[nomecinema].push({
          nomecinema: nomecinema,
          nomefilme: item.nomefilme,
          idcinema: item.idcinema,
          idsessao: item.idsessao,
          dtsessao: item.dtsessao,
          horainicio: item.horainicio,
        });
      });
      return cinemasAgrupados;
    } catch (error) {
      throw new HttpException(error.status, error.response);
    }
  }
}

type SessoesDisponiveisReturn = {
  nomefilme: string;
  idcinema: string;
  nomecinema: string;
  idsessao: string;
  dtsessao: string;
  horainicio: string;
};
