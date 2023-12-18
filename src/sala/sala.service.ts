import { ConflictException, HttpException, Injectable } from "@nestjs/common";
import { CreateSalaDto } from "./dto/sala.dto";
import { PrismaClient } from "@prisma/client";
import { FilmesService } from "src/filmes/filmes.service";
import { AvaliacaoService } from "src/avaliacao/avaliacao.service";
import { format } from "date-fns";

type SessionData = {
  horaInicio: string;
  nomeFilme: string;
  vlEntrada: number;
  avaliacao: number;
};

@Injectable()
export class SalaService {
  constructor(
    private prisma: PrismaClient,
    private filmesService: FilmesService,
    private avaliacaoService: AvaliacaoService,
  ) {}

  async getAll() {
    try {
      return await this.prisma.sala.findMany();
    } catch (error) {
      console.log("[SALA_GETALL]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async getOne(id: string) {
    try {
      const Sala = await this.prisma.sala.findUnique({
        where: {
          id,
        },
      });

      return Sala;
    } catch (error) {
      console.log("[SALA_GETONE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async create(dto: CreateSalaDto) {
    try {
      const Sala = await this.prisma.sala.findFirst({
        where: {
          nome: dto.nome,
        },
      });

      if (Sala) return new ConflictException("Sala já cadastrada");

      const newSala = await this.prisma.sala.create({
        data: dto,
      });

      return newSala;
    } catch (error) {
      console.log("[SALA_CREATE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async edit(id: string, dto: CreateSalaDto) {
    try {
      const Sala = await this.prisma.sala.findUnique({
        where: {
          id,
        },
      });

      if (!Sala) return new ConflictException("Sala não encontrada");

      const updatedSala = await this.prisma.sala.update({
        where: {
          id,
        },
        data: dto,
      });

      return updatedSala;
    } catch (error) {
      console.log("[SALA_EDIT]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async delete(id: string) {
    try {
      if (!id) return new ConflictException("Id da Sala é necessário");

      const Sala = await this.prisma.sala.deleteMany({
        where: {
          id,
        },
      });

      return Sala;
    } catch (error) {
      console.log("[SALA_DELETE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async getQuadroDeHorariosBySala() {
    try {
      const salasComSessoes = await this.prisma.sala.findMany({
        select: {
          nome: true,
          sessao: {
            select: {
              id: true,
              idCinema: true,
              dtSessao: true,
              horaInicio: true,
              vlEntrada: true,
              idFilme: true,
            },
            orderBy: {
              dtSessao: "asc",
            },
          },
        },
      });

      const quadroDeHorarios = await Promise.all(
        salasComSessoes.map(async (sala) => {
          const sessoesComAvaliacoes = await Promise.all(
            sala.sessao.map(async (sessao) => {
              const avaliacao = await this.avaliacaoService.getAvaliacao(
                sessao.idFilme,
              );
              const nomeFilme = await this.filmesService.getNomeFilmeExternal(
                sessao.idFilme,
              );

              const capaFilme = await this.filmesService.getCapaFilmeExternal(
                sessao.idFilme,
              );

              return {
                idCinema: sessao.idCinema,
                idSessao: sessao.id,
                idFilme: sessao.idFilme,
                dataSessao: format(sessao.dtSessao, "dd/MM/yyyy"),
                horaInicio: sessao.horaInicio,
                nomeFilme: nomeFilme,
                capaUrl: capaFilme,
                vlEntrada: sessao.vlEntrada,
                avaliacao: avaliacao.nota,
              };
            }),
          );

          return {
            nome: sala.nome,
            sessoes: sessoesComAvaliacoes,
          };
        }),
      );

      return quadroDeHorarios;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }
}
