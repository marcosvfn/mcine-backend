import { ConflictException, HttpException, Injectable } from "@nestjs/common";
import { PrismaClient } from "@prisma/client";
import { AssentoDto } from "./dto/assento.dto";
import { FilmesService } from "src/filmes/filmes.service";

@Injectable()
export class AssentoService {
  constructor(
    private prisma: PrismaClient,
    private filmesService: FilmesService,
  ) {}

  async getAll() {
    try {
      return await this.prisma.assento.findMany();
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getOne(id: string) {
    try {
      const Assento = await this.prisma.assento.findUnique({
        where: {
          id,
        },
      });

      return Assento;
    } catch (error) {
      console.log("[ASSENTO_GETONE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async create(dto: AssentoDto) {
    try {
      const newAssento = await this.prisma.assento.create({
        data: dto,
      });

      return newAssento;
    } catch (error) {
      console.log("[ASSENTO_CREATE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async edit(id: string, dto: AssentoDto) {
    try {
      const Assento = await this.prisma.assento.findUnique({
        where: {
          id,
        },
      });

      if (!Assento) return new ConflictException("Assento não encontrado");

      const updatedAssento = await this.prisma.assento.update({
        where: {
          id,
        },
        data: dto,
      });

      return updatedAssento;
    } catch (error) {
      console.log("[ASSENTO_EDIT]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async delete(id: string) {
    try {
      if (!id) return new ConflictException("Id do Assento é necessário");

      const assento = this.prisma.$transaction(async (transactionClient) => {
        const assento = await transactionClient.assento.deleteMany({
          where: {
            id,
          },
        });

        return assento;
      });

      return assento;
    } catch (error) {
      console.log("[ASSENTO_DELETE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async getAssentosDisponiveis(idSessao: string) {
    try {
      const assentos = await this.prisma.assento.findMany({
        where: {
          idSessao,
        },
        orderBy: {
          numero: "asc",
        },
      });

      const sessao = await this.prisma.sessao.findUnique({
        where: {
          id: idSessao,
        },
      });

      const nomeFilme = await this.filmesService.getNomeFilmeExternal(
        sessao.idFilme,
      );

      return {
        sessao: {
          ...sessao,
          nomeFilme,
        },
        assentos,
      };
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }
}
