import { HttpException, Injectable } from "@nestjs/common";
import { PrismaClient } from "@prisma/client";
import { AvaliacaoDto } from "./dto/avaliacao.dto";

@Injectable()
export class AvaliacaoService {
  constructor(private prisma: PrismaClient) {}

  async getAll() {
    return await this.prisma.avaliacao.findMany();
  }

  async createAvaliacao(dto: AvaliacaoDto) {
    try {
      return await this.prisma.avaliacao.create({
        data: dto,
      });
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getAvaliacao(idFilme: string) {
    try {
      const avaliacoes = await this.prisma.avaliacao.findMany({
        where: {
          idFilme,
        },
      });

      const valoresAvaliacoes = avaliacoes.map((av) => av.valor);

      const mean =
        valoresAvaliacoes.reduce((accumulate, value) => accumulate + value, 0) /
        valoresAvaliacoes.length;

      return { nota: Math.round(mean) };
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }
}
