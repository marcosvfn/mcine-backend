import { ConflictException, HttpException, Injectable } from "@nestjs/common";
import { CreateSessaoDto } from "./dto/sessao.dto";
import { Prisma, PrismaClient } from "@prisma/client";
import { DefaultArgs } from "@prisma/client/runtime/library";
import { addDays, addHours, format } from "date-fns";
import { FilmesService } from "src/filmes/filmes.service";
import { CinemaService } from "src/cinema/cinema.service";
import { AvaliacaoService } from "src/avaliacao/avaliacao.service";

@Injectable()
export class SessaoService {
  constructor(
    private prisma: PrismaClient,
    private filmesService: FilmesService,
    private cinemaService: CinemaService,
    private avaliacaoService: AvaliacaoService,
  ) {}

  async getAll() {
    try {
      const sessoes = await this.prisma.sessao.findMany({
        include: {
          sala: true,
        },
      });

      if (sessoes && sessoes.length > 0) {
        const sessoesCompletas = await Promise.all(
          sessoes.map(async (sesh) => {
            const nomeFilme = await this.prisma
              .$queryRaw`SELECT nome FROM "public"."Filmes" WHERE id = ${sesh.idFilme}`;

            const nomeCinema = await this.prisma
              .$queryRaw`SELECT nome FROM "public"."Cinema" WHERE id = ${sesh.idCinema}`;

            return {
              ...sesh,
              nomeFilme: nomeFilme[0].nome || null,
              nomeCinema: nomeCinema[0].nome || null,
            };
          }),
        );

        return sessoesCompletas;
      }

      return sessoes;
    } catch (error) {
      console.log("[SESSAO_GETALL]", error);
      throw new HttpException(error.status, error.message);
    }
  }

  async getOne(id: string) {
    try {
      const Sessao = await this.prisma.sessao.findUnique({
        where: {
          id,
        },
      });

      if (Sessao) {
        const nomeFilme = await this.prisma
          .$queryRaw`SELECT nome FROM "public"."Filmes" WHERE id = ${Sessao.idFilme}`;

        const nomeCinema = await this.prisma
          .$queryRaw`SELECT nome FROM "public"."Cinema" WHERE id = ${Sessao.idCinema}`;
        return {
          ...Sessao,
          nomeFilme: nomeFilme[0].nome,
          nomeCinema: nomeCinema[0].nome,
        };
      }
      return { ...Sessao, nomeFilme: null, nomeCinema: null };
    } catch (error) {
      console.log("[SESSAO_GETONE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async create(dto: CreateSessaoDto) {
    try {
      const Sessao = this.prisma.$transaction(async (transactionClient) => {
        const { agendarSemana, ...insertData } = dto;

        const salaOcupada = await transactionClient.sessao.findFirst({
          where: {
            idSala: dto.idSala,
            horaInicio: dto.horaInicio,
            dtSessao: dto.dtSessao,
          },
        });

        if (salaOcupada)
          throw new ConflictException("Sala ocupada no horário informado");

        const newSessoes = await this.createSessions(
          transactionClient,
          insertData,
          agendarSemana,
        );
        return newSessoes;
      });

      return Sessao;
    } catch (error) {
      console.log("[SESSAO_CREATE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async edit(id: string, dto: CreateSessaoDto) {
    try {
      const sessaoUpdated = this.prisma.$transaction(
        async (transactionClient) => {
          const { agendarSemana, ...insertData } = dto;

          const Sessao = await transactionClient.sessao.findUnique({
            where: {
              id,
            },
          });

          if (!Sessao) throw new ConflictException("Sessão não encontrada");

          const salaOcupada = await transactionClient.sessao.findFirst({
            where: {
              idSala: dto.idSala,
              horaInicio: dto.horaInicio,
              dtSessao: dto.dtSessao,
            },
          });

          if (salaOcupada && dto.horaInicio !== Sessao.horaInicio)
            throw new ConflictException("Sala ocupada no horário informado");

          const sala = await transactionClient.sala.findUnique({
            where: {
              id: insertData.idSala,
            },
          });

          const capacidade = sala.capacidade;

          await transactionClient.ticket.deleteMany({
            where: {
              idSessao: id,
            },
          });

          await transactionClient.assento.deleteMany({
            where: {
              idSessao: id,
            },
          });

          const updatedSessao = await transactionClient.sessao.update({
            where: {
              id,
            },
            data: insertData,
          });

          const newAssentos = [];
          for (let i = 1; i <= capacidade; i++) {
            newAssentos.push({
              numero: i,
              idSessao: updatedSessao.id,
              reservado: false,
            });
          }

          await transactionClient.assento.createMany({
            data: newAssentos,
          });
          return updatedSessao;
        },
      );

      return sessaoUpdated;
    } catch (error) {
      console.log("[SESSAO_EDIT]", error);
    }
  }

  async delete(id: string) {
    try {
      if (!id) return new ConflictException("Id da Sessão é necessário");

      const sessao = this.prisma.$transaction(async (transactionClient) => {
        const deleteSessao = await transactionClient.sessao.deleteMany({
          where: {
            id,
          },
        });

        return deleteSessao;
      });
      return sessao;
    } catch (error) {
      console.log("[SESSAO_DELETE]", error);
    }
  }

  async createSessions(
    transactionClient: Omit<
      PrismaClient<Prisma.PrismaClientOptions, never, DefaultArgs>,
      "$connect" | "$disconnect" | "$on" | "$transaction" | "$use" | "$extends"
    >,
    insertData:
      | (Prisma.Without<
          Prisma.SessaoCreateInput,
          Prisma.SessaoUncheckedCreateInput
        > &
          Prisma.SessaoUncheckedCreateInput)
      | (Prisma.Without<
          Prisma.SessaoUncheckedCreateInput,
          Prisma.SessaoCreateInput
        > &
          Prisma.SessaoCreateInput),
    agendarSemana: boolean,
  ) {
    const newSessoes = [];

    if (agendarSemana) {
      const dtSessao = new Date(insertData.dtSessao);

      for (let i = 0; i < 7; i++) {
        const newDate = addDays(dtSessao, i);

        const sessaoData = {
          ...insertData,
          dtSessao: new Date(newDate),
        };

        const salaOcupada = await transactionClient.sessao.findFirst({
          where: {
            idSala: insertData.idSala,
            horaInicio: insertData.horaInicio,
            dtSessao: newDate,
          },
        });

        if (salaOcupada)
          throw new ConflictException("Sala ocupada no horário informado");

        const newSessao = await transactionClient.sessao.create({
          data: sessaoData,
        });

        newSessoes.push(newSessao);

        const sala = await transactionClient.sala.findUnique({
          where: { id: newSessao.idSala },
          select: { capacidade: true },
        });

        if (sala) {
          const capacidade = sala.capacidade;
          const assentos = [];

          for (let i = 1; i <= capacidade; i++) {
            assentos.push({
              numero: i,
              idSessao: newSessao.id,
              reservado: false,
            });
          }

          await transactionClient.assento.createMany({
            data: assentos,
          });
        }
      }
    } else {
      const newSessao = await transactionClient.sessao.create({
        data: insertData,
      });

      newSessoes.push(newSessao);
      const sala = await transactionClient.sala.findUnique({
        where: { id: newSessao.idSala },
        select: { capacidade: true },
      });

      if (sala) {
        const capacidade = sala.capacidade;
        const assentos = [];

        for (let i = 1; i <= capacidade; i++) {
          assentos.push({
            numero: i,
            idSessao: newSessao.id,
            reservado: false,
          });
        }

        await transactionClient.assento.createMany({
          data: assentos,
        });
      }
    }

    return newSessoes;
  }

  async getTotalArrecadadoByFilme() {
    try {
      const sessoes = await this.prisma.sessao.findMany({
        select: {
          vlEntrada: true,
          idFilme: true,
          ticket: true,
        },
      });

      const arrecadacaoPorFilme = {};

      for (const sessao of sessoes) {
        const nomeFilme = await this.filmesService.getNomeFilmeExternal(
          sessao.idFilme,
        );

        for (const _ of sessao.ticket) {
          if (!arrecadacaoPorFilme[nomeFilme]) {
            arrecadacaoPorFilme[nomeFilme] = 0;
          }

          arrecadacaoPorFilme[nomeFilme] += Number(sessao.vlEntrada);
        }
      }
      return arrecadacaoPorFilme;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getTotalArrecadadoSemana() {
    try {
      const today = new Date();
      const lastWeek = new Date(today.getTime() - 7 * 24 * 60 * 60 * 1000); // 7 dias atrás

      const sessoes = await this.prisma.sessao.findMany({
        select: {
          vlEntrada: true,
          idFilme: true,
          ticket: true,
        },
        where: {
          AND: [
            {
              dtSessao: {
                gte: lastWeek,
              },
            },
            {
              dtSessao: {
                lt: today,
              },
            },
          ],
        },
      });

      const arrecadacaoPorFilme = {
        valor: 0,
      };

      for (const sessao of sessoes) {
        for (const _ of sessao.ticket) {
          arrecadacaoPorFilme.valor += Number(sessao.vlEntrada);
        }
      }
      return arrecadacaoPorFilme;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getTopFilmesAssistidos() {
    try {
      const sessoes = await this.prisma.sessao.findMany({
        select: {
          vlEntrada: true,
          idFilme: true,
          ticket: true,
        },
        where: {
          dtSessao: {
            lt: new Date(),
          },
        },
      });

      const visualizacoesPorFilme = {};

      for (const sessao of sessoes) {
        const nomeFilme = await this.filmesService.getNomeFilmeExternal(
          sessao.idFilme,
        );

        for (const _ of sessao.ticket) {
          if (!visualizacoesPorFilme[nomeFilme]) {
            visualizacoesPorFilme[nomeFilme] = 0;
          }

          visualizacoesPorFilme[nomeFilme] += 1;
        }
      }

      return visualizacoesPorFilme;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getTopFilmesAssistidosSemana() {
    try {
      const today = new Date();
      const lastWeek = new Date(today.getTime() - 7 * 24 * 60 * 60 * 1000); // 7 dias atrás

      const sessoes = await this.prisma.sessao.findMany({
        select: {
          vlEntrada: true,
          idFilme: true,
          ticket: true,
        },
        where: {
          AND: [
            {
              dtSessao: {
                gte: lastWeek,
              },
            },
            {
              dtSessao: {
                lt: today,
              },
            },
          ],
        },
      });

      const visualizacoesPorFilme = {};

      for (const sessao of sessoes) {
        const nomeFilme = await this.filmesService.getNomeFilmeExternal(
          sessao.idFilme,
        );

        for (const _ of sessao.ticket) {
          if (!visualizacoesPorFilme[nomeFilme]) {
            visualizacoesPorFilme[nomeFilme] = 0;
          }

          visualizacoesPorFilme[nomeFilme] += 1;
        }
      }

      return visualizacoesPorFilme;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getUltimasVendas() {
    try {
      const ultimasVendas = await this.prisma.ticket.findMany({
        select: {
          nomeReserva: true,
          createdAt: true,
          sessao: {
            select: {
              idCinema: true,
              vlEntrada: true,
            },
          },
        },
        orderBy: {
          createdAt: "desc",
        },
        take: 5,
      });

      const nomeCinema = await this.cinemaService.getNomeCinemaExternal(
        ultimasVendas[0].sessao.idCinema,
      );

      const formattedData = ultimasVendas.map((item) => {
        return {
          nomecinema: nomeCinema,
          valor: item.sessao.vlEntrada,
          nomereserva: item.nomeReserva,
          datareserva: format(
            addHours(new Date(item.createdAt), 3),
            "yyyy-MM-dd hh:mm:ss",
          ),
        };
      });

      return formattedData;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getFaturamentoAnualByCinema() {
    try {
      const vendas = await this.prisma.ticket.findMany({
        select: {
          nomeReserva: true,
          createdAt: true,
          sessao: {
            select: {
              idCinema: true,
              vlEntrada: true,
              dtSessao: true,
            },
          },
        },
        orderBy: {
          createdAt: "asc",
        },
      });

      const meses = {
        "01": "Jan",
        "02": "Fev",
        "03": "Mar",
        "04": "Abr",
        "05": "Mai",
        "06": "Jun",
        "07": "Jul",
        "08": "Ago",
        "09": "Set",
        "10": "Out",
        "11": "Nov",
        "12": "Dez",
      };

      const nomeCinema = await this.cinemaService.getNomeCinemaExternal(
        vendas[0].sessao.idCinema,
      );

      const faturamentoMensal = {
        id: nomeCinema,
        data: Array.from({ length: 12 }, (_, i) => ({
          x: meses[String(i + 1).padStart(2, "0")],
          y: 0,
        })),
      };

      vendas.forEach((venda) => {
        const mesVenda = venda.sessao.dtSessao.getMonth() + 1;
        const valorVenda = venda.sessao.vlEntrada;

        faturamentoMensal.data[mesVenda - 1].y += Number(valorVenda);
      });

      return faturamentoMensal;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getSessoesPassadasEFuturasByFilme(idFilme: string) {
    const today = new Date();

    const sessoes = await this.prisma.sessao.findMany({
      where: {
        idFilme: idFilme,
      },
    });

    const sesoesWithExtraData = await Promise.all(
      sessoes.map(async (sessao) => {
        const nomeFilme =
          await this.filmesService.getNomeFilmeExternal(idFilme);

        const capaUrl = await this.filmesService.getCapaFilmeExternal(idFilme);

        const avaliacao: LocalAvaliacao[] = await this.prisma
          .$queryRaw`SELECT a."valor" FROM "public"."Avaliacao" a where a."idFilme" = ${idFilme}`;

        return {
          ...sessao,
          nomeFilme: nomeFilme,
          avaliacao: avaliacao.length > 0 ? avaliacao[0].valor : 0,
          capaUrl: capaUrl,
        };
      }),
    );

    const sessoesPassadas = {};
    const sessoesFuturas = {};

    for (const sessao of sesoesWithExtraData) {
      const dataSessao = new Date(sessao.dtSessao);

      const formattedDate = `${dataSessao.getDate()}/${
        dataSessao.getMonth() + 1
      }/${dataSessao.getFullYear()}`;

      if (dataSessao < today) {
        if (!sessoesPassadas[formattedDate]) {
          sessoesPassadas[formattedDate] = { data: formattedDate, sessoes: [] };
        }
        sessoesPassadas[formattedDate].sessoes.push(sessao);
      } else {
        if (!sessoesFuturas[formattedDate]) {
          sessoesFuturas[formattedDate] = { data: formattedDate, sessoes: [] };
        }
        sessoesFuturas[formattedDate].sessoes.push(sessao);
      }
    }

    const sessoesPassadasArray = Object.values(sessoesPassadas);
    const sessoesFuturasArray = Object.values(sessoesFuturas);

    return {
      SessoesPassadas: sessoesPassadasArray,
      SessoesFuturas: sessoesFuturasArray,
    };
  }
}

type LocalAvaliacao = {
  valor: number;
};
