import { ConflictException, HttpException, Injectable } from "@nestjs/common";
import { PrismaClient } from "@prisma/client";
import { TicketoDto } from "./dto/ticket.dto";
import { SessaoService } from "src/sessao/sessao.service";

@Injectable()
export class TicketService {
  constructor(
    private prisma: PrismaClient,
    private sessaoService: SessaoService,
  ) {}

  async createTicket(ticketListDto: TicketoDto[]) {
    try {
      const newTickets = this.prisma.$transaction(async (transactionClient) => {
        const newTicketsInserted = await transactionClient.ticket.createMany({
          data: ticketListDto,
        });

        if (newTicketsInserted) {
          // Reserva os assentos
          const assentosIdsToUpdate = ticketListDto.map(
            (ticket) => ticket.idAssento,
          );

          const assentosStatus = await transactionClient.assento.findMany({
            where: {
              id: {
                in: assentosIdsToUpdate,
              },
            },
          });

          const assentoJaReservado = assentosStatus.some(
            (assento) => assento.reservado,
          );

          if (assentoJaReservado)
            throw new ConflictException(
              "Assento selecionado já está reservado",
            );

          await transactionClient.assento.updateMany({
            where: {
              id: {
                in: assentosIdsToUpdate,
              },
            },
            data: {
              reservado: true,
            },
          });
        }

        return newTicketsInserted;
      });
      return newTickets;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async deleteTicket(id: string) {
    try {
      const deletedTicket = this.prisma.$transaction(
        async (transactionClient) => {
          const ticketToDelete = await transactionClient.ticket.findUnique({
            where: {
              id,
            },
          });
          const deleted = await transactionClient.ticket.deleteMany({
            where: {
              id,
            },
          });

          if (deleted) {
            // Libera assento
            await transactionClient.assento.updateMany({
              where: {
                id: ticketToDelete.idAssento,
              },
              data: {
                reservado: false,
              },
            });
          }

          return deleted;
        },
      );
      return deletedTicket;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async deleteTicketsByCpf(cpfReserva: string) {
    try {
      const deletedTickets = this.prisma.$transaction(
        async (transactionClient) => {
          const ticketsToDelete = await transactionClient.ticket.findMany({
            where: {
              cpfReserva,
            },
          });

          if (!ticketsToDelete.length)
            throw new ConflictException(
              "Não foram encontrados tickets nesse cpf!",
            );

          const idsToDelete = ticketsToDelete.map((ticket) => ticket.id);

          const deleted = await transactionClient.ticket.deleteMany({
            where: {
              id: {
                in: idsToDelete,
              },
            },
          });

          if (deleted) {
            // Libera os assentos
            const assentosIdsToUpdate = ticketsToDelete.map(
              (ticket) => ticket.idAssento,
            );

            await transactionClient.assento.updateMany({
              where: {
                id: {
                  in: assentosIdsToUpdate,
                },
              },
              data: {
                reservado: false,
              },
            });
          }
          return deleted;
        },
      );
      return deletedTickets;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getAll() {
    try {
      return await this.prisma.ticket.findMany();
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getOne(id: string) {
    try {
      return await this.prisma.ticket.findUnique({
        where: {
          id,
        },
      });
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getTicketsByCpf(cpf: string) {
    try {
      let returnResponse = [];
      const ticketsData = await this.prisma.ticket.findMany({
        where: {
          cpfReserva: cpf,
        },
        include: {
          assento: {
            select: {
              numero: true,
            },
          },
          sessao: {
            select: {
              dtSessao: true,
              horaInicio: true,
              vlEntrada: true,
              idFilme: true,
              sala: {
                select: {
                  nome: true,
                },
              },
            },
          },
        },
      });

      returnResponse = ticketsData;

      if (ticketsData.length) {
        const response = await Promise.all(
          ticketsData.map(async (ticket) => {
            const nomeFilme = await this.prisma
              .$queryRaw`SELECT nome FROM "public"."Filmes" WHERE id = ${ticket.sessao.idFilme}`;

            return {
              ...ticket,
              nomeFilme: nomeFilme[0].nome,
            };
          }),
        );
        returnResponse = response;
      }
      return returnResponse;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getTicketsInfoBySessao(idSessao: string) {
    try {
      const sessao = await this.sessaoService.getOne(idSessao);
      const tickets = await this.prisma.ticket.findMany({
        where: {
          idSessao,
        },
        select: {
          id: true,
          nomeReserva: true,
          cpfReserva: true,
          createdAt: true,
          assento: {
            select: {
              id: true,
              numero: true,
            },
          },
        },
      });
      const assentos = await await this.prisma.assento.findMany({
        where: {
          idSessao,
        },
        orderBy: {
          numero: "asc",
        },
      });

      return {
        sessao,
        tickets,
        assentos,
      };
    } catch (error) {
      throw new HttpException(error.status, error.response);
    }
  }
}
