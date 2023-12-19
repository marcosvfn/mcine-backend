import { ConflictException, HttpException, Injectable } from "@nestjs/common";
import { EditCinemaDto } from "./dto/cinema.dto";
import { PrismaClient } from "@prisma/client";

@Injectable()
export class CinemaService {
  constructor(private prisma: PrismaClient) {}

  async getAll() {
    try {
      return await this.prisma.cinema.findMany();
    } catch (error) {
      console.log("[CINEMA_GETALL]", error);
    }
  }

  async getOne(id: string) {
    try {
      const Cinema = await this.prisma.cinema.findUnique({
        where: {
          id,
        },
      });

      if (!Cinema) return new ConflictException("Cinema não encontrado");

      return Cinema;
    } catch (error) {
      console.log("[CINEMA_GETONE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async create(dto: EditCinemaDto) {
    try {
      const newCinema = await this.prisma.$transaction(
        async (transactionClient) => {
          const Cinema = await transactionClient.cinema.findFirst({
            where: {
              nome: dto.nome,
            },
          });
          if (Cinema) throw new ConflictException("Cinema já cadastrado");

          const newCinema = await transactionClient.cinema.create({
            data: {
              nome: dto.nome,
            },
          });

          if (newCinema) {
            // Create new Schema for the cinema
            await transactionClient.$executeRaw`SELECT clone_schema('defaultschema', ${newCinema.id}, 'NODATA')`;

            const insertUsuarioCinemaData = dto.usuariosCinema.map(
              (usuario) => ({
                idCinema: newCinema.id,
                isAdmin: usuario.isAdmin,
                idUsuario: usuario.idUsuario,
              }),
            );

            await transactionClient.usuarioCinema.createMany({
              data: insertUsuarioCinemaData,
            });
          }

          return newCinema;
        },
      );

      return newCinema;
    } catch (error) {
      console.log("[CINEMA_CREATE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async edit(id: string, dto: EditCinemaDto) {
    try {
      const updatedCinema = this.prisma.$transaction(
        async (transactionClient) => {
          const Cinema = await transactionClient.cinema.findUnique({
            where: {
              id,
            },
          });

          if (!Cinema) throw new ConflictException("Cinema não encontrado");

          const updatedCinema = await transactionClient.cinema.update({
            where: {
              id,
            },
            data: {
              nome: dto.nome,
            },
          });

          // Atualiza os usuários
          await transactionClient.usuarioCinema.deleteMany({
            where: {
              idCinema: id,
            },
          });

          const usuarioCinemaDataToUpdate = dto.usuariosCinema.map((item) => ({
            isAdmin: item.isAdmin,
            idUsuario: item.idUsuario,
            idCinema: id,
          }));

          await transactionClient.usuarioCinema.createMany({
            data: usuarioCinemaDataToUpdate,
          });

          return updatedCinema;
        },
      );

      return updatedCinema;
    } catch (error) {
      console.log("[CINEMA_EDIT]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async delete(id: string) {
    try {
      const deleted = await this.prisma.$transaction(
        async (transactionClient) => {
          if (!id) throw new ConflictException("Id do Cinema é necessário");

          const Cinema = await transactionClient.cinema.deleteMany({
            where: {
              id,
            },
          });

          if (Cinema) {
            await transactionClient.$executeRaw`SELECT drop_schema(${id})`;
          }

          return Cinema;
        },
      );
      return deleted;
    } catch (error) {
      console.log("[CINEMA_DELETE]", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async getUsuariosByIdCinema(id: string) {
    try {
      const response = await this.prisma.usuarioCinema.findMany({
        where: {
          idCinema: id,
        },
        include: {
          cinema: true,
          user: true,
        },
      });

      const noPasswordResponse = response.map((item) => ({
        ...item,
        user: {
          nome: item.user.nome,
          email: item.user.email,
        },
      }));

      return noPasswordResponse;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getNomeCinemaExternal(idCinema: string) {
    const queryResult = await this.prisma
      .$queryRaw`SELECT nome FROM "public"."Cinema" WHERE id = ${idCinema}`;
    return queryResult[0].nome;
  }

  async getTotalArrecadadoSemanaCorrente() {
    try {
      const cinemaList = await this.prisma.cinema.findMany();

      const tenantIds = cinemaList
        .map((cinema) => cinema.id)
        .filter((id) => id !== "root");

      const qryResult = await this.prisma
        .$queryRaw`SELECT * FROM get_total_arrecadado_tickets_semana_corrente(${tenantIds})`;

      return qryResult;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getTotalArrecadadoNoAnoByCinema() {
    try {
      const cinemaList = await this.prisma.cinema.findMany();

      const tenantIds = cinemaList
        .map((cinema) => cinema.id)
        .filter((id) => id !== "root");

      const qryResult: TotalArrecadadoByAnoReturn[] = await this.prisma
        .$queryRaw`SELECT * FROM get_total_arrecadado_cinema(${tenantIds})`;

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

      const dadosFormatados = [];

      const cinemas = {};
      cinemaList
        .filter((item) => item.id !== process.env.ROOT_TENANT_IDENTIFIER)
        .map((item) => item.nome)
        .forEach((id) => {
          cinemas[id] = {
            id,
            data: Array.from({ length: 12 }, (_, i) => ({
              x: meses[String(i + 1).padStart(2, "0")],
              y: 0,
            })),
          };
        });

      qryResult.forEach((item) => {
        const cinema = cinemas[item.nomecinema];
        const mesIndex = Number(String(item.mesreferencia).split("-")[1]) - 1;
        cinema.data[mesIndex].y = parseInt(String(item.totalarrecadado));
      });

      for (const cinemaId in cinemas) {
        dadosFormatados.push(cinemas[cinemaId]);
      }

      return dadosFormatados;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async getUltimasVendas() {
    try {
      const cinemaList = await this.prisma.cinema.findMany();

      const tenantIds = cinemaList
        .map((cinema) => cinema.id)
        .filter((id) => id !== "root");

      const qryResult: UltimasVendas[] = await this.prisma
        .$queryRaw`SELECT * FROM get_ultimas_vendas(${tenantIds})`;

      const formattedResult = qryResult
        .sort((a, b) => {
          const dateA = new Date(a.datareserva).getTime();
          const dateB = new Date(b.datareserva).getTime();

          return dateB - dateA;
        })
        .slice(0, 5);
      return formattedResult;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }
}

type TotalArrecadadoByAnoReturn = {
  nomecinema: string;
  mesreferencia: Date;
  totalarrecadado: number;
};

type UltimasVendas = {
  nomereserva: string;
  nomecinema: string;
  valor: number;
  datareserva: string;
};
