import {
  Body,
  ConflictException,
  HttpException,
  Injectable,
} from "@nestjs/common";
import { CreateUserDto, EmailDto } from "./dto/user.dto";
import { hash } from "bcrypt";
import { PrismaClient } from "@prisma/client";

@Injectable()
export class UsuarioService {
  constructor(private prisma: PrismaClient) {}

  async create(dto: CreateUserDto) {
    try {
      const user = await this.prisma.usuario.findUnique({
        where: {
          email: dto.email,
        },
      });

      if (user) throw new ConflictException("Email já cadastrado");

      const { senha, ...userInfo } = dto;

      return this.prisma.$transaction(async (transactionClient) => {
        const newUser = await transactionClient.usuario.create({
          data: {
            ...userInfo,
            senha: await hash(dto.senha, 10),
          },
        });

        const { senha, id, ...result } = newUser;
        return result;
      });
    } catch (error) {
      console.log("USER_CREATE", error);
      throw new HttpException(error.response, error.status);
    }
  }

  async update(id: number, dto: CreateUserDto) {
    try {
      const { ...userInfo } = dto;

      return this.prisma.$transaction(async (transactionClient) => {
        const updateUser = await transactionClient.usuario.update({
          where: {
            id,
          },
          data: {
            ...userInfo,
            senha: await hash(dto.senha, 10),
          },
        });

        const { senha, id: idUser, ...result } = updateUser;

        return result;
      });
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async findByEmail(email: string) {
    return await this.prisma.usuario.findUnique({
      where: {
        email,
      },
    });
  }

  async getOne(id: number) {
    const user = await this.prisma.usuario.findUnique({
      where: {
        id,
      },
    });

    const { senha, ...result } = user;

    return result;
  }

  async findById(id: number) {
    let searchId: number;

    if (isNaN(id)) {
      searchId = 0;
    } else {
      searchId = id;
    }
    return await this.prisma.usuario.findUnique({
      where: {
        id: searchId,
      },
    });
  }

  async getAll() {
    const users = await this.prisma.usuario.findMany();
    const result = users.map((user) => {
      const { senha, ...rest } = user;
      return rest;
    });
    return result;
  }

  async findByIdCinema(idCinema: string) {
    return await this.prisma.usuario.findMany({
      include: {
        usuarioCinema: {
          where: {
            idCinema: idCinema,
          },
        },
      },
    });
  }

  async verificaVinculoUsuarioCinema(idUsuario: number, idCinema: string) {
    return await this.prisma.usuarioCinema.findFirst({
      where: {
        AND: [{ idUsuario: idUsuario }, { idCinema: idCinema }],
      },
    });
  }

  async deleteUser(idUsuario: number) {
    return await this.prisma.usuario.deleteMany({
      where: {
        id: idUsuario,
      },
    });
  }

  async getCinemasByUserEmail(dto: EmailDto) {
    try {
      const user = await this.prisma.usuario.findUnique({
        where: {
          email: dto.email,
        },
        include: {
          usuarioCinema: {
            include: {
              cinema: true,
            },
          },
        },
      });

      if (!user) {
        throw new ConflictException("Usuário não encontrado");
      }

      const cinemas = user.usuarioCinema.map((uc) => ({
        ...uc.cinema,
        isAdmin: uc.isAdmin,
      }));

      return cinemas;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }
}
