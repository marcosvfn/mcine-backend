import { Injectable } from "@nestjs/common";
import { PrismaClient } from "@prisma/client";

@Injectable()
export class SchemasService {
  constructor(private prismaService: PrismaClient) {}

  async create(idCinema: string) {
    await this.prismaService
      .$executeRaw`SELECT clone_schema('defaultschema', ${idCinema}, 'NODATA')`;
  }

  async delete(idCinema: string) {
    await this.prismaService.$executeRaw`SELECT drop_schema(${idCinema})`;
  }
}
