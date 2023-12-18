import { Module } from "@nestjs/common";
import { CinemaService } from "./cinema.service";
import { CinemaController } from "./cinema.controller";
import { JwtService } from "@nestjs/jwt";
import { SchemasService } from "src/schemas/schemas.service";

@Module({
  providers: [CinemaService, JwtService, SchemasService],
  controllers: [CinemaController],
})
export class CinemaModule {}
