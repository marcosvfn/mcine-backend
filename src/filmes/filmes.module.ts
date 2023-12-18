import { Module } from "@nestjs/common";
import { FilmesService } from "./filmes.service";
import { FilmesController } from "./filmes.controller";
import { JwtService } from "@nestjs/jwt";
import { CinemaService } from "src/cinema/cinema.service";
import { SchemasService } from "src/schemas/schemas.service";
import { AvaliacaoService } from "src/avaliacao/avaliacao.service";

@Module({
  providers: [
    FilmesService,
    JwtService,
    CinemaService,
    SchemasService,
    AvaliacaoService,
  ],
  controllers: [FilmesController],
})
export class FilmesModule {}
