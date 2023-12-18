import { Module } from "@nestjs/common";
import { SalaService } from "./sala.service";
import { SalaController } from "./sala.controller";
import { JwtService } from "@nestjs/jwt";
import { FilmesService } from "src/filmes/filmes.service";
import { AvaliacaoService } from "src/avaliacao/avaliacao.service";
import { CinemaService } from "src/cinema/cinema.service";

@Module({
  providers: [
    SalaService,
    JwtService,
    FilmesService,
    AvaliacaoService,
    CinemaService,
  ],
  controllers: [SalaController],
})
export class SalaModule {}
