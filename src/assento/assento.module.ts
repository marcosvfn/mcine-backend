import { Module } from "@nestjs/common";
import { AssentoService } from "./assento.service";
import { AssentoController } from "./assento.controller";
import { JwtService } from "@nestjs/jwt";
import { CinemaService } from "src/cinema/cinema.service";
import { FilmesService } from "src/filmes/filmes.service";
import { AvaliacaoService } from "src/avaliacao/avaliacao.service";

@Module({
  providers: [
    AssentoService,
    JwtService,
    FilmesService,
    CinemaService,
    AvaliacaoService,
  ],
  controllers: [AssentoController],
})
export class AssentoModule {}
