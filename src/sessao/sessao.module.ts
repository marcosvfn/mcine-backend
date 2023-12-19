import { Module } from "@nestjs/common";
import { SessaoService } from "./sessao.service";
import { SessaoController } from "./sessao.controller";
import { JwtService } from "@nestjs/jwt";
import { FilmesService } from "src/filmes/filmes.service";
import { CinemaService } from "src/cinema/cinema.service";
import { AssentoService } from "src/assento/assento.service";
import { AvaliacaoService } from "src/avaliacao/avaliacao.service";

@Module({
  providers: [
    SessaoService,
    JwtService,
    AvaliacaoService,
    FilmesService,
    CinemaService,
    AssentoService,
    AvaliacaoService,
  ],
  controllers: [SessaoController],
})
export class SessaoModule {}
