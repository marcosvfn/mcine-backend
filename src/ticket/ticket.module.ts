import { Module } from "@nestjs/common";
import { TicketService } from "./ticket.service";
import { TicketController } from "./ticket.controller";
import { JwtService } from "@nestjs/jwt";
import { SessaoService } from "src/sessao/sessao.service";
import { FilmesService } from "src/filmes/filmes.service";
import { CinemaService } from "src/cinema/cinema.service";
import { AssentoService } from "src/assento/assento.service";
import { AvaliacaoService } from "src/avaliacao/avaliacao.service";

@Module({
  providers: [
    TicketService,
    JwtService,
    SessaoService,
    FilmesService,
    CinemaService,
    AssentoService,
    AvaliacaoService,
  ],
  controllers: [TicketController],
})
export class TicketModule {}
