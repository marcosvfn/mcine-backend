import { Module } from "@nestjs/common";
import { AvaliacaoService } from "./avaliacao.service";
import { AvaliacaoController } from "./avaliacao.controller";
import { JwtService } from "@nestjs/jwt";

@Module({
  providers: [AvaliacaoService, JwtService],
  controllers: [AvaliacaoController],
})
export class AvaliacaoModule {}
