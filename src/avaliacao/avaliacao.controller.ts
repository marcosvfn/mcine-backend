import { Body, Controller, Get, Param, Post, UseGuards } from "@nestjs/common";
import { AvaliacaoService } from "./avaliacao.service";
import { JwtGuard } from "src/auth/guards/jwt.guard";
import { AvaliacaoDto } from "./dto/avaliacao.dto";

@Controller("avaliacao")
export class AvaliacaoController {
  constructor(private avaliacaoService: AvaliacaoService) {}

  @UseGuards(JwtGuard)
  @Get("all")
  async getAll() {
    return await this.avaliacaoService.getAll();
  }

  @Get(":id")
  async getAvaliacoesByFilme(@Param("id") idFilme: string) {
    return await this.avaliacaoService.getAvaliacao(idFilme);
  }

  @Post("new")
  async create(@Body() dto: AvaliacaoDto) {
    return await this.avaliacaoService.createAvaliacao(dto);
  }
}
