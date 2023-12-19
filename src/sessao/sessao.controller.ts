import {
  Body,
  Controller,
  Delete,
  Get,
  Param,
  Patch,
  Post,
  UseGuards,
} from "@nestjs/common";
import { JwtGuard } from "src/auth/guards/jwt.guard";
import { SessaoService } from "./sessao.service";
import { CreateSessaoDto } from "./dto/sessao.dto";

@Controller("sessao")
export class SessaoController {
  constructor(private sessaoService: SessaoService) {}

  @UseGuards(JwtGuard)
  @Get("all")
  async getAll() {
    return await this.sessaoService.getAll();
  }

  @Get("filme/:id")
  async getSessoesPassadasEFuturasByFilme(@Param("id") idFilme: string) {
    return await this.sessaoService.getSessoesPassadasEFuturasByFilme(idFilme);
  }

  @UseGuards(JwtGuard)
  @Get("vendas")
  async getUltimasVendas() {
    return await this.sessaoService.getUltimasVendas();
  }

  @UseGuards(JwtGuard)
  @Get("faturamento")
  async getFaturamentoAnual() {
    return await this.sessaoService.getFaturamentoAnualByCinema();
  }

  @UseGuards(JwtGuard)
  @Get("total/filme")
  async getTotalArrecadadoByFilme() {
    return await this.sessaoService.getTotalArrecadadoByFilme();
  }

  @UseGuards(JwtGuard)
  @Get("total/semana")
  async getTotalArrecadadoSemana() {
    return await this.sessaoService.getTotalArrecadadoSemana();
  }

  @UseGuards(JwtGuard)
  @Get("visualizacoes/all")
  async getTopFilmesAssistidos() {
    return await this.sessaoService.getTopFilmesAssistidos();
  }

  @UseGuards(JwtGuard)
  @Get("visualizacoes/semana")
  async getTopFilmesAssistidosSemana() {
    return await this.sessaoService.getTopFilmesAssistidosSemana();
  }

  @UseGuards(JwtGuard)
  @Get(":id")
  async getOne(@Param("id") id: string) {
    return await this.sessaoService.getOne(id);
  }

  @UseGuards(JwtGuard)
  @Post("new")
  async create(@Body() dto: CreateSessaoDto) {
    return await this.sessaoService.create(dto);
  }

  @UseGuards(JwtGuard)
  @Patch(":id")
  async edit(@Param("id") id: string, @Body() dto: CreateSessaoDto) {
    return await this.sessaoService.edit(id, dto);
  }

  @UseGuards(JwtGuard)
  @Delete(":id")
  async delete(@Param("id") id: string) {
    return await this.sessaoService.delete(id);
  }
}
