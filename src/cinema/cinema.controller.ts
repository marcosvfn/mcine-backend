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
import { CinemaService } from "./cinema.service";
import { EditCinemaDto } from "./dto/cinema.dto";

@Controller("cinema")
export class CinemaController {
  constructor(private cinemaService: CinemaService) {}

  @Get("all")
  async getAll() {
    return await this.cinemaService.getAll();
  }

  @UseGuards(JwtGuard)
  @Get("total")
  async getTotalSemanaCorrente() {
    return await this.cinemaService.getTotalArrecadadoSemanaCorrente();
  }

  @UseGuards(JwtGuard)
  @Get("faturamento")
  async getTotalFaturamentoAnual() {
    return await this.cinemaService.getTotalArrecadadoNoAnoByCinema();
  }

  @UseGuards(JwtGuard)
  @Get("vendas")
  async getUltimasVendas() {
    return await this.cinemaService.getUltimasVendas();
  }

  @UseGuards(JwtGuard)
  @Get("/usuarios/:id")
  async getUsuariosByIdCinema(@Param("id") id: string) {
    return await this.cinemaService.getUsuariosByIdCinema(id);
  }

  @UseGuards(JwtGuard)
  @Get(":id")
  async getOne(@Param("id") id: string) {
    return await this.cinemaService.getOne(id);
  }

  @UseGuards(JwtGuard)
  @Post("new")
  async create(@Body() dto: EditCinemaDto) {
    return await this.cinemaService.create(dto);
  }

  @UseGuards(JwtGuard)
  @Patch(":id")
  async edit(@Param("id") id: string, @Body() dto: EditCinemaDto) {
    return await this.cinemaService.edit(id, dto);
  }

  @UseGuards(JwtGuard)
  @Delete(":id")
  async delete(@Param("id") id: string) {
    return await this.cinemaService.delete(id);
  }
}
