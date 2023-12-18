import {
  Body,
  Controller,
  Delete,
  Get,
  Param,
  Patch,
  Post,
  Request,
  UseGuards,
} from "@nestjs/common";
import { JwtGuard } from "src/auth/guards/jwt.guard";
import { FilmesService } from "./filmes.service";
import { CreateFilmeDto } from "./dto/filmes.dto";

@Controller("filmes")
export class FilmesController {
  constructor(private filmesService: FilmesService) {}

  @Get("all")
  async getAll() {
    return await this.filmesService.getAll();
  }

  @Get("sessoes/:id")
  async getSessoesDisponiveis(@Param("id") idFilme: string) {
    return await this.filmesService.getCinemasSessoesDisponiveisByFilme(
      idFilme,
    );
  }

  @Get("disponiveis")
  async getDisponiveis() {
    return await this.filmesService.getFilmesDisponiveis();
  }

  @UseGuards(JwtGuard)
  @Get("total")
  async get() {
    return await this.filmesService.getTotalArrecadadoByFilme();
  }

  @UseGuards(JwtGuard)
  @Get("visualizacoes/all")
  async getMaisVisualizados() {
    return await this.filmesService.getFilmesMaisVisualizados();
  }

  @UseGuards(JwtGuard)
  @Get("visualizacoes/semana")
  async getMaisVisualizadosSemana() {
    return await this.filmesService.getFilmesMaisVisualizadosSemana();
  }

  @UseGuards(JwtGuard)
  @Get(":id")
  async getOne(@Param("id") id: string) {
    return await this.filmesService.getOne(id);
  }

  @UseGuards(JwtGuard)
  @Post("new")
  async create(@Body() dto: CreateFilmeDto) {
    return await this.filmesService.create(dto);
  }

  @UseGuards(JwtGuard)
  @Patch(":id")
  async edit(@Param("id") id: string, @Body() dto: CreateFilmeDto) {
    return await this.filmesService.edit(id, dto);
  }

  @UseGuards(JwtGuard)
  @Delete(":id")
  async delete(@Param("id") id: string) {
    return await this.filmesService.delete(id);
  }
}
