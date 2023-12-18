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
import { SalaService } from "./sala.service";
import { CreateSalaDto } from "./dto/sala.dto";

@Controller("sala")
export class SalaController {
  constructor(private salaService: SalaService) {}

  @UseGuards(JwtGuard)
  @Get("all")
  async getAll() {
    return await this.salaService.getAll();
  }

  @Get("quadro")
  async getQuadroHorarioBySala() {
    return await this.salaService.getQuadroDeHorariosBySala();
  }

  @UseGuards(JwtGuard)
  @Get(":id")
  async getOne(@Param("id") id: string) {
    return await this.salaService.getOne(id);
  }

  @UseGuards(JwtGuard)
  @Post("new")
  async create(@Body() dto: CreateSalaDto) {
    return await this.salaService.create(dto);
  }

  @UseGuards(JwtGuard)
  @Patch(":id")
  async edit(@Param("id") id: string, @Body() dto: CreateSalaDto) {
    return await this.salaService.edit(id, dto);
  }

  @UseGuards(JwtGuard)
  @Delete(":id")
  async delete(@Param("id") id: string) {
    return await this.salaService.delete(id);
  }
}
