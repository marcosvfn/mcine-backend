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
import { AssentoService } from "./assento.service";
import { AssentoDto } from "./dto/assento.dto";

@Controller("assento")
export class AssentoController {
  constructor(private assentoService: AssentoService) {}

  @Get("disponiveis/:id")
  async getAssentosDisponiveis(@Param("id") id: string) {
    return await this.assentoService.getAssentosDisponiveis(id);
  }

  @UseGuards(JwtGuard)
  @Get("all")
  async getAll() {
    return await this.assentoService.getAll();
  }

  @UseGuards(JwtGuard)
  @Get(":id")
  async getOne(@Param("id") id: string) {
    return await this.assentoService.getOne(id);
  }

  @UseGuards(JwtGuard)
  @Post("new")
  async create(@Body() dto: AssentoDto) {
    return await this.assentoService.create(dto);
  }

  @UseGuards(JwtGuard)
  @Patch(":id")
  async edit(@Param("id") id: string, @Body() dto: AssentoDto) {
    return await this.assentoService.edit(id, dto);
  }

  @UseGuards(JwtGuard)
  @Delete(":id")
  async delete(@Param("id") id: string) {
    return await this.assentoService.delete(id);
  }
}
