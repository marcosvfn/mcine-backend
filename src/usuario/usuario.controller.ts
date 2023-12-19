import {
  Controller,
  Get,
  Param,
  UseGuards,
  Post,
  Body,
  Delete,
  Patch,
} from "@nestjs/common";
import { UsuarioService } from "./usuario.service";
import { JwtGuard } from "src/auth/guards/jwt.guard";
import { CreateUserDto, EmailDto } from "./dto/user.dto";
import { CreateUserGuard, RootGuard } from "src/schemas/guards/root.guard";

@Controller("usuario")
export class UsuarioController {
  constructor(private userService: UsuarioService) {}

  @UseGuards(JwtGuard, RootGuard)
  @Get("all")
  async getAll() {
    return await this.userService.getAll();
  }

  @UseGuards(JwtGuard)
  @Post("cinemas")
  async getCinemasByEmail(@Body() email: EmailDto) {
    return await this.userService.getCinemasByUserEmail(email);
  }

  @UseGuards(JwtGuard)
  @Get(":id")
  async getUserProfile(@Param("id") id: number) {
    return await this.userService.findById(id);
  }

  @UseGuards(CreateUserGuard)
  @Post("new")
  async create(@Body() dto: CreateUserDto) {
    return await this.userService.create(dto);
  }

  @UseGuards(JwtGuard, RootGuard)
  @Delete(":id")
  async delete(@Param("id") id: number) {
    return await this.userService.deleteUser(id);
  }

  @UseGuards(JwtGuard, RootGuard)
  @Patch(":id")
  async update(@Param("id") id: number, @Body() dto: CreateUserDto) {
    return await this.userService.update(id, dto);
  }
}
