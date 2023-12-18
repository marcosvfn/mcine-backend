import { Body, Controller, Post, Request, UseGuards } from "@nestjs/common";
import { CreateUserDto } from "src/usuario/dto/user.dto";
import { UsuarioService } from "src/usuario/usuario.service";
import { LoginDto } from "./dto/auth.dto";
import { AuthService } from "./auth.service";
import { RerfeshJwtGuard } from "./guards/refresh.guard";

@Controller("auth")
export class AuthController {
  constructor(private authService: AuthService) {}

  @Post("login")
  async login(@Body() dto: LoginDto) {
    return await this.authService.login(dto);
  }

  @UseGuards(RerfeshJwtGuard)
  @Post("refresh")
  async refreshToken(@Request() req) {
    return await this.authService.refreshToken(req.user);
  }
}
