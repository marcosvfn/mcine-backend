import { Module } from "@nestjs/common";
import { AuthController } from "./auth.controller";
import { AuthService } from "./auth.service";
import { UsuarioService } from "src/usuario/usuario.service";
import { JwtService } from "@nestjs/jwt";

@Module({
  controllers: [AuthController],
  providers: [AuthService, UsuarioService, JwtService],
})
export class AuthModule {}
