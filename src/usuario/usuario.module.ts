import { Module } from "@nestjs/common";
import { UsuarioService } from "./usuario.service";
import { UsuarioController } from "./usuario.controller";
import { JwtService } from "@nestjs/jwt";
import { SchemasService } from "src/schemas/schemas.service";

@Module({
  providers: [UsuarioService, JwtService, SchemasService],
  controllers: [UsuarioController],
})
export class UsuarioModule {}
