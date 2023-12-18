import { Module } from "@nestjs/common";
import { SchemasService } from "./schemas.service";
import { SchemasController } from "./schemas.controller";
import { JwtService } from "@nestjs/jwt";

@Module({
  providers: [SchemasService, JwtService],
  controllers: [SchemasController],
})
export class SchemasModule {}
