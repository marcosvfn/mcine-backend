import { Body, Controller, Post, UseGuards } from '@nestjs/common';
import { SchemasService } from './schemas.service';
import { CreateSchemaDto } from './dto/schemas.dto';
import { JwtGuard } from 'src/auth/guards/jwt.guard';

@Controller('schemas')
export class SchemasController {
  constructor(private schemaService: SchemasService) {}

  @UseGuards(JwtGuard)
  @Post('new')
  async createNewSchema(@Body() dto: CreateSchemaDto) {
    return await this.schemaService.create(dto.schemaName);
  }
}
