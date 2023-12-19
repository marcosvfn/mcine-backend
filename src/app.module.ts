import { Global, Module } from "@nestjs/common";
import { AppController } from "./app.controller";
import { AppService } from "./app.service";
import { ConfigModule } from "@nestjs/config";
import { AuthModule } from "./auth/auth.module";
import { SchemasModule } from "./schemas/schemas.module";
import { FilmesModule } from "./filmes/filmes.module";
import { CinemaModule } from "./cinema/cinema.module";
import { SessaoModule } from "./sessao/sessao.module";
import { SalaModule } from "./sala/sala.module";
import { prismaClientProvider } from "./prisma-provider";
import { PrismaClientManager } from "./prisma.service";
import { PrismaClient } from "@prisma/client";
import { UsuarioModule } from "./usuario/usuario.module";
import { AssentoModule } from "./assento/assento.module";
import { TicketModule } from "./ticket/ticket.module";
import { AvaliacaoModule } from "./avaliacao/avaliacao.module";

@Global()
@Module({
  imports: [
    ConfigModule.forRoot(),
    UsuarioModule,
    AuthModule,
    SchemasModule,
    FilmesModule,
    CinemaModule,
    SessaoModule,
    SalaModule,
    AssentoModule,
    TicketModule,
    AvaliacaoModule,
  ],
  controllers: [AppController],
  providers: [AppService, prismaClientProvider, PrismaClientManager],
  exports: [PrismaClient],
})
export class AppModule {}
