import { ContextIdFactory, NestFactory } from "@nestjs/core";
import { AppModule } from "./app.module";
import { ValidationPipe } from "@nestjs/common";
import { AggregateByTenantContextIdStrategy } from "./prisma-provider";

async function bootstrap() {
  const app = await NestFactory.create(AppModule);

  app.enableCors();

  ContextIdFactory.apply(new AggregateByTenantContextIdStrategy());

  app.useGlobalPipes(
    new ValidationPipe({
      whitelist: true,
      forbidNonWhitelisted: true,
      transform: true,
    }),
  );

  await app.listen(8000);
}
bootstrap();
