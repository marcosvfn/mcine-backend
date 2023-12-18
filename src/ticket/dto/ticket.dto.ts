import { IsString } from "class-validator";

export class TicketoDto {
  @IsString()
  @IsString()
  cpfReserva: string;

  @IsString()
  nomeReserva: string;

  @IsString()
  idSessao: string;

  @IsString()
  idAssento: string;
}
