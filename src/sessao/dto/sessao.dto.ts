import { IsString, IsNumber, IsBoolean, IsDateString } from "class-validator";

export class CreateSessaoDto {
  @IsString()
  idCinema: string;

  @IsString()
  idFilme: string;

  @IsNumber()
  vlEntrada: number;

  @IsString()
  horaInicio: string;

  @IsString()
  idSala: string;

  @IsDateString()
  dtSessao: string;

  @IsBoolean()
  agendarSemana: boolean;
}
