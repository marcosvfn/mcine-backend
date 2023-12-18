import { IsBoolean, IsEmail, IsNumber, IsString } from "class-validator";

export class AssentoDto {
  @IsString()
  @IsNumber()
  numero: number;

  @IsString()
  idSessao: string;

  @IsBoolean()
  reservado: boolean;
}
