import { IsString, IsNumber } from 'class-validator';

export class CreateSalaDto {
  @IsString()
  @IsString()
  idCinema: string;

  @IsString()
  nome: string;

  @IsNumber()
  capacidade: number;
}
