import { IsNumber, IsString } from "class-validator";

export class AvaliacaoDto {
  @IsString()
  idFilme: string;

  @IsNumber()
  valor: number;
}
