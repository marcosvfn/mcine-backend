import { IsBoolean, IsDateString, IsString } from "class-validator";

export class CreateFilmeDto {
  @IsString()
  @IsString()
  nome: string;

  @IsString()
  sinopse: string;

  @IsDateString()
  dtLancamento: string;

  @IsString()
  capaUrl: string;

  @IsString()
  linkTrailer: string;

  @IsBoolean()
  disponivel: boolean;
}

export type FilmesMaisVisualizadosResponse = {
  cinemanome: string;
  filmenome: string;
  quantidadeticketsvendidos: number;
};
