import { IsArray, IsNumber, IsString } from "class-validator";

export class CreateCinemaDto {
  @IsNumber()
  idUsuario: string;

  @IsString()
  nome: string;
}

type UsuariosCinemaDto = {
  isAdmin: boolean;
  idUsuario: number;
};

export class EditCinemaDto {
  @IsString()
  nome: string;

  @IsArray()
  usuariosCinema: UsuariosCinemaDto[];
}
