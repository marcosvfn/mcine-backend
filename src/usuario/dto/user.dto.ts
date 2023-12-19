import { IsEmail, IsString } from "class-validator";

type CinemaInfo = {
  idCinema: string;
  isAdmin: boolean;
};

export class CreateUserDto {
  @IsString()
  nome: string;

  @IsEmail()
  email: string;

  @IsString()
  senha: string;
}

export class EmailDto {
  @IsEmail()
  email: string;
}
