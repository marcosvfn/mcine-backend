import {
  HttpException,
  Injectable,
  UnauthorizedException,
} from "@nestjs/common";
import { LoginDto } from "./dto/auth.dto";
import { UsuarioService } from "src/usuario/usuario.service";
import { compare } from "bcrypt";
import { JwtService } from "@nestjs/jwt";

const EXPIRE_TIME = 24 * 60 * 60 * 1000;

@Injectable()
export class AuthService {
  constructor(
    private userService: UsuarioService,
    private jwtService: JwtService,
  ) {}

  async login(dto: LoginDto) {
    try {
      const user = await this.validateUser(dto);
      const payload = {
        username: user.email,
        sub: {
          name: user.nome,
        },
      };

      return {
        user,
        backendTokens: {
          accessToken: await this.jwtService.signAsync(payload, {
            expiresIn: "1d",
            secret: process.env.JWT_SECRET_KEY,
          }),
          refreshToken: await this.jwtService.signAsync(payload, {
            expiresIn: "7d",
            secret: process.env.JWT_REFRESH_TOKEN_KEY,
          }),
          expiresIn: new Date().setTime(new Date().getTime() + EXPIRE_TIME),
        },
      };
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  async validateUser(dto: LoginDto) {
    const user = await this.userService.findByEmail(dto.username);

    if (user && (await compare(dto.password, user.senha))) {
      const { senha, ...result } = user;
      return result;
    }

    throw new UnauthorizedException();
  }

  async refreshToken(user: any) {
    try {
      const payload = {
        username: user.username,
        sub: user.sub,
      };

      return {
        accessToken: await this.jwtService.signAsync(payload, {
          expiresIn: "1d",
          secret: process.env.JWT_SECRET_KEY,
        }),
        refreshToken: await this.jwtService.signAsync(payload, {
          expiresIn: "7d",
          secret: process.env.JWT_REFRESH_TOKEN_KEY,
        }),
        expiresIn: new Date().setTime(new Date().getTime() + EXPIRE_TIME),
      };
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }
}
