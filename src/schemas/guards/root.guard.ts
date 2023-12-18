import {
  CanActivate,
  ConflictException,
  ExecutionContext,
  HttpException,
  Injectable,
  UnauthorizedException,
} from "@nestjs/common";
import { UsuarioService } from "src/usuario/usuario.service";
import { checkIsRootTenant } from "src/utils/checkRootTenant";
import { Request } from "express";
import { JwtService } from "@nestjs/jwt";

@Injectable()
export class RootGuard implements CanActivate {
  constructor(
    private usuarioService: UsuarioService,
    private jwtService: JwtService,
  ) {}

  async canActivate(context: ExecutionContext): Promise<boolean> {
    try {
      const request = context.switchToHttp().getRequest();
      const token = this.extractTokenFromHeader(request);

      if (!token) throw new UnauthorizedException();

      const payload = await this.jwtService.verifyAsync(token, {
        secret: process.env.JWT_SECRET_KEY,
      });

      const isRootTenant = checkIsRootTenant(request);

      if (!isRootTenant)
        throw new ConflictException(
          "Somente usuários administradores podem cadastrar novos usuários",
        );

      const user = await this.usuarioService.findByEmail(payload.username);

      const usuarioVinculadoAoCinema =
        await this.usuarioService.verificaVinculoUsuarioCinema(
          user.id,
          String(request.headers["tenantid"]),
        );

      if (!usuarioVinculadoAoCinema)
        throw new ConflictException("Usuário não está vinculado ao cinema");

      return true;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  private extractTokenFromHeader(request: Request) {
    if (request.headers.authorization) {
      const [type, token] = request.headers.authorization.split(" ") ?? [];
      return type === "Bearer" ? token : undefined;
    }
    throw new UnauthorizedException();
  }
}

export class CreateUserGuard implements CanActivate {
  constructor() {}

  async canActivate(context: ExecutionContext): Promise<boolean> {
    try {
      const request = context.switchToHttp().getRequest();
      const token = this.extractTokenFromHeader(request);

      if (!token) throw new UnauthorizedException();

      const isRootTenant = checkIsRootTenant(request);

      if (!isRootTenant)
        throw new ConflictException(
          "Somente usuários administradores podem cadastrar novos usuários",
        );
      return true;
    } catch (error) {
      throw new HttpException(error.response, error.status);
    }
  }

  private extractTokenFromHeader(request: Request) {
    if (request.headers.authorization) {
      const [type, token] = request.headers.authorization.split(" ") ?? [];
      return type === "Bearer" ? token : undefined;
    }
    throw new UnauthorizedException();
  }
}
