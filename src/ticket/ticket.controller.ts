import {
  Body,
  Controller,
  Delete,
  Get,
  Param,
  Post,
  UseGuards,
} from "@nestjs/common";
import { JwtGuard } from "src/auth/guards/jwt.guard";
import { TicketoDto } from "./dto/ticket.dto";
import { TicketService } from "./ticket.service";

@Controller("ticket")
export class TicketController {
  constructor(private ticketService: TicketService) {}

  @Get("all")
  async getAll() {
    return await this.ticketService.getAll();
  }

  @UseGuards(JwtGuard)
  @Get("sessao/:id")
  async getTicketsInfoBySessao(@Param("id") idSessao: string) {
    return await this.ticketService.getTicketsInfoBySessao(idSessao);
  }

  @Get("cpf/:cpf")
  async getTicketsByCpf(@Param("cpf") cpf: string) {
    return await this.ticketService.getTicketsByCpf(cpf);
  }

  @UseGuards(JwtGuard)
  @Get(":id")
  async getOne(@Param("id") id: string) {
    return await this.ticketService.getOne(id);
  }

  @Post("new")
  async create(@Body() dto: TicketoDto[]) {
    return await this.ticketService.createTicket(dto);
  }

  @Delete("delete/:cpf")
  async deleteTicketByCpf(@Param("cpf") cpfReserva: string) {
    return await this.ticketService.deleteTicketsByCpf(cpfReserva);
  }

  @Delete(":id")
  async deleteTicket(@Param("id") id: string) {
    return await this.ticketService.deleteTicket(id);
  }
}
