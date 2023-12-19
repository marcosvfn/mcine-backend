import { Prisma, PrismaClient } from "@prisma/client";
import { Injectable, OnModuleDestroy } from "@nestjs/common";
import { DefaultArgs } from "@prisma/client/runtime/library";

@Injectable()
export class PrismaClientManager implements OnModuleDestroy {
  private clients: { [key: string]: PrismaClient } = {};

  async getClient(tenantId: string): Promise<PrismaClient> {
    let client: PrismaClient<Prisma.PrismaClientOptions, never, DefaultArgs>;

    const rootClient = new PrismaClient({
      datasources: {
        db: {
          url: process.env.DATABASE_URL,
        },
      },
    });

    if (tenantId === "root") {
      client = rootClient;
    } else {
      client = this.clients[tenantId];
      if (!client) {
        const databaseUrl = process.env.DATABASE_URL.replace(
          "public",
          tenantId,
        );

        client = new PrismaClient({
          datasources: {
            db: {
              url: databaseUrl,
            },
          },
        });
      }

      this.clients[tenantId] = client;
    }

    return client;
  }

  async onModuleDestroy() {
    await Promise.all(
      Object.values(this.clients).map((client) => client.$disconnect()),
    );
  }
}
