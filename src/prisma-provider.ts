import { FactoryProvider, Scope } from "@nestjs/common";
import { PrismaClient } from "@prisma/client";
import { PrismaClientManager } from "./prisma.service";
import { Request } from "express";
import { REQUEST } from "@nestjs/core";
import {
  ContextId,
  ContextIdFactory,
  ContextIdStrategy,
  HostComponentInfo,
} from "@nestjs/core";

const tenants = new Map<string, ContextId>();

export class AggregateByTenantContextIdStrategy implements ContextIdStrategy {
  attach(contextId: ContextId, request: Request) {
    const tenantId = String(request.headers["tenantid"]);

    let tenantSubTreeId: ContextId;

    if (tenants.has(tenantId)) {
      tenantSubTreeId = tenants.get(tenantId);
    } else {
      tenantSubTreeId = ContextIdFactory.create();
      tenants.set(tenantId, tenantSubTreeId);
    }

    return {
      resolve: (info: HostComponentInfo) =>
        info.isTreeDurable ? tenantSubTreeId : contextId,
      payload: { tenantId },
    };
  }
}

export interface ContextPayload {
  tenantId: string;
}

export const prismaClientProvider: FactoryProvider<PrismaClient> = {
  provide: PrismaClient,
  scope: Scope.REQUEST,
  durable: true,
  useFactory: (ctxPayload: ContextPayload, manager: PrismaClientManager) => {
    return manager.getClient(ctxPayload.tenantId);
  },
  inject: [REQUEST, PrismaClientManager],
};
