import { Request } from "express";
import { rootSchemaIdentifier } from "src/constants/constants";

export const checkIsRootTenant = (req: Request) => {
  const tenantid = String(req.headers["tenantid"]);

  return tenantid === rootSchemaIdentifier;
};
