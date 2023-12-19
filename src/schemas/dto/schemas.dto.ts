import { IsString } from "class-validator";

export class CreateSchemaDto {
  @IsString()
  schemaName: string;
}
