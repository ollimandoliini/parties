CREATE TABLE "event" (
    "id" SERIAL8 PRIMARY KEY UNIQUE,
    "name" VARCHAR NOT NULL,
    "organizer" VARCHAR NOT NULL,
    "description" VARCHAR NOT NULL,
    "start_time" TIMESTAMP WITH TIME ZONE NOT NULL,
    "location" VARCHAR NOT NULL,
    "invites_are_public" BOOLEAN NOT NULL
);

CREATE TABLE "invite" (
    "id" SERIAL8  PRIMARY KEY UNIQUE,
    "event" INT8 NOT NULL,
    "code" VARCHAR NOT NULL
);

ALTER TABLE "invite" ADD CONSTRAINT "invite_event_fkey" FOREIGN KEY("event") REFERENCES "event"("id") ON DELETE CASCADE  ON UPDATE RESTRICT;
    
CREATE TABLE "invitee" (
    "id" SERIAL8  PRIMARY KEY UNIQUE,
    "invite" INT8 NOT NULL,
    "name" VARCHAR NOT NULL,
    "status" VARCHAR NOT NULL
);

ALTER TABLE "invitee" ADD CONSTRAINT "invitee_invite_fkey" FOREIGN KEY("invite") REFERENCES "invite"("id") ON DELETE CASCADE  ON UPDATE RESTRICT;
