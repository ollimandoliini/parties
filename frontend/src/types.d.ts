type Event = IEvent;

interface IEvent {
  name: string;
  description: string;
  startTime: string;
  location: string;
}

type Invite = IInvite;

interface IInvite {
  code: string;
  invitees: Invitee[];
}

type Status = "Accepted" | "Tentative" | "Declined" | "Unknown";

type Invitee = IInvitee;

interface IInvitee {
  name: string;
  status: Status;
}
type WithId<Type> = {id: number} & Type

