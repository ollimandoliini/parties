type Event = IEvent;

interface IEvent {
  name: string;
  startTime: string;
  description: string;
}

type Invite = IInvite;

interface IInvite {
  code: string;
  invitees: string[];
}
