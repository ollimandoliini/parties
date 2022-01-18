import axios, { AxiosResponse } from "axios";
import React, { SyntheticEvent, useEffect, useState } from "react";
import { useParams } from "react-router";
import useAdminApi from "../hooks/useAdminApi";
import { useNavigate } from "react-router-dom";

interface InviteCardProps {
  event: WithId<IEvent>;
  invite: WithId<IInvite>;
  invites: WithId<IInvite>[];
  setInvites: React.Dispatch<React.SetStateAction<WithId<IInvite>[]>>;
}

const InviteCard: React.FC<InviteCardProps> = ({
  event,
  invite,
  invites,
  setInvites,
}) => {
  const api = useAdminApi();
  const onClickRemove = () => {
    (async () => {
      await api.delete(`events/${event.id}/invites/${invite.id}`);
      setInvites(invites.filter((inv) => inv.id !== invite.id));
    })();
  };

  const urlCharacterMapping: Record<string, string> = {
    ä: "a",
    ö: "o",
    å: "å",
    " ": "-",
  };

  const eventUrlName = event.name
    .split("")
    .map(char => char.toLowerCase())
    .map((char) => urlCharacterMapping[char] || char)
    .join("")
    .slice(0, 30)

  const mPort = window.location.port ? ":" + window.location.port : ""
  const inviteUrl = `${window.location.protocol}//${window.location.hostname}${mPort}/e/${event.id}/${eventUrlName}/${invite.code}`;

  return (
    <div>
      <p>{inviteUrl}</p>
      <ol>
        {invite.invitees.map((invitee, index) => (
          <li key={index}>
            {invitee.name} {invitee.status}
          </li>
        ))}
      </ol>
      <div>
        <sup onClick={onClickRemove}>x</sup>
      </div>
    </div>
  );
};

interface InviteFormProps {
  eventId: string;
  invites: WithId<IInvite>[];
  setInvites: React.Dispatch<React.SetStateAction<WithId<IInvite>[]>>;
}

const InviteForm: React.FC<InviteFormProps> = ({
  eventId,
  invites,
  setInvites,
}) => {
  const [newInvitee, setNewInvitee] = useState("");
  const [invitees, setInvitees] = useState<string[]>([]);
  const api = useAdminApi();

  const onSubmit = (e: SyntheticEvent) => {
    e.preventDefault();
    if (newInvitee && !invitees.includes(newInvitee)) {
      setInvitees([...invitees, newInvitee]);
      setNewInvitee("");
    }
  };

  const onClickRemove = (inviteeName: string) => () => {
    setInvitees(invitees.filter((invitee) => invitee !== inviteeName));
  };

  const onClickCreateInvite = () => {
    (async () => {
      const inviteResponse = await api.post(`events/${eventId}/invites`);
      const inviteId = inviteResponse.data.id;
      await Promise.all(
        invitees.map((invitee) =>
          api.post(`events/${eventId}/invites/${inviteId}/invitees`, {
            name: invitee,
          })
        )
      );
      const response = await api.get(`events/${eventId}/invites/${inviteId}`);
      setInvitees([]);
      setInvites([...invites, response.data]);
    })();
  };

  return (
    <div>
      <h2>New invite</h2>
      <form onSubmit={onSubmit} autoComplete="off">
        <label htmlFor="name">Name</label>
        <input
          id="name"
          type="text"
          onChange={(e) => setNewInvitee(e.target.value)}
          value={newInvitee}
        />
        <button type="submit">Add</button>
        {invitees.map((invitee, i) => (
          <div key={i}>
            {invitee} <sup onClick={onClickRemove(invitee)}>x</sup>
          </div>
        ))}
        <button onClick={onClickCreateInvite}>Create Invite</button>
      </form>
    </div>
  );
};

const EventAdmin: React.FC = () => {
  const { eventId } = useParams();
  const api = useAdminApi();
  const [eventData, setEventData] = useState<IEvent | null>(null);
  const [invites, setInvites] = useState<WithId<IInvite>[]>([]);
  const navigate = useNavigate();

  const onClickRemove = () => {
    (async () => {
      await api.delete(`/events/${eventId}`);
      navigate("/my-events/");
    })();
  };

  useEffect(() => {
    (async () => {
      try {
        const event: AxiosResponse<IEvent> = await api.get(`events/${eventId}`);
        setEventData(event.data);
        const invites: AxiosResponse<WithId<IInvite>[]> = await api.get(
          `events/${eventId}/invites`
        );
        setInvites(invites.data);
      } catch (error: unknown) {
        if (axios.isAxiosError(error) && error.response?.status === 404) {
          navigate("/404");
        } else {
          throw error;
        }
      }
    })();
  }, [api, eventId, navigate]);

  if (!eventId || !eventData) {
    return null;
  }

  const eventDataWithId = { id: Number(eventId), ...eventData };

  return (
    <>
      <div>
        <h1>{eventData?.name}</h1>
        <h2>Invites</h2>
        {invites.map((invite) => (
          <InviteCard
            key={invite.id}
            event={eventDataWithId}
            invite={invite}
            invites={invites}
            setInvites={setInvites}
          />
        ))}
        <InviteForm
          eventId={eventId}
          invites={invites}
          setInvites={setInvites}
        />
        <button onClick={onClickRemove}>Delete event</button>
      </div>
    </>
  );
};

export default EventAdmin;
