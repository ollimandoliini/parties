import axios, { AxiosResponse } from "axios";
import React, { SyntheticEvent, useEffect, useState } from "react";
import { useParams } from "react-router";
import useApi from "../hooks/useApi";
import { useNavigate } from "react-router-dom";


interface InviteCardProps {
  eventId: string;
  invite: WithId<IInvite>;
  invites: WithId<IInvite>[];
  setInvites: React.Dispatch<React.SetStateAction<WithId<IInvite>[]>>;
}

const InviteCard: React.FC<InviteCardProps> = ({
  eventId,
  invite,
  invites,
  setInvites,
}) => {
  const api = useApi();
  const onClickRemove = () => {
    (async () => {
      await api.delete(`events/${eventId}/invites/${invite.id}`);
      setInvites(invites.filter((inv) => inv.id !== invite.id));
    })();
  };

  return (
    <div>
      <code>{invite.code}</code>
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

const InviteForm: React.FC<InviteFormProps> = ({ eventId, invites, setInvites }) => {
  const [newInvitee, setNewInvitee] = useState("");
  const [invitees, setInvitees] = useState<string[]>([]);
  const api = useApi();

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
      setInvites([...invites, response.data])
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
  const api = useApi();
  const [eventData, setEventData] = useState<IEvent | null>(null);
  const [invites, setInvites] = useState<WithId<IInvite>[]>([]);
  const navigate = useNavigate();

  const onClickRemove = () => {
    (async () => {
      await api.delete(`/events/${eventId}`);
      navigate('/my-events/')
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

  if (!eventId) {
    return null;
  }

  return (
    <>
      <div>
        <h1>{eventData?.name}</h1>
        <h2>Invites</h2>
        {invites.map((invite) => (
          <InviteCard
            key={invite.id}
            eventId={eventId}
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
