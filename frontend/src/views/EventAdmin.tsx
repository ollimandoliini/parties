import axios, { AxiosResponse } from "axios";
import React, { SyntheticEvent, useEffect, useState } from "react";
import { useParams } from "react-router";
import useApi from "../hooks/useApi";
import { useNavigate } from "react-router-dom";
import {
  Box,
  Button,
  FormControl,
  FormLabel,
  Heading,
  Input,
  List,
  ListItem,
} from "@chakra-ui/react";


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
    <Box borderWidth="3px" borderRadius="lg">
      <code>{invite.code}</code>
      <List>
        {invite.invitees.map((invitee, index) => (
          <ListItem key={index}>
            {invitee.name} {invitee.status}
          </ListItem>
        ))}
      </List>
      <div>
        <sup onClick={onClickRemove}>x</sup>
      </div>
    </Box>
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
    <Box>
      <h2>New invite</h2>
      <form onSubmit={onSubmit} autoComplete="off">
        <FormControl>
          <FormLabel htmlFor="name">Name</FormLabel>
          <Input
            id="name"
            type="text"
            label="Name"
            onChange={(e) => setNewInvitee(e.target.value)}
            value={newInvitee}
          />
        </FormControl>
        <Button type="submit">Add</Button>
        {invitees.map((invitee, i) => (
          <div key={i}>
              {invitee} <sup onClick={onClickRemove(invitee)}>x</sup>
          </div>
        ))}
        <Button onClick={onClickCreateInvite}>Create Invite</Button>
      </form>
    </Box>
  );
};

const EventAdmin: React.FC = () => {
  const { eventId } = useParams();
  const api = useApi();
  const [eventData, setEventData] = useState<IEvent | null>(null);
  const [invites, setInvites] = useState<WithId<IInvite>[]>([]);
  const navigate = useNavigate();

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
      <Box>
        <Heading as="h1">{eventData?.name}</Heading>
        {invites.map((invite) => (
          <InviteCard
            key={invite.id}
            eventId={eventId}
            invite={invite}
            invites={invites}
            setInvites={setInvites}
          />
        ))}
      </Box>
      <InviteForm
        eventId={eventId}
        invites={invites}
        setInvites={setInvites}
      />
    </>
  );
};

export default EventAdmin;
