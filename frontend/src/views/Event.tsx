import axios, { AxiosResponse } from "axios";
import { format } from "date-fns";
import React, { useEffect, useState } from "react";
import { useNavigate, useParams } from "react-router";
import usePublicApi from "../hooks/usePublicApi";
import "./Event.scss";

const InviteeRSVPContainer: React.FC<{
  eventId: number;
  inviteCode: string;
  invitee: WithId<Invitee>;
}> = ({ eventId, inviteCode, invitee }) => {
  const api = usePublicApi();

  const onChange =
    (status: string) => (event: React.ChangeEvent<HTMLInputElement>) => {
      (async () => {
        try {
          await api.patch(
            `/event/${eventId}/invite-code/${inviteCode}/invitee/${invitee.id}`,
            { status }
          );
        } catch (e) {
          console.log(e);
        }
      })();
    };

  return (
    <div className="invitee-rsvp">
      <div>{invitee.name}</div>
      <div className="invitee-rsvp">
        <input
          id="accepted"
          value="Accepted"
          name={`rsvp-${invitee.id}`}
          type="radio"
          onChange={onChange("Accepted")}
        />
        <label>Yes</label>
        <input
          id="tentative"
          value="Tentative"
          name={`rsvp-${invitee.id}`}
          type="radio"
          onChange={onChange("Tentative")}
        />
        <label>Maybe</label>
        <input
          id="declined"
          value="Declined"
          name={`rsvp-${invitee.id}`}
          type="radio"
          onChange={onChange("Declined")}
        />
        <label>No</label>
      </div>
    </div>
  );
};

interface EventParams {
  eventId: string;
  eventName: string;
  inviteCode: string;
}

const Event = () => {
  const { eventId, inviteCode } = useParams<
    keyof EventParams
  >() as EventParams;
  const api = usePublicApi();
  const [eventInvite, setEventInvite] = useState<EventInvite | null>(null);
  const navigate = useNavigate();

  useEffect(() => {
    (async () => {
      try {
        const eventInviteResponse: AxiosResponse<EventInvite> = await api.get(
          `event/${eventId}/invite-code/${inviteCode}`
        );
        setEventInvite(eventInviteResponse.data);
      } catch (error: unknown) {
        if (axios.isAxiosError(error) && error.response?.status === 404) {
          navigate("/404");
        } else {
          throw error;
        }
      }
    })();
  }, [api, eventId, inviteCode, navigate]);

  if (!eventInvite) {
    return <></>;
  }

  return (
    <>
      <div>
        {eventInvite.invitees.map((invitee) => invitee.name).join(", ")}
      </div>
      <h1>{eventInvite.eventInfo.name}</h1>
      <div>{eventInvite.eventInfo.location}</div>
      <div>
        {format(new Date(eventInvite.eventInfo.startTime), "dd.MM.yyyy HH:mm")}
      </div>
      <p>{eventInvite.eventInfo.description}</p>

      <h3>RSVP</h3>
      <div>
        {eventInvite.invitees.map((invitee) => (
          <InviteeRSVPContainer
            key={invitee.id}
            eventId={Number(eventId)}
            inviteCode={inviteCode}
            invitee={invitee}
          />
        ))}
      </div>
    </>
  );
};

export default Event;
