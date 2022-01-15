import { AxiosResponse } from "axios";
import { format } from "date-fns";
import React, { useEffect, useState } from "react";
import { Link } from "react-router-dom";
import useAdminApi from "../hooks/useAdminApi";
import "./MyAccount.scss";

interface EventRowProps {
  event: WithId<IEvent>;
  myEvents: WithId<IEvent>[];
  setMyEvents: React.Dispatch<React.SetStateAction<WithId<IEvent>[]>>;
}

const EventRow: React.FC<EventRowProps> = ({
  event,
  myEvents,
  setMyEvents,
}) => {
  return (
    <div className="my-event-card">
      <a href={`/my-events/${event.id}`}>
        <h3>{event.name}</h3>
        <div>{format(new Date(event.startTime), "dd.MM.yyyy HH:mm")}</div>
        <p>{event.description}</p>
      </a>
    </div>
  );
};

const MyAccount: React.FC = () => {
  const api = useAdminApi();
  const [myEvents, setMyEvents] = useState<WithId<IEvent>[]>([]);

  useEffect(() => {
    (async () => {
      try {
        const events: AxiosResponse<WithId<IEvent>[]> = await api.get("events");
        setMyEvents(events.data);
      } catch (e) {
        console.log(e);
      }
    })();
  }, [api]);

  return (
    <>
      <h1>My events</h1>
      <div className="my-event-cards-container">
        {myEvents.map((event: WithId<IEvent>) => (
          <EventRow
            key={event.id}
            event={event}
            myEvents={myEvents}
            setMyEvents={setMyEvents}
          />
        ))}
      </div>
      <Link to="/create-event">Create new event</Link>
    </>
  );
};

export default MyAccount;
