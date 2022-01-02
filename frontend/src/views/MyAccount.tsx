import { Box, Button } from "@chakra-ui/react";
import { AxiosResponse } from "axios";
import React, { useEffect, useState } from "react";
import { Link } from "react-router-dom";
import useApi from "../hooks/useApi";



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
  const api = useApi();
  const onClickRemove = () => {
    (async () => {
      await api.delete(`/events/${event.id}`);
      setMyEvents(myEvents.filter(event_ => event_.id !== event.id))
    })();
  };

  return (
    <Box bg="tomato" w="100%" p={4} ctolor="white">
      <Link to={`/my-events/${event.id}`}>
        <h3>{event.name}</h3>
        <p>{event.description}</p>
      </Link>
      <sup onClick={onClickRemove}>x</sup>
    </Box>
  );
};

const MyAccount: React.FC = () => {
  const api = useApi();
  const [myEvents, setMyEvents] = useState<WithId<IEvent>[]>([]);

  useEffect(() => {
    (async () => {
      const events: AxiosResponse<WithId<IEvent>[]> = await api.get("events");
      setMyEvents(events.data);
    })();
  }, [api]);

  return (
    <>
      <h1>My events</h1>
      <div>
        {myEvents.map((event: WithId<IEvent>) => (
          <EventRow
            key={event.id}
            event={event}
            myEvents={myEvents}
            setMyEvents={setMyEvents}
          />
        ))}
      </div>
      <Button>
        <Link to="/create-event">Create new event</Link>
      </Button>
    </>
  );
};

export default MyAccount;
