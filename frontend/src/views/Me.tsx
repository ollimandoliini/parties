import { Box, Button } from "@chakra-ui/react";
import React, { useEffect, useState } from "react";
import { Link } from "react-router-dom";
import useApi from "../hooks/useApi";

type WithId<Type> = {
  id: number;
  data: Type;
};

const EventRow: React.FC<{ event: WithId<IEvent> }> = ({ event }) => {
  return (
    <Box bg='tomato' w='100%' p={4} color='white'>
        <Link to={`/me/my-events/${event.id}`}>
          <h3>{event.data.name}</h3>
          <p>{event.data.description}</p>
        </Link>
      </Box>
  );
};

const MyAccount: React.FC = () => {
  const api = useApi();
  const [myEvents, setMyEvents] = useState([]);

  useEffect(() => {
    (async () => {
      const events = await api.get("events");
      setMyEvents(events.data);
    })();
  }, [api]);

  return (
    <>
      <h1>My events</h1>
      <div>
        {myEvents.map((event: WithId<IEvent>) => (
          <EventRow key={event.id} event={event} />
        ))}
      </div>
      <Button>
        <Link to="/create-event">Create new event</Link>
      </Button>
    </>
  );
};

export default MyAccount;
