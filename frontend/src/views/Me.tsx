import React, { useEffect, useState } from "react";
import { Link } from "react-router-dom";
import useApi from "../hooks/useApi";

const MyAccount: React.FC = () => {
  const api = useApi()
  const [myEvents, setMyEvents] = useState([]);

  useEffect(() => {
    (async () => {
      const events = await api.get('events')
      setMyEvents(events.data)
      })()
  }, [api])

  return (
    <div>
      <h1>Hi hello!</h1>
      <Link to="/createEvent">Create new event</Link>
      {myEvents.map((myEvent: any) => (
        <p>{myEvent.name}</p>
      ))}
    </div>
  );
};

export default MyAccount;
