import React from "react";
import { useParams } from "react-router";

const Event = () => {
  const params = useParams();
  return (
    <>
      <h1>Event</h1>
      <p>{params.eventId}</p>
      <p>{params.inviteCode}</p>
    </>
  );
};

export default Event;
