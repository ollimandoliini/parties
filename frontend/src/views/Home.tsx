import { useAuth0 } from "@auth0/auth0-react";
import React from "react";

const Home = () => {
  const { isAuthenticated } = useAuth0();
  return (
    <>
      <p>Hi hello! With Invitee.io you can organize events and send out invitations for your invitees.</p>

      {isAuthenticated && <a href="/my-events">Take me to my events</a>}
    </>
  );
};

export default Home;
