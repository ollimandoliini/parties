import { useAuth0 } from "@auth0/auth0-react";
import React from "react";

const Home = () => {
  const { isAuthenticated } = useAuth0();
  return (
    <>
      <h1>Invite.to</h1>
      {isAuthenticated && <a href="/my-events">Take me to my events</a>}
    </>
  );
};

export default Home;
