import { useAuth0 } from "@auth0/auth0-react";
import React from "react";
import { Outlet } from "react-router-dom";
import "./App.css";

const App = () => {
   const {loginWithRedirect} = useAuth0()
  return (
    <div className="page-wrap">
      <div className="page-header">
        <button onClick={() => loginWithRedirect({redirectUri: "/me"})}>Login</button>
        <h1>Moro</h1>
      </div>
      <div className="page-main">
        <Outlet />
      </div>
    </div>
  );
};

export default App;
