import { useAuth0 } from "@auth0/auth0-react";
import React from "react";
import { Outlet } from "react-router-dom";
import "./App.css";

const App = () => {
  const { loginWithRedirect, logout, isAuthenticated } = useAuth0();
  return (
    <div className="page-wrap">
      <div className="page-header">
        {!isAuthenticated ? (
          <button
            onClick={() =>
              loginWithRedirect({ redirectUri:  `${window.location.origin}/my-events` })
            }
          >
            Login
          </button>
        ) : (
          <button onClick={() => logout({ returnTo: window.location.origin })}>
            Logout
          </button>
        )}
      </div>
      <div className="page-main">
        <Outlet />
      </div>
    </div>
  );
};

export default App;
