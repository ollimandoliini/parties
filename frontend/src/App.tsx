import { useAuth0 } from "@auth0/auth0-react";
import React from "react";
import { Outlet } from "react-router-dom";
import "./App.css";

const Header = () => {
  const { loginWithRedirect, logout, isAuthenticated, user } = useAuth0();
  return (
    <div className="page-header">
      <a href="/">
        <h1>Invitee.to</h1>
      </a>
      <div className="login-button-container">
        {!isAuthenticated ? (
          <button
            onClick={() =>
              loginWithRedirect({
                redirectUri: `${window.location.origin}/my-events`,
              })
            }
          >
            Login / Signup
          </button>
        ) : (
          <>
            <div>Logged in as: {user?.name}</div>
            <button
              onClick={() => logout({ returnTo: window.location.origin })}
            >
              Logout
            </button>
          </>
        )}
      </div>
    </div>
  );
};

const App = () => {
  return (
    <div className="page-wrap">
      <Header />
      <div className="page-main">
        <Outlet />
      </div>
    </div>
  );
};

export default App;
