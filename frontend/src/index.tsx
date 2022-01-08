import React from "react";
import ReactDOM from "react-dom";
import "./index.scss";
import App from "./App";
import EventCodePrompt from "./views/EventCodePrompt";
import Event from "./views/Event";
import CreateEvent from "./views/CreateEvent";
import { Route, Routes, BrowserRouter } from "react-router-dom";
import MyEvents from "./views/MyAccount";
import { Auth0Provider } from "@auth0/auth0-react";
import EventAdmin from "./views/EventAdmin";
import NotFound from "./views/NotFound";
import Home from "./views/Home";

ReactDOM.render(
    <Auth0Provider
      domain="dev-7xdjfw10.eu.auth0.com"
      clientId="Bv0t7ip01uXIfvfs3r0vP0KKJWdkZCnL"
      redirectUri={window.location.origin}
      audience="https://dev-7xdjfw10.eu.auth0.com/api/v2/"
      scope="read:current_user update:current_user_metadata"
    >
      <BrowserRouter>
        <Routes>
          <Route path="/" element={<App />}>
            <Route path="/" element={<Home />} />
            <Route path="e/:eventid" element={<EventCodePrompt />} />
            <Route path="e/:eventId/:inviteCode" element={<Event />} />
            <Route path="create-event" element={<CreateEvent />} />
            <Route path="my-events/:eventId" element={<EventAdmin />} />
            <Route path="my-events" element={<MyEvents />} />
            <Route path="*" element={<NotFound />} />
          </Route>
        </Routes>
      </BrowserRouter>
    </Auth0Provider>,
  document.getElementById("root")
);
