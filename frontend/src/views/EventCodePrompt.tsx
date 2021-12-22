import React, { useState } from "react";
import { useParams, useNavigate } from "react-router-dom";

const EventCodePrompt = () => {
  const [code, setCode] = useState("");
  const params = useParams();
  const navigate = useNavigate()

  const onSubmit = (event: React.SyntheticEvent) => {
    event.preventDefault();
    console.log(`event: ${params.eventId}, code: ${code}`);
    setCode("");
    navigate(code)
  };

  return (
    <form onSubmit={onSubmit}>
      <label>
        Invite code:
        <input
          type="text"
          value={code}
          onChange={(event) => setCode(event.target.value)}
          autoFocus
        />
      </label>
      <input type="submit" value="Submit" />
    </form>
  );
};

export default EventCodePrompt;
