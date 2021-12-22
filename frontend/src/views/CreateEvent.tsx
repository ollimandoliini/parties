import React, { useState } from "react";
import axios from "axios";

const url = "http://localhost:8080/events";

const CreteEvent = () => {
  const [newEvent, setNewEvent] = useState({
    name: "",
    description: "",
  });

  const onSubmit = async (event: React.SyntheticEvent) => {
    event.preventDefault();
    try {
      const token = localStorage.getItem("jwt");
      const result = await axios.post(url, newEvent, {
        headers: { Authorization: `Bearer ${token}` },
      });
      console.log(result)
    } catch (e) {
      console.log(e);
    }

    setNewEvent({ name: "", description: "" });
  };
  return (
    <form onSubmit={onSubmit}>
      <label>Name</label>
      <input
        type="text"
        value={newEvent.name}
        onChange={(event) =>
          setNewEvent({ ...newEvent, name: event.target.value })
        }
      />
      <label>
        Description
        <input
          type="text"
          value={newEvent.description}
          onChange={(event) =>
            setNewEvent({ ...newEvent, description: event.target.value })
          }
        />
      </label>
      <input type="submit" value="Submit" />
    </form>
  );
};

export default CreteEvent;
