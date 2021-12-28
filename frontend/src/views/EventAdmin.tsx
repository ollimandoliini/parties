import axios, { AxiosResponse } from "axios";
import React, { useEffect, useState } from "react";
import { useParams } from "react-router";
import useApi from "../hooks/useApi";
import { useNavigate } from "react-router-dom";
import { Box, Button, FormControl, FormErrorMessage, FormLabel, Heading, Input, List, ListItem } from "@chakra-ui/react";
import { useForm } from "react-hook-form";

const InviteCard: React.FC<{ invite: IInvite }> = ({ invite }) => {
  return (
    <Box>
      <code>{invite.code}</code>
      <List>
        {invite.invitees.map((invitee, index) => (
          <ListItem key={index}>{invitee}</ListItem>
        ))}
      </List>
    </Box>
  );
};

const InviteForm = () => {
  const {
    handleSubmit,
    register,
    control,
    formState: { errors, isSubmitting },
  } = useForm();
  const [invitees, setInvitees] = useState([])
  const api = useApi();


  // const onAdd = async (newEvent) => {
  //   try {
  //     const result = await api.post("events", {
  //       ...newEvent,
  //       startTime: newEvent.startTime.toISOString(),
  //     });
  //     console.log(result);
  //   } catch (e) {
  //     console.log(e);
  //   }
  // };

  return (
    // <form onSubmit={handleSubmit(onSubmit)} autoComplete="off">
    <form autoComplete="off">
      <FormControl isInvalid={errors.name}>
        <FormLabel htmlFor="name">Name</FormLabel>
        <Input
          id="name"
          type="text"
          label="Name"
          {...register("name", {
            required: "This is required",
            minLength: { value: 4, message: "Minimum length should be 4" },
          })}
        />
        <FormErrorMessage>
          {errors.name && errors.name.message}
        </FormErrorMessage>
      </FormControl>

        <Button type="submit" isLoading={isSubmitting}>
          Submit
        </Button>
    </form>
  );
};

const EventAdmin: React.FC = () => {
  const { eventId } = useParams();
  const api = useApi();
  const [eventData, setEventData] = useState<IEvent | null>(null);
  const [invites, setInvites] = useState<IInvite[]>([]);
  const navigate = useNavigate();

  useEffect(() => {
    (async () => {
      try {
        const event: AxiosResponse<IEvent> = await api.get(`events/${eventId}`);
        setEventData(event.data);
        const invites: AxiosResponse<IInvite[]> = await api.get(
          `events/${eventId}/invites`
        );
        setInvites(invites.data);
      } catch (error: unknown) {
        if (axios.isAxiosError(error) && error.response?.status === 404) {
          navigate("/404");
        } else {
          throw error;
        }
      }
    })();
  }, [api, eventId, navigate]);

  return (
    <Box>
      <Heading as="h1">{eventData?.name}</Heading>
      {invites.map((invite) => (
        <InviteCard invite={invite} />
      ))}
    </Box>
  );
};

export default EventAdmin;
