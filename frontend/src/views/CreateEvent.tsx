import { Controller, useForm } from "react-hook-form";
import {
  Button,
  FormControl,
  FormErrorMessage,
  FormLabel,
  Input,
} from "@chakra-ui/react";
import React from "react";
import useApi from "../hooks/useApi";
import DatePicker, { registerLocale } from "react-datepicker";
import "react-datepicker/dist/react-datepicker.css";
import "../react-datepicker.css";
import gb from "date-fns/locale/en-GB";

registerLocale("gb", gb);

interface NewEvent {
  name: string;
  description: string;
  location: string;
  startTime: Date;
}

const CreteEvent = () => {
  const {
    handleSubmit,
    register,
    control,
    formState: { errors, isSubmitting },
  } = useForm();
  const api = useApi();

  const onSubmit = async (newEvent: NewEvent) => {
    try {
      const result = await api.post("events", {
        ...newEvent,
        startTime: newEvent.startTime.toISOString(),
      });
      console.log(result);
    } catch (e) {
      console.log(e);
    }
  };
  return (
    <form onSubmit={handleSubmit(onSubmit)} autoComplete="off">
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
      <FormControl isInvalid={errors.description}>
        <FormLabel htmlFor="description">Description</FormLabel>
        <Input
          id="description"
          type="text"
          {...register("description", {
            required: "This is required",
            minLength: { value: 4, message: "Minimum length should be 4" },
          })}
        />
        <FormErrorMessage>
          {errors.description && errors.description.message}
        </FormErrorMessage>
      </FormControl>
      <FormControl isInvalid={errors.location}>
        <FormLabel htmlFor="location">Location</FormLabel>
        <Input
          id="location"
          type="text"
          {...register("location", {
            required: "This is required",
            minLength: { value: 4, message: "Minimum length should be 4" },
          })}
        />
        <FormErrorMessage>
          {errors.location && errors.location.message}
        </FormErrorMessage>
      </FormControl>

      <FormControl isInvalid={errors.description}>
        <FormLabel htmlFor="published-date">Start time</FormLabel>
        <Controller
          name="startTime"
          control={control}
          render={({ field }) => (
            <DatePicker
              showTimeSelect
              dateFormat="dd.MM.yyyy HH:mm"
              isClearable={true}
              timeFormat="HH:mm"
              calendarStartDay={1}
              onChange={(date) => field.onChange(date)}
              selected={field.value}
              minDate={new Date()}
            />
          )}
        />
      </FormControl>
      <Button type="submit" isLoading={isSubmitting}>
        Submit
      </Button>
    </form>
  );
};

export default CreteEvent;
