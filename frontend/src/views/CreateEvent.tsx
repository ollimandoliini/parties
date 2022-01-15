import { Controller, useForm } from "react-hook-form";
import React from "react";
import useAdminApi from "../hooks/useAdminApi";
import DatePicker from "react-datepicker";
import "react-datepicker/dist/react-datepicker.css";
import "../react-datepicker.css";
import { useNavigate } from "react-router";
import { AxiosResponse } from "axios";


interface NewEvent {
  name: string;
  description: string;
  location: string;
  startTime: Date;
}

interface Id {
  id: number
}

const CreteEvent = () => {
  const {
    handleSubmit,
    register,
    control,
    formState: { errors },
  } = useForm();
  const api = useAdminApi();
  const navigate = useNavigate();

  const onSubmit = async (newEvent: NewEvent) => {
    try {
      const result: AxiosResponse<Id> = await api.post("events", {
        ...newEvent,
        startTime: newEvent.startTime.toISOString(),
      });
      navigate(`/my-events/${result.data.id}`)
    } catch (e) {
      console.log(e);
    }
  };
  return (
    <form onSubmit={handleSubmit(onSubmit)} autoComplete="off">
        <label htmlFor="name">Name</label>
        <input
          id="name"
          type="text"
          {...register("name", {
            required: "This is required",
            minLength: { value: 4, message: "Minimum length should be 4" },
          })}
        />
        <div>
          {errors.name && errors.name.message}
        </div>
        <label htmlFor="description">Description</label>
        <input
          id="description"
          type="text"
          {...register("description", {
            required: "This is required",
            minLength: { value: 4, message: "Minimum length should be 4" },
          })}
        />
        <div>
          {errors.description && errors.description.message}
        </div>
        <label htmlFor="location">Location</label>
        <input
          id="location"
          type="text"
          {...register("location", {
            required: "This is required",
            minLength: { value: 4, message: "Minimum length should be 4" },
          })}
        />
        <div>
          {errors.location && errors.location.message}
        </div>

        <label htmlFor="published-date">Start time</label>
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
      <button type="submit">
        Submit
      </button>
    </form>
  );
};

export default CreteEvent;
