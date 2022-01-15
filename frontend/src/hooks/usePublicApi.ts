import { useRef } from "react";
import axios from "axios";


const baseUrl = "/api/public/";

const usePublicApi = () => {
  const api = useRef(
    axios.create({
      baseURL: baseUrl,
    })
  );
  return api.current;
};

export default usePublicApi
