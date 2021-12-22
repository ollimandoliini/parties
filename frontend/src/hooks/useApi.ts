import { useRef, useEffect } from "react";
import axios from "axios";

import { useAuth0 } from "@auth0/auth0-react";

const baseUrl = "http://localhost:8080/";

const useApi = () => {
  const { getAccessTokenSilently, loginWithRedirect } = useAuth0();
  const api = useRef(
    axios.create({
      baseURL: baseUrl,
    })
  );
  useEffect(() => {
    if (api.current) {
      const requestInterceptorId = api.current.interceptors.request.use(
        async (config) => {
          const token = await getAccessTokenSilently();
          if (config && config.headers) {
            config.headers.authorization = `Bearer ${token}`;
            config.cancelToken = axios.CancelToken.source().token;
            return config;
          }
        }
      );

      const responseInterceptorId = api.current.interceptors.response.use(
        undefined,
        async (error) => {
          if (error.config && error.response && error.response.status === 401) {
            await loginWithRedirect({
              redirect_uri: window.location.origin,
            });
          }

          return Promise.reject(error);
        }
      );
      return () => {
        api.current.interceptors.request.eject(requestInterceptorId);
        api.current.interceptors.response.eject(responseInterceptorId);
      };
    }
  });
  return api.current;
};

export default useApi
