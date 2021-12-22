import React from "react";
import { Navigate, Route, RouteProps, useLocation } from "react-router-dom";

function PrivateRoute({ children }: { children: JSX.Element }) {
    const isLoggedIn = Boolean(localStorage.getItem("jwt"));
    let location = useLocation();
    return !isLoggedIn ? <Navigate to="/login" state={{ from: location }} /> : children;
}

export default PrivateRoute;
