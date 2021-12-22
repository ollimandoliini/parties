import React from "react";
import styles from "./Form.module.scss";

const Form: React.FC<{ onSubmit: (e: React.FormEvent) => void }> = ({
  children,
  onSubmit,
}) => {
  return (
    <div className={styles.form}>
      <form onSubmit={onSubmit}>{children}</form>
    </div>
  );
};

export default Form;
