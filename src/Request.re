type name = string;

type request 'slots =
  | Launch
  | SessionEnded
  | Intent name 'slots;
