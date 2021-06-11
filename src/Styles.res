let makeStyle = ReactDOM.Style.make
let combineStyle = ReactDOM.Style.combine

type variables = {
  red: string,
  redStart: string,
  redEnd: string,
  blueStart: string,
  blueEnd: string,
  black: string,
  shadow: string,
}
let variables = {
  red: "#DF7878",
  redStart: "#fff5f5",
  redEnd: "#fa9393",
  blueStart: "#f2f7ff",
  blueEnd: "#a6c9ff",
  black: "#5C5C5C",
  shadow: "#ccc",
}

let flexCenter = makeStyle(~display="flex", ~justifyContent="center", ~alignItems="center", ())
let flexCenterColumn = flexCenter->combineStyle(makeStyle(~flexDirection="column", ()))

let buttonStyle = makeStyle(
  ~backgroundColor=variables.red,
  ~padding="10px 20px",
  ~border="none",
  ~fontWeight="bold",
  ~color="black",
  ~boxShadow=`0 0 10px ${variables.shadow}`,
  ~textTransform="uppercase",
  ~cursor="pointer",
  (),
)
