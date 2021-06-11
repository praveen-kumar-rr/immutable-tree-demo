type t = {
  innerWidth: int,
  innerHeight: int,
}
@val external window: t = "window"

let toFn = (f, _) => f

module Hooks = {
  let useFocus = () => {
    let (focus, setFocus) = React.useState(() => false)
    let onFocus = _ => setFocus(_ => true)
    let onBlur = _ => setFocus(_ => false)

    (focus, onFocus, onBlur)
  }

  let useHover = () => {
    let (isHovered, setIsHovered) = React.useState(() => false)
    let onMouseOver = _ => setIsHovered(_ => true)
    let onMouseOut = _ => setIsHovered(_ => false)

    (isHovered, onMouseOver, onMouseOut, setIsHovered)
  }

  let useLog = value => {
    React.useEffect1(() => {
      value->Js.log
      None
    }, [value])
  }
}
