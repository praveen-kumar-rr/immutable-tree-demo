let makeStyle = ReactDOM.Style.make
let combineStyle = ReactDOM.Style.combine

module Button = {
  @react.component
  let make = (~children, ~onClick, ~disabled, ~isInput=false) => {
    let (isHovered, onMouseOver, onMouseOut, setIsHovered) = Bindings.Hooks.useHover()
    let buttonStyle =
      isHovered && !disabled
        ? Styles.buttonStyle->combineStyle(makeStyle(~color=Styles.variables.black, ()))
        : Styles.buttonStyle
    let style =
      (
        disabled
          ? buttonStyle->combineStyle(makeStyle(~opacity="0.6", ~cursor="default", ()))
          : buttonStyle
      )->combineStyle(makeStyle(~margin=isInput ? "0 0 30px 0" : "0", ()))

    React.useEffect1(() => {
      if disabled {
        setIsHovered(_ => false)
      }
      None
    }, [disabled])

    <button disabled onMouseOver onMouseOut style onClick> {children} </button>
  }
}

module Input = {
  @react.component
  let make = (~value, ~onChange, ~hint, ~disabled) => {
    let (focus, onFocus, onBlur) = Bindings.Hooks.useFocus()

    <div style={makeStyle(~position="relative", ~marginBottom="30px", ())}>
      <input
        value
        onChange
        onFocus
        onBlur
        disabled
        style={makeStyle(
          ~padding="7px 2px",
          ~minWidth="100px",
          ~borderColor={focus ? "black" : Styles.variables.black},
          ~borderRadius="0",
          (),
        )}
      />
      <p
        style={makeStyle(
          ~position="absolute",
          ~top="32px",
          ~padding="0",
          ~margin="0",
          ~color=Styles.variables.black,
          ~fontSize="12px",
          (),
        )}>
        {hint->React.string}
      </p>
    </div>
  }
}
