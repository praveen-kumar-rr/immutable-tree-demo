%%raw(`import "./App.css"`)

let makeStyle = ReactDOM.Style.make
let combineStyle = ReactDOM.Style.combine
module Button = Components.Button
module Input = Components.Input

let arrowId = "arrow_id"
let gradientId = "node_color"
let highlightGradientId = "node_highlight_color"

let setClearTimeout = (f, v) => {
  let timeoutId = Js.Global.setTimeout(f, v)
  () => timeoutId->Js.Global.clearTimeout
}

module IntTree = ImmutableTree.Make({
  type t = int
  let compare = compare
})

type state = {
  tree: IntTree.t,
  searched: array<int>,
  insertProgress: bool,
  deleteProgress: bool,
  traversed: array<int>,
  previousTree: list<IntTree.t>,
  nextTree: list<IntTree.t>,
}

type action =
  | Insert(int)
  | Delete(int)
  | Search(int)
  | ClearSearch
  | AutoInsert
  | AutoDelete
  | RangeSearch(int, int)
  | Traverse(int)
  | ClearTraverse
  | Max(int)
  | Min(int)
  | Prev
  | Next

let state = {
  tree: IntTree.empty(),
  searched: [],
  insertProgress: false,
  deleteProgress: false,
  traversed: [],
  previousTree: list{},
  nextTree: list{},
}

let isDisabled = ({insertProgress, deleteProgress, traversed}) =>
  insertProgress || deleteProgress || traversed != []

let updatePreviousTree = state => {...state, previousTree: list{state.tree, ...state.previousTree}}
let updateNextTree = state => {...state, nextTree: list{state.tree, ...state.nextTree}}
let clearNextTree = state => {...state, nextTree: list{}}
let insertToTree = (state, value) => {
  ...state,
  tree: state.tree->IntTree.insert(value),
}
let insertProgress = (state, insertProgress) => {...state, insertProgress: insertProgress}
let deleteProgress = (state, deleteProgress) => {...state, deleteProgress: deleteProgress}
let deleteFromTree = (state, value) => {
  ...state,
  tree: state.tree->IntTree.deleteNode(value),
}

let reducer = (state, action) =>
  switch action {
  | AutoInsert =>
    let nextValue = state.tree->IntTree.getMax->Belt.Option.getWithDefault(-1) + 1
    nextValue <= 14
      ? state->updatePreviousTree->insertToTree(nextValue)->clearNextTree->insertProgress(true)
      : state->insertProgress(false)
  | AutoDelete =>
    let nextValue = state.tree->IntTree.getMax
    switch nextValue {
    | None => state->deleteProgress(false)
    | Some(d) => state->updatePreviousTree->deleteFromTree(d)->clearNextTree->deleteProgress(true)
    }
  | Insert(value) => state->updatePreviousTree->insertToTree(value)->clearNextTree
  | Delete(value) => state->updatePreviousTree->deleteFromTree(value)->clearNextTree
  | Search(value) => {
      ...state,
      searched: state.tree
      ->IntTree.search(value)
      ->Belt.Option.map(v => [v])
      ->Belt.Option.getWithDefault([]),
    }
  | ClearSearch => {...state, searched: []}
  | RangeSearch(start, end) => {...state, searched: state.tree->IntTree.searchRange(start, end)}
  | Traverse(value) => {...state, traversed: state.traversed->Js.Array2.concat([value])}
  | ClearTraverse => {...state, traversed: []}
  | Max(value) | Min(value) => {...state, searched: [value]}
  | Prev => {
      ...state,
      tree: state.previousTree->Belt.List.head->Belt.Option.getWithDefault(state.tree),
      previousTree: state.previousTree->Belt.List.tail->Belt.Option.getWithDefault(list{}),
      nextTree: list{state.tree, ...state.nextTree},
    }
  | Next => {
      ...state,
      tree: state.nextTree->Belt.List.head->Belt.Option.getWithDefault(state.tree),
      nextTree: state.nextTree->Belt.List.tail->Belt.Option.getWithDefault(list{}),
      previousTree: list{state.tree, ...state.previousTree},
    }
  }

module NodeComponent = {
  @react.component
  let make = (~text: string, ~x: int, ~y: int, ~highlight: bool) => {
    open Belt
    let xpx = `${x->Int.toString}px`
    let ypx = `${y->Int.toString}px`
    let fillId = highlight ? highlightGradientId : gradientId
    <svg width="50px" height="50px" x={xpx} y={ypx}>
      <circle
        cx={"25px"} cy={"25px"} r={"20px"} fill={`url(#${fillId})`} stroke={Styles.variables.black}
      />
      <text
        x="50%"
        y="50%"
        dominantBaseline="middle"
        textAnchor="middle"
        fontSize="13px"
        stroke={Styles.variables.black}
        strokeWidth="1px">
        {text->React.string}
      </text>
    </svg>
  }
}

type treePosition = Left | Right | Root

module RadialGradient = {
  @react.component
  let make = () =>
    <radialGradient id={gradientId} x1="0%" y1="0%" x2="0%" y2="100%" cx="50%" cy="10%">
      <stop
        offset="0%" style={makeStyle(~stopColor={Styles.variables.blueStart}, ~stopOpacity="1", ())}
      />
      <stop
        offset="100%" style={makeStyle(~stopColor={Styles.variables.blueEnd}, ~stopOpacity="1", ())}
      />
    </radialGradient>
}

module RadialGradient2 = {
  @react.component
  let make = () =>
    <radialGradient id={highlightGradientId} x1="0%" y1="0%" x2="0%" y2="100%" cx="50%" cy="10%">
      <stop
        offset="0%" style={makeStyle(~stopColor={Styles.variables.redStart}, ~stopOpacity="1", ())}
      />
      <stop
        offset="100%" style={makeStyle(~stopColor={Styles.variables.redEnd}, ~stopOpacity="1", ())}
      />
    </radialGradient>
}

module Marker = {
  @react.component
  let make = () =>
    <svg>
      <defs>
        <marker
          id={arrowId}
          viewBox={"0 0 20 20"}
          refX="10"
          refY="10"
          markerWidth="7"
          markerHeight="7"
          orient="auto-start-reverse">
          <path d="M0 0 L0 20 L20 10" fill={Styles.variables.black} stroke="black" />
        </marker>
      </defs>
    </svg>
}

module Arrow = {
  @react.component
  let make = (~startX: int, ~endX: int, ~startY: int, ~endY: int) => {
    let s = Belt.Int.toString
    let path = `M ${startX->s} ${startY->s} L ${endX->s} ${endY->s}`
    <g>
      <path
        fill="none"
        strokeWidth="1px"
        stroke={Styles.variables.black}
        d={path}
        markerStart={`url(#${arrowId})`}
      />
    </g>
  }
}

module TreeComponent = {
  let tupleVal2 = ((_, x)) => x
  let rec makeTreeComponent = (tree, position, level, x, searched, traversed) =>
    switch tree {
    | None => (x, React.null)
    | Some(t) =>
      switch t->IntTree.getData {
      | None => (x, React.null)
      | Some(d) => {
          let highlight =
            searched->Belt.Array.some(s => s == d) || traversed->Belt.Array.some(s => s == d)
          let (leftX, leftComp) =
            t->IntTree.getLeft->makeTreeComponent(Left, level + 1, x, searched, traversed)
          let current =
            <NodeComponent text={d->Belt.Int.toString} x={leftX} y={level * 70} highlight />
          let (rightX, rightComp) =
            t
            ->IntTree.getRight
            ->makeTreeComponent(Right, level + 1, leftX + 50, searched, traversed)
          (
            rightX,
            <g>
              {leftComp}
              {current}
              {rightComp}
              {switch position {
              | Left =>
                <Arrow
                  startX={leftX + 35}
                  startY={level * 70 + 5}
                  endX={rightX + 20}
                  endY={level * 70 - 35}
                />
              | Right =>
                <Arrow
                  startX={leftX + 20} startY={level * 70 + 3} endX={x - 10} endY={level * 70 - 31}
                />
              | Root => React.null
              }}
            </g>,
          )
        }
      }
    }

  @react.component
  let make = (
    ~tree: option<IntTree.t>,
    ~position: treePosition,
    ~level: int,
    ~searched: array<int>,
    ~traversed: array<int>,
  ) => tree->makeTreeComponent(position, level, 0, searched, traversed)->tupleVal2
}

let useContinuosInsertion = (dispatch, state) => {
  React.useEffect2(() => {
    state.insertProgress ? setClearTimeout(() => AutoInsert->dispatch, 300)->Some : None
  }, (dispatch, state))
}

let useContinuosDeletion = (dispatch, state) => {
  React.useEffect2(() => {
    state.deleteProgress ? setClearTimeout(() => AutoDelete->dispatch, 300)->Some : None
  }, (dispatch, state))
}

module AutoInsertDelete = {
  @react.component
  let make = (~dispatch, ~state) => {
    let disabled = state->isDisabled
    let autoInsertDisabled = disabled || state.tree != IntTree.empty()
    let autoDeleteDisabled = disabled || state.tree == IntTree.empty()

    useContinuosInsertion(dispatch, state)
    useContinuosDeletion(dispatch, state)

    <div style={Styles.flexCenter}>
      <div>
        <Button disabled={autoInsertDisabled} onClick={_ => AutoInsert->dispatch}>
          {"Insert 0 to 14"->React.string}
        </Button>
      </div>
      <div style={makeStyle(~margin="0 0 0 1px", ())}>
        <Button disabled={autoDeleteDisabled} onClick={_ => AutoDelete->dispatch}>
          {"Delete all"->React.string}
        </Button>
      </div>
    </div>
  }
}

let useNumberInput = () => {
  let (value, setValue) = React.useState(() => 0)
  let onChange = e =>
    ReactEvent.Form.target(e)["value"]
    ->Belt.Int.fromString
    ->Belt.Option.getWithDefault(0)
    ->Bindings.toFn
    ->setValue
  (value, onChange)
}

module TraversalComponent = {
  @react.component
  let make = (~dispatch, ~state) => {
    let disabled = state->isDisabled
    let traverse = (traverseFn, ()) => {
      let i = ref(0)
      let _ = state.tree->traverseFn((fns, a) => {
        i := i.contents + 1
        list{setClearTimeout(() => dispatch(Traverse(a)), 800 * i.contents), ...fns}
      }, list{})

      let _ = setClearTimeout(() => ClearTraverse->dispatch, 1000 * i.contents)
    }
    let preOrderTraverse = IntTree.traversePreOrder->traverse
    let postOrderTraverse = IntTree.traversePostOrder->traverse
    let inOrderTraverse = IntTree.traverseInOrder->traverse

    <div style={Styles.flexCenter}>
      <div>
        <Button disabled onClick={_ => preOrderTraverse()} isInput={true}>
          {"Pre Order"->React.string}
        </Button>
      </div>
      <div style={makeStyle(~margin="0 0 0 1px", ())}>
        <Button disabled onClick={_ => postOrderTraverse()} isInput={true}>
          {"Post Order"->React.string}
        </Button>
      </div>
      <div style={makeStyle(~margin="0 0 0 1px", ())}>
        <Button disabled onClick={_ => inOrderTraverse()} isInput={true}>
          {"In Order"->React.string}
        </Button>
      </div>
    </div>
  }
}

module UndoRedoComponent = {
  @react.component
  let make = (~dispatch, ~state) => {
    let disabled = state->isDisabled
    let isPrevDisabled = disabled || state.previousTree == list{}
    let isNextDisabled = disabled || state.nextTree == list{}

    <div style={Styles.flexCenter}>
      <div>
        <Button disabled={isPrevDisabled} onClick={_ => Prev->dispatch}>
          {"Undo"->React.string}
        </Button>
      </div>
      <div style={makeStyle(~margin="0 0 0 1px", ())}>
        <Button disabled={isNextDisabled} onClick={_ => Next->dispatch}>
          {"Redo"->React.string}
        </Button>
      </div>
    </div>
  }
}

module RangeSearchComponent = {
  @react.component
  let make = (~dispatch, ~state) => {
    let disabled = state->isDisabled
    let (startValue, onStartChange) = useNumberInput()
    let (endValue, onEndChange) = useNumberInput()

    <div style={Styles.flexCenter->combineStyle(makeStyle(~flexWrap="wrap", ()))}>
      <div style={Styles.flexCenter}>
        <div>
          <Input
            disabled
            value={startValue->Belt.Int.toString}
            onChange={onStartChange}
            hint="Range Start"
          />
        </div>
        <div style={makeStyle(~margin="0 0 0 1px", ())}>
          <Input
            disabled value={endValue->Belt.Int.toString} onChange={onEndChange} hint="Range End"
          />
        </div>
      </div>
      <div style={Styles.flexCenter}>
        <div style={makeStyle(~margin="0 0 0 1px", ())}>
          <Button
            disabled onClick={_ => RangeSearch(startValue, endValue)->dispatch} isInput={true}>
            {"Search Range"->React.string}
          </Button>
        </div>
      </div>
    </div>
  }
}

module InsertDeleteSearch = {
  @react.component
  let make = (~dispatch, ~state) => {
    let disabled = state->isDisabled
    let restrict = v => v > 999 ? 999 : v < 0 ? 0 : v
    let (value, setValue) = React.useState(() => 0)

    let onChange = e =>
      ReactEvent.Form.target(e)["value"]
      ->Belt.Int.fromString
      ->Belt.Option.getWithDefault(0)
      ->restrict
      ->Bindings.toFn
      ->setValue

    <div style={Styles.flexCenter->combineStyle(makeStyle(~flexWrap="wrap", ()))}>
      <div>
        <Input
          disabled value={value->Belt.Int.toString} onChange hint="Enter a value between 0 to 999"
        />
      </div>
      <div style={Styles.flexCenter->combineStyle(makeStyle(~margin="0 0 0 1px", ()))}>
        <div>
          <Button disabled onClick={_ => value->Insert->dispatch} isInput={true}>
            {"Insert"->React.string}
          </Button>
        </div>
        <div style={makeStyle(~margin="0 0 0 1px", ())}>
          <Button disabled onClick={_ => value->Delete->dispatch} isInput={true}>
            {"Delete"->React.string}
          </Button>
        </div>
        <div style={makeStyle(~margin="0 0 0 1px", ())}>
          <Button disabled onClick={_ => value->Search->dispatch} isInput={true}>
            {"Search"->React.string}
          </Button>
        </div>
      </div>
    </div>
  }
}

module MaxMin = {
  @react.component
  let make = (~dispatch, ~state) => {
    let disabled = state->isDisabled
    let onMax = _ =>
      switch state.tree->IntTree.getMax {
      | None => ()
      | Some(v) => Max(v)->dispatch->ignore
      }
    let onMin = _ =>
      switch state.tree->IntTree.getMin {
      | None => ()
      | Some(v) => Min(v)->dispatch->ignore
      }

    <div style={Styles.flexCenter}>
      <div>
        <Button disabled onClick={_ => onMax()} isInput={true}> {"Max"->React.string} </Button>
      </div>
      <div style={makeStyle(~margin="0 0 0 1px", ())}>
        <Button disabled onClick={_ => onMin()} isInput={true}> {"Min"->React.string} </Button>
      </div>
    </div>
  }
}

module Message = {
  @react.component
  let make = (~msg) =>
    <div>
      <h3
        style={makeStyle(
          ~color=Styles.variables.shadow,
          ~textAlign="center",
          ~whiteSpace="pre-wrap",
          (),
        )}>
        {msg->React.string}
      </h3>
    </div>
}

let useSearchAutoClear = (state, dispatch) => React.useEffect2(() => {
    state.searched != [] ? setClearTimeout(() => ClearSearch->dispatch, 2 * 1000)->Some : None
  }, (state, dispatch))

let makeH3 = (~children, ~style) => <h3 style> {children} </h3>
let makeH4 = (~children, ~style) => <h4 style> {children} </h4>

module HeaderByWidth = {
  @react.component
  let make = (~children, ~style) => {
    let comp = Bindings.window.innerWidth <= 450 ? makeH4 : makeH3
    comp(~children, ~style)
  }
}

module Brand = {
  @react.component
  let make = () => {
    <HeaderByWidth
      style={makeStyle(~margin="0 0 4px 0", ~padding="0", ~color=Styles.variables.black, ())}>
      <a href="/" style={makeStyle(~textDecoration="none", ~color=Styles.variables.black, ())}>
        {"@rpkumar/immutable-tree"->React.string}
      </a>
    </HeaderByWidth>
  }
}

module NavBar = {
  @react.component
  let make = () =>
    <div
      style={makeStyle(
        ~display="flex",
        ~justifyContent="space-between",
        ~alignItems="center",
        ~padding="9px 35px 9px 15px",
        ~margin="0 0 20px 0",
        ~boxShadow=`0 4px 10px ${Styles.variables.shadow}`,
        (),
      )}>
      <div> <Brand /> </div>
      <div style={Styles.flexCenter}>
        <div>
          <a href="https://www.npmjs.com/package/@rpkumar/immutable-tree" target="_blank">
            <img src="/img/npm.svg" width="40px" alt="npm" />
          </a>
        </div>
        <div style={makeStyle(~margin="0 0 0 20px", ())}>
          <a href="https://github.com/praveen-kumar-rr/immutable-tree#readme" target="_blank">
            <img src="https://github.com/favicon.ico" width="22px" alt="github" />
          </a>
        </div>
      </div>
    </div>
}

@react.component
let make = () => {
  let toFloat = Belt.Int.toFloat
  let (state, dispatch) = React.useReducer(reducer, state)
  let calcWidth = 72. *. (state.tree->IntTree.getLength->toFloat *. 0.7)
  let height = 140. *. (state.tree->IntTree.getHeight->toFloat *. 0.7)
  let heightPx = height < 120. ? "120px" : `${height->Belt.Float.toString}px`
  let widthPx = `${calcWidth->Belt.Float.toString}px`
  let searched = state.searched
  let traversed = state.traversed
  let showMessage = state.tree == IntTree.empty()

  useSearchAutoClear(state, dispatch)

  <div>
    <NavBar />
    <div style={Styles.flexCenter->combineStyle(makeStyle(~flexWrap="wrap", ()))}>
      <div style={makeStyle(~margin="4px 10px", ())}> <InsertDeleteSearch dispatch state /> </div>
      <div style={makeStyle(~margin="4px 10px", ())}> <RangeSearchComponent dispatch state /> </div>
      <div style={makeStyle(~margin="4px 10px", ())}> <TraversalComponent dispatch state /> </div>
      <div style={makeStyle(~margin="4px 10px", ())}> <MaxMin dispatch state /> </div>
    </div>
    <div style={Styles.flexCenter->combineStyle(makeStyle(~flexWrap="wrap", ()))}>
      <div style={makeStyle(~margin="4px 10px", ())}> <AutoInsertDelete dispatch state /> </div>
      <div style={makeStyle(~margin="4px 10px", ())}> <UndoRedoComponent dispatch state /> </div>
    </div>
    {showMessage
      ? <div style={Styles.flexCenter->combineStyle(makeStyle(~margin="40px 0 0 0", ()))}>
          <Message msg="Start by inserting or click on \n \"INSERT 0 TO 14\"" />
        </div>
      : <div
          style={makeStyle(
            ~width="100%",
            ~height="500px",
            ~margin="10px 0 0 0",
            ~overflow="scroll",
            (),
          )}>
          <div style={makeStyle(~width=widthPx, ~height=heightPx, ~margin="auto", ())}>
            <svg width={widthPx} height={heightPx}>
              <Marker />
              <RadialGradient />
              <RadialGradient2 />
              <TreeComponent tree={state.tree->Some} position={Root} level={1} searched traversed />
            </svg>
          </div>
        </div>}
  </div>
}
