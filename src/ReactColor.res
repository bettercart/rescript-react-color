open RescriptTinycolor
open Belt

type colorChange = {
  "hex": string,
  "hsl": TinyColor.hsl,
  "hsv": TinyColor.hsv,
  "oldHue": float,
  "rgb": TinyColor.rgb,
}

module ChromePicker = {
  @module("react-color") @react.component
  external make: (
    ~color: string,
    ~style: ReactDOM.style=?,
    ~onChange: colorChange => unit,
  ) => React.element = "ChromePicker"
}

type inputChange = {
  r: option<string>,
  g: option<string>,
  b: option<string>,
  a: option<string>,
  hex: option<string>,
  hsla: TinyColor.hsla,
}

module EditableInput = {
  @module("react-color/lib/components/common") @react.component
  external make: (
    ~label: string,
    ~value: string,
    ~style: {"input": ReactDOM.Style.t, "label": ReactDOM.Style.t},
    ~onChange: (inputChange, ReactEvent.Form.t) => unit,
  ) => React.element = "EditableInput"
}

type saturationChange = {
  h: int,
  s: float,
  v: float,
  a: float,
  source: string,
}

module Saturation = {
  @module("react-color/lib/components/common") @react.component
  external make: (
    ~hsl: TinyColor.hsla,
    ~hsv: TinyColor.hsva,
    ~style: ReactDOM.style=?,
    ~onChange: saturationChange => unit,
  ) => React.element = "Saturation"
}

type hueChange = {
  h: int,
  s: float,
  l: float,
  a: float,
  source: string,
}

module Hue = {
  @module("react-color/lib/components/common") @react.component
  external make: (
    ~hsl: TinyColor.hsla,
    ~style: ReactDOM.style=?,
    ~onChange: hueChange => unit,
  ) => React.element = "Hue"
}

module Alpha = {
  @module("react-color/lib/components/common") @react.component
  external make: (
    ~hsl: TinyColor.hsla,
    ~rgb: TinyColor.rgba,
    ~style: ReactDOM.style=?,
    ~onChange: hueChange => unit,
  ) => React.element = "Alpha"
}

module Checkboard = {
  @module("react-color/lib/components/common") @react.component
  external make: (~boxShadow: string, ~borderRadius: string) => React.element = "Checkboard"
}

let buildRgbaString = color => TinyColor.make(color["rgb"])->TinyColor.toRgbString
let buildHexString = color => TinyColor.make(color["rgb"])->TinyColor.toHexString

let isValidHex = hex => {
  let lh = Js.String.startsWith("#", hex) ? 1 : 0

  if Js.String.length(hex) != 4 + lh && Js.String.length(hex) < 7 + lh {
    let color = TinyColor.make(hex)
    TinyColor.isValid(color)
  } else {
    false
  }
}

let removeHash = Js.String.replace("#", "")
let colorToString = c => {
  Js.Float.toString(float_of_int(c))
}

let setRange = (~small, ~big, value) =>
  if value < small {
    small
  } else if value > big {
    big
  } else {
    value
  }

let setAlphaWithRange = setRange(~small=0., ~big=100.)
let setChannelRange = setRange(~small=0, ~big=255)
let divBy100 = v => v /. 100.

let parseChannel = channel => {
  let value = Option.getWithDefault(channel, "0")
  /* value can still be an empty string */
  switch int_of_string_opt(value) {
  | Some(v) => setChannelRange(v)
  | None => 0
  }
}

let parseAlpha = alpha => {
  let value = Option.getWithDefault(alpha, "0")
  /* value can still be an empty string */
  switch float_of_string_opt(value) {
  | Some(v) => setAlphaWithRange(v) |> divBy100
  | None => 0.
  }
}

module SketchFields = {
  module Styles = {
    let input = ReactDOM.Style.make(
      ~width="100%",
      ~padding="4px 10% 3px",
      ~border="none",
      ~boxShadow="inset 0 0 0 1px #4f5d7f" /* mono-600 */,
      ~fontSize="11px",
      ~backgroundColor="transparent",
      ~borderRadius="4px",
      ~color="white",
      (),
    )
    let label = ReactDOM.Style.make(
      ~display="block",
      ~textAlign="center",
      ~fontSize="11px",
      ~color="#d4dfed" /* mono-200 */,
      ~paddingTop="3px",
      ~paddingBottom="4px",
      ~textTransform="capitalize",
      ~userSelect="none",
      (),
    )
  }

  type update =
    | Hex
    | R
    | G
    | B
    | A

  type state = {
    r: int,
    g: int,
    b: int,
    a: float,
    hex: string,
    hsl: TinyColor.hsla,
  }

  @react.component
  let make = (~onChange, ~r, ~g, ~b, ~a, ~hsl, ~hex) => {
    let (value, setValue) = React.useState(_ => {r: r, g: g, b: b, a: a, hsl: hsl, hex: hex})

    let handleChange = (source, data: inputChange, _event) => {
      let (newValue, color) = switch source {
      | Hex => ({...value, hex: Option.getExn(data.hex)}, Some(TinyColor.make(data.hex)))
      | R =>
        let newR = parseChannel(data.r)

        (
          {...value, r: newR},
          TinyColor.makeFromRgba({
            r: newR,
            g: value.g,
            b: value.b,
            a: value.a,
          }),
        )
      | G =>
        let newG = parseChannel(data.g)

        (
          {...value, g: newG},
          TinyColor.makeFromRgba({
            r: value.r,
            g: newG,
            b: value.b,
            a: value.a,
          }),
        )
      | B =>
        let newB = parseChannel(data.b)

        (
          {...value, b: newB},
          TinyColor.makeFromRgba({
            r: value.r,
            g: value.g,
            b: newB,
            a: value.a,
          }),
        )
      | A =>
        let newA = parseAlpha(data.a)
        (
          {...value, hsl: hsl, a: newA},
          TinyColor.makeFromHsla({h: hsl.h, s: hsl.a, l: hsl.l, a: newA}),
        )
      }

      setValue(_ => newValue)
      onChange(color)
    }

    <div className="flex">
      <div className="mr-1" style={ReactDOM.Style.make(~flex="2 2", ())}>
        <EditableInput
          style={"input": Styles.input, "label": Styles.label}
          label="hex"
          value={hex |> removeHash}
          onChange={handleChange(Hex)}
        />
      </div>
      <div className="flex-1 mr-1">
        <EditableInput
          style={"input": Styles.input, "label": Styles.label}
          label="r"
          value={colorToString(r)}
          onChange={handleChange(R)}
        />
      </div>
      <div className="flex-1 mr-1">
        <EditableInput
          style={"input": Styles.input, "label": Styles.label}
          label="g"
          value={colorToString(g)}
          onChange={handleChange(G)}
        />
      </div>
      <div className="flex-1 mr-1">
        <EditableInput
          style={"input": Styles.input, "label": Styles.label}
          label="b"
          value={colorToString(b)}
          onChange={handleChange(B)}
        />
      </div>
      <div className="flex-1">
        <EditableInput
          style={"input": Styles.input, "label": Styles.label}
          label="a"
          value={Js.Float.toString(Js.Math.round(a *. 100.))}
          onChange={handleChange(A)}
        />
      </div>
    </div>
  }
}

type data = {
  r: option<string>,
  g: option<string>,
  b: option<string>,
  a: option<string>,
  hex: option<string>,
}

module Sketch = {
  @react.component
  let make = (~value, ~onChange, ~footer) => {
    let (color, setColor) = React.useState(_ => TinyColor.makeFromString(value))

    switch (
      Option.map(color, TinyColor.toHsl),
      Option.map(color, TinyColor.toHsv),
      Option.map(color, TinyColor.toRgb),
      Option.map(color, TinyColor.toHex),
      color,
    ) {
    | (Some(hsla), Some(hsva), Some(rgba), Some(hex), Some(col)) =>
      <div className="bg-mono-800 w-full h-full rounded border-p">
        <div className="w-full h-40 overflow-hidden relative">
          <Saturation
            hsl={h: hsla.h, s: hsla.s, l: hsla.l, a: hsla.a}
            hsv={h: hsva.h, s: hsva.s, v: hsva.v, a: hsva.a}
            onChange={({h, s, v, a, source: _}) => {
              switch TinyColor.makeFromHsva({h: h, s: s, v: v, a: a}) {
              | Some(c) =>
                onChange(TinyColor.toRgbString(c))
                setColor(_ => Some(c))
              | None => ()
              }
            }}
          />
        </div>
        <div className="flex">
          <div className="py-1 flex-1">
            <div
              className="w-full overflow-hidden relative"
              style={ReactDOM.Style.make(~height="10px", ())}>
              <Hue
                hsl={h: hsla.h, s: hsla.s, l: hsla.l, a: hsla.a}
                onChange={({h, s, l, a, source: _}) => {
                  let newColor = TinyColor.makeFromHsla({h: h, s: s, l: l, a: a})
                  switch newColor {
                  | Some(c) =>
                    onChange(TinyColor.toRgbString(c))
                    setColor(_ => Some(c))
                  | None => ()
                  }
                }}
              />
            </div>
            <div
              className="w-full h-2 pb-1 mt-1 overflow-hidden relative"
              style={ReactDOM.Style.make(~height="10px", ())}>
              <Alpha
                rgb={r: rgba.r, g: rgba.g, b: rgba.b, a: rgba.a}
                hsl={h: hsla.h, s: hsla.s, l: hsla.l, a: hsla.a}
                onChange={({h, s, l, a, source: _}) => {
                  switch TinyColor.makeFromHsla({h: h, s: s, l: l, a: a}) {
                  | Some(c) =>
                    onChange(TinyColor.toRgbString(c))
                    setColor(_ => Some(c))
                  | None => ()
                  }
                }}
              />
            </div>
          </div>
          <div className="w-6 relative m-1 mr-0 rounded">
            <div
              className="absolute h-full inset-0"
              style={ReactDOM.Style.make(
                ~background=TinyColor.toRgbString(col),
                ~borderRadius="2px",
                (),
              )}
            />
            <Checkboard
              boxShadow="rgba(0, 0, 0, 0.15) 0px 0px 0px 1px inset, rgba(0, 0, 0, 0.25) 0px 0px 4px inset"
              borderRadius="2px"
            />
          </div>
        </div>
        <SketchFields
          r={rgba.r}
          g={rgba.g}
          b={rgba.b}
          a={rgba.a}
          hsl=hsla
          hex
          onChange={newColor => {
            setColor(_ => newColor)
            switch newColor {
            | Some(c) => onChange(TinyColor.toRgbString(c))
            | None => ()
            }
          }}
        />
        footer
      </div>
    /*
      This case exist when bs-tinycolor can't parse a valid color,
      it woudn't happen since we ensure to always got a color and
      TinyColor doesn't give back wrong colors.
 */
    | _ => React.null
    }
  }
}
