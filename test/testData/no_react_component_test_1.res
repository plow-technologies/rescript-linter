@react.component
let make = () => {
  <div>
  <input value="" onChange={ignore} />
  </div>
}

module Inner = {
  @react.component
  let make = () => {
    <div>
    <input value="" onChange={ignore} />
    </div>
  }
}
