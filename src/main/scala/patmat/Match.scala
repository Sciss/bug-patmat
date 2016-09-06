package patmat

trait Match {
  def requestInput[Res](in: UGB.Input { type Value = Res }): Res = in match {
    case i: UGB.Input.Scalar =>
      UGB.Input.Scalar.Value(-1)

    case i: UGB.Input.Stream =>
      UGB.Input.Stream.Value(1, 44100.0, Nil)

    case i: UGB.Input.Buffer =>
      UGB.Input.Buffer.Value(1L, 2, async = false)

    case i: UGB.Input.AttrValue =>
      UGB.Input.AttrValue.Value(None)

    case i: UGB.Input.BufferOut => UGB.Unit
    case    UGB.Input.StopSelf  => UGB.Unit
    case i: UGB.Input.Action    => UGB.Input.Action .Value
    case i: UGB.Input.DiskOut   => UGB.Input.DiskOut.Value(i.numChannels)

    case _ => throw new IllegalStateException(s"Unsupported input request $in")
  }
}