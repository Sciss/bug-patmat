package patmat

object UGB {
  // --------------------------------------------

  /** A pure marker trait to rule out some type errors. */
  trait Key
  /** A scalar value found in the attribute map. */
  final case class AttributeKey(name: String) extends Key
  //  /** A entry in a proc's scan map. */
  //  final case class InputKey    (name: String) extends Key
  //  /** A buffer source found in the attribute map. */
  //  final case class BufferKey(name: String) extends Key

  // final case class NumChannels(value: Int) extends UGB.Value

  /** A pure marker trait to rule out some type errors. */
  trait Value {
    def async: Boolean
  }
  case object Unit extends Value {
    final val async = false
  }
  type Unit = Unit.type

  object Input {
    //    object Scan {
    //      final case class Value(numChannels: Int) extends UGB.Value {
    //        def async = false
    //        override def productPrefix = "Input.Scan.Value"
    //        override def toString = s"$productPrefix(numChannels = $numChannels)"
    //      }
    //    }
    //    final case class Scan(name: String, fixed: Int) extends Input {
    //      type Key    = ScanKey
    //      type Value  = Scan.Value
    //
    //      def key = ScanKey(name)
    //
    //      override def productPrefix = "Input.Scan"
    //    }

    object Stream {
      def EmptySpec = Spec(0.0, 0)

      final case class Spec(maxSpeed: Double, interp: Int) {
        /** Empty indicates that the stream is solely used for information
          * purposes such as `BufChannels`.
          */
        def isEmpty: Boolean = interp == 0

        /** Native indicates that the stream will be transported by the UGen
          * itself, i.e. via `DiskIn` or `VDiskIn`.
          */
        def isNative: Boolean = interp == -1

        override def productPrefix = "Input.Stream.Spec"

        override def toString = f"$productPrefix(maxSpeed = $maxSpeed%1.1f, interp = $interp)"
      }
      final case class Value(numChannels: Int, sampleRate: Double, specs: List[Spec]) extends UGB.Value {
        override def productPrefix = "Input.Stream.Value"
        override def toString = s"$productPrefix(numChannels = $numChannels, spec = ${specs.mkString("[", ",", "]")})"
        def async = false
      }
    }
    final case class Stream(name: String, spec: Stream.Spec) extends Input {
      type Key    = AttributeKey
      type Value  = Stream.Value

      def key = AttributeKey(name)

      override def productPrefix = "Input.Stream"
    }

    object DiskOut {
      final case class Value(numChannels: Int) extends UGB.Value {
        def async = false
        override def productPrefix = "Input.DiskOut.Value"
        override def toString = s"$productPrefix(numChannels = $numChannels)"
      }
    }
    final case class DiskOut(name: String, numChannels: Int) extends Input {
      type Key    = AttributeKey
      type Value  = DiskOut.Value

      def key = AttributeKey(name)

      override def productPrefix = "Input.DiskOut"
    }

    object Scalar {
      final case class Value(numChannels: Int) extends UGB.Value {
        def async = false
        override def productPrefix = "Input.Scalar.Value"
        override def toString = s"$productPrefix(numChannels = $numChannels)"
      }
    }
    /** Specifies access to a scalar attribute as a control signal.
      *
      * @param name                 name (key) of the attribute
      * @param requiredNumChannels  the required number of channels or `-1` if no specific requirement
      * @param defaultNumChannels   the default  number of channels or `-1` if no default provided
      */
    final case class Scalar(name: String, requiredNumChannels: Int, defaultNumChannels: Int) extends Input {
      type Key    = AttributeKey
      type Value  = Scalar.Value

      def key = AttributeKey(name)

      override def productPrefix = "Input.Scalar"
    }

    object AttrValue {
      final case class Value(peer: Option[Any]) extends UGB.Value {
        def async = false
        override def productPrefix = "Input.AttrValue.Value"
      }
    }
    /** Specifies access to a an attribute's value at build time.
      *
      * @param name   name (key) of the attribute
      */
    final case class AttrValue(name: String) extends Input {
      type Key    = AttributeKey
      type Value  = AttrValue.Value

      def key = AttributeKey(name)

      override def productPrefix = "Input.AttrValue"
    }

    object Buffer {
      /** Maximum number of samples (channels multiplied by frames)
        * prepared on-the-fly. If the number of samples exceeds this
        * value, use asynchronous preparation.
        */
      final val AsyncThreshold = 65536

      final case class Value(numFrames: Long, numChannels: Int, async: Boolean) extends UGB.Value {
        override def productPrefix = "Input.Buffer.Value"
        override def toString = s"$productPrefix(numFrames = $numFrames, numChannels = $numChannels, async = $async)"
      }
    }
    /** Specifies access to a random access buffer.
      *
      * @param name         name (key) of the attribute referring to an object that
      *                     can be buffered (e.g. audio grapheme)
      */
    final case class Buffer(name: String) extends Input {
      type Key    = AttributeKey
      type Value  = Buffer.Value

      def key = AttributeKey(name)

      override def productPrefix = "Input.Buffer"
    }

    /** Specifies access to an empty buffer that will be
      * written to disk when the encompassing graph finishes.
      */
    final case class BufferOut(artifact: String, action: String, numFrames: Int, numChannels: Int)
      extends Input with Key {

      type Key    = BufferOut
      type Value  = Unit

      def key = this

      override def productPrefix = "Input.BufferOut"
    }

    object Action {
      case object Value extends UGB.Value {
        def async = false
        override def productPrefix = "Input.Action.Value"
      }
    }
    /** Specifies access to an action.
      *
      * @param name   name (key) of the attribute referring to an action
      */
    final case class Action(name: String) extends Input {
      type Key    = AttributeKey
      type Value  = Action.Value.type

      def key = AttributeKey(name)

      override def productPrefix = "Input.Action"
    }

    case object StopSelf extends Input with Key {
      type Key    = StopSelf.type
      type Value  = Unit

      def key = this

      override def productPrefix = "Input.StopSelf"
    }
  }
  trait Input {
    type Key   <: UGB.Key
    type Value <: UGB.Value

    def key: Key
  }
}