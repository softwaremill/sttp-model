package sttp

/** Most model classes contain both serialisation & parsing functionality, following these conventions:
  *
  *   - `.toString` returns a representation of the model class in a format as in an HTTP request/response. For example,
  *     for an uri this will be `http://...`, for a header `[name]: [value]`, etc.
  *   - `[SthCompanionObject].parse(serialized: String): Either[String, Sth]`: returns an error message or an instance
  *     of the model class
  *   - `[SthCompanionObject].unsafeParse(serialized: String): Sth`: returns an instance of the model class or in case
  *     of an error, *throws an exception*.
  *   - `[SthCompanionObject].unsafeApply(values)`: creates an instance of the model class; validates the input values
  *     and in case of an error, *throws an exception*. An error could be e.g. that the input values contain characters
  *     outside of the allowed range
  *   - `[SthCompanionObject].safeApply(...): Either[String, Sth]`: same as above, but doesn't throw exceptions.
  *     Instead, returns an error message or the model class instance
  *   - `[SthCompanionObject].apply(...): Sth`: creates the model type, without validation, and without throwing
  *     exceptions
  */
package object model
