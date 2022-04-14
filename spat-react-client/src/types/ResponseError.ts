type ResponseError = Error & {
  status?: number
}

export default ResponseError;