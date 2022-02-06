import * as E from 'fp-ts/Either'

type ValidationError =
  | { type: 'EmptyName' }
  | { type: 'InvalidName' }
  | { type: 'AlreadyExists'; existing: string }
  | { type: 'EmptyExpression' }

export const validateBinding = (
  str: string,
  code: string,
  bindings: string[]
): E.Either<ValidationError, string> => {
  const cleanString = str.trim()
  if (cleanString.length < 1) {
    return E.left({ type: 'EmptyName' })
  }
  const firstChar = cleanString.charAt(0)
  if (firstChar !== firstChar.toLowerCase()) {
    return E.left({ type: 'InvalidName' })
  }
  if (bindings.includes(cleanString)) {
    return E.left({
      type: 'AlreadyExists',
      existing: cleanString,
    })
  }
  if (code.length < 1) {
    return E.left({ type: 'EmptyExpression' })
  }
  return E.right(cleanString)
}

export const showError = (err: ValidationError): string => {
  switch (err.type) {
    case 'EmptyName':
      return 'Name must be non-empty'
    case 'AlreadyExists':
      return `${err.existing} already exists!`
    case 'EmptyExpression':
      return 'Expression must not be empty'
    case 'InvalidName':
      return 'Identifier must start with a lower-case letter'
  }
}
