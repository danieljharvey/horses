import * as E from 'fp-ts/Either'

export type ValidationError =
    | { type: 'EmptyName' }
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
    }
}
