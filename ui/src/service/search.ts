import axios from 'axios'
import { Either, left, right } from 'fp-ts/lib/Either'
import { TypeSearchRequest, TypeSearchResponse } from '../types/'

// search-based API calls

const baseUrl = process.env.REACT_APP_MIMSA_API_URL

export const typeSearch = (
    typeSearchRequest: TypeSearchRequest
): Promise<Either<string, TypeSearchResponse>> =>
    axios
        .post(`${baseUrl}/search/type/`, typeSearchRequest)
        .then(a => a.data)
        .then(right)
        .catch(e => Promise.resolve(left(e.response.data)))
