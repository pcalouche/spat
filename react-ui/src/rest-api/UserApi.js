import {baseUrl, handleJsonResponse, jsonHeaders} from './Api';

export const fetchUser = async (username) => {
    const response = await fetch(baseUrl + '/users/' + username, {
        headers: jsonHeaders()
    });

    return handleJsonResponse(response);
};

export const fetchUserByToken = async () => {
    const response = await fetch(baseUrl + '/users/token', {
        headers: jsonHeaders()
    });

    return handleJsonResponse(response);
};

export const fetchUsers = async () => {
    const response = await fetch(baseUrl + '/users', {
        headers: jsonHeaders()
    });
    return handleJsonResponse(response);
};