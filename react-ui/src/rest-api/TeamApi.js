import {baseUrl, handleJsonResponse, jsonHeaders} from './Api';

export const fetchTeams = async () => {
    const response = await fetch(baseUrl + '/teams', {
        headers: jsonHeaders()
    });
    return handleJsonResponse(response);
};

