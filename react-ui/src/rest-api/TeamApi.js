import {TOKEN} from '../util/SecurityUtils';

export const fetchTeams = () => {
    return fetch('http://localhost:10000/spat/rest-services/api/teams', {
        headers: {'Authorization': TOKEN}
    }).then(response => {
        return response.json();
    });
};