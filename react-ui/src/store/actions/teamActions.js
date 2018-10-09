import * as teamApi from '../../rest-api/TeamApi';

export const LOAD_TEAMS = 'LOAD_TEAMS';
export const ADD_TEAM = 'ADD_TEAM';
export const EDIT_TEAM = 'EDIT_TEAM';
export const DELETE_TEAM = 'DELETE_TEAM';

// const fetchTeams = () => {
//     // return fetch('http://localhost:10000/spat/rest-services/api/teams', {
//     //     headers: {'Authorization': TOKEN}
//     // }).then(results => {
//     //     return results.json();
//     // }).then(data => {
//     //     this.setState({teams: data});
//     // }).catch(error => {
//     //     console.error(error);
//     // });
//
//     // return fetch('http://localhost:10000/spat/rest-services/api/teams', {
//     //     headers: {'Authorization': TOKEN}
//     // }).then(results => {
//     //     console.info(results.json());
//     //     return results.json();
//     // }).catch(error => {
//     //     console.error(error);
//     // });
//     // return fetch('http://localhost:10000/spat/rest-services/api/team5s', {
//     //     headers: {'Authorization': TOKEN}
//     // }).then(response => {
//     //     return response.json();
//     // }).catch(error => {
//     //     return error;
//     // });
//
//     return fetch('http://localhost:10000/spat/rest-services/api/teams', {
//         headers: {'Authorization': TOKEN}
//     }).then(response => {
//         return response.json();
//     });
// };

export const showTeams = (list) => {
    return {
        type: LOAD_TEAMS,
        list: list
    };
};

export const loadTeams = () => {
    return dispatch => {
        // return getTeams()
        //     .then(results => results.json()).then(data => dispatch(showTeams(data)))
        //     .catch(error => {
        //         console.info('here');
        //         dispatch(showTeams([]));
        //     });

        // return getTeams()
        //     .then(response => response.json().then(teams => dispatch({type: LOAD_TEAMS, list: teams})),
        //         // error => dispatch(([{id: 1, name: 'Team Error'}]))
        //         error => dispatch({type: LOAD_TEAMS, list: []})
        //     );


        // return getTeams()
        //     .then(response => response.json().then(teams => dispatch({type: LOAD_TEAMS, list: teams})).catch(error => dispatch({type: LOAD_TEAMS, list: []}))
        //     );

        return teamApi.fetchTeams().then(
            teams => dispatch({type: LOAD_TEAMS, list: teams}),
            error => {
                console.info('here');
                throw error;
                // dispatch({type: LOAD_TEAMS, list: [{id: 1, name: 'Error team'}]});
            }
        ).catch(error => {
            console.info(error);
            dispatch({type: LOAD_TEAMS, list: [{id: 1, name: 'Error team'}]});
        });

        // return fetch('http://localhost:10000/spat/rest-services/api/teams2', {
        //     headers: {'Authorization': TOKEN}
        // }).then(response => {
        //         return response.json();
        //     },
        //     error => {
        //         console.info('error 1');
        //         dispatch({type: LOAD_TEAMS, list: [{id: 1, name: 'Error team'}]});
        //     }
        // ).then(teams => {
        //         dispatch({type: LOAD_TEAMS, list: teams});
        //     },
        //     error => {
        //         console.info('error 2');
        //         dispatch({type: LOAD_TEAMS, list: [{id: 1, name: 'Error team'}]});
        //     }
        // ).catch(error => {
        //     dispatch({type: LOAD_TEAMS, list: [{id: 1, name: 'Error team'}]});
        // });

        // return fetch('http://localhost:10000/spat/rest-services/api/teams2', {
        //     headers: {'Authorization': TOKEN}
        // }).then(response => {
        //         return response.json();
        //     },
        //     error => {
        //         console.error('error 1');
        //         dispatch({type: LOAD_TEAMS, list: [{id: 1, name: 'Error team'}]});
        //     }
        // );

        // return getTeams().then(
        //     teams => dispatch({type: LOAD_TEAMS, list: teams})
        // ).catch(error => dispatch({type: LOAD_TEAMS, list: [{id: 1, name: 'Error team'}]}));
    };
    // return dispatch => {
    //     setTimeout(() => {
    //         dispatch((() => {
    //             return {
    //                 type: LOAD_TEAMS,
    //                 list: [{id: 3, name: 'Team 3'}]
    //             };
    //         })());
    //         dispatch(showTeams([{id: 3, name: 'Team 3'}]));
    //     }, 1500);
    //
    //
    // };
};