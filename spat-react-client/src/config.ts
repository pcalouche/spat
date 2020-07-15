// Other environments and properties could be configured here
type Config = {
  apiUrl: string
}

const config = {
  localhost: {
    apiUrl: 'http://localhost:10000'
  }
};

let configToUse: Config;
if (window.location.hostname === 'localhost') {
  configToUse = config.localhost;
} else {
  configToUse = config.localhost;
}

export default configToUse;