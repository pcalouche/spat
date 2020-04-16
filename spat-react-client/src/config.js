// Other environments and properties could be configured here
const config = {
  'localhost': {
    apiUrl: 'http://localhost:10000'
  }
};

export default config[window.location.hostname] || config['localhost'];