import { Elm } from './Main.elm'

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}


const app = Elm.Main.init();

app.ports.copy.subscribe((message) => {
    try {
      navigator.clipboard.writeText(message);
      console.log('Content copied to clipboard');
    } catch (err) {
      console.error('Failed to copy: ', err);
    }
});
