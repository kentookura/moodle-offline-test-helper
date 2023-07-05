import { Elm } from './Main.elm'

const app = Elm.Main.init();

app.ports.copy.subscribe((message) => {
    try {
      navigator.clipboard.writeText(message);
      console.log('Content copied to clipboard');
    } catch (err) {
      console.error('Failed to copy: ', err);
    }
  console.log(message);
 // document.querySelector('#copy').select();
 // document.execCommand('copy');
});