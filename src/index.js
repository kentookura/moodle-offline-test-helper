import { Elm } from './Main.elm'

const app = Elm.Main.init();

app.ports.copy.subscribe((message) => {
    try {
      navigator.clipboard.writeText(message);
      console.log('Content copied to clipboard');
    } catch (err) {
      console.error('Failed to copy: ', err);
    }
});

app.ports.notifyEditor.subscribe((message) => {
    try {
      let ev = new CustomEvent('setEditorContent', {bubbles: true, detail: message})
      dispatchEvent(ev)
    } catch (err) {
      console.error('failed to set editor content', err);
    }
});