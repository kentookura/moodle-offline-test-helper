import { Elm } from './Main.elm'

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}


const app = Elm.Main.init();
