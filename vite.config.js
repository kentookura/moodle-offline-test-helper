import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  plugins: [elmPlugin()],
  assetsInclude:['**/*.mp4'],
  base: 'https://kentookura.github.io/moodle-offline-test-helper',

});
