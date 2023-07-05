import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import monacoEditorPlugin from "vite-plugin-monaco-editor";

export default defineConfig({
  //plugins: [elmPlugin(), monacoEditorPlugin({languageWorkers: ['javascript'], entry: })],
  plugins: [elmPlugin(), monacoEditorPlugin({})],
});
