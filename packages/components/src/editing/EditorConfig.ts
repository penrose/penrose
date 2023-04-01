import { editor } from "monaco-editor";
import MonacoEditor, { useMonaco, Monaco } from "@monaco-editor/react";
import { initVimMode, VimMode } from "monaco-vim";
import { MutableRefObject } from "react";
import { StyleHelpResolver } from "./languages/StyleConfig";
// import ShapeProps from "../../../../src/components/ShapeProps.vue";


export interface PenroseCalls {
    /// In vim mode, this is called when the user calls :w
    onWrite?: () => void;
    onChange(value: string): void;
    resampleDiagram: () => Promise<void>;
}


export function addKeybinds(editorRef: React.MutableRefObject<editor.IStandaloneCodeEditor | null>,
    penroseCalls: PenroseCalls,
) {

    const monaco = useMonaco()
    const onWrite = penroseCalls.onWrite
    if (monaco !== null && onWrite !== undefined) {

        editorRef.current?.addCommand(
            monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter,
            onWrite
        );

        editorRef.current?.addCommand(
            monaco.KeyMod.Shift | monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter,
            penroseCalls.resampleDiagram
        );
    }

}


export function addCommands(editorRef: React.MutableRefObject<editor.IStandaloneCodeEditor | null>,
    penroseCalls: PenroseCalls,
) {

    const monaco = useMonaco()
    const onWrite = penroseCalls.onWrite
    if (monaco !== null && onWrite !== undefined) {

        editorRef.current?.addCommand(
            monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter,
            onWrite
        );

        editorRef.current?.addCommand(
            monaco.KeyMod.Shift | monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter,
            penroseCalls.resampleDiagram
        );
    }

}

export function addVimCommands(penroseCalls: PenroseCalls) {
    const onWrite = penroseCalls.onWrite;
    const resample = penroseCalls.resampleDiagram;

    if (onWrite && !VimMode.Vim.SET_WRITE) {
        // HACK to prevent multiple definitions of :w
        VimMode.Vim.SET_WRITE = true;
        VimMode.Vim.defineEx("write", "w", onWrite);
    }

    if (resample && !VimMode.Vim.SET_RESAMPLE) {
        // HACK to prevent multiple definitions of :w
        VimMode.Vim.SET_RESAMPLE = true;
        VimMode.Vim.defineEx("resample", "r", resample);
    }

}