import { Dispatcher, State } from "../reducer";
import BlueButton from "./BlueButton";

export default function SettingsPanel({
  dispatch,
  state,
}: {
  dispatch: Dispatcher;
  state: State;
}) {
  return (
    <div style={{ padding: "1em" }}>
      <h1>settings</h1>
      <div>
        <div>
          {state.settings.githubUser !== null ? (
            <span>
              signed in as {state.settings.githubUser.username}{" "}
              <BlueButton
                onClick={() =>
                  dispatch({
                    kind: "CHANGE_SETTINGS",
                    settings: { ...state.settings, githubUser: null },
                  })
                }
              >
                sign out
              </BlueButton>
            </span>
          ) : (
            <span>not signed in</span>
          )}
        </div>
        <div>
          <label>
            <input
              type={"checkbox"}
              checked={state.settings.vimMode}
              onChange={(e) =>
                dispatch({
                  kind: "CHANGE_SETTINGS",
                  settings: {
                    ...state.settings,
                    vimMode: !state.settings.vimMode,
                  },
                })
              }
            />
            vim mode
          </label>
        </div>
      </div>
    </div>
  );
}
