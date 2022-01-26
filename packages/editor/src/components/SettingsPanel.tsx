import { Dispatcher, ISettings } from "../state/reducer";
import BlueButton from "./BlueButton";

export default function SettingsPanel({
  dispatch,
  settings,
}: {
  dispatch: Dispatcher;
  settings: ISettings;
}) {
  return (
    <div style={{ padding: "1em" }}>
      <h1>settings</h1>
      <div>
        <div>
          {settings.githubUser !== null ? (
            <span>
              signed in as {settings.githubUser.username}{" "}
              <BlueButton
                onClick={() =>
                  dispatch({
                    kind: "CHANGE_SETTINGS",
                    settings: { githubUser: null },
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
              checked={settings.vimMode}
              onChange={(e) =>
                dispatch({
                  kind: "CHANGE_SETTINGS",
                  settings: {
                    vimMode: !settings.vimMode,
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
