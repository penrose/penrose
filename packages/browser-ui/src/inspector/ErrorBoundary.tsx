import * as React from "react";
class ErrorBoundary extends React.Component<{ children: JSX.Element }> {
  public readonly state = { hasError: false };

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  public static getDerivedStateFromError(error: never): { hasError: boolean } {
    // Update state so the next render will show the fallback UI.
    return { hasError: true };
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  public componentDidCatch(error: never, errorInfo: never): void {
    // You can also log the error to an error reporting service
  }

  public render(): JSX.Element {
    if (this.state.hasError) {
      // You can render any custom fallback UI
      return <h1>Something went wrong.</h1>;
    }

    return this.props.children;
  }
}
export default ErrorBoundary;
