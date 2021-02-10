import * as React from "react";
class ErrorBoundary extends React.Component<{ children: any }> {
  public readonly state = { hasError: false };

  public static getDerivedStateFromError(error: any) {
    // Update state so the next render will show the fallback UI.
    return { hasError: true };
  }

  public componentDidCatch(error: any, errorInfo: any) {
    // You can also log the error to an error reporting service
  }

  public render() {
    if (this.state.hasError) {
      // You can render any custom fallback UI
      return <h1>Something went wrong.</h1>;
    }

    return this.props.children;
  }
}
export default ErrorBoundary;
