import { Toaster } from "@/components/ui/sonner";
import { TooltipProvider } from "@/components/ui/tooltip";
import NotFound from "@/pages/NotFound";
import { Route, Switch } from "wouter";
import ErrorBoundary from "./components/ErrorBoundary";
import { ThemeProvider } from "./contexts/ThemeContext";
import DashboardProfessional from "./pages/DashboardProfessional";
import TransactionsProfessional from "./pages/TransactionsProfessional";
import RulesAdvanced from "./pages/RulesAdvanced";
import Audit from "./pages/Audit";

function Router() {
  return (
    <Switch>
      <Route path={"/"} component={DashboardProfessional} />
      <Route path={"/dashboard"} component={DashboardProfessional} />
      <Route path={"/transactions"} component={TransactionsProfessional} />
      <Route path={"/rules"} component={RulesAdvanced} />
      <Route path={"/audit"} component={Audit} />
      <Route path={"/404"} component={NotFound} />
      {/* Final fallback route */}
      <Route component={NotFound} />
    </Switch>
  );
}

function App() {
  return (
    <ErrorBoundary>
      <ThemeProvider
        defaultTheme="light"
      >
        <TooltipProvider>
          <Toaster />
          <Router />
        </TooltipProvider>
      </ThemeProvider>
    </ErrorBoundary>
  );
}

export default App;
