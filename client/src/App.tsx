import { Toaster } from "@/components/ui/sonner";
import { TooltipProvider } from "@/components/ui/tooltip";
import NotFound from "@/pages/NotFound";
import { Route, Switch } from "wouter";
import ErrorBoundary from "./components/ErrorBoundary";
import { ThemeProvider } from "./contexts/ThemeContext";
import DashboardLayout from "./components/DashboardLayout";
import DashboardProfessional from "./pages/DashboardProfessional";
import TransactionsProfessional from "./pages/TransactionsProfessional";
import Rules from "./pages/Rules";
import Audit from "./pages/Audit";
import TransactionSimulator from "./pages/TransactionSimulator";

function Router() {
  return (
    <DashboardLayout>
      <Switch>
        <Route path={"/"} component={DashboardProfessional} />
        <Route path={"/dashboard"} component={DashboardProfessional} />
        <Route path={"/transactions"} component={TransactionsProfessional} />
        <Route path="/rules" component={Rules} />
        <Route path={"/audit"} component={Audit} />
        <Route path={"/simulator"} component={TransactionSimulator} />
        <Route path={"/404"} component={NotFound} />
        <Route component={NotFound} />
      </Switch>
    </DashboardLayout>
  );
}

function App() {
  return (
    <ErrorBoundary>
      <ThemeProvider defaultTheme="light">
        <TooltipProvider>
          <Toaster />
          <Router />
        </TooltipProvider>
      </ThemeProvider>
    </ErrorBoundary>
  );
}

export default App;
