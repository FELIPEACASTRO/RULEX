/**
 * useFocusTrap - Hook para gerenciamento de focus em modais e dialogs
 * 
 * FIX: BUG-003 (Focus management em modal)
 * 
 * Implementa focus trap conforme WCAG 2.1 guidelines:
 * - Foca primeiro elemento focável ao abrir
 * - Mantém focus dentro do modal
 * - Restaura focus ao fechar
 * - Suporta navegação por Tab/Shift+Tab
 * 
 * @version 1.0.0
 */

import { useEffect, useRef } from 'react';

interface UseFocusTrapOptions {
  isOpen: boolean;
  onClose?: () => void;
  initialFocus?: HTMLElement | null;
  restoreFocus?: boolean;
}

const FOCUSABLE_ELEMENTS = [
  'a[href]',
  'button:not([disabled])',
  'textarea:not([disabled])',
  'input:not([disabled])',
  'select:not([disabled])',
  '[tabindex]:not([tabindex="-1"])',
];

export function useFocusTrap({
  isOpen,
  onClose,
  initialFocus,
  restoreFocus = true,
}: UseFocusTrapOptions) {
  const containerRef = useRef<HTMLElement | null>(null);
  const previousFocusRef = useRef<HTMLElement | null>(null);

  useEffect(() => {
    if (!isOpen) return;

    // Salva elemento com focus atual
    previousFocusRef.current = document.activeElement as HTMLElement;

    // Aguarda renderização do modal
    const timer = setTimeout(() => {
      const container = containerRef.current;
      if (!container) return;

      // Foca elemento inicial ou primeiro focável
      const elementToFocus =
        initialFocus ||
        container.querySelector<HTMLElement>(FOCUSABLE_ELEMENTS.join(','));

      if (elementToFocus) {
        elementToFocus.focus();
      }
    }, 0);

    return () => clearTimeout(timer);
  }, [isOpen, initialFocus]);

  useEffect(() => {
    if (!isOpen) {
      // Restaura focus ao fechar
      if (restoreFocus && previousFocusRef.current) {
        previousFocusRef.current.focus();
      }
      return;
    }

    const handleKeyDown = (event: KeyboardEvent) => {
      const container = containerRef.current;
      if (!container) return;

      // ESC para fechar
      if (event.key === 'Escape' && onClose) {
        event.preventDefault();
        onClose();
        return;
      }

      // Tab navigation
      if (event.key === 'Tab') {
        const focusableElements = Array.from(
          container.querySelectorAll<HTMLElement>(FOCUSABLE_ELEMENTS.join(','))
        );

        if (focusableElements.length === 0) return;

        const firstElement = focusableElements[0];
        const lastElement = focusableElements[focusableElements.length - 1];
        const activeElement = document.activeElement as HTMLElement;

        // Shift+Tab no primeiro elemento -> vai para último
        if (event.shiftKey && activeElement === firstElement) {
          event.preventDefault();
          lastElement.focus();
          return;
        }

        // Tab no último elemento -> vai para primeiro
        if (!event.shiftKey && activeElement === lastElement) {
          event.preventDefault();
          firstElement.focus();
          return;
        }
      }
    };

    document.addEventListener('keydown', handleKeyDown);
    return () => document.removeEventListener('keydown', handleKeyDown);
  }, [isOpen, onClose, restoreFocus]);

  return containerRef;
}

/**
 * useFocusReturn - Hook simples para restaurar focus
 */
export function useFocusReturn(isActive: boolean) {
  const previousFocusRef = useRef<HTMLElement | null>(null);

  useEffect(() => {
    if (isActive) {
      previousFocusRef.current = document.activeElement as HTMLElement;
    } else if (previousFocusRef.current) {
      previousFocusRef.current.focus();
    }
  }, [isActive]);
}

/**
 * useAutoFocus - Hook para focar elemento automaticamente
 */
export function useAutoFocus<T extends HTMLElement>(shouldFocus: boolean = true) {
  const ref = useRef<T>(null);

  useEffect(() => {
    if (shouldFocus && ref.current) {
      ref.current.focus();
    }
  }, [shouldFocus]);

  return ref;
}

/**
 * useFocusVisible - Hook para detectar navegação por teclado
 */
export function useFocusVisible() {
  const isFocusVisibleRef = useRef(false);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === 'Tab') {
        isFocusVisibleRef.current = true;
      }
    };

    const handleMouseDown = () => {
      isFocusVisibleRef.current = false;
    };

    document.addEventListener('keydown', handleKeyDown);
    document.addEventListener('mousedown', handleMouseDown);

    return () => {
      document.removeEventListener('keydown', handleKeyDown);
      document.removeEventListener('mousedown', handleMouseDown);
    };
  }, []);

  return isFocusVisibleRef;
}
