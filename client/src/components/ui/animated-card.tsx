/**
 * AnimatedCard - Card com animações e microinterações
 *
 * Adiciona animações suaves de entrada e hover effects
 *
 * @version 1.0.0
 */

import { motion, HTMLMotionProps } from 'framer-motion';
import { cn } from '@/lib/utils';
import { forwardRef } from 'react';

interface AnimatedCardProps extends HTMLMotionProps<'div'> {
  delay?: number;
  hover?: boolean;
}

export const AnimatedCard = forwardRef<HTMLDivElement, AnimatedCardProps>(
  ({ className, delay = 0, hover = true, children, ...props }, ref) => {
    return (
      <motion.div
        ref={ref}
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{
          duration: 0.4,
          delay,
          ease: [0.25, 0.1, 0.25, 1],
        }}
        whileHover={
          hover
            ? {
                y: -4,
                transition: { duration: 0.2 },
              }
            : undefined
        }
        className={cn(
          'rounded-lg border bg-card text-card-foreground shadow-sm transition-shadow',
          hover && 'hover:shadow-md',
          className
        )}
        {...props}
      >
        {children}
      </motion.div>
    );
  }
);

AnimatedCard.displayName = 'AnimatedCard';
