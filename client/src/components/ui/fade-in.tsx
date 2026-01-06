/**
 * FadeIn - Componente para animações de fade-in
 *
 * Wrapper para adicionar animações de entrada em qualquer elemento
 *
 * @version 1.0.0
 */

import { motion, HTMLMotionProps, Variants } from 'framer-motion';
import { ReactNode } from 'react';

interface FadeInProps extends Omit<HTMLMotionProps<'div'>, 'children'> {
  children: ReactNode;
  delay?: number;
  direction?: 'up' | 'down' | 'left' | 'right' | 'none';
  duration?: number;
  distance?: number;
}

const directionOffset = {
  up: { y: 20 },
  down: { y: -20 },
  left: { x: 20 },
  right: { x: -20 },
  none: {},
};

export function FadeIn({
  children,
  delay = 0,
  direction = 'up',
  duration = 0.5,
  distance = 20,
  ...props
}: FadeInProps) {
  const offset = directionOffset[direction];
  const adjustedOffset =
    direction === 'up' || direction === 'down'
      ? { y: (offset.y || 0) * (distance / 20) }
      : direction === 'left' || direction === 'right'
      ? { x: (offset.x || 0) * (distance / 20) }
      : {};

  const variants: Variants = {
    hidden: {
      opacity: 0,
      ...adjustedOffset,
    },
    visible: {
      opacity: 1,
      x: 0,
      y: 0,
      transition: {
        duration,
        delay,
        ease: [0.25, 0.1, 0.25, 1],
      },
    },
  };

  return (
    <motion.div
      initial="hidden"
      animate="visible"
      variants={variants}
      {...props}
    >
      {children}
    </motion.div>
  );
}
