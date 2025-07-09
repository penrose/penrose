import { describe, it, expect } from 'vitest';
import { findOptimalMatching, getExampleNamesAndTrios, compileTrio } from "../src/utils.js";

describe('findOptimalMatching', () => {
  // Helper function to calculate total cost of a matching
  const calculateTotalCost = <T>(
    matching: [T, T][],
    metric: (a: T, b: T) => number
  ): number => {
    return matching.reduce((total, [a, b]) => total + metric(a, b), 0);
  };

  // Helper function for Euclidean distance between 2D points
  const euclideanDistance = (p1: [number, number], p2: [number, number]): number => {
    const dx = p1[0] - p2[0];
    const dy = p1[1] - p2[1];
    return Math.sqrt(dx * dx + dy * dy);
  };

  describe('edge cases', () => {
    it('should handle empty sets', () => {
      const setA = new Set<number>();
      const setB = new Set<number>();
      const result = findOptimalMatching(setA, setB, (a, b) => Math.abs(a - b));
      expect(result).toEqual([]);
    });

    it('should handle single element sets', () => {
      const setA = new Set([1]);
      const setB = new Set([2]);
      const result = findOptimalMatching(setA, setB, (a, b) => Math.abs(a - b));
      expect(result).toEqual([[1, 2]]);
      expect(result).toHaveLength(1);
    });

    it('should handle one empty set', () => {
      const setA = new Set([1, 2]);
      const setB = new Set<number>();
      const result = findOptimalMatching(setA, setB, (a, b) => Math.abs(a - b));
      expect(result).toEqual([]);
    });
  });

  describe('equal size sets', () => {
    it('should find optimal matching for simple case', () => {
      const setA = new Set([1, 2]);
      const setB = new Set([3, 4]);
      const result = findOptimalMatching(setA, setB, (a, b) => Math.abs(a - b));
      
      expect(result).toHaveLength(2);
      // Should match 1->3, 2->4 (total cost: 2+2=4) rather than 1->4, 2->3 (total cost: 3+1=4)
      // Both are optimal in this case, but algorithm should be deterministic
      const totalCost = calculateTotalCost(result, (a, b) => Math.abs(a - b));
      expect(totalCost).toBe(4);
    });

    it('should find optimal matching where greedy fails', () => {
      const setA = new Set([1, 2, 3]);
      const setB = new Set([2, 3, 4]);
      const result = findOptimalMatching(setA, setB, (a, b) => Math.abs(a - b));
      
      expect(result).toHaveLength(3);
      // Optimal should be 1->2, 2->3, 3->4 (total cost: 1+1+1=3)
      // Greedy might choose 1->2, 2->2, 3->3 which would be suboptimal
      const totalCost = calculateTotalCost(result, (a, b) => Math.abs(a - b));
      expect(totalCost).toBe(3);
    });

    it('should handle identical elements', () => {
      const setA = new Set([1, 2, 3]);
      const setB = new Set([1, 2, 3]);
      const result = findOptimalMatching(setA, setB, (a, b) => Math.abs(a - b));
      
      expect(result).toHaveLength(3);
      const totalCost = calculateTotalCost(result, (a, b) => Math.abs(a - b));
      expect(totalCost).toBe(0); // Best possible matching cost
    });
  });

  describe('unequal size sets', () => {
    it('should handle larger first set', () => {
      const setA = new Set([1, 2, 3, 4]);
      const setB = new Set([2, 3]);
      const result = findOptimalMatching(setA, setB, (a, b) => Math.abs(a - b));
      
      expect(result).toHaveLength(2);
      // Should match the two closest pairs
      const totalCost = calculateTotalCost(result, (a, b) => Math.abs(a - b));
      expect(totalCost).toBe(2); // 1->2 and 3->3, or 2->2 and 3->3
    });

    it('should handle larger second set', () => {
      const setA = new Set([2, 3]);
      const setB = new Set([1, 2, 3, 4]);
      const result = findOptimalMatching(setA, setB, (a, b) => Math.abs(a - b));
      
      expect(result).toHaveLength(2);
      const totalCost = calculateTotalCost(result, (a, b) => Math.abs(a - b));
      expect(totalCost).toBe(0); // Perfect matches: 2->2 and 3->3
    });
  });

  describe('2D point matching', () => {
    it('should find optimal matching for 2D points', () => {
      const setA = new Set<[number, number]>([[0, 0], [1, 1]]);
      const setB = new Set<[number, number]>([[0, 1], [1, 0]]);
      const result = findOptimalMatching(setA, setB, euclideanDistance);
      
      expect(result).toHaveLength(2);
      const totalCost = calculateTotalCost(result, euclideanDistance);
      // Both possible matchings have same cost: 2 (each distance is 1)
      expect(totalCost).toBeCloseTo(2, 5);
    });

    it('should find optimal matching for complex 2D case', () => {
      const setA = new Set<[number, number]>([[0, 0], [10, 0], [0, 10]]);
      const setB = new Set<[number, number]>([[1, 0], [0, 1], [10, 10]]);
      const result = findOptimalMatching(setA, setB, euclideanDistance);
      
      expect(result).toHaveLength(3);
      // Optimal matching should be:
      // [0,0] -> [1,0] (distance: 1)
      // [0,10] -> [0,1] (distance: 9)  
      // [10,0] -> [10,10] (distance: 10)
      // Total: 20
      const totalCost = calculateTotalCost(result, euclideanDistance);
      expect(totalCost).toBeCloseTo(20, 5);
    });
  });

  describe('string matching', () => {
    it('should find optimal matching for strings', () => {
      const setA = new Set(['hello', 'world']);
      const setB = new Set(['help', 'word']);
      const levenshteinDistance = (s1: string, s2: string): number => {
        const matrix = [];
        for (let i = 0; i <= s2.length; i++) {
          matrix[i] = [i];
        }
        for (let j = 0; j <= s1.length; j++) {
          matrix[0][j] = j;
        }
        for (let i = 1; i <= s2.length; i++) {
          for (let j = 1; j <= s1.length; j++) {
            if (s2.charAt(i - 1) === s1.charAt(j - 1)) {
              matrix[i][j] = matrix[i - 1][j - 1];
            } else {
              matrix[i][j] = Math.min(
                matrix[i - 1][j - 1] + 1,
                matrix[i][j - 1] + 1,
                matrix[i - 1][j] + 1
              );
            }
          }
        }
        return matrix[s2.length][s1.length];
      };

      const result = findOptimalMatching(setA, setB, levenshteinDistance);
      expect(result).toHaveLength(2);
      
      // Should match 'hello' -> 'help' (distance: 2) and 'world' -> 'word' (distance: 1)
      const totalCost = calculateTotalCost(result, levenshteinDistance);
      expect(totalCost).toBe(3);
    });
  });

  describe('special cost cases', () => {
    it('should handle all equal costs', () => {
      const setA = new Set([1, 2, 3]);
      const setB = new Set([4, 5, 6]);
      const result = findOptimalMatching(setA, setB, () => 1);
      
      expect(result).toHaveLength(3);
      const totalCost = calculateTotalCost(result, () => 1);
      expect(totalCost).toBe(3);
    });

    it('should handle very large costs', () => {
      const setA = new Set([1, 2]);
      const setB = new Set([3, 4]);
      const result = findOptimalMatching(setA, setB, (a, b) => (a - b) * 1000000);
      
      expect(result).toHaveLength(2);
      // Should still find optimal matching despite large numbers
      const totalCost = calculateTotalCost(result, (a, b) => (a - b) * 1000000);
      expect(totalCost).toBe(-4000000); // 1->3 (-2M) + 2->4 (-2M)
    });
  });

  describe('correctness verification', () => {
    it('should produce deterministic results', () => {
      const setA = new Set([1, 2, 3]);
      const setB = new Set([4, 5, 6]);
      const metric = (a: number, b: number) => Math.abs(a - b);
      
      const result1 = findOptimalMatching(setA, setB, metric);
      const result2 = findOptimalMatching(setA, setB, metric);
      
      expect(result1).toEqual(result2);
    });

    it('should ensure each element is matched at most once', () => {
      const setA = new Set([1, 2, 3, 4]);
      const setB = new Set([5, 6, 7]);
      const result = findOptimalMatching(setA, setB, (a, b) => Math.abs(a - b));
      
      const usedA = new Set();
      const usedB = new Set();
      
      for (const [a, b] of result) {
        expect(usedA.has(a)).toBe(false);
        expect(usedB.has(b)).toBe(false);
        expect(setA.has(a)).toBe(true);
        expect(setB.has(b)).toBe(true);
        usedA.add(a);
        usedB.add(b);
      }
    });

    it('should handle floating point precision', () => {
      const setA = new Set([0.1, 0.2]);
      const setB = new Set([0.3, 0.4]);
      const result = findOptimalMatching(setA, setB, (a, b) => Math.abs(a - b));
      
      expect(result).toHaveLength(2);
      const totalCost = calculateTotalCost(result, (a, b) => Math.abs(a - b));
      expect(totalCost).toBeCloseTo(0.4, 10);
    });
  });
});