/**
 * The MIT License (MIT)

 * Copyright (c) 2020 Eyas Ranjous <eyas.ranjous@gmail.com>

 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Adapted from https://github.com/datastructures-js/heap/blob/master/src/heap.js 
 * to allow increasing node priority by adding a node_to_index map
 *
 */

export interface ICompare<T> {
  (a: T, b: T): number;
}

export default class Heap<T> {
  private leaf: T | undefined = undefined;
  private node_to_index: Map<T, number> = new Map();

  /**
   * @param {function} compare
   */
  constructor(private compare: ICompare<T>, private nodes: T[] = []) {
    nodes.forEach((node: T, i: number) => this.node_to_index.set(node, i));
  }

  /**
   * Converts the heap to a cloned array without sorting.
   * @public
   * @returns {Array}
   */
  toArray(): T[] {
    return Array.from(this.nodes);
  }

  /**
   * Checks if a parent has a left child
   * @private
   */
  private hasLeftChild(parentIndex: number) {
    const leftChildIndex = parentIndex * 2 + 1;
    return leftChildIndex < this.size();
  }

  /**
   * Checks if a parent has a right child
   * @private
   */
  private hasRightChild(parentIndex: number) {
    const rightChildIndex = parentIndex * 2 + 2;
    return rightChildIndex < this.size();
  }

  /**
   * Compares two nodes
   * @private
   */
  private compareAt(i: number, j: number) {
    return this.compare(this.nodes[i], this.nodes[j]);
  }

  /**
   * Swaps two nodes in the heap
   * @private
   */
  private swap(i: number, j: number) {
    this.node_to_index.set(this.nodes[i], j);
    this.node_to_index.set(this.nodes[j], i);

    const temp = this.nodes[i];
    this.nodes[i] = this.nodes[j];
    this.nodes[j] = temp;
  }

  /**
   * Checks if parent and child should be swapped
   * @private
   */
  private shouldSwap(parentIndex: number, childIndex: number) {
    if (parentIndex < 0 || parentIndex >= this.size()) {
      return false;
    }

    if (childIndex < 0 || childIndex >= this.size()) {
      return false;
    }

    return this.compareAt(parentIndex, childIndex) > 0;
  }

  /**
   * Compares children of a parent
   * @private
   */
  private compareChildrenOf(parentIndex: number) {
    if (!this.hasLeftChild(parentIndex) && !this.hasRightChild(parentIndex)) {
      return -1;
    }

    const leftChildIndex = parentIndex * 2 + 1;
    const rightChildIndex = parentIndex * 2 + 2;

    if (!this.hasLeftChild(parentIndex)) {
      return rightChildIndex;
    }

    if (!this.hasRightChild(parentIndex)) {
      return leftChildIndex;
    }

    const compare = this.compareAt(leftChildIndex, rightChildIndex);
    return compare > 0 ? rightChildIndex : leftChildIndex;
  }

  /**
   * Compares two children before a position
   * @private
   */
  private compareChildrenBefore(
    index: number,
    leftChildIndex: number,
    rightChildIndex: number
  ) {
    const compare = this.compareAt(rightChildIndex, leftChildIndex);

    if (compare <= 0 && rightChildIndex < index) {
      return rightChildIndex;
    }

    return leftChildIndex;
  }

  /**
   * Recursively bubbles up a node if it's in a wrong position
   * @private
   */
  private heapifyUp(startIndex: number) {
    let childIndex = startIndex;
    let parentIndex = Math.floor((childIndex - 1) / 2);

    while (this.shouldSwap(parentIndex, childIndex)) {
      this.swap(parentIndex, childIndex);
      childIndex = parentIndex;
      parentIndex = Math.floor((childIndex - 1) / 2);
    }
  }

  /**
   * Recursively bubbles down a node if it's in a wrong position
   * @private
   */
  private heapifyDown(startIndex: number) {
    let parentIndex = startIndex;
    let childIndex = this.compareChildrenOf(parentIndex);

    while (this.shouldSwap(parentIndex, childIndex)) {
      this.swap(parentIndex, childIndex);
      parentIndex = childIndex;
      childIndex = this.compareChildrenOf(parentIndex);
    }
  }

  /**
   * Recursively bubbles down a node before a given index
   * @private
   */
  heapifyDownUntil(index: number) {
    let parentIndex = 0;
    let leftChildIndex = 1;
    let rightChildIndex = 2;
    let childIndex;

    while (leftChildIndex < index) {
      childIndex = this.compareChildrenBefore(
        index,
        leftChildIndex,
        rightChildIndex
      );

      if (this.shouldSwap(parentIndex, childIndex)) {
        this.swap(parentIndex, childIndex);
      }

      parentIndex = childIndex;
      leftChildIndex = parentIndex * 2 + 1;
      rightChildIndex = parentIndex * 2 + 2;
    }
  }

  /**
   * Inserts a new value into the heap
   * @public
   * @param {number|string|object} value
   * @returns {Heap}
   */
  insert(value: T): Heap<T> {
    this.nodes.push(value);
    this.heapifyUp(this.size() - 1);
    if (this.leaf || this.compare(value, this.leaf as T) > 0) {
      this.leaf = value;
    }
    return this;
  }

  /**
   * Removes and returns the root node in the heap
   * @public
   * @returns {number|string|object}
   */
  extractRoot(): T | undefined {
    if (this.isEmpty()) {
      return undefined;
    }

    const root = this.root();
    this.nodes[0] = this.nodes[this.size() - 1];
    this.nodes.pop();
    this.heapifyDown(0);

    if (root === this.leaf) {
      this.leaf = this.root();
    }

    return root;
  }

  increase_priority(node: T) {
    const index = this.node_to_index.get(node);
    if (index) {
      this.heapifyUp(index);
    }
  }

  /**
   * Applies heap sort and return the values sorted by priority
   * @public
   * @returns {array}
   */
  sort(): T[] {
    for (let i = this.size() - 1; i > 0; i -= 1) {
      this.swap(0, i);
      this.heapifyDownUntil(i);
    }
    return this.nodes;
  }

  /**
   * Fixes node positions in the heap
   * @public
   * @returns {Heap}
   */
  fix(): Heap<T> {
    // fix node positions
    for (let i = Math.floor(this.size() / 2) - 1; i >= 0; i -= 1) {
      this.heapifyDown(i);
    }

    // fix leaf value
    for (let i = Math.floor(this.size() / 2); i < this.size(); i += 1) {
      const value = this.nodes[i];
      if (this.leaf || this.compare(value, this.leaf as T) > 0) {
        this.leaf = value;
      }
    }

    return this;
  }

  /**
   * Verifies that all heap nodes are in the right position
   * @public
   * @returns {boolean}
   */
  isValid(): boolean {
    const isValidRecursive: (parentIndex: number) => boolean = (
      parentIndex: number
    ) => {
      let isValidLeft = true;
      let isValidRight = true;

      if (this.hasLeftChild(parentIndex)) {
        const leftChildIndex = parentIndex * 2 + 1;
        if (this.compareAt(parentIndex, leftChildIndex) > 0) {
          return false;
        }
        isValidLeft = isValidRecursive(leftChildIndex);
      }

      if (this.hasRightChild(parentIndex)) {
        const rightChildIndex = parentIndex * 2 + 2;
        if (this.compareAt(parentIndex, rightChildIndex) > 0) {
          return false;
        }
        isValidRight = isValidRecursive(rightChildIndex);
      }

      return isValidLeft && isValidRight;
    };

    return isValidRecursive(0);
  }

  /**
   * Returns the root node in the heap
   * @public
   * @returns {number|string|object}
   */
  root(): T | undefined {
    if (this.isEmpty()) {
      return undefined;
    }

    return this.nodes[0];
  }

  /**
   * Returns a leaf node in the heap
   * @public
   * @returns {number|string|object}
   */
  getLeaf(): T | undefined {
    return this.leaf;
  }

  /**
   * Returns the number of nodes in the heap
   * @public
   * @returns {number}
   */
  size(): number {
    return this.nodes.length;
  }

  /**
   * Checks if the heap is empty
   * @public
   * @returns {boolean}
   */
  isEmpty(): boolean {
    return this.size() === 0;
  }

  /**
   * Clears the heap
   * @public
   */
  clear() {
    this.nodes = [];
    this.leaf = undefined;
  }

  /**
   * Builds a heap from a array of values
   * @public
   * @static
   * @param {array} values
   * @param {function} compare
   * @returns {Heap}
   */
  static heapify<T>(values: T[], compare: ICompare<T>): Heap<T> {
    return new Heap(compare, values).fix();
  }

  /**
   * Checks if a list of values is a valid heap
   * @public
   * @static
   * @param {array} values
   * @param {function} compare
   * @returns {boolean}
   */
  static isHeapified<T>(values: T[], compare: ICompare<T>): boolean {
    return new Heap(compare, values).isValid();
  }
}
