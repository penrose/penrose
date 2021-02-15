// @ts-nocheck
// This doesn't need to strictly follow the grammar in types.d.ts

// For linear-algebra-paper-simple.sty and linear-algebra-paper-simple.ast.json
// The `selEnvs` are in the same order as the selectors in linear-algebra-paper-simple.sty

export const selEnvs: SelEnv[] = [
  {
    sTypeVarMap: {},
    varProgTypeMap: {},
    sErrors: [],
    skipBlock: false,
    header: {
      tag: "Just",
      contents: {
        start: {
          line: 1,
          col: 0,
        },
        end: {
          line: 1,
          col: 5,
        },
        tag: "Namespace",
        contents: {
          start: {
            line: 1,
            col: 0,
          },
          end: {
            line: 1,
            col: 5,
          },
          tag: "Identifier",
          value: "const",
          type: "identifier",
        },
      },
    },
  },
  {
    sTypeVarMap: {},
    varProgTypeMap: {},
    sErrors: [],
    skipBlock: false,
    header: {
      tag: "Just",
      contents: {
        start: {
          line: 14,
          col: 0,
        },
        end: {
          line: 14,
          col: 1,
        },
        tag: "Namespace",
        contents: {
          start: {
            line: 14,
            col: 0,
          },
          end: {
            line: 14,
            col: 1,
          },
          tag: "Identifier",
          value: "C",
          type: "identifier",
        },
      },
    },
  },
  {
    sTypeVarMap: {},
    varProgTypeMap: {},
    sErrors: [],
    skipBlock: false,
    header: {
      tag: "Just",
      contents: {
        start: {
          line: 28,
          col: 0,
        },
        end: {
          line: 28,
          col: 7,
        },
        tag: "Namespace",
        contents: {
          start: {
            line: 28,
            col: 0,
          },
          end: {
            line: 28,
            col: 7,
          },
          tag: "Identifier",
          value: "testing",
          type: "identifier",
        },
      },
    },
  },
  {
    sTypeVarMap: {
      U: {
        start: {
          line: 51,
          col: 7,
        },
        end: {
          line: 51,
          col: 18,
        },
        tag: "Identifier",
        value: "VectorSpace",
        type: "identifier",
      },
    },
    varProgTypeMap: {
      U: [
        {
          tag: "StyProgT",
        },
        {
          start: {
            line: 51,
            col: 19,
          },
          end: {
            line: 51,
            col: 20,
          },
          tag: "StyVar",
          contents: {
            start: {
              line: 51,
              col: 19,
            },
            end: {
              line: 51,
              col: 20,
            },
            tag: "Identifier",
            value: "U",
            type: "identifier",
          },
        },
      ],
    },
    sErrors: [],
    skipBlock: false,
    header: {
      tag: "Just",
      contents: {
        start: {
          line: 51,
          col: 7,
        },
        end: {
          line: 51,
          col: 20,
        },
        tag: "Selector",
        head: {
          start: {
            line: 51,
            col: 7,
          },
          end: {
            line: 51,
            col: 20,
          },
          tag: "DeclPatterns",
          contents: [
            {
              start: {
                line: 51,
                col: 7,
              },
              end: {
                line: 51,
                col: 20,
              },
              tag: "DeclPattern",
              type: {
                start: {
                  line: 51,
                  col: 7,
                },
                end: {
                  line: 51,
                  col: 18,
                },
                tag: "Identifier",
                value: "VectorSpace",
                type: "identifier",
              },
              id: {
                start: {
                  line: 51,
                  col: 19,
                },
                end: {
                  line: 51,
                  col: 20,
                },
                tag: "StyVar",
                contents: {
                  start: {
                    line: 51,
                    col: 19,
                  },
                  end: {
                    line: 51,
                    col: 20,
                  },
                  tag: "Identifier",
                  value: "U",
                  type: "identifier",
                },
              },
            },
          ],
        },
        namespace: null,
      },
    },
  },
  {
    sTypeVarMap: {
      u: {
        start: {
          line: 94,
          col: 7,
        },
        end: {
          line: 94,
          col: 13,
        },
        tag: "Identifier",
        value: "Vector",
        type: "identifier",
      },
      U: {
        start: {
          line: 94,
          col: 17,
        },
        end: {
          line: 94,
          col: 28,
        },
        tag: "Identifier",
        value: "VectorSpace",
        type: "identifier",
      },
    },
    varProgTypeMap: {
      u: [
        {
          tag: "StyProgT",
        },
        {
          start: {
            line: 94,
            col: 14,
          },
          end: {
            line: 94,
            col: 15,
          },
          tag: "StyVar",
          contents: {
            start: {
              line: 94,
              col: 14,
            },
            end: {
              line: 94,
              col: 15,
            },
            tag: "Identifier",
            value: "u",
            type: "identifier",
          },
        },
      ],
      U: [
        {
          tag: "StyProgT",
        },
        {
          start: {
            line: 94,
            col: 29,
          },
          end: {
            line: 94,
            col: 30,
          },
          tag: "StyVar",
          contents: {
            start: {
              line: 94,
              col: 29,
            },
            end: {
              line: 94,
              col: 30,
            },
            tag: "Identifier",
            value: "U",
            type: "identifier",
          },
        },
      ],
    },
    sErrors: [],
    skipBlock: false,
    header: {
      tag: "Just",
      contents: {
        start: {
          line: 94,
          col: 7,
        },
        end: {
          line: 95,
          col: 12,
        },
        tag: "Selector",
        head: {
          start: {
            line: 94,
            col: 7,
          },
          end: {
            line: 94,
            col: 30,
          },
          tag: "DeclPatterns",
          contents: [
            {
              start: {
                line: 94,
                col: 7,
              },
              end: {
                line: 94,
                col: 15,
              },
              tag: "DeclPattern",
              type: {
                start: {
                  line: 94,
                  col: 7,
                },
                end: {
                  line: 94,
                  col: 13,
                },
                tag: "Identifier",
                value: "Vector",
                type: "identifier",
              },
              id: {
                start: {
                  line: 94,
                  col: 14,
                },
                end: {
                  line: 94,
                  col: 15,
                },
                tag: "StyVar",
                contents: {
                  start: {
                    line: 94,
                    col: 14,
                  },
                  end: {
                    line: 94,
                    col: 15,
                  },
                  tag: "Identifier",
                  value: "u",
                  type: "identifier",
                },
              },
            },
            {
              start: {
                line: 94,
                col: 17,
              },
              end: {
                line: 94,
                col: 30,
              },
              tag: "DeclPattern",
              type: {
                start: {
                  line: 94,
                  col: 17,
                },
                end: {
                  line: 94,
                  col: 28,
                },
                tag: "Identifier",
                value: "VectorSpace",
                type: "identifier",
              },
              id: {
                start: {
                  line: 94,
                  col: 29,
                },
                end: {
                  line: 94,
                  col: 30,
                },
                tag: "StyVar",
                contents: {
                  start: {
                    line: 94,
                    col: 29,
                  },
                  end: {
                    line: 94,
                    col: 30,
                  },
                  tag: "Identifier",
                  value: "U",
                  type: "identifier",
                },
              },
            },
          ],
        },
        where: {
          start: {
            line: 95,
            col: 6,
          },
          end: {
            line: 95,
            col: 12,
          },
          tag: "RelationPatterns",
          contents: [
            {
              start: {
                line: 95,
                col: 6,
              },
              end: {
                line: 95,
                col: 12,
              },
              tag: "RelPred",
              name: {
                start: {
                  line: 95,
                  col: 6,
                },
                end: {
                  line: 95,
                  col: 8,
                },
                tag: "Identifier",
                value: "In",
                type: "identifier",
              },
              args: [
                {
                  start: {
                    line: 95,
                    col: 9,
                  },
                  end: {
                    line: 95,
                    col: 10,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 95,
                      col: 9,
                    },
                    end: {
                      line: 95,
                      col: 10,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 95,
                        col: 9,
                      },
                      end: {
                        line: 95,
                        col: 10,
                      },
                      tag: "Identifier",
                      value: "u",
                      type: "identifier",
                    },
                  },
                },
                {
                  start: {
                    line: 95,
                    col: 11,
                  },
                  end: {
                    line: 95,
                    col: 12,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 95,
                      col: 11,
                    },
                    end: {
                      line: 95,
                      col: 12,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 95,
                        col: 11,
                      },
                      end: {
                        line: 95,
                        col: 12,
                      },
                      tag: "Identifier",
                      value: "U",
                      type: "identifier",
                    },
                  },
                },
              ],
            },
          ],
        },
        namespace: null,
      },
    },
  },
  {
    sTypeVarMap: {
      u: {
        start: {
          line: 121,
          col: 7,
        },
        end: {
          line: 121,
          col: 13,
        },
        tag: "Identifier",
        value: "Vector",
        type: "identifier",
      },
      v: {
        start: {
          line: 121,
          col: 17,
        },
        end: {
          line: 121,
          col: 23,
        },
        tag: "Identifier",
        value: "Vector",
        type: "identifier",
      },
      U: {
        start: {
          line: 122,
          col: 5,
        },
        end: {
          line: 122,
          col: 16,
        },
        tag: "Identifier",
        value: "VectorSpace",
        type: "identifier",
      },
    },
    varProgTypeMap: {
      u: [
        {
          tag: "StyProgT",
        },
        {
          start: {
            line: 121,
            col: 14,
          },
          end: {
            line: 121,
            col: 15,
          },
          tag: "StyVar",
          contents: {
            start: {
              line: 121,
              col: 14,
            },
            end: {
              line: 121,
              col: 15,
            },
            tag: "Identifier",
            value: "u",
            type: "identifier",
          },
        },
      ],
      v: [
        {
          tag: "StyProgT",
        },
        {
          start: {
            line: 121,
            col: 24,
          },
          end: {
            line: 121,
            col: 25,
          },
          tag: "StyVar",
          contents: {
            start: {
              line: 121,
              col: 24,
            },
            end: {
              line: 121,
              col: 25,
            },
            tag: "Identifier",
            value: "v",
            type: "identifier",
          },
        },
      ],
      U: [
        {
          tag: "StyProgT",
        },
        {
          start: {
            line: 122,
            col: 17,
          },
          end: {
            line: 122,
            col: 18,
          },
          tag: "StyVar",
          contents: {
            start: {
              line: 122,
              col: 17,
            },
            end: {
              line: 122,
              col: 18,
            },
            tag: "Identifier",
            value: "U",
            type: "identifier",
          },
        },
      ],
    },
    sErrors: [],
    skipBlock: false,
    header: {
      tag: "Just",
      contents: {
        start: {
          line: 121,
          col: 7,
        },
        end: {
          line: 123,
          col: 41,
        },
        tag: "Selector",
        head: {
          start: {
            line: 121,
            col: 7,
          },
          end: {
            line: 121,
            col: 25,
          },
          tag: "DeclPatterns",
          contents: [
            {
              start: {
                line: 121,
                col: 7,
              },
              end: {
                line: 121,
                col: 15,
              },
              tag: "DeclPattern",
              type: {
                start: {
                  line: 121,
                  col: 7,
                },
                end: {
                  line: 121,
                  col: 13,
                },
                tag: "Identifier",
                value: "Vector",
                type: "identifier",
              },
              id: {
                start: {
                  line: 121,
                  col: 14,
                },
                end: {
                  line: 121,
                  col: 15,
                },
                tag: "StyVar",
                contents: {
                  start: {
                    line: 121,
                    col: 14,
                  },
                  end: {
                    line: 121,
                    col: 15,
                  },
                  tag: "Identifier",
                  value: "u",
                  type: "identifier",
                },
              },
            },
            {
              start: {
                line: 121,
                col: 17,
              },
              end: {
                line: 121,
                col: 25,
              },
              tag: "DeclPattern",
              type: {
                start: {
                  line: 121,
                  col: 17,
                },
                end: {
                  line: 121,
                  col: 23,
                },
                tag: "Identifier",
                value: "Vector",
                type: "identifier",
              },
              id: {
                start: {
                  line: 121,
                  col: 24,
                },
                end: {
                  line: 121,
                  col: 25,
                },
                tag: "StyVar",
                contents: {
                  start: {
                    line: 121,
                    col: 24,
                  },
                  end: {
                    line: 121,
                    col: 25,
                  },
                  tag: "Identifier",
                  value: "v",
                  type: "identifier",
                },
              },
            },
          ],
        },
        with: {
          start: {
            line: 122,
            col: 5,
          },
          end: {
            line: 122,
            col: 18,
          },
          tag: "DeclPatterns",
          contents: [
            {
              start: {
                line: 122,
                col: 5,
              },
              end: {
                line: 122,
                col: 18,
              },
              tag: "DeclPattern",
              type: {
                start: {
                  line: 122,
                  col: 5,
                },
                end: {
                  line: 122,
                  col: 16,
                },
                tag: "Identifier",
                value: "VectorSpace",
                type: "identifier",
              },
              id: {
                start: {
                  line: 122,
                  col: 17,
                },
                end: {
                  line: 122,
                  col: 18,
                },
                tag: "StyVar",
                contents: {
                  start: {
                    line: 122,
                    col: 17,
                  },
                  end: {
                    line: 122,
                    col: 18,
                  },
                  tag: "Identifier",
                  value: "U",
                  type: "identifier",
                },
              },
            },
          ],
        },
        where: {
          start: {
            line: 123,
            col: 6,
          },
          end: {
            line: 123,
            col: 41,
          },
          tag: "RelationPatterns",
          contents: [
            {
              start: {
                line: 123,
                col: 6,
              },
              end: {
                line: 123,
                col: 21,
              },
              tag: "RelPred",
              name: {
                start: {
                  line: 123,
                  col: 6,
                },
                end: {
                  line: 123,
                  col: 16,
                },
                tag: "Identifier",
                value: "Orthogonal",
                type: "identifier",
              },
              args: [
                {
                  start: {
                    line: 123,
                    col: 17,
                  },
                  end: {
                    line: 123,
                    col: 18,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 123,
                      col: 17,
                    },
                    end: {
                      line: 123,
                      col: 18,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 123,
                        col: 17,
                      },
                      end: {
                        line: 123,
                        col: 18,
                      },
                      tag: "Identifier",
                      value: "u",
                      type: "identifier",
                    },
                  },
                },
                {
                  start: {
                    line: 123,
                    col: 20,
                  },
                  end: {
                    line: 123,
                    col: 21,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 123,
                      col: 20,
                    },
                    end: {
                      line: 123,
                      col: 21,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 123,
                        col: 20,
                      },
                      end: {
                        line: 123,
                        col: 21,
                      },
                      tag: "Identifier",
                      value: "v",
                      type: "identifier",
                    },
                  },
                },
              ],
            },
            {
              start: {
                line: 123,
                col: 24,
              },
              end: {
                line: 123,
                col: 31,
              },
              tag: "RelPred",
              name: {
                start: {
                  line: 123,
                  col: 24,
                },
                end: {
                  line: 123,
                  col: 26,
                },
                tag: "Identifier",
                value: "In",
                type: "identifier",
              },
              args: [
                {
                  start: {
                    line: 123,
                    col: 27,
                  },
                  end: {
                    line: 123,
                    col: 28,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 123,
                      col: 27,
                    },
                    end: {
                      line: 123,
                      col: 28,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 123,
                        col: 27,
                      },
                      end: {
                        line: 123,
                        col: 28,
                      },
                      tag: "Identifier",
                      value: "u",
                      type: "identifier",
                    },
                  },
                },
                {
                  start: {
                    line: 123,
                    col: 30,
                  },
                  end: {
                    line: 123,
                    col: 31,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 123,
                      col: 30,
                    },
                    end: {
                      line: 123,
                      col: 31,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 123,
                        col: 30,
                      },
                      end: {
                        line: 123,
                        col: 31,
                      },
                      tag: "Identifier",
                      value: "U",
                      type: "identifier",
                    },
                  },
                },
              ],
            },
            {
              start: {
                line: 123,
                col: 34,
              },
              end: {
                line: 123,
                col: 41,
              },
              tag: "RelPred",
              name: {
                start: {
                  line: 123,
                  col: 34,
                },
                end: {
                  line: 123,
                  col: 36,
                },
                tag: "Identifier",
                value: "In",
                type: "identifier",
              },
              args: [
                {
                  start: {
                    line: 123,
                    col: 37,
                  },
                  end: {
                    line: 123,
                    col: 38,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 123,
                      col: 37,
                    },
                    end: {
                      line: 123,
                      col: 38,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 123,
                        col: 37,
                      },
                      end: {
                        line: 123,
                        col: 38,
                      },
                      tag: "Identifier",
                      value: "v",
                      type: "identifier",
                    },
                  },
                },
                {
                  start: {
                    line: 123,
                    col: 40,
                  },
                  end: {
                    line: 123,
                    col: 41,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 123,
                      col: 40,
                    },
                    end: {
                      line: 123,
                      col: 41,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 123,
                        col: 40,
                      },
                      end: {
                        line: 123,
                        col: 41,
                      },
                      tag: "Identifier",
                      value: "U",
                      type: "identifier",
                    },
                  },
                },
              ],
            },
          ],
        },
        namespace: null,
      },
    },
  },
  {
    sTypeVarMap: {
      v: {
        start: {
          line: 154,
          col: 7,
        },
        end: {
          line: 154,
          col: 13,
        },
        tag: "Identifier",
        value: "Vector",
        type: "identifier",
      },
      U: {
        start: {
          line: 155,
          col: 5,
        },
        end: {
          line: 155,
          col: 16,
        },
        tag: "Identifier",
        value: "VectorSpace",
        type: "identifier",
      },
      w: {
        start: {
          line: 155,
          col: 20,
        },
        end: {
          line: 155,
          col: 26,
        },
        tag: "Identifier",
        value: "Vector",
        type: "identifier",
      },
    },
    varProgTypeMap: {
      v: [
        {
          tag: "StyProgT",
        },
        {
          start: {
            line: 154,
            col: 14,
          },
          end: {
            line: 154,
            col: 15,
          },
          tag: "StyVar",
          contents: {
            start: {
              line: 154,
              col: 14,
            },
            end: {
              line: 154,
              col: 15,
            },
            tag: "Identifier",
            value: "v",
            type: "identifier",
          },
        },
      ],
      U: [
        {
          tag: "StyProgT",
        },
        {
          start: {
            line: 155,
            col: 17,
          },
          end: {
            line: 155,
            col: 18,
          },
          tag: "StyVar",
          contents: {
            start: {
              line: 155,
              col: 17,
            },
            end: {
              line: 155,
              col: 18,
            },
            tag: "Identifier",
            value: "U",
            type: "identifier",
          },
        },
      ],
      w: [
        {
          tag: "StyProgT",
        },
        {
          start: {
            line: 155,
            col: 27,
          },
          end: {
            line: 155,
            col: 28,
          },
          tag: "StyVar",
          contents: {
            start: {
              line: 155,
              col: 27,
            },
            end: {
              line: 155,
              col: 28,
            },
            tag: "Identifier",
            value: "w",
            type: "identifier",
          },
        },
      ],
    },
    sErrors: [],
    skipBlock: false,
    header: {
      tag: "Just",
      contents: {
        start: {
          line: 154,
          col: 7,
        },
        end: {
          line: 156,
          col: 40,
        },
        tag: "Selector",
        head: {
          start: {
            line: 154,
            col: 7,
          },
          end: {
            line: 154,
            col: 15,
          },
          tag: "DeclPatterns",
          contents: [
            {
              start: {
                line: 154,
                col: 7,
              },
              end: {
                line: 154,
                col: 15,
              },
              tag: "DeclPattern",
              type: {
                start: {
                  line: 154,
                  col: 7,
                },
                end: {
                  line: 154,
                  col: 13,
                },
                tag: "Identifier",
                value: "Vector",
                type: "identifier",
              },
              id: {
                start: {
                  line: 154,
                  col: 14,
                },
                end: {
                  line: 154,
                  col: 15,
                },
                tag: "StyVar",
                contents: {
                  start: {
                    line: 154,
                    col: 14,
                  },
                  end: {
                    line: 154,
                    col: 15,
                  },
                  tag: "Identifier",
                  value: "v",
                  type: "identifier",
                },
              },
            },
          ],
        },
        with: {
          start: {
            line: 155,
            col: 5,
          },
          end: {
            line: 155,
            col: 28,
          },
          tag: "DeclPatterns",
          contents: [
            {
              start: {
                line: 155,
                col: 5,
              },
              end: {
                line: 155,
                col: 18,
              },
              tag: "DeclPattern",
              type: {
                start: {
                  line: 155,
                  col: 5,
                },
                end: {
                  line: 155,
                  col: 16,
                },
                tag: "Identifier",
                value: "VectorSpace",
                type: "identifier",
              },
              id: {
                start: {
                  line: 155,
                  col: 17,
                },
                end: {
                  line: 155,
                  col: 18,
                },
                tag: "StyVar",
                contents: {
                  start: {
                    line: 155,
                    col: 17,
                  },
                  end: {
                    line: 155,
                    col: 18,
                  },
                  tag: "Identifier",
                  value: "U",
                  type: "identifier",
                },
              },
            },
            {
              start: {
                line: 155,
                col: 20,
              },
              end: {
                line: 155,
                col: 28,
              },
              tag: "DeclPattern",
              type: {
                start: {
                  line: 155,
                  col: 20,
                },
                end: {
                  line: 155,
                  col: 26,
                },
                tag: "Identifier",
                value: "Vector",
                type: "identifier",
              },
              id: {
                start: {
                  line: 155,
                  col: 27,
                },
                end: {
                  line: 155,
                  col: 28,
                },
                tag: "StyVar",
                contents: {
                  start: {
                    line: 155,
                    col: 27,
                  },
                  end: {
                    line: 155,
                    col: 28,
                  },
                  tag: "Identifier",
                  value: "w",
                  type: "identifier",
                },
              },
            },
          ],
        },
        where: {
          start: {
            line: 156,
            col: 6,
          },
          end: {
            line: 156,
            col: 40,
          },
          tag: "RelationPatterns",
          contents: [
            {
              start: {
                line: 156,
                col: 6,
              },
              end: {
                line: 156,
                col: 13,
              },
              tag: "RelPred",
              name: {
                start: {
                  line: 156,
                  col: 6,
                },
                end: {
                  line: 156,
                  col: 8,
                },
                tag: "Identifier",
                value: "In",
                type: "identifier",
              },
              args: [
                {
                  start: {
                    line: 156,
                    col: 9,
                  },
                  end: {
                    line: 156,
                    col: 10,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 156,
                      col: 9,
                    },
                    end: {
                      line: 156,
                      col: 10,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 156,
                        col: 9,
                      },
                      end: {
                        line: 156,
                        col: 10,
                      },
                      tag: "Identifier",
                      value: "v",
                      type: "identifier",
                    },
                  },
                },
                {
                  start: {
                    line: 156,
                    col: 12,
                  },
                  end: {
                    line: 156,
                    col: 13,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 156,
                      col: 12,
                    },
                    end: {
                      line: 156,
                      col: 13,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 156,
                        col: 12,
                      },
                      end: {
                        line: 156,
                        col: 13,
                      },
                      tag: "Identifier",
                      value: "U",
                      type: "identifier",
                    },
                  },
                },
              ],
            },
            {
              start: {
                line: 156,
                col: 16,
              },
              end: {
                line: 156,
                col: 22,
              },
              tag: "RelPred",
              name: {
                start: {
                  line: 156,
                  col: 16,
                },
                end: {
                  line: 156,
                  col: 20,
                },
                tag: "Identifier",
                value: "Unit",
                type: "identifier",
              },
              args: [
                {
                  start: {
                    line: 156,
                    col: 21,
                  },
                  end: {
                    line: 156,
                    col: 22,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 156,
                      col: 21,
                    },
                    end: {
                      line: 156,
                      col: 22,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 156,
                        col: 21,
                      },
                      end: {
                        line: 156,
                        col: 22,
                      },
                      tag: "Identifier",
                      value: "v",
                      type: "identifier",
                    },
                  },
                },
              ],
            },
            {
              start: {
                line: 156,
                col: 25,
              },
              end: {
                line: 156,
                col: 40,
              },
              tag: "RelPred",
              name: {
                start: {
                  line: 156,
                  col: 25,
                },
                end: {
                  line: 156,
                  col: 35,
                },
                tag: "Identifier",
                value: "Orthogonal",
                type: "identifier",
              },
              args: [
                {
                  start: {
                    line: 156,
                    col: 36,
                  },
                  end: {
                    line: 156,
                    col: 37,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 156,
                      col: 36,
                    },
                    end: {
                      line: 156,
                      col: 37,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 156,
                        col: 36,
                      },
                      end: {
                        line: 156,
                        col: 37,
                      },
                      tag: "Identifier",
                      value: "v",
                      type: "identifier",
                    },
                  },
                },
                {
                  start: {
                    line: 156,
                    col: 39,
                  },
                  end: {
                    line: 156,
                    col: 40,
                  },
                  tag: "SEBind",
                  contents: {
                    start: {
                      line: 156,
                      col: 39,
                    },
                    end: {
                      line: 156,
                      col: 40,
                    },
                    tag: "StyVar",
                    contents: {
                      start: {
                        line: 156,
                        col: 39,
                      },
                      end: {
                        line: 156,
                        col: 40,
                      },
                      tag: "Identifier",
                      value: "w",
                      type: "identifier",
                    },
                  },
                },
              ],
            },
          ],
        },
        namespace: null,
      },
    },
  },
  {
    sTypeVarMap: {
      x2: {
        start: {
          line: 212,
          col: 7,
        },
        end: {
          line: 212,
          col: 13,
        },
        tag: "Identifier",
        value: "Vector",
        type: "identifier",
      },
    },
    varProgTypeMap: {
      x2: [
        {
          tag: "SubProgT",
        },
        {
          start: {
            line: 212,
            col: 15,
          },
          end: {
            line: 212,
            col: 17,
          },
          tag: "SubVar",
          contents: {
            start: {
              line: 212,
              col: 15,
            },
            end: {
              line: 212,
              col: 17,
            },
            tag: "Identifier",
            value: "x2",
            type: "identifier",
          },
        },
      ],
    },
    sErrors: [],
    skipBlock: false,
    header: {
      tag: "Just",
      contents: {
        start: {
          line: 212,
          col: 7,
        },
        end: {
          line: 212,
          col: 17,
        },
        tag: "Selector",
        head: {
          start: {
            line: 212,
            col: 7,
          },
          end: {
            line: 212,
            col: 17,
          },
          tag: "DeclPatterns",
          contents: [
            {
              start: {
                line: 212,
                col: 7,
              },
              end: {
                line: 212,
                col: 17,
              },
              tag: "DeclPattern",
              type: {
                start: {
                  line: 212,
                  col: 7,
                },
                end: {
                  line: 212,
                  col: 13,
                },
                tag: "Identifier",
                value: "Vector",
                type: "identifier",
              },
              id: {
                start: {
                  line: 212,
                  col: 15,
                },
                end: {
                  line: 212,
                  col: 17,
                },
                tag: "SubVar",
                contents: {
                  start: {
                    line: 212,
                    col: 15,
                  },
                  end: {
                    line: 212,
                    col: 17,
                  },
                  tag: "Identifier",
                  value: "x2",
                  type: "identifier",
                },
              },
            },
          ],
        },
        namespace: null,
      },
    },
  },
];

// All possibilities of variables, before filtering
export const possibleSubsts: Subst[][] = [
  [],
  [],
  [],
  [
    {
      U: "X",
    },
  ],
  [
    {
      u: "x1",
      U: "X",
    },
    {
      u: "x2",
      U: "X",
    },
  ],
  [
    {
      u: "x1",
      v: "x2",
      U: "X",
    },
    {
      u: "x2",
      v: "x1",
      U: "X",
    },
  ],
  [
    {
      v: "x1",
      U: "X",
      w: "x2",
    },
    {
      v: "x2",
      U: "X",
      w: "x1",
    },
  ],
  [{}],
];

// Final substitutions (after filtering with relational statements, empties, etc.)
export const correctSubsts: Subst[][] = [
  [],
  [],
  [],
  [
    {
      U: "X",
    },
  ],
  [
    {
      u: "x1",
      U: "X",
    },
    {
      u: "x2",
      U: "X",
    },
  ],
  [
    {
      u: "x1",
      v: "x2",
      U: "X",
    },
  ],
  [
    {
      v: "x1",
      U: "X",
      w: "x2",
    },
  ],
  [{}],
];
