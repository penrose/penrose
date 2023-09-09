sepEndBy1[ITEM, SEP] -> $ITEM (_ $SEP _ $ITEM):* (_ $SEP):? {%
  ([first, rest]) => {
    const restNodes = rest.map((ts: any[]) => ts[3]);
    return _.concat(first, ...restNodes);
  }
%}

sepEndBy[ITEM, SEP] 
  -> null {% d => [] %} 
  | $ITEM (_ $SEP _ $ITEM):* (_ $SEP):? {%
    ([first, rest]) => {
      const restNodes = rest.map((ts: any[]) => ts[3]);
      return _.concat(first, ...restNodes);
    }
  %}

sepBy1[ITEM, SEP] -> $ITEM (_ $SEP _ $ITEM):* {%
  ([first, rest]) => {
    const restNodes = rest.map((ts: any[]) => ts[3]);
    return _.concat(first, ...restNodes);
  }
%}

sepBy[ITEM, SEP] 
  -> null {% d => [] %} 
  | $ITEM (_ $SEP _ $ITEM):* {%
    ([first, rest]) => {
      const restNodes = rest.map((ts: any[]) => ts[3]);
      return _.concat(first, ...restNodes);
    }
  %}
