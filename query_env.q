{[other_namespaces;excluded_namespaces]
 / global namespace is represented by empty symbol `
 namespaces:$[other_namespaces; `, key `; enlist `] except excluded_namespaces union `q`Q`h`j`o;

 get_func_var_from_namespace:{[namespace]
  / function that returns all variables and functions of namespace
  snamespace:$[namespace=`; ""; ".", string namespace];
  :(system "v ", snamespace), (system "f ", snamespace)
  };

 get_doc:{[namespace; variable]
  / creates a dictionary of documentation about VARIABLE in NAMESPACE
  / get variable/function using value
  x: value $[namespace=`; variable; `$".", (string namespace), ".", string variable];
  t: type x;
  / helper function to get string body
  max_body: 100; / maximum length
  truncate_string:{[max_body;s] $[max_body < count s; (max_body# s), "..."; s]}[max_body];

  / get parameters for composition and projection
  get_param:{[function]
   t: type function;
   v: value function;
   :$[
    / projection
    (100 = type first v) & (t = 104);
    / take unspecified parameters
    enlist[`param]!enlist (value[v[0]][1]) where
    ({$[101 = type x; x = (::); 0b]} each (1_v)), (((count value[v[0]][1]) - (count v) - 1)#1b);
    / composition
    (100 = type last v) & (t = 105); enlist[`param]!enlist value[last v][1];
    / other stuff are nyi
    ()
    ]
   };

  / output documentation dictionary
  :$[
   / is a table, give cols
   .Q.qt x; `type`cols! t, enlist cols x;
   / is a dictionary give keys
   t = 99 ; `type`keys! t, enlist key x;
   / is a lambda, give parameters, file and body excerpt
   t = 100; {[t;x;f] v:value x;
    result: `type`param`body! t,(enlist v[1]), enlist f (reverse v)[0];
    if[0 < count (reverse v)[2]; result[`file]: (reverse v)[2]; if[-1 <> count (reverse v)[1]; result[`line]: (reverse v)[1]]];
    :result }[t;x;truncate_string];
   / projection, composition, iteration
   t within (104;111); (`type`body! t, enlist truncate_string .Q.s x), get_param[x];
   / other only give type
   (enlist `type)!enlist t
   ]
  };

 / compose the above two functions together
 generate:{[f;g;namespace]
  vars:f[namespace];
  :vars! g[namespace] each vars
  } [get_func_var_from_namespace; get_doc];

 / putting it all together
 :.j.j namespaces! generate each namespaces
 }
