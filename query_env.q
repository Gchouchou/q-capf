/
Read all functions and variables from a remote session
-handle :host:port(:username:password) for remote query or defaults to local
-other_namespaces if we want to return variables from other from other namespaces except global namespace
-except "namespace1,namespace2..." to remove certain namespaces
-file "path/to/file" to output the json to a file or defaults to stout
\

/ read command arguments
args: .Q.opt .z.x

handle: `$args[`handle];
other_namespaces: `other_namespaces in key args;
/ exclude the default q Q h j o namespaces
excluded_namespaces: $[other_namespaces & `except in key args; {`$x} each vs args[`except]; ()] union `q`Q`h`j`o;

/
We create a json string where every variable has their type
tables have their row names
lambda have their parameter list, file location and a short excerpt of the body
compositions and projections only get the excerpt of the body
dictionaries have the list of keys
the json will have the structure of namespace:  variables : type, doc, etc..
The function will only look at depth 1 since it is not possible to determine
the difference between a dictionary and a sub sub namespace.
\
get_variable_dict:{[other_namespaces;excluded_namespaces]
 / global namespace is represented by empty symbol `
 namespaces:$[other_namespaces; `, key `; enlist `] except excluded_namespaces;

 get_func_var_from_namespace:{[namespace]
  / function that returns all variables and functions of namespace
  snamespace:$[namespace=`; ""; ".", string namespace];
  :(system "v ", snamespace), (system "f ", snamespace)
  };

 get_doc:{[namespace; variable]
  / creates a dictionary of documentation about VARIABLE in NAMESPACE
  / get variable/function using eval
  x: eval $[namespace=`; variable; `$".", (string namespace), ".", string variable];
  t: type x;
  / helper function to get string body
  max_body: 100; / maximum length
  truncate_string:{[max_body;s] $[max_body < count s; (max_body# s), "..."; s]}[max_body];

  / output documentation dictionary
  :$[
   / is a table, give cols
   .Q.qt x; `type`cols! t, enlist cols x;
   / is a dictionary give keys
   t = 99 ; `type`keys! t, enlist key x;
   / is a lambda, give parameters, file and body excerpt
   t = 100; `type`param`file`body! t,(enlist (value x)[1]), (enlist (value x)[6]), enlist truncate_string (value x)[8];
   / projection, composition, iteration
   t within (104;111); `type`body! t, enlist truncate_string (value x)[8];
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
 :namespaces! generate each namespaces
 };

/ no handle, just evaluate it locally
if[not `handle in key args; handle:eval];

/ oneshot query result while collecting backtrace
result: handle (.Q.trpd; get_variable_dict; (other_namespaces; excluded_namespaces); {"error: ",x,"\nbacktrace:\n", .Q.sbt y});

/ it should always return a dictionary, if we get a string it's the error
/ and we print the error and exit with -1
if[10h=type result; -2 result; exit -1];

if[
 `file in key args;
 file: args[`file];
 file:`$ $[":" <> first file; ":", string file; file];
 output_handle: hopen file;
 output_handle .j.j result;
 hclose output_handle
 ];

if[
 not `file in key args;
 / output to stout
 1 .j.j result
 ];

exit 0
