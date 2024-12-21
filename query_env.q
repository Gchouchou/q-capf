/
Read all functions and variables from a remote session
-handle :host:port(:username:password) for remote query or defaults to local
-other_namespaces if we want to return variables from other from other namespaces.
-except "namespace1,namespace2..." to remove certain namespaces
-file "path/to/file" to output the json to a file defaults to stout
\

/ read command arguments
args: .Q.opt .z.x

handle: `$args[`handle];
other_namespaces: `other_namespaces in key args;
/ exclude the default q Q h j o namespaces
excluded_namespaces: $[other_namespaces; {`$x} each vs args[`except]; ()] union `q`Q`h`j`o;

/
We create a json string where
every variable has their type
tables have their row names
lambda have their parameter list,
file location
and a short excerpt of the body
compositions and projections only get the excerpt of the body
dictionaries have the list of keys
the json will also have the structure of namespace: list of variables and functions
The function will only look at depth 1 since it is hard to determine
the difference between a dictionary and a sub sub namespace.
\
get_variable_dict:{[other_namespaces;excluded_namespaces]
 namespaces:`global,$[other_namespaces; (key `) except excluded_namespaces; ()];

 get_func_var_from_namespace:{[namespace]
  / function that returns all variables and functions of namespace
  snamespace:$[namespace=`global;"";".",string namespace];
  :(system "v ", snamespace), (system "f ", snamespace)
  };

 get_doc:{[namespace; variable]
  / creates a dictionary of documentation about VARIABLE in NAMESPACE
  / get variable/function using eval
  x: eval $[namespace=`global; variable; `$".", (string namespace), ".", string variable];
  t: type x;
  / helper function to get string body
  max_body: 100; / maximum length
  get_body:{[max_body;x] $[max_body < count .Q.s x; (max_body# .Q.s x), "..."; .Q.s x]}[max_body];

  / output documentation dictionary
  :$[
   / is a table, give cols
   .Q.qt x; `type`cols! t, enlist cols x;
   / is a dictionary give keys
   t = 99 ; `type`keys! t, enlist key x;
   / is a lambda, give parameters, file and body excerpt
   t = 100; `type`param`file`body! t,(enlist (value x)[1]), (enlist (value x)[6]), enlist get_body x;
   / projection, composition, iteration
   t within (104;111); `type`body! t, enlist get_body x;
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
 vdict: namespaces! generate each namespaces;
 :vdict
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
 file:$[":" <> first string file; `$":", string file; file];
 output_handle: hopen file
 ];

if[
 not `file in key args;
 / output to stout
 output_handle:-1
 ];

output_handle each .j.j result;

/ close handle
if[`file in key args; hclose output_handle];

exit 0
