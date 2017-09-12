dimension (100,100);
neighbourhood N = [0,1], S = [0,-1];
state {boolean alive = false; }
function f() {}
updater {
  neighbour t = me;
  boolean b = true;
  fabs = true;
  f = 10 + f();
  f = abs + 10;
  t:alive = true;
  me:f = 10;
  iterate x over [N, S]
    t = x;
  int i = f + 10;      
}
mapper { return(0); }
