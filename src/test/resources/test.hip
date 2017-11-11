int width = 100;
int height = 100;
dimension (100,100);
neighbourhood
    N = [0,1], S = [0,-1], W = [-1,0], E = [1,0];
state {boolean alive = false; }
function f() {}
updater {

}
mapper {
    // FOLLOWING IS NOT ALLOWED
    //for i = 1 to 3;
    //cell[width-1,height-1]
    //    iterate nbr over [N,S];
    return(0);
}
// for loop in initialiser
initialiser corners {
    for i = 0 to (width-1) {
        for j = 0 to (height-1) {
            cell [i, j] {
                if ((i == 0) || (i == (width-1))) then {
                    if (j == 0) then
                        iterate nbr over all
                            nbr:alive = true;
                    if (j == (height-1)) then
                        iterate nbr over all
                            nbr:alive = true;
                    else
                        alive = false;
                }
                else
                    alive = false;
            }
        }
      }
}

// body of for loop is empty
function empty_for() {
    for i = 0 to 6 step 1;
}
function empty_itr(int x, int y) {
    cell[x,y]
        iterate nbr over [N, S, W];
}

// body of the loop is a single statement
function single_stmt_for() {
    int count = 0;
    for i = 0 to 6
        count = count + 1;
}
function single_stmt_itr(int x, int y) {
    cell[x,y]
        iterate nbr over all
            nbr:alive = false;
}

// body of the loop is a block of statements
function block_stmts_for(int exp, int bse) {
    int pow_res = bse;
    int mult_res = 0;
    float root_res = bse;
    for i = 1 to exp {
        pow_res = pow_res * bse;
        mult_res = mult_res + bse;
        root_res = root_res / bse;
    }
}
function block_stmts_itr(int x, int y) {
    cell[x,y]
        iterate nbr over [N, S, E, W] {
            if (nbr:alive == true) then
                nbr:alive = false;
            else
                nbr:alive = true;
        }
}

// start, end, and step expressions are constants, positive, negative, complex expressions
function expr_for(int end) {
    int stp = -1;
    for strt = 5 to end step stp;
}

// nested loop
function nest_for(int rows, int cols) {
    for i = 0 to rows
        for j = i to cols;
}
function nest_itr(int x, int y) {
    cell[x,y]
        iterate top_btm over [N,S] {
            iterate lft_rgt over [E,W]
                lft_rgt:alive = true;
        }
}

// set of neighbours being iterated over is others, all, set
function set_nbrs_itr(int x, int y) {
    cell[x,y]
        iterate n_all over all;
    cell[x,y]
        iterate n_others over others;
    cell[x,y]
        iterate n_set over [N,E];
}

