$TITLE SOUTH SUDAN PMP MODEL OF AGRICULTURAL PRODUCTION
* South Sudan Agricultural Production Model, v.6
* Kelly M. Cobourn and Zeya Zhang, July 2019
* Virginia Tech
* contact: kellyc13@vt.edu

$offsymlist offsymxref
option limrow = 0;
option limcol = 0;
option nlp = CONOPT;
option solprint = off;

***************************************************************************
*********************  PART 1. SETS & PARAMETERS  *************************
***************************************************************************

sets
i        crop /cassava,groundnuts,maize,sesame,sorghum/
l        inputs /land,fertilizer/
calib    calibration data /xbar1,xbar2,ybar/
econ     economic data /p,eta,c1,c2/
cycles   cycles calibration data /ybarN/;
alias (i,j);

********** CALIBRATION, ECONOMIC, AND CYCLES INPUT DATA FILES *************
* Land use is for the country in aggregate. Land use is from FAOSTAT for 2016.
* Land areas are in ha.
table    calibdata(i,calib) observed land allocation
$ondelim
$include calibdata_v6.csv
$offdelim
;

** Link with CYCLES by extracting 3 variables: fertilizer applications (kg/ha),
** yield for each crop at that level of fertilizer applications (kg/ha), and
** the yield elasticity (sensitivity of crop yield to N fertilizer applications
** at the observed level of N fertilizer use.)
table  cyclesdata(i,cycles) cycles output
$ondelim
$include cyclesdata_v6.csv
$offdelim
;

** Economic input files include crop prices are from World Food Program and are
** converted into USD/kg. Supply elasticities are from the economic literature
** for similar countries or regions. Costs of production for land (c1) are in
** USD/ha; for N fertilizer (c2) in USD/kg. Inputs other than land and
** fertilizer are assumed to be used in fixed proportion to land and are
** included in c1.
table    price(i,econ) price input data
$ondelim
$include price_v6.csv
$offdelim
;

table    productioncost(i,econ) production cost of land and fertilizer input data
$ondelim
$include productioncost_v6.csv
$offdelim
;

table    supplyelasticity(i,econ) supply elasticity by crop
                  eta
cassava          0.38
groundnuts       0.38
maize            0.49
sesame           0.38
sorghum          0.24;

scalar
b1               agricultural land base;
b1 = sum(i, calibdata(i,'xbar1'));

parameters
qbar(i)          reference crop production
b(i)             land relative to total production value;
qbar(i) = calibdata(i,'xbar1')*calibdata(i,'ybar');
calibdata(i,'xbar2') = calibdata(i,'xbar2')*calibdata(i,'xbar1');
b(i) = sqr(calibdata(i,'xbar1'))/(price(i,'p')*qbar(i));


***************************************************************************
**************  PART 2. PRODUCTION FUNCTION CALIBRATION  ******************
***************************************************************************

** Set elasticities of substitution between land and N for each crop.
parameters
sigma(i)         substitution elasticity by crop and technology
rho(i)           production function elasticity parameter;
sigma(i) = 0.5;
rho(i) = (sigma(i) - 1)/sigma(i);
display rho;

** Ensure that calibration criteria are satisfied. The first criterion
** requires that cc1 > 0 for all i. The second criterion requires that
** cc2 < 0 for all i.
parameters
cc1(i)           calibration criteria 1
flag1(i)         flag violations of cc1
psi(i)           term inside cc2
cc2(i)           calibration criteria 2
flag2(i)         flag violations of cc2;

cc1(i) = supplyelasticity(i,'eta') - cyclesdata(i,'ybarN')/(1 - cyclesdata(i,'ybarN'));
flag1(i) = 1$(cc1(i) lt 0);
abort$(sum(i, flag1(i)) gt 0) "cc1 not satisfied";

psi(i) = sigma(i)*cyclesdata(i,'ybarN')/(supplyelasticity(i,'eta')*(1
         - cyclesdata(i,'ybarN')));
cc2(i) = b(i)*supplyelasticity(i,'eta')*(1 - psi(i)) - sum(j$(ord(j) ne ord(i)),
         b(j)*supplyelasticity(j,'eta')*sqr(1 + (1/supplyelasticity(j,'eta')))*(1 + psi(j)
         - cyclesdata(j,'ybarN')));
flag2(i) = 1$(cc2(i) gt 0);
abort$(sum(i,flag2(i)) gt 0) "cc2 not satisfied";

scalars
term             indicator equal to card(i) when all deltas converge /0/
toler            tolerance for convergence /0.001/;

parameters
delta0(i)        myopic CES production function parameter
adj(i)           adjustment term using myopic delta
error(i)         absolute value of change in delta
converge(i)      indicator equal to one when delta converges;
delta0(i) = supplyelasticity(i,'eta')/(1 + supplyelasticity(i,'eta'));
adj(i) = 1 - (b(i)/(delta0(i)*(1-delta0(i))))/(sum(j,
         (b(j)/(delta0(j)*(1-delta0(j)))) +
         (sigma(j)*b(j)*cyclesdata(j,'ybarN')/(delta0(j)*(delta0(j)
         - cyclesdata(j,'ybarN'))))));

variables
delta(i)         production function homogeneity parameters
beta(i,l)        production function share parameters
dummy            dummy objective;
positive variables delta,beta;

equations
etacal(i)        calibration to exogenous supply elasticity
nresp(i)         calibration against agronomic yield response to N
betas(i)         summation constraint for share parameters
edummy           dummy objective function;
etacal(i).. supplyelasticity(i,'eta') =e= (delta(i)/(1-delta(i)))*adj(i);
Nresp(i).. cyclesdata(i,'ybarN')*((beta(i,'land')*(calibdata(i,'xbar1')**rho(i))) +
          (beta(i,'fertilizer')*(calibdata(i,'xbar2')**rho(i)))) =e=
          delta.l(i)*beta(i,'fertilizer')*(calibdata(i,'xbar2')**rho(i));
betas(i).. sum(l, beta(i,l)) =e= 1;
edummy.. dummy =e= 0;

** Solve supply elasticity system of equations.
model selast /etacal,edummy/;
while(term lt card(i),
solve selast maximizing dummy using nlp;
*Test for convergence in the deltas
         error(i) = abs(delta0(i) - delta.l(i));
         converge(i)$(error(i) lt toler) = 1;
         term = sum(i, converge(i));
*Update values for delta in the adjustment term if convergence test fails
         delta0(i) = delta.l(i);
         adj(i) = 1 - (b(i)/(delta0(i)*(1-delta0(i))))/(sum(j,
                  (b(j)/(delta0(j)*(1-delta0(j)))) +
                  (sigma(j)*b(j)*cyclesdata(j,'ybarN')/(delta0(j)*(delta0(j)
                  - cyclesdata(j,'ybarN'))))));
);

** Solve system of equations for share parameters.
model nelast /Nresp,betas,edummy/;
solve nelast maximizing dummy using nlp;
display delta.l,beta.l;

parameters
mu(i)            scale parameters
lbar1            initial shadow value of land
lambda(i,l)      calibrated factor shadow values
soccost(i,l)     social cost of inputs;

mu(i) =  qbar(i)/((beta.l(i,'land')*(calibdata(i,'xbar1')**rho(i))) +
          (beta.l(i,'fertilizer')*calibdata(i,'xbar2')**rho(i)))**(delta.l(i)/rho(i));
lbar1 = sum(i, (price(i,'p')*qbar(i)*(delta.l(i) - cyclesdata(i,'ybarN'))
        - productioncost(i,'c1')*calibdata(i,'xbar1'))*calibdata(i,'xbar1'))/sum(i, sqr(calibdata(i,'xbar1')));
lambda(i,'land') = price(i,'p')*qbar(i)*(delta.l(i) - cyclesdata(i,'ybarN'))/calibdata(i,'xbar1')
             - (productioncost(i,'c1') + lbar1);
lambda(i,'fertilizer') = price(i,'p')*qbar(i)*cyclesdata(i,'ybarN')/calibdata(i,'xbar2')
             - productioncost(i,'c2');

display rho,delta.l,beta.l,mu,lbar1,lambda;

** Check that calibrated model reproduces observed land allocation, fertilizer
** applications, and production.
parameters
simprice(i,econ) price for simulation
simproductioncost(i,econ) production costs for simulation;
simprice(i,econ) = price(i,econ);
simproductioncost(i,econ) = productioncost(i,econ);

parameters
simcost(i,l)     simulated cost of inputs;
simcost(i,'land') = simproductioncost(i,'c1') + lbar1 + lambda(i,'land');
simcost(i,'fertilizer') = simproductioncost(i,'c2') + lambda(i,'fertilizer');

variables
x(i,l)           simulated input use
q(i)             simulated production
qprofit(i)       quasi-profit function
tprofit          total profit;
positive variables x,q,qprofit;

equations
production(i)    production function
quasiprofit(i)   quasi-profit function
obj              objective function
resconl          land constraint
resconf          fertilizer constraint;

production(i).. q(i) =e= mu(i)*(sum(l, beta.l(i,l)*x(i,l)**rho(i)))**(delta.l(i)/rho(i));
quasiprofit(i).. qprofit(i) =e= simprice(i,'p')*q(i) - sum(l, simcost(i,l)*x(i,l));
obj.. tprofit =e= sum(i, qprofit(i));
resconl.. sum(i, x(i,'land')) =e= b1;
resconf.. sum(i, x(i,'fertilizer')) =e= sum(i, calibdata(i,'xbar2'));

x.lo(i,l) = 0.001;

model profitmax /production,quasiprofit,obj,resconl,resconf/;
solve profitmax maximizing tprofit using nlp;

file     MINT_v6_calibout /MINT_v6_calibration_output.txt/;
MINT_v6_calibout.pc = 5;
put      MINT_v6_calibout;
put      'year','region','crop',
         'simulated land area (ha)', 'observed land area (ha)',
         'simulated N use (kg)', 'observed N use (kg)',
         'simulated production (kg)','observed production (kg)'/;
loop(i, put '2016', 'pongo', i.tl,
         x.l(i,'land'), calibdata(i,'xbar1'),
         x.l(i,'fertilizer'), calibdata(i,'xbar2'),
         q.l(i), qbar(i)/);


***************************************************************************
****************  PART 3. CROP PRODUCTION SIMULATION  *********************
***************************************************************************

** Update price and production cost parameters for simulation
table    simulationprice(i,econ) simulation price input data
$ondelim
$include simprice.csv
$offdelim
;

table    simulationproductioncost(i,econ) simulation production cost input data
$ondelim
$include simproductioncost.csv
$offdelim
;

simprice(i,econ) = simulationprice(i,econ);
simproductioncost(i,econ) = simulationproductioncost(i,econ);
simcost(i,'land') = simproductioncost(i,'c1') + lbar1 + lambda(i,'land');
simcost(i,'fertilizer') = simproductioncost(i,'c2') + lambda(i,'fertilizer');

** Solve simulation problem
solve profitmax maximizing tprofit using nlp;

** Generate and report post-optimal parameters
parameters
fertsubsidy(i)   fertilizer subsidy (%)
landarea(i)      land allocation by crop (ha)
simyield(i)      crop yield (kg per ha)
Nuse(i)          N fertilizer use (kg)
Nrate(i)         N fertilizer rate (kg per ha);

fertsubsidy(i) = 100*(productioncost(i,'c2') - simproductioncost(i,'c2'))/productioncost(i,'c2');
landarea(i) = x.l(i,'land');
simyield(i) = q.l(i)/x.l(i,'land');
Nuse(i) = x.l(i,'fertilizer');
Nrate(i) = x.l(i,'fertilizer')/x.l(i,'land');

file     MINT_v6_simout /MINT_v6_simulation_output.txt/;
MINT_v6_simout.pc = 5;
put      MINT_v6_simout;
put      'year','region','crop','fert_subsidy (%)','land area (ha)',
         'production (kg)','yield (kg/ha)','Nfert (kg/ha)'/;
loop(i, put '2016', 'pongo', i.tl, fertsubsidy(i), landarea(i),
         q.l(i), simyield(i), Nuse(i), Nrate(i)/);

