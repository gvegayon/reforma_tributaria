rm(list=ls())

# Parametros para la simulacion
uf            <- 23700
ingresos      <- seq(500000,6000000,by=50000)
pcompra       <- c(1000, 1500, 2000, 3000, 5000)
pcompra_sim   <- seq(1000, 5000, by=10)
tasa_retorno  <- seq(from=1.1,to=3,by=.2)
ingobjetivo   <- 564329 # Ingreso objetivo a comparar

# Tambien podemos asumir una tasa de retorno real
# en base a datos observados. Asumiendo un aumento de 3% anual a un plazo de
# 20 agnos.
tasa_retorno  <- seq(from=1.1,to=round(1.03^20, digits=2),by=.1)
tasa_retorno  <- c(1, 1.1,1.25,1.5,2)

# Rentas minimas para financiar los creditos a 20 agnos
# usando el simulador de SBIF (se toma la renta mas baja)
rentas_minimas <- c(1000, 128604*4,
  1500, 192905*4,
  2000, 257207*4,
  3000, 385572*4,
  5000, 642779*4)
rentas_minimas <- matrix(rentas_minimas, byrow=T, ncol=2)
rentas_minimas <- as.data.frame(rentas_minimas)
colnames(rentas_minimas) <- c("precio","ingreso")
modelo <- lm(precio~ingreso, data=rentas_minimas)

tasa_implicita <- function(income, pcompra, pventa) {
  impuesto_hoy <- impuesto(income)
  impuesto_ref <- impuesto(income + (pventa-pcompra)/12)
  
  return((impuesto_ref - impuesto_hoy)/pventa*100)
}

Rcpp::sourceCpp(code='
#include <Rcpp.h>
using namespace Rcpp;

/* Funcion que calcula impuesto */
// [[Rcpp::export]]
double impuesto(double income) {
  double btrate, trate, lbound;
  
  if      (income <= 567702 ) {
  btrate = 0;
  trate  = 0;
  lbound = 0;
  }
  else if (income <= 1261560) {
  btrate = 0;
  trate  = 0.04;
  lbound = 567702;
  }
  else if (income <= 2102600) {
  btrate = 0.022;
  trate  = 0.08;
  lbound = 1261560;
  }
  else if (income <= 2943640) {
  btrate = 0.0452;
  trate  = 0.135;
  lbound = 2102600;
  }
  else if (income <= 3784680) {
  btrate = 0.0709;
  trate  = 0.23;
  lbound = 2943640;
  }
  else if (income <= 5046240) {
  btrate = 0.1062;
  trate  = 0.304;
  lbound = 3784680;
  }
  else if (income <= 6307800) {
  btrate = 0.1557;
  trate  = 0.355;
  lbound = 5046240;
  }
  else                        {
  btrate = 0.1955;
  trate  = .355;
  lbound = 6307800;
  }
  
  /* Calculando el monto de impuesto */
  return (btrate*lbound + (income-lbound)*trate)*12;
}

/* Calcula el impuesto a pagar */
// [[Rcpp::export]]
NumericMatrix simula_impuesto(
  NumericVector ingresos,
  NumericVector pcompra,
  NumericVector tasa_retorno,
  double uf = 23700.0
){

  int Ni = ingresos.size();
  int Np = pcompra.size();
  int Nt = tasa_retorno.size();
  double t0 = 0.0;
  double tT = 0.0;
  double pventa = 0.0;

  NumericMatrix out(Ni*Np*Nt,5);
 
  int j=-1;
  for(int i=0;i<Ni;i++)
    for(int pc=0;pc<Np;pc++)
      for(int pv=0;pv<Nt;pv++)
      {
        out(++j,0) = ingresos[i];
        out(j,1)   = pcompra[pc];
        out(j,2)   = tasa_retorno[pv]*pcompra[pc];
        out(j,3)   = tasa_retorno[pv];

        /* Calculando tasa */
        pventa = pcompra[pc]*tasa_retorno[pv]*uf;
        t0     = impuesto(ingresos[i]) ;
        tT     = impuesto(ingresos[i] + (pventa-pcompra[pc]*uf)/12.0) ;      

        out(j,4)   = (tT-t0)/pventa*100.0;
      }
  return out;

}
                  ', rebuild=TRUE)


resultados <- 
  simula_impuesto(ingresos,pcompra_sim,tasa_retorno)

resultados <- as.data.frame(resultados)
colnames(resultados) <- c("ingreso","pcompra","pventa","ganancia","tasa")
resultados$ganancia <- (resultados$ganancia-1)*100
resultados <- resultados[with(resultados, order(pcompra,ingreso)),]
# View(subset(resultados, round(ganancia)==10 & pcompra==1000))
# Corrigiendo
resultados$credito_max <- predict(modelo, resultados)
resultados <- subset(resultados, pcompra<=credito_max)

###############################################################################
# Grafico
###############################################################################

# Colores
colores <- terrain.colors(length(pcompra))
etiqx   <- range(ingresos)
etiqx   <- round(seq(etiqx[1],etiqx[2], length.out=6), digits=0)
etiqxl  <- formatC(etiqx/1000, format="f",big.mark=".",digits=0)

etiqy   <- range(resultados$tasa)
etiqy   <- round(seq(etiqy[1], etiqy[2], length.out=6),digits=2)
etiqyl  <- paste(
  formatC(etiqy, format="f", digits=2), 
  "%", sep="")

pdf("tasa_implicita_impuesto_2da_vivienda.pdf", paper="letter")
old.par <- par(no.readonly = TRUE)
par(mfcol=c(3,2), cex=.7, las=1, mai=c(.55,.75,.55,.2))
for (tr in tasa_retorno) {
	# Iniciando grafico
	plot(NULL, xlim=range(resultados$ingreso), ylim=range(resultados$tasa),
	ylab="", xlab="", axes=FALSE,
	main=sprintf("\nPrecio de Venta %s%%",formatC(tr*100, digits=0,format="f")))

	# Anadiendo lineas
	for (pc in 1:length(pcompra)) {
		pc2 <- pcompra[pc]
		r2 <- subset(resultados, ganancia==(tr-1)*100 & pcompra==pc2)
		with(r2, lines(y=tasa,x=ingreso, type="l", lty=pc))
	}
  
  # Linea de ingreso promedio
  abline(v=ingobjetivo)
  r2   <- subset(resultados, ganancia==(tr-1)*100)
	maxt <- max(r2$tasa[which(r2$ingreso <= 564329)])
  text(x=ingobjetivo, y=mean(range(etiqy))*1.5,
       labels=sprintf(
         "Tasa máxima para\ningreso de $%s: %.2f%%",
         formatC(ingobjetivo, big.mark=".", digits=0,
                 format="f"),
         maxt), pos=4)
  
	# Etiquetas
	axis(1,etiqx, etiqxl)
	axis(2,etiqy, etiqyl)
}

# Texto y etiqueta 
plot(NULL, xlim=c(0,100), ylim=c(0,100), axes=FALSE, ylab="", xlab="")
par(mai=rep(.4,4), oma=rep(.4,4), new=FALSE)

legend("top",
	legend=formatC(pcompra, big.mark=".", digits=0, format="f"), 
	lty=1:length(pcompra),
	title="Precio de compra (UF)")

text(50,5,
	expression("Tasa Implícita"~"="~frac(Delta~"Impuesto","Precio Venta")))

par(old.par)
par(oma=c(0,0,2,0))
mtext("Nivel de ingresos (miles de $)",1,line=4)
mtext("Tasa Implícita (% de precio venta)", 2,line=3)
mtext("Tasa de Implícita de impuesto por venta de segunda propiedad",
	3,line=5.2)

par(old.par)
dev.off()
system("evince tasa_implicita* &")

