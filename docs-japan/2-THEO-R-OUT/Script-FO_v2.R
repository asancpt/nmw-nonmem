DATA = Theoph
colnames(DATA) = c("ID", "BWT", "DOSE", "TIME", "DV")
DATA[,"ID"] = as.numeric(as.character(DATA[,"ID"]))

require(lattice)
xyplot(DV ~ TIME | as.factor(ID), data=DATA, type="b")

## INIT STEP
IDs = unique(DATA[,"ID"]) ; IDs
nID = length(IDs) ; nID

nTheta = 3
nEta = 3
nEps = 2
nPara = nTheta + nEta*(nEta + 1)/2 + nEps ; nPara

GNames = outer("G", 1:nEta, paste0)[1,] ; GNames
HNames = outer("H", 1:nEps, paste0)[1,] ; HNames

iOM = (nTheta + 1):(nTheta + nEta*(nEta + 1)/2) ; iOM
iSG = (nTheta + nEta*(nEta + 1)/2 + 1):(nTheta + nEta*(nEta + 1)/2 + nEps) ; iSG

IE = c(2, 50, 0.1)
LB = rep(0, nTheta)
UB = rep(1e6, nTheta)

alpha = 0.1 - log((IE - LB)/(UB - LB)/(1 - (IE - LB)/(UB - LB))) ; alpha

p0 = rep(0.1, nTheta)
p1 = exp(p0 - alpha)
p2 = p1/(p1 + 1)*(UB - LB) + LB ; p2

OM0 = matrix(c(0.2, 0.1, 0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.2), nrow=nEta) ; OM0
SG0 = diag(c(0.1, 0.1)) ; SG0

EXPP1 = exp(0.1)
ScaleVar = function(VarMat, dim1)
{
  M1 = chol(VarMat)
  V1 = diag(M1)
  M2 = abs(10 * (M1 - diag(V1, nrow=dim1))) + diag(V1/EXPP1, nrow=dim1)
  return(t(M2))
}
OMscl = ScaleVar(OM0, nEta) ; OMscl
SGscl = ScaleVar(SG0, nEps) ; SGscl

DesclVar = function(mUCP, mSCL)
{
  nRow = dim(mUCP)[1]
  maT = matrix(nrow=nRow, ncol=nRow)

  for (i in 1:nRow) {
    for (j in 1:nRow) {
      if (i==j) {
        maT[i,j] = exp(mUCP[i,j]) * mSCL[i,j]
      } else if(i > j) {
        maT[i,j] = mUCP[i,j] * mSCL[i,j]
      } else {
        maT[i,j] = 0
      }
    }
  }
  return(maT %*% t(maT))
}
DesclVar(matrix(rep(0.1, nEta*nEta), nrow=nEta), OMscl)
DesclVar(diag(rep(0.1, nEps)), SGscl)

ltv2mat = function(vec)
{
  LENGTH = length(vec)
  DIM = round((sqrt(8*LENGTH+1)-1)/2,0)
  if (DIM*(DIM+1)/2 != LENGTH) return(NULL)
  mat = matrix(nrow=DIM, ncol=DIM)
  for (m in 1:DIM) {
    for (n in 1:DIM) {
      k = max(m,n)
      l = min(m,n)
      p = k*(k - 1)/2 + l
      mat[m,n] = vec[p]
    }
  }
  return(mat)
}

s2o = function(p)
{
  p0 = p[1:nTheta]
  p1 = exp(p0 - alpha)
  p2 = p1/(p1 + 1)*(UB - LB) + LB
  OM = DesclVar(ltv2mat(p[iOM]), OMscl)
  SG = DesclVar(diag(p[iSG]), SGscl)
  Res = list(p2, OM, SG)
  names(Res) = c("THETA", "OMEGA", "SIGMA")
  return(Res)
}
s2o(rep(0.1, nPara))

## EST STEP
PRED = function(THETA, ETA)
{
# Extern: DATA[,c("ID", "TIME", "DV")], cID, DOSE 
  KA   = THETA[1]*exp(ETA[1])
  V    = THETA[2]*exp(ETA[2])
  K    = THETA[3]*exp(ETA[3])
  TIME = DATA[DATA$ID == cID, "TIME"]
  
  TERM1 = KA/(KA - K)
  TERM2 = K /(KA - K)
  TERM3 = 320/V*TERM1 
  TERM4 = exp(-K*TIME)
  TERM5 = exp(-KA*TIME) 

  F  = TERM3*(TERM4 - TERM5)
  G1 = -F*TERM2 + KA*TIME*TERM3*TERM5
  G2 = -F
  G3 = F*TERM2 - K*TIME*TERM3*TERM4
  H1 = F
  H2 = 1
  return(cbind(F, G1, G2, G3, H1, H2))
}

OBJ0 = function(p)
{
# External: DATA, nTheta, nEta, nEps
  if (STEP == "EST") {
    r = s2o(p)
    THETA = r$THETA
    OM = r$OMEGA
    SG = r$SIGMA
  } else {
    THETA = p[1:nTheta]
    OM = ltv2mat(p[iOM])
    SG = diag(p[iSG])
  }

  Oi = vector(length=nID)
  for (i in 1:nID) {
    cID <<- IDs[i] # for PRED
    FGH = PRED(THETA, rep(0, nEta))
    Fi = FGH[, "F"]
    Gi = FGH[, GNames, drop=FALSE]
    Hi = FGH[, HNames, drop=FALSE]
    Ri = DATA[DATA$ID == cID, "DV"] - Fi
    Ci = Gi %*% OM %*% t(Gi) + diag(diag(Hi %*% SG %*% t(Hi)))
    Oi[i] = determinant(Ci, logarithm=TRUE)$modulus[[1]] + t(Ri) %*% solve(Ci) %*% Ri
  }
  return(sum(Oi))
}
STEP = "EST"
OBJ0(rep(0.1, nPara))

StartTime = Sys.time()
r0 = optim(rep(0.1, nPara), OBJ0, method="L-BFGS-B")
difftime(Sys.time(), StartTime)
r0
OBJ0(r0$par)

r1 = s2o(r0$par) ; r1

## COV STEP
STEP = "COV"
p = c(r1$THETA, r1$OMEGA[upper.tri(r1$OMEGA, diag=TRUE)], diag(r1$SIGMA))
OBJ0(p)

require(numDeriv)
Rmat = hessian(OBJ0, p)/2 ; Rmat

OiS0 = function(p)
{
  THETA = p[1:nTheta]
  OM  = ltv2mat(p[iOM])
  SG  = diag(p[iSG])

  FGH = PRED(THETA, rep(0, nEta))  
  Fi  = FGH[, "F"]
  Gi  = FGH[, GNames, drop=FALSE]
  Hi  = FGH[, HNames, drop=FALSE]
  Ri  = DATA[DATA$ID == cID,"DV"] - Fi
  Ci  = Gi %*% OM %*% t(Gi) + diag(diag(Hi %*% SG %*% t(Hi)))
  return(determinant(Ci, logarithm=TRUE)$modulus[[1]] + t(Ri) %*% solve(Ci) %*% Ri)
}

CalcSmat = function(p)
{
  Smat = matrix(rep(0, nPara*nPara), nrow=nPara, ncol=nPara)
  for (i in 1:nID) {
    cID <<- IDs[i] # for PRED
    gr = grad(OiS0, p)
    Smat = Smat + gr %*% t(gr)
  }
  return(Smat/4)
}

Smat = CalcSmat(p) ; Smat
invR = solve(Rmat) ; invR
Cov = invR %*% Smat %*% invR ; Cov
SE = sqrt(diag(Cov)) ; SE
Correl = cov2cor(Cov) ; Correl
InvCov = Rmat %*% solve(Smat) %*% Rmat ; InvCov
EigenVal = sort(eigen(Correl)$values) ; EigenVal # Sorted Eigenvalues


## POSTHOC ETA (EBE)
ObjEta = function(ETA)
{
# External: THETA, invOM, SG, cID, DATA[,"DV"] 
  FGH = PRED(THETA, ETA)
  Ri  = DATA[DATA$ID == cID, "DV"] - FGH[,"F"]
  
  if (INTER == TRUE) {
    Hi = FGH[, HNames, drop=FALSE]
  } else {
    FGH0 = PRED(THETA, rep(0, nEta))
    Hi = FGH0[, HNames, drop=FALSE]
  }
  
## Slower version
  Vi = diag(diag(Hi %*% SG %*% t(Hi)))
  iSum = log(det(Vi)) + t(Ri) %*% solve(Vi) %*% Ri + t(ETA) %*% invOM %*% ETA

## Faster version
#  Vi    = diag(Hi %*% SG %*% t(Hi))
#  iSum = sum(log(Vi) + Ri*Ri/Vi) + t(ETA) %*% invOM %*% ETA

  return(iSum)
}

INTER = FALSE
THETA = r1$THETA
invOM = solve(r1$OMEGA)
SG = r1$SIGMA
EBE = cbind(ID=IDs, ETA1=0, ETA2=0, ETA3=0)
for (i in 1:nID) {
  cID <<- i
  EBE[i,2:4] = optim(rep(0, nEta), ObjEta, method="BFGS")$par
} ; EBE

## sdtab
SqrtInvCov = function(M)
{
  EigenResult = eigen(as.matrix(M))
  EigenVector = EigenResult$vectors
  EigenValues = abs(EigenResult$values)
  return(EigenVector %*% diag(1/sqrt(EigenValues)) %*% t(EigenVector))
}

OM = r1$OMEGA 
tSD = vector()
for (i in 1:nID) {
  cID  = IDs[i]
  FGH0 = PRED(THETA, rep(0, nEta))
  G0i  = FGH0[, GNames, drop=FALSE]
  H0i  = FGH0[, HNames, drop=FALSE]
  R0i  = DATA[DATA$ID == cID, "DV"] - FGH0[,"F"]
  C0i  = G0i %*% OM %*% t(G0i) + diag(diag(H0i %*% SG %*% t(H0i)))
  WRES = SqrtInvCov(C0i) %*% R0i

  tSD = rbind(tSD, cbind(DATA[DATA$ID == cID, c("ID", "TIME", "DV")], PRED=FGH0[,"F"], RES=R0i, WRES, G0i, H0i))
} ; tSD

