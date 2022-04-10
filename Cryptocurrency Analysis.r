library(TSA)

BTC = read.csv("d:/shared drives/MA5781/Bitcoin.csv")
bitcoin.ts = ts(data=rev(BTC[,3]), frequency=365, start=c(2013, 119))

ETH = read.csv("d:/shared drives/MA5781/Ethereum.csv")
ethereum.ts = ts(data=rev(ETH[,3]), frequency=365, start=c(2015, 220))

LTC = read.csv("d:/shared drives/MA5781/Litecoin.csv")
litecoin.ts = ts(data=rev(LTC[,3]), frequency=365, start=c(2013, 119))

LINK = read.csv("d:/shared drives/MA5781/Chainlink.csv")
chainlink.ts = ts(data=rev(LINK[,3]), frequency=365, start=c(2017, 264))

par(mfrow=c(1,1))
plot(bitcoin.ts, type='l', log="y", ylim=c(0.1, 50000), main="Time Series Plot of Selected Cryptocurrencies", ylab="Dollars ($)", xlab="Time")
lines(ethereum.ts, col='red')
lines(litecoin.ts, col='blue')
lines(chainlink.ts, col='green')
legend('bottomright', legend=c('Bitcoin','Ethereum','Litecoin','Chainlink'), col=c('black','red','blue','green'), lty=1)

eacf(bitcoin.ts)
eacf(ethereum.ts)
eacf(litecoin.ts)
eacf(chainlink.ts)

dbitcoin.ts = na.omit(100*(bitcoin.ts-zlag(bitcoin.ts))/zlag(bitcoin.ts))
dethereum.ts = na.omit(100*(ethereum.ts-zlag(ethereum.ts))/zlag(ethereum.ts))
dlitecoin.ts = na.omit(100*(litecoin.ts-zlag(litecoin.ts))/zlag(litecoin.ts))
dchainlink.ts = na.omit(100*(chainlink.ts-zlag(chainlink.ts))/zlag(chainlink.ts))

plot(dbitcoin.ts, type='l', ylim=c(-100, 100), main="Daily Percent Change of Selected Cryptocurrencies", ylab="Percent (%)", xlab="Time")
lines(dethereum.ts, col='red')
lines(dlitecoin.ts, col='blue')
lines(dchainlink.ts, col='green')
legend('bottomright', legend=c('Bitcoin','Ethereum','Litecoin','Chainlink'), col=c('black','red','blue','green'), lty=1)

eacf(dbitcoin.ts)
eacf(dethereum.ts)
eacf(dlitecoin.ts)
eacf(dchainlink.ts)
mean(dbitcoin.ts); var(dbitcoin.ts)
mean(dethereum.ts); var(dethereum.ts)
mean(dlitecoin.ts); var(dlitecoin.ts)
mean(dchainlink.ts); var(dchainlink.ts)

btcacf = acf(bitcoin.ts, plot=FALSE, drop.lag.0=FALSE)
btcacf$lag = btcacf$lag*365
etcacf = acf(ethereum.ts, plot=FALSE, drop.lag.0=FALSE)
etcacf$lag = etcacf$lag*365
ltcacf = acf(litecoin.ts, plot=FALSE, drop.lag.0=FALSE)
ltcacf$lag = ltcacf$lag*365
ctcacf = acf(chainlink.ts, plot=FALSE, drop.lag.0=FALSE)
ctcacf$lag = ctcacf$lag*365

par(mfrow=c(2,2), oma=c(0, 0, 2, 0))
plot(btcacf, main="Bitcoin")
plot(etcacf, main="Ethereum")
plot(ltcacf, main="Litecoin")
plot(ctcacf, main="Chainlink")
mtext("Time Series ACF Plots", outer = TRUE, cex = 1.5)

pbtcacf = pacf(bitcoin.ts, plot=FALSE, drop.lag.0=FALSE)
pbtcacf$lag = pbtcacf$lag*365
petcacf = pacf(ethereum.ts, plot=FALSE, drop.lag.0=FALSE)
petcacf$lag = petcacf$lag*365
pltcacf = pacf(litecoin.ts, plot=FALSE, drop.lag.0=FALSE)
pltcacf$lag = pltcacf$lag*365
pctcacf = pacf(chainlink.ts, plot=FALSE, drop.lag.0=FALSE)
pctcacf$lag = pctcacf$lag*365

par(mfrow=c(2,2), oma=c(0, 0, 2, 0))
plot(pbtcacf, main="Bitcoin")
plot(petcacf, main="Ethereum")
plot(pltcacf, main="Litecoin")
plot(pctcacf, main="Chainlink")
mtext("Time Series PACF Plots", outer = TRUE, cex = 1.5)

dbtcacf = acf(dbitcoin.ts, plot=FALSE, drop.lag.0=FALSE)
dbtcacf$lag = dbtcacf$lag*365
detcacf = acf(dethereum.ts, plot=FALSE, drop.lag.0=FALSE)
detcacf$lag = detcacf$lag*365
dltcacf = acf(dlitecoin.ts, plot=FALSE, drop.lag.0=FALSE)
dltcacf$lag = dltcacf$lag*365
dctcacf = acf(dchainlink.ts, plot=FALSE, drop.lag.0=FALSE)
dctcacf$lag = dctcacf$lag*365

par(mfrow=c(2,2), oma=c(0, 0, 2, 0))
plot(dbtcacf, main="Bitcoin")
plot(detcacf, main="Ethereum")
plot(dltcacf, main="Litecoin")
plot(dctcacf, main="Chainlink")
mtext("Percent Change ACF Plots", outer = TRUE, cex = 1.5)

pdbtcacf = pacf(dbitcoin.ts, plot=FALSE, drop.lag.0=FALSE)
pdbtcacf$lag = pdbtcacf$lag*365
pdetcacf = pacf(dethereum.ts, plot=FALSE, drop.lag.0=FALSE)
pdetcacf$lag = pdetcacf$lag*365
pdltcacf = pacf(dlitecoin.ts, plot=FALSE, drop.lag.0=FALSE)
pdltcacf$lag = pdltcacf$lag*365
pdctcacf = pacf(dchainlink.ts, plot=FALSE, drop.lag.0=FALSE)
pdctcacf$lag = pdctcacf$lag*365

par(mfrow=c(2,2), oma=c(0, 0, 2, 0))
plot(pdbtcacf, main="Bitcoin")
plot(pdetcacf, main="Ethereum")
plot(pdltcacf, main="Litecoin")
plot(pdctcacf, main="Chainlink")
mtext("Percent Change PACF Plots", outer = TRUE, cex = 1.5)

#bitcoin
yt_btc = bitcoin.ts[3:length(bitcoin.ts)]
yt1_btc = bitcoin.ts[2:(length(bitcoin.ts)-1)]
yt2_btc = bitcoin.ts[1:(length(bitcoin.ts)-2)]
par(mfrow=c(2,2), oma=c(0, 0, 2, 0))
plot(yt_btc ~ yt1_btc, main="Time Series, Lag 1")
plot(yt_btc ~ yt2_btc, main="Time Series, Lag 2")

yt_dbtc = dbitcoin.ts[3:length(dbitcoin.ts)]
yt1_dbtc = dbitcoin.ts[2:(length(dbitcoin.ts)-1)]
yt2_dbtc = dbitcoin.ts[1:(length(dbitcoin.ts)-2)]
plot(yt_dbtc ~ yt1_dbtc, main="Percent Change, Lag 1")
plot(yt_dbtc ~ yt2_dbtc, main="Percent Change, Lag 2")
mtext("Bitcoin Lag Scatterplots", outer = TRUE, cex = 1.5)

#ethereum
yt_etc = ethereum.ts[3:length(ethereum.ts)]
yt1_etc = ethereum.ts[2:(length(ethereum.ts)-1)]
yt2_etc = ethereum.ts[1:(length(ethereum.ts)-2)]
par(mfrow=c(2,2), oma=c(0, 0, 2, 0))
plot(yt_etc ~ yt1_etc, main="Time Series, Lag 1")
plot(yt_etc ~ yt2_etc, main="Time Series, Lag 2")

yt_detc = dethereum.ts[3:length(dethereum.ts)]
yt1_detc = dethereum.ts[2:(length(dethereum.ts)-1)]
yt2_detc = dethereum.ts[1:(length(dethereum.ts)-2)]
plot(yt_detc ~ yt1_detc, main="Percent Change, Lag 1")
plot(yt_detc ~ yt2_detc, main="Percent Change, Lag 2")
mtext("Ethereum Lag Scatterplots", outer = TRUE, cex = 1.5)

#litecoin
yt_ltc = litecoin.ts[3:length(litecoin.ts)]
yt1_ltc = litecoin.ts[2:(length(litecoin.ts)-1)]
yt2_ltc = litecoin.ts[1:(length(litecoin.ts)-2)]
par(mfrow=c(2,2), oma=c(0, 0, 2, 0))
plot(yt_ltc ~ yt1_ltc, main="Time Series, Lag 1")
plot(yt_ltc ~ yt2_ltc, main="Time Series, Lag 2")

yt_dltc = dlitecoin.ts[3:length(dlitecoin.ts)]
yt1_dltc = dlitecoin.ts[2:(length(dlitecoin.ts)-1)]
yt2_dltc = dlitecoin.ts[1:(length(dlitecoin.ts)-2)]
plot(yt_dltc ~ yt1_dltc, main="Percent Change, Lag 1")
plot(yt_dltc ~ yt2_dltc, main="Percent Change, Lag 2")
mtext("Litecoin Lag Scatterplots", outer = TRUE, cex = 1.5)

#chainlink
yt_ctc = chainlink.ts[3:length(chainlink.ts)]
yt1_ctc = chainlink.ts[2:(length(chainlink.ts)-1)]
yt2_ctc = chainlink.ts[1:(length(chainlink.ts)-2)]
par(mfrow=c(2,2), oma=c(0, 0, 2, 0))
plot(yt_ctc ~ yt1_ctc, main="Time Series, Lag 1")
plot(yt_ctc ~ yt2_ctc, main="Time Series, Lag 2")

yt_dctc = dchainlink.ts[3:length(dchainlink.ts)]
yt1_dctc = dchainlink.ts[2:(length(dchainlink.ts)-1)]
yt2_dctc = dchainlink.ts[1:(length(dchainlink.ts)-2)]
plot(yt_dctc ~ yt1_dctc, main="Percent Change, Lag 1")
plot(yt_dctc ~ yt2_dctc, main="Percent Change, Lag 2")
mtext("Chainlink Lag Scatterplots", outer = TRUE, cex = 1.5)

