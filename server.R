library(shiny)
source("ea_loader.r")


shinyServer(function(input, output) {
                ## init agent DS if necessary
                if (!"EA.agent.ds" %in% ls()) {
                        EA.agent.ds <- EA.agent.dataSource();
                    }

                # this is the base (virtual) class that provides generic interfaces
                EA.agent <- setRefClass (
                    Class = "EA.agent",

                    fields = c ("stock", "period", "date", "hist", "model", "market", "fund", "risk", "financing", "trade"),

                    methods = list (

                        initialize = function (bbgID=NULL, ticker=NULL, feed.source = "HK", ...) {
                            ## ticker/bbgID control
                            if (is.null(bbgID)) {
                                bbgID <- EA.data3.lookupBBGID.equity (ticker, feed.source);
                            }

                            ticker <- EA.data3.lookupTicker.equity(bbgID);

                            stock <<- list(bbgID = bbgID, ticker = ticker,
                                           exchange = EA.data3.lookupExchangeInfo(bbgID));
                            #                           attrib   = EA.data3.dsm.ref$execSQL.generic.get ("equity_attrib", bbgID));

                            ## search for hedging instruments
                            ## default to index
                            stock$hedge <<- data.frame(BBG_ID = stock$exchange$index_BBG_ID,
                                                       round_lot_size = 1, stringsAsFactors=F);

                            date   <<- list();
                            model  <<- list();
                            market <<- list();
                            fund   <<- list(err = list());

                            risk <<- list(max.loss = -0.25, util.max = 2,
                                          dist.tail.drop.pct=0.005,
                                          cash = 100000);

                            trade <<- list();
                        },

                        init.period = function(p.use=NULL, refit.ic.market.model=F, reconfig.trade=F, refit.stockFund=F) {
                            ## load period data
                            periodData  <-EA.period.loadPeriodData(stock$bbgID, p.use);

                            periodDates <- periodData$dates;
                            period <<- list(dates   = periodDates,
                                            last    = tail(periodDates$period.last, 1),
                                            current = tail(periodDates$period, 1),
                                            next.date.a = tail(periodDates$date.a, 1));
                            period$current.char <<- as.character(period$current);

                            ## XXXX for now, save everyting into hist
                            hist <<- periodData;

                            if (length(hist$px) > 21) {
                                hist$px <<- tail(hist$px,21);
                            }

                            if (length(hist$con) > 21) {
                                hist$con <<- tail(hist$con,21);
                            }

                            # load period cal
                            date$fwd.cal <<- EA.period.genCal(period$dates, period$current);

                            ff.mm  <- NULL;

                            # add market IC base
                            #            d.period <- (filter(date$fwd.cal, progress.d > 0))$progress.d;
                            hist.p.use  <- hist$avail[which(hist$avail >= period$current-30 & hist$avail < period$current)];
                            hist.p.use.char  <- as.character(hist.p.use);

                            hist.px.with.act  <- lapply(hist.p.use.char, function (p.name) {
                                rbind(hist$px[[p.name]][, c("progress.d", "s.PTD", "i.PTD")], c(1,hist$return.period[p.name,]))
                            })
                            names(hist.px.with.act)  <- hist.p.use.char;

                            market$ic <<- EA.stanObjManager.load.ic.market(stock$bbgID, period$current,
                                                                           filter(date$fwd.cal, progress.d > 0),
                                                                           load.simple = T,
                                                                           refit  = refit.ic.market.model,
                                                                           hist.px.with.act);
                            if (refit.ic.market.model) {
                                reconfig.trade  <- T;
                            }
                            trade  <<- EA.stanObjManager.load.tradingConfig(stock$bbgID, period$current,
                                                                            filter(date$fwd.cal, progress.d > 0),
                                                                            load.simple = T,
                                                                            reconfig = reconfig.trade);


                            ## add stock fund model
                            #            model  <<- EA.stanObjManager.load.stockFundModel(stock$bbgID, period$current, periodData, market$ic$beta.k,
                            #                                                             refit=refit.stockFund);
                            #            ## XXXX for now, put it here
                            #            model$target.FY  <<- EA.period.decodePeriod(period$current)$FY[1] + 1;

                            ## load actual into array
                            #            act <- EA.data3.dsm.ref$execSQL.generic.get ("financial_actual", stock$bbgID);
                            act <- data.frame();
                            if (nrow(act) > 0) {
                                act.measure  <- unique(act$measure);
                                act.FY  <- unique(act$FY);
                                act.sum  <- array(NA, dim=c(length(act.measure),length(act.FY),2));
                                dimnames(act.sum)  <- list(act.measure, act.FY, c("value", "growth"));
                                for (i in 1:nrow(act)) {
                                    act.sum[act$measure[i], as.character(act$FY[i]), "value"]  <- log(act$value[i]);
                                }

                                if(length(act.FY) > 1) {
                                    for (i in 2:length(act.FY)) {
                                        act.sum[,i,"growth"]  <- act.sum[,i,"value"] - act.sum[,i-1,"value"];
                                    }
                                }

                                fund$act.sum  <<- act.sum;
                            }

                            if (length(model$fit.m) > 0) {
                                con.sum.start <- array(NA, dim=c(length(model$fit.m), 2, 2));
                                dimnames(con.sum.start)  <- list(names(model$fit.m), c("mean", "sd"), c("value", "growth"));
                                con.sum.start[,,"value"]  <- t(sapply(model$fit.m, function (x) tail(summary(x$stock, pars="log_con")$summary[,c("mean", "sd")],1)));

                                ## div by 2 because we are on FY+1
                                if (!is.null(fund$act.sum)) {
                                    con.sum.start[,"mean","growth"]  <- (con.sum.start[,"mean","value"] -
                                                                             act.sum[dimnames(con.sum.start)[[1]],dim(act.sum)[2],"value"])/2;
                                    con.sum.start[,"sd",  "growth"]  <- con.sum.start[,"sd","value"]/2;
                                }

                                fund$con.sum.start  <<- con.sum.start;
                            }

                        },

                        init.day = function (setDate=NULL) {

                            # config date
                            if(is.null(setDate)) {
                                today <- as.Date(EA.util.epoch2LocalDateTime(as.numeric(Sys.time()), stock$exchange$time_zone));
                            } else {
                                today <- as.Date(setDate);
                            }

                            ## find today's d.progress
                            today.progress <- date$fwd.cal[which(date$fwd.cal$date == today), "progress.d"];

                            if(length(today.progress) == 0) {
                                # we are on a non-trading day, set today to next
                                if (length(date$fwd.cal[which(date$fwd.cal$date > today), "date"]) == 0) {
                                    # we are done with current period
                                    date <<- list (err = "end of period");
                                    return (NULL);
                                } else {
                                    today <- date$fwd.cal[which(date$fwd.cal$date > today), "date"][1];
                                }

                                today.progress <- date$fwd.cal[which(date$fwd.cal$date == today), "progress.d"];
                            }

                            days.to.eop.t <- nrow(date$fwd.cal %>% filter (date >= today));

                            date$days.to.eop.t  <<- days.to.eop.t;
                            date$days.to.eop.c  <<- as.numeric(tail(date$fwd.cal$date,1) - today)+1;
                            date$today          <<- today;
                            date$today.progress <<- today.progress;
                            date$today.N        <<- date$fwd.cal[which(date$fwd.cal$date == today), "progress.N"];

                            # find last trading day
                            if(today == date$fwd.cal$date[1]) {
                                last.trade.day <- tail(hist$px[[as.character(period$last)]]$date,1);
                            } else {
                                last.trade.day  <- date$fwd.cal$date[tail(which(date$fwd.cal$date<today),1)];
                            }
                            date$last.trade.day <<- last.trade.day;
                            date$last.trade.day.N <<- max(0, date$today.N-1);

                            # PTD alpha calibration
                            market.PTD <- hist$px[[period$current.char]] %>% filter (date < today) %>% select(date, progress.d, ts_epoch, px_close_adj_s, px_close_adj_i, log.px.s, log.px.i, s.PTD, i.PTD) %>% mutate (a.PTD = s.PTD - market$ic$beta.k * i.PTD);
                            names(market.PTD)[4:5]  <- c("px.s", "px.i");

                            ## add row for today if needed
                            if(today > tail(market.PTD$date,1)) {
                                market.PTD <- bind_rows(market.PTD, data.frame(date=today, progress.d=today.progress));
                            }

                            market$PTD <<- market.PTD;

                            market.d.i.today <- head(which(market$ic$d.ref == today.progress),1);
                            market$current <<- list(sum = data.frame(brokerID = "0",
                                                                     log.forecast.change = as.numeric(NA),
                                                                     ref.d    = market.d.i.today,
                                                                     stringsAsFactors=F),
                                                    data = market$ic$data);

                            ## load financing data
                            financing <<- setup.financing();
                        },

                        get.market.current.TR1D.stock = function () {
                            list(TR_1D_close = as.numeric(NA));
                        },
                        get.market.current.TR1D.index = function () {
                            list(TR_1D_close = as.numeric(NA));
                        },

                        get.market.current.hedge = function () {
                            NA;
                        },

                        get.market.current = function () {
                            # current alpha
                            ret1D.index <- get.market.current.TR1D.index();
                            ret1D.stock <- get.market.current.TR1D.stock();

                            alpha.1D <- ret1D.stock$TR_1D_close - market$ic$beta.k * ret1D.index$TR_1D_close;

                            # add to cumulative
                            tmp.mkt.yes <- select(filter(market$PTD, date == .self$date$last.trade.day),
                                                  i.PTD, s.PTD, a.PTD);

                            market$PTD[nrow(market$PTD), c("i.PTD", "s.PTD", "a.PTD")] <<-
                                tmp.mkt.yes + c(ret1D.index$TR_1D_close, ret1D.stock$TR_1D_close, alpha.1D, alpha.1D);
                            market$PTD$px.s[nrow(market$PTD)] <<- ret1D.stock$today$px_last;

                            ret  <- list(ret1D.index = ret1D.index,
                                         ret1D.stock = ret1D.stock,
                                         return.PTD  = tail(market$PTD,1)[, c("i.PTD", "s.PTD", "a.PTD")]);
                            names(ret$return.PTD) <- c("index", "stock", "alpha");

                            return(ret);

                            # update implied growth
                            market$implied.fund['con.now'] <<- market$implied.fund['con.start'] * exp(tail(market$PTD$a.PTD,1))
                            market$implied.fund['growth.now'] <<- log(market$implied.fund['con.now']/market$implied.fund['act.last'])/2;

                            # load return obj
                            ret <- market$current;
                            ret$ret1D.index <- ret1D.index;
                            ret$ret1D.stock <- ret1D.stock;

                            ret$sum$log.forecast.change <- market$PTD$a.PTD[nrow(market$PTD)];
                            ret$return.PTD <- tail(market$PTD,1)[, c("i.PTD", "s.PTD", "a.PTD")];
                            names(ret$return.PTD) <- c("index", "stock", "alpha");

                            ret;
                        },

                        get.fund.current = function (refit.ic.brokers=F) {
                            list(sum = data.frame(), data = NULL);
                        },

                        get.port.current = function() {
                            return(c(0,0,0));
                        },

                        ## daily, continuous compounding
                        setup.financing = function () {
                            list(ir.cash.lend    = 1e-9,
                                 ir.cash.borrow  = log(1.03)/365,
                                 ir.stock.borrow = log(1.02)/360,
                                 ir.index.borrow = log(1.01)/360);
                        },

                        compute.financing = function () {

                        },


                        action = function (trading.cost=0.0025, save.forecast.fit=T) {
                            # load current position
                            print ("loading port");
                            data.port <- get.port.current();

                            # load market data
                            print ("loading hedging inst data");
                            hedge.px.last <- get.market.current.hedge();

                            print ("loading market data");
                            data.mkt  <- get.market.current();

                            ## set date.ref.N (yes vs today)
                            date.ref.useYes  <- .self$action.forecast.useYesterday();
                            if (date.ref.useYes) {
                                date.ref.N  <- date$last.trade.day.N;
                            } else {
                                date.ref.N  <- date$today.N;
                            }

                            aux.gen.forecast.rho.N  <- function (name) {
                                ## set data access names
                                ic.data.name  <- paste("cum",name,sep="_");

                                dd  <- with(market$ic,
                                            list(N_p = data$N_p, N_d = 2,
                                                 cum_inst = data[[ic.data.name]][, c(date.ref.N, data$N_d)],
                                                 cum_inst_tilde1 = data.mkt$return.PTD[1,name],
                                                 progress_d = date$today.progress));

                                if (date.ref.N < trade[[name]]$rho.N$config) {
                                    dd$N_f  <- 1;
                                    hist.N.use  <- date.ref.N;
                                    stan.model  <- EA.stanObjManager.model.forecast.mo.rho0;
                                } else {
                                    if (name == "stock") {
                                        prior.param.name  <- "alpha";
                                    } else {
                                        prior.param.name  <- name;
                                    }

                                    hist.N.use  <- trade[[name]]$rho.N$config:date.ref.N;
                                    dd$N_f  <- length(hist.N.use);

                                    dd  <- c(dd, with (market$ic,
                                                       list(cum_inst1 = data[[ic.data.name]][, hist.N.use],
                                                            cum_inst2 = data[[ic.data.name]][, data$N_d],
                                                            p_pos_norm_k_beta_phi = market$ic$prior.param[hist.N.use, "k.phi", prior.param.name],
                                                            p_pos_norm_h_raw_mean = market$ic$prior.param[hist.N.use, "log.h.mu", prior.param.name],
                                                            p_pos_norm_k_beta_lambda = rep(4, dd$N_f),
                                                            p_pos_norm_h_raw_sd      = rep(1, dd$N_f))));

                                    if (dd$N_f > 1) {
                                        ts.name  <- paste (substring(name,1,1),"PTD", sep=".");
                                        dd$cum_inst_tilde1  <- merge (market$PTD, select (filter (date$fwd.cal, progress.N %in% hist.N.use), date))[,ts.name];
                                        dd$cum_inst_tilde1[dd$N_f]  <- data.mkt$return.PTD[1,name];
                                    } else {
                                        for (dd.f.name in c("p_pos_norm_k_beta_phi", "p_pos_norm_k_beta_lambda", "p_pos_norm_h_raw_mean",
                                                            "p_pos_norm_h_raw_sd", "cum_inst_tilde1")) {
                                            dd[[dd.f.name]] <- array(dd[[dd.f.name]],1);
                                        }
                                        dd$cum_inst1  <- matrix(dd$cum_inst1, ncol=1);
                                    }

                                    stan.model  <- EA.stanObjManager.model.forecast.mo.rhoNm;

                                }

                                if (dd$N_f > 2) {
                                    ff  <-  EA.stanEngine.sampling(stan.model, data=dd);
                                } else {
                                    ff  <-  EA.stanEngine.sampling(stan.model, data=dd, cores=1);
                                }

                                if (save.forecast.fit) {
                                    trade[[name]]$rho.N$forecast <<- ff;
                                }

                                if (dd$N_f == 1) {
                                    ff.i.pick  <- 1;
                                } else {
                                    ## required reporting: today, yes, min_sd
                                    ff.i.pick  <- c(dd$N_f-1, dd$N_f);

                                    if (dd$N_f > 3) {
                                        ## are there history that is useful to add?
                                        cum.inst.corr  <- cor(dd$cum_inst1);

                                        ff.i.pick.add  <- sapply(c(dd$N_f-1, dd$N_f), function (d.ref) {
                                            rho.sq.ref  <- summary(ff, pars=sprintf("rho_sq[%d]",d.ref))$summary[,"50%"];

                                            rho.seq.joint  <- sapply(1:(d.ref-1), function(i.prev) {
                                                rho.sq.prev  <- summary(ff, pars=sprintf("rho_sq[%d]",i.prev))$summary[,"50%"];
                                                rho.prev.ref  <- cum.inst.corr[i.prev, d.ref];
                                                (rho.sq.prev - 2 * sqrt(rho.sq.ref * rho.sq.prev) * rho.prev.ref + rho.sq.ref) / (1 - rho.prev.ref^2);
                                            })

                                            if (any(rho.seq.joint > rho.sq.ref)) {
                                                which.max(rho.seq.joint);
                                            } else {
                                                NULL;
                                            }
                                        })

                                        ff.i.pick  <- unique(c(ff.i.pick.add, ff.i.pick));
                                    }
                                }

                                ## XXXX close.date is an approx
                                return(lapply(ff.i.pick, function (i) {
                                    list(engine     = "rho.N",
                                         close.date = tail(date$fwd.cal$date,1) + 3,
                                         close.N    = market$ic$data$N_d,
                                         ref.N      = hist.N.use[i],
                                         ff.post    = extract(ff, pars=sprintf("cum_inst_tilde2[%d]",i))[[1]])}));
                            }

                            aux.gen.forecast.cum.flat  <- function (name) {
                                ## set data access names
                                ic.data.name  <- paste("cum",name,sep="_");

                                ## generate cum.flat forecast
                                if (!is.null(trade[[name]]$cum.flat$config)) {
                                    range.array  <- trade[[name]]$cum.flat$config
                                    fwd.forecast.range  <- NULL;
                                    for (i in 1:nrow(range.array)) {
                                        if (date.ref.N >= range.array[i,"from"] && date$today.N < range.array[i,"to"]) {
                                            fwd.forecast.range <- (date.ref.N+1):range.array[i,"to"];
                                            break;
                                        }
                                    }

                                    if (!is.null(fwd.forecast.range)) {
                                        dd  <- with(market$ic,
                                                    list(N_p = data$N_p, N_d = 2,
                                                         N_f = length(fwd.forecast.range),
                                                         cum_inst1 = data[[ic.data.name]][, date.ref.N],
                                                         cum_inst2 = data[[ic.data.name]][, fwd.forecast.range, drop=F],
                                                         cum_inst_tilde1 = data.mkt$return.PTD[1,name]));

                                        ff  <- EA.stanEngine.sampling(EA.stanObjManager.model.forecast.mo.f1m, data = dd);

                                        if (save.forecast.fit) {
                                            trade[[name]]$cum.flat$forecast  <<- ff;
                                        }

                                        return(lapply(1:dd$N_f, function (i) {
                                            close.N  <- fwd.forecast.range[i];
                                            list(engine = "cum.flat",
                                                 close.date = (filter(date$fwd.cal,progress.N == close.N))$date,
                                                 close.N    = close.N,
                                                 ref.N      = date.ref.N,
                                                 ff.post    = extract(ff, pars=sprintf("cum_inst_tilde2[%d]",i))[[1]])}));
                                    }
                                }
                                NULL;
                            }


                            aux.handle.ts  <- function (name) {
                                ret.default  <- list(trade=c(0,NA,NA));
                                names(ret.default$trade)  <- c("open.pos", "close.N", "engine");


                                # optimize over each forecast
                                fwd.process.list  <- c(aux.gen.forecast.cum.flat(name),aux.gen.forecast.rho.N(name));

                                oo  <- sapply(fwd.process.list, function (fwd.forecast) {
                                    post.period.alpha <- fwd.forecast$ff.post;
                                    days.fwd  <- as.numeric(fwd.forecast$close.date - date$today + 1);

                                    post.N <- length(post.period.alpha);
                                    tail.drop <- round(post.N*risk$dist.tail.drop.pct);
                                    dd <- list(N = post.N - 2 * tail.drop,
                                               period_return = post.period.alpha[(tail.drop+1):(post.N-tail.drop)],
                                               ir_cash_lend    = financing$ir.cash.lend    * days.fwd,
                                               ir_cash_borrow  = financing$ir.cash.borrow  * days.fwd,
                                               ir_stock_borrow = financing$ir.stock.borrow * days.fwd,
                                               ir_index_borrow = financing$ir.index.borrow * days.fwd,
                                               trading_cost    = trading.cost,
                                               util_bound_gain = risk$util.max,
                                               config_max_loss = risk$max.loss,
                                               cash_start = 1 + data.port['cap_adj'],
                                               w_s_done = data.port['w_s'],
                                               alpha_done = data.port['alpha_done'],
                                               alpha_now  = data.mkt$return.PTD[1,name]);

                                    if (name %in% c("alpha", "stock")) {

                                        ## adjust for bid/ask spead in alpha_now
                                        if (mean(post.period.alpha) >= data.mkt$return.PTD[1, name]) {

                                            ## long into ask
                                            if (data.mkt$ret1D.stock$today$px_ask != data.mkt$ret1D.stock$today$px_last &&
                                                data.mkt$ret1D.stock$today$px_ask != -1) {
                                                ## need to make adjustment
                                                dd$alpha_now <- dd$alpha_now + with (data.mkt$ret1D.stock$today, log(px_ask/px_last));
                                            }
                                        } else {
                                            ## short into bid
                                            if (data.mkt$ret1D.stock$today$px_bid != data.mkt$ret1D.stock$today$px_last &&
                                                data.mkt$ret1D.stock$today$px_bid != -1) {
                                                dd$alpha_now <- dd$alpha_now + with (data.mkt$ret1D.stock$today, log(px_bid/px_last));
                                            }
                                        }
                                    }

                                    dd$w_s_range <- EA.agent.util.computeOptRange(dd);
                                    opt <- optimizing (EA.stanObjManager.model.allocation, data=dd)

                                    opt$par['w_s'];
                                })

                                gc();

                                data.frame(name = name, trade.open = oo,
                                           close.N    = sapply(fwd.process.list, function (x) x$close.N),
                                           close.date = as.Date(sapply(fwd.process.list, function (x) x$close.date), origin="1970-1-1"),
                                           ref.N      = sapply(fwd.process.list, function (x) x$ref.N),
                                           engine     = sapply(fwd.process.list, function (x) x$engine),
                                           stringsAsFactors=F);

                            }

                            cbind(data.frame(ticker = stock$ticker, beta.k = market$ic$beta.k,
                                             open.N = date$today.N, open.d = date$today.progress, stringsAsFactors=F),
                                  do.call(rbind, lapply(c("alpha", "stock", "index"), aux.handle.ts)));

                        },

                        action.forecast.useYesterday  = function() {
                            F;
                        },

                        plot.IC = function (plot.alpha=T, plot.stock=F, plot.index=F, use.progress=T) {
                            if (!("alpha" %in% names(market$ic))) {
                                market$ic <<- EA.stanObjManager.load.ic.market(stock$bbgID, period$current,
                                                                               filter(date$fwd.cal, progress.d > 0),
                                                                               load.simple = F, refit  = F);
                            }

                            EA.vis.IC.market(.self, plot.stock=plot.stock, plot.alpha=plot.alpha, plot.index=plot.index, use.progress=use.progress);

                        },

                        get.fwd.corr = function (to.N=NULL, base.N=NULL) {
                            if (is.null(to.N)) {
                                to.N  <- market$ic$data$N_d;
                            }

                            if (is.null(base.N)) {
                                base.N  <- date$last.trade.day.N
                            }

                            sapply (c("alpha", "stock", "index"), function (name) {
                                x <- market$ic$data[[paste("cum", name,sep="_")]];
                                sapply((base.N+1):to.N, function (i) cor(cbind(x[,base.N], x[,i] - x[,base.N]))[2,1])
                            });
                        },

                        plot.fwd.corr  = function (to.N=NULL, base.N=NULL) {
                            fwd.corr.array  <- get.fwd.corr(to.N, base.N);

                            sapply(dimnames(fwd.corr.array)[[2]], function (name) {
                                dev.new();
                                plot(fwd.corr.array[, name], type="b", main = paste (stock$ticker, name));
                            });
                        },


                        plot.forecast = function (ts.name, type, fwd.N=NULL) {
                            if (type == "rho.N") {
                                fwd.N <- nrow(summary(trade[[ts.name]][[type]]$forecast, pars="fwd_inst_tilde")$summary);
                            } else if (type == "cum.flat") {
                                f.sum  <- summary(trade[[ts.name]][[type]]$forecast, pars="fwd_inst_tilde")$summary;

                                if (is.null(fwd.N)) {
                                    fwd.N  <- which.max(abs(f.sum[,"mean"]));
                                } else if (fwd.N < 0 || fwd.N > nrow(f.sum)) {
                                    print ("fwd.N out of range");
                                    return (NULL);
                                }
                            } else {
                                return (NULL);
                            }

                            f.post  <- extract(trade[[ts.name]][[type]]$forecast, pars=sprintf("fwd_inst_tilde[%d]",fwd.N))[[1]];

                            dev.new();
                            plot(ecdf(f.post));
                            abline(h=seq(0,1,0.05), col="gray");
                            f.post.range.round  <- round(range(f.post)/0.05) * 0.05;
                            abline(v=seq(f.post.range.round[1], f.post.range.round[2], 0.05), col="gray");
                            abline(v=0, col="red");

                        },

                        action.old = function (trading.cost=0.005) {

                            # load fund forecast
                            #            print ("loading fund");
                            #            data.fund <- get.fund.current();

                            # load current position
                            print ("loading port");
                            data.port <- get.port.current();

                            # load market data
                            print ("loading hedging inst data");
                            hedge.px.last <- get.market.current.hedge();

                            print ("loading market data");
                            data.mkt  <- get.market.current();

                            ## go through each strategy
                            exec.generic  <- function (name, range.list, func.gen.forecast, func.resolve.N.close=function(x) x) {
                                ret.default  <- list(trade=c(0,NA));
                                names(ret.default$trade)  <- c("open.pos", "close.N");

                                ## are we in range?
                                range.array  <- range.list[[name]];
                                active.trading.regions  <- NULL;
                                for (i in 1:nrow(range.array)) {
                                    if (date$today.N >= range.array[i,"from"] && date$today.N < range.array[i,"to"]) {
                                        active.trading.regions  <- range.array[i,];
                                        break;
                                    }
                                }

                                if (is.null(active.trading.regions)) {
                                    return (ret.default);
                                }

                                ## set
                                ic.data.name  <- paste("cum",name,sep="_");
                                if (name == "stock") {
                                    PTD.name  <- "s.PTD";
                                } else if (name == "index") {
                                    PTD.name  <- "i.PTD";
                                } else {
                                    PTD.name  <- "a.PTD";
                                }

                                ff  <- func.gen.forecast (active.trading.regions,name);
                                ff.post  <- extract(ff, pars="cum_inst_tilde2")[[1]];

                                oo <- sapply(1:dim(ff.post)[2], function (i) {
                                    post.period.alpha <- ff.post[,i];

                                    post.N <- length(post.period.alpha);
                                    tail.drop <- round(post.N*risk$dist.tail.drop.pct);
                                    dd <- list(N = post.N - 2 * tail.drop,
                                               period_return = post.period.alpha[(tail.drop+1):(post.N-tail.drop)],
                                               ir_cash_lend    = financing$cash.lend,
                                               ir_cash_borrow  = financing$cash.borrow,
                                               ir_stock_borrow = financing$stock.borrow,
                                               ir_index_borrow = financing$index.borrow,
                                               trading_cost    = trading.cost,
                                               util_bound_gain = risk$util.max,
                                               config_max_loss = risk$max.loss,
                                               cash_start = 1 + data.port['cap_adj'],
                                               w_s_done = data.port['w_s'],
                                               alpha_done = data.port['alpha_done'],
                                               alpha_now  = data.mkt$return.PTD[1,name]);

                                    if (name %in% c("alpha", "stock")) {

                                        ## adjust for bid/ask spead in alpha_now
                                        if (mean(post.period.alpha) >= data.mkt$return.PTD[1, name]) {

                                            ## long into ask
                                            if (data.mkt$ret1D.stock$today$px_ask != data.mkt$ret1D.stock$today$px_last &&
                                                data.mkt$ret1D.stock$today$px_ask != -1) {
                                                ## need to make adjustment
                                                dd$alpha_now <- dd$alpha_now + with (data.mkt$ret1D.stock$today, log(px_ask/px_last));
                                            }
                                        } else {
                                            ## short into bid
                                            if (data.mkt$ret1D.stock$today$px_bid != data.mkt$ret1D.stock$today$px_last &&
                                                data.mkt$ret1D.stock$today$px_bid != -1) {
                                                dd$alpha_now <- dd$alpha_now + with (data.mkt$ret1D.stock$today, log(px_bid/px_last));
                                            }
                                        }
                                    }

                                    dd$w_s_range <- EA.agent.util.computeOptRange(dd);
                                    opt <- optimizing (EA.stanObjManager.model.allocation, data=dd)

                                    opt$par['w_s'];
                                })

                                oo.abs  <- abs(oo)
                                if (any(oo.abs > 0.01)) {
                                    oo.abs.max.i  <- which.max(oo.abs);

                                    ret.default$trade['open.pos']  <- oo[oo.abs.max.i];
                                    ret.default$trade['close.N']   <- func.resolve.N.close(oo.abs.max.i);
                                }

                                ret.default$fit  <- ff;
                                ret.default$opt  <- oo;

                                ret.default;
                            }

                            exec.forecast.cum.flat  <- function (active.trading.regions, name) {
                                ic.data.name  <- paste("cum",name,sep="_");

                                dd  <- with(market$ic,
                                            list(N_p = data$N_p, N_d = 2,
                                                 N_f = active.trading.regions["to"] - date$today.N,
                                                 cum_inst1 = data[[ic.data.name]][, date$today.N],
                                                 cum_inst2 = data[[ic.data.name]][,(date$today.N+1):active.trading.regions["to"],drop=F],
                                                 cum_inst_tilde1 = data.mkt$return.PTD[1,name]));

                                EA.stanEngine.sampling(EA.stanObjManager.model.forecast.mo.f1m, data = dd);
                            }

                            exec.resolve.N.cum.flat  <- function (x) {
                                date$today.N + x;
                            }

                            exec.forecast.rho0  <- function (active.trading.regions, name) {
                                ic.data.name  <- paste("cum",name,sep="_");

                                dd  <- with(market$ic,
                                            list(N_p = data$N_p,
                                                 cum_inst_p = data[[ic.data.name]][, "1"],
                                                 cum_inst_tilde1 = data.mkt$return.PTD[1,name]));
                                EA.stanEngine.sampling(EA.stanObjManager.model.forecast.mo.rho0, data = dd,cores=1);
                            }

                            exec.resolve.N.rho0  <- function (x) {
                                market$ic$data$N_d;
                            }

                            exec.forecast.rho  <- function (active.trading.regions, name) {
                                ic.data.name  <- paste("cum",name,sep="_");

                                if (name == "stock") {
                                    prior.param.name  <- "alpha";
                                } else {
                                    prior.param.name  <- name;
                                }

                                dd  <- with (market$ic,
                                             list(N_p = data$N_p, N_d = 2,
                                                  cum_inst = data[[ic.data.name]][, c(date$today.N, data$N_d)],
                                                  cum_inst_tilde_PTD = data.mkt$return.PTD[1,name],
                                                  p_pos_norm_k_beta_phi = market$ic$prior.param[date$today.N, "k.phi", prior.param.name],
                                                  p_pos_norm_k_beta_lambda = 4,
                                                  p_pos_norm_h_raw_mean = market$ic$prior.param[date$today.N, "log.h.mu", prior.param.name],
                                                  p_pos_norm_h_raw_sd   = 1));

                                EA.stanEngine.sampling(EA.stanObjManager.model.aggForecast.m, data=dd, cores=1);

                            }

                            exec.resolve.N.rho  <- function (x) {
                                market$ic$data$N_d;
                            }

                            trade.target.sum.list  <- lapply(names(market$ic$trading$strats), function (strat.name) {
                                strat.range.list  <- market$ic$trading$strats[[strat.name]];

                                ## dispatch
                                if(strat.name == "rho") {
                                    ret  <- lapply(names(strat.range.list), exec.generic,strat.range.list, exec.forecast.rho, exec.resolve.N.rho);
                                } else if(strat.name == "rho0") {
                                    ret  <- lapply(names(strat.range.list), exec.generic,strat.range.list, exec.forecast.rho0, exec.resolve.N.rho0);
                                } else if (strat.name == "cum.flat") {
                                    ret  <- lapply(names(strat.range.list), exec.generic,strat.range.list, exec.forecast.cum.flat, exec.resolve.N.cum.flat);
                                }
                                names(ret)  <- names(strat.range.list);

                                .self$trade[[strat.name]]  <- ret;

                                cbind(data.frame(strat=strat.name, inst = names(strat.range.list), stringsAsFactors=F),
                                      t(sapply(ret, function(x) x$trade)));
                            });

                            ret  <- do.call(rbind,trade.target.sum.list);

                            return (ret);




                            ## check anything to do
                            trade.cum.flat  <- exec.cum.flat(market$ic$trading$strats$cum.flat);

                            recover();


                            # create forecast
                            period.alpha.forecast <- gen.aggForecast(data.mkt, data.fund);
                            if (!is.null(period.alpha.forecast$err)) {
                                print (period.alpha.forecast$err);
                                return (NULL);
                            }

                            # period alpha forecast
                            post.period.alpha    <- extract(period.alpha.forecast$fit, pars="current_alpha_period")[[1]] + mean.adj;

                            # prepare data for opt/decision making
                            post.N <- length(post.period.alpha);
                            tail.drop <- round(post.N*risk$dist.tail.drop.pct);
                            dd <- list(N = post.N - 2 * tail.drop,
                                       period_return = post.period.alpha[(tail.drop+1):(post.N-tail.drop)],
                                       ir_cash_lend    = financing$cash.lend,
                                       ir_cash_borrow  = financing$cash.borrow,
                                       ir_stock_borrow = financing$stock.borrow,
                                       ir_index_borrow = financing$index.borrow,
                                       trading_cost    = trading.cost,
                                       util_bound_gain = risk$util.max,
                                       config_max_loss = risk$max.loss,
                                       cash_start = 1 + data.port['cap_adj'],
                                       w_s_done = data.port['w_s'],
                                       alpha_done = data.port['alpha_done'],
                                       alpha_now  = data.mkt$return.PTD[1,'stock']);

                            ## adjust for bid/ask spead in alpha_now
                            if (summary(period.alpha.forecast$fit, pars="current_alpha_period")$summary[, "mean"] >= data.mkt$return.PTD[1, "alpha"]) {
                                ## long into ask
                                if (data.mkt$ret1D.stock$today$px_ask != data.mkt$ret1D.stock$today$px_last &&
                                    data.mkt$ret1D.stock$today$px_ask != -1) {
                                    ## need to make adjustment
                                    dd$alpha_now <- dd$alpha_now + with (data.mkt$ret1D.stock$today, log(px_ask/px_last));
                                }
                            } else {
                                ## short into bid
                                if (data.mkt$ret1D.stock$today$px_bid != data.mkt$ret1D.stock$today$px_last &&
                                    data.mkt$ret1D.stock$today$px_bid != -1) {
                                    dd$alpha_now <- dd$alpha_now + with (data.mkt$ret1D.stock$today, log(px_bid/px_last));
                                }
                            }

                            dd$w_s_range <- EA.agent.util.computeOptRange(dd);

                            opt <- optimizing (EA.stanObjManager.model.allocation, data=dd)


                            ## consider forward time structure and direction
                            ## if we are scaling down, do all immeidately
                            ## if we are non-informative zone, do 50%
                            ## if we are in informative zone and we are betting against under-reaction, do all
                            ##
                            ## over-reaction:
                            ##  - market in informative zone
                            ##  - market_alpha_PTD * target.exp < 0
                            ## do so near peak of sd_x

                            c(list(fit.alpha.period     = period.alpha.forecast,
                                   post.fwd.alpha.gross = post.period.alpha - dd$alpha_now,
                                   opt  = list(opt=opt, data = dd),
                                   data.mkt = data.mkt,
                                   trade.ref = rbind(data.mkt$ret1D.index$today, data.mkt$ret1D.stock$today, hedge.px.last)),
                              gen.trade(opt$par['w_s'], data.mkt$ret1D.stock$today$px_last,
                                        data.mkt$return.PTD$index, hedge.px.last$px_last,
                                        mean(post.period.alpha), dd$alpha_now, data.port));
                        }
                    )
                    )

                    EA.agent.util.computeOptRange <- function (dd) {
                    with(dd, {
                        ## cash return contrained by max_loss
                        ## cash_start (default = 1)
                        ## cash_keep  = cash_start - cash_spend
                        ## cash_spend = (stock_delta + stock_done) * adj_cost_open
                        ## stock_fut  = (stock_delta * stock_done * exp(net_fwd_wealth)) * exp(fwd_return - financing)
                        ## cash_spend_fut = stock_fut * adj_cost_close
                        ## cash_keep_fut  = cash_keep
                        ## cash_fut  = cash_spend_fut + cash_keep_fut
                        ## cash_fut  = exp(max_loss)

                        ## net_fwd_return  = fwd_return-financing
                        ## net_done_return = (alpha_now - alpha_done) - financing_done

                        ## cash_start - (stock_delta + stock_done) * adj_cost_open + (stock_delta + stock_done * exp(net_done_return)) * exp(net_fwd_return) * adj_cost_close = exp(max_loss)
                        ## cash_start - stock_delta * adj_cost_open - stock_done * adj_cost_open + stock_delta * exp(net_fwd_return) * adj_cost_close + stock_done*exp(net_done_return) * exp(net_fwd_return) * adj_cost_close = exp(max_loss)

                        ## cash_start + stock_done(exp(net_done_return+net_fwd_return) * adj_cost_close - adj_cost_open) +
                        ##   stock_delta * (exp(net_fwd_return)*adj_cost_close - adj_cost_open) = exp(max_loss)
                        ##
                        ## stock_delta * (exp(net_fwd_return)*adj_cost_close - adj_cost_open) =
                        ##  exp(max_loss) - cash_start - stock_done(exp(net_done_return+net_fwd_return) * adj_cost_close - adj_cost_open)

                        ## stock_delta = (exp(max_loss) - cash_start - stock_done(exp(net_done_return+net_fwd_return) * adj_cost_close - adj_cost_open))/(exp(net_fwd_return)*adj_cost_close - adj_cost_open)

                        w_s_done_exp_alpha_sofar <- w_s_done * exp(alpha_now - alpha_done);
                        adj_trading_cost_buy <- 1+trading_cost;
                        adj_trading_cost_sell <- 1-trading_cost;

                        fwd_return <- period_return - alpha_now;
                        net_fwd_return_long <- fwd_return-ir_index_borrow;
                        net_fwd_return_short <- fwd_return+ir_stock_borrow;
                        net_fwd_wealth_long <- exp(net_fwd_return_long)  * adj_trading_cost_sell;
                        net_fwd_wealth_short <- exp(net_fwd_return_short) * adj_trading_cost_buy;

                        fwd_return_max <- max(fwd_return);
                        fwd_return_min <- min(fwd_return);
                        net_done_return <- alpha_now - alpha_done;

                        if (w_s_done < 0) {
                            done_mtm_adj <- w_s_done * (exp(net_done_return) * adj_trading_cost_buy  - adj_trading_cost_sell);
                        } else if (w_s_done > 0) {
                            done_mtm_adj <- w_s_done * (exp(net_done_return) * adj_trading_cost_sell - adj_trading_cost_buy);
                        } else {
                            done_mtm_adj <- 0;
                        }

                        if (fwd_return_max > log(adj_trading_cost_sell/adj_trading_cost_buy)) {
                            denom <- max(net_fwd_wealth_short) - adj_trading_cost_sell;

                            # first we assumed to close existing and re-open
                            nom <- exp(config_max_loss) - cash_start - done_mtm_adj;
                            w_s_lower <- nom/denom;

                            if (w_s_done < 0 && w_s_lower < w_s_done) {
                                # we don't need to close existing
                                X_stock_done_term <- w_s_done_exp_alpha_sofar * max(net_fwd_wealth_short) - w_s_done * adj_trading_cost_sell;
                                nom <- exp(config_max_loss) - cash_start - X_stock_done_term;
                                w_s_delta_lower <- nom/denom;
                                w_s_lower       <- w_s_delta_lower + w_s_done
                            }
                        } else {
                            w_s_lower <- 100;
                        }

                        ## for upper_bound, max_loss happens when max_lower_stock_return,
                        ## we buy stock then sell
                        ## assume borrowing rate = 0 for now
                        if (fwd_return_min < log(adj_trading_cost_sell/adj_trading_cost_buy)) {
                            tmp_min_fwd_wealth <- min(net_fwd_wealth_long);
                            denom <- tmp_min_fwd_wealth - adj_trading_cost_buy;

                            # first we assumed to close existing and re-open
                            nom <- exp(config_max_loss) - cash_start - done_mtm_adj;
                            w_s_upper <- nom/denom;

                            if (w_s_done > 0 && w_s_upper > w_s_done) {
                                # we don't need to close existing
                                X_stock_done_term <-  w_s_done_exp_alpha_sofar * tmp_min_fwd_wealth - w_s_done * adj_trading_cost_buy;
                                nom <- exp(config_max_loss) - cash_start - X_stock_done_term;

                                w_s_delta_upper <- nom/denom;
                                w_s_upper       <- w_s_delta_upper + w_s_done;
                            }

                            ## consider leverage financing
                            ## cash_keep_fut = cash_keep * cash_borrow_rate
                            ##               = (cash_start -s * Co) * exp(b)
                            ## cash_spend_fut = s * exp(r) * Cc
                            ## (cash_start - s * Co) * exp(b) + s * exp(r) * Cc = exp(L)
                            ## s * exp(r) * Cc - s * Co * exp(b) + cash_start * exp(b) = exp(L)
                            ## s(exp(r)*Cc - Co * exp(b)) = exp(L) - cash_start * exp(b)
                            ## s = (exp(L) - cash_start * exp(b))/(exp(r)*Cc - Co * exp(b))
                            if (w_s_upper > cash_start) {
                                w_s_upper <- (exp(config_max_loss)-cash_start * exp(ir_cash_borrow))/
                                    (tmp_min_fwd_wealth - adj_trading_cost_buy * exp(ir_cash_borrow));

                                if (w_s_upper < cash_start) {
                                    w_s_upper <- cash_start;
                                }
                            }
                        } else {
                            w_s_upper <- -100;
                        }

                        c(w_s_lower, w_s_upper);
                    })
                    }

                EA.vis.alphaFundModel <- function(ff) {
                    measure.i <- ff$measure.link.i;
                    if (is.null(measure.i)) {
                        measure.i <- ff$data$I_con_measure_alpha_link;
                    }

                    print (measure.i);

                    if (mode(ff$fit) == "list") {
                        if (length(ff$fit) == 1) {
                            stanFit <- ff$fit$fixed;
                        } else {
                            stanFit <- ff$fit$gp;
                        }
                    } else {
                        stanFit <- ff$fit;
                    }

                    post.change.con <- do.call("cbind", extract(stanFit, pars=sprintf("log_change_con[%d,%d]",
                                                                                      measure.i, 1:ff$data$N_p)))


                    post.alpha <- extract(stanFit, pars="mkt_alpha")[[1]];

                    sum.change.con <- with (ff,summary(stanFit,pars=sprintf("log_change_con[%d,%d]",
                                                                            measure.i, 1:data$N_p))$summary[, "50%"])
                    sum.alpha <- summary(stanFit, pars="mkt_alpha")$summary[, "50%"];

                    data.range <- range(c(post.change.con, post.alpha));

                    plot(-1,-1, ylim=data.range, xlim=data.range, xlab="con change", ylab="alpha");


                    sapply (1:(dim(post.alpha)[2]), function (x) {
                        points (post.change.con[, x], post.alpha[, x], pch=8, cex=0.2, col="blue");
                    });

                    points (sum.change.con, sum.alpha, pch=19, col="green");
                    text (sum.change.con, sum.alpha, cex=1.5, pos=round(runif(1,1,4)), lab=1:(dim(post.alpha)[2]), col="orange");

                    EA.vis.aux.plot.grid();
                    abline (h=0, v=0);
                    abline (b=1, a=0, col="red");
                }

                EA.vis.alphaFundModel.plotFitVsData.market <- function (ff, type) {

                    if (type == "stock") {
                        fit.par.name = "log_px_k[1,%d]";
                    } else if (type == "index") {
                        fit.par.name = "log_px_k[2,%d]";
                    } else {
                        return (NULL);
                    }

                    y.ext  <- summary(ff$fit, pars=sprintf(fit.par.name, 1:(ff$data$N_p+1)))$summary[, "mean"];
                    y.data <- ff$data$log_px_obs[,,type]
                    if (ff$data$last_px_obs_N < ff$data$N_mkt_obs) {
                        y.data[ff$data$N_p+1, -(1:ff$data$last_px_obs_N)] <- NA;
                    }

                    y.range <- range(c(y.ext, y.data), na.rm=T)

                    plot(y.ext, ylim=y.range, type="b", main=type, pch=8, col="red");

                    with (ff$data, sapply (1:N_p, function (p.i) points(rep(p.i, N_mkt_obs), y.data[p.i, ], col="blue")));
                    with (ff$data, points(rep(N_p+1, last_px_obs_N), y.data[N_p+1, 1:last_px_obs_N], col="blue"));

                }

                EA.vis.alphaFundModel.plotFitVsData.con <- function (ff, type) {
                    #    y.ext <- summary(ff$fit, pars="log_con")$summary[, "mean"];
                    #    y.data.list <- with(ff$data, lapply (1:(N_p+1), function (p.i) log_con_obs[p.i, 1:N_con_comp_act[p.i], "EPS"]));
                    y.ext <- summary(ff$fit, pars=sprintf("log_con[%d,1]", 1:(ff$data$N_p+1)))$summary[, "mean"];
                    y.data.list <- with(ff$data, lapply (1:(N_p+1), function (p.i) log_con_obs[p.i, 1:N_con_comp_act[p.i], "REV"]));

                    plot(y.ext, ylim=range(c(y.ext, unlist(y.data.list))), type="b", main="con EPS", col="red", pch=8);

                    with(ff$data, lapply (1:(N_p+1), function (p.i) points(rep(p.i, length(y.data.list[[p.i]])), y.data.list[[p.i]], col="blue")));
                    lines(sapply(y.data.list, mean), type="b", col="purple");

                    y.data.list

                }



                EA.vis.aux.plot.grid <- function () {
                    abline (h=(-20:20)/20, col="gray", lty=2);
                    abline (h=(-10:10)/10, col="gray");
                    abline (v=(-20:20)/20, col="gray", lty=2);
                    abline (v=(-10:10)/10, col="gray");
                }

                EA.vis.periodAlphaForecast <- function(period.alpha.forecast) {
                    post.period <- extract(period.alpha.forecast$fit, pars="current_tilde")[[1]];


                    plot(ecdf(post.period));

                    EA.vis.aux.plot.grid();
                    abline(v = (period.alpha.forecast$comp %>% filter (brokerID != 0))$log.forecast.change,
                           col="blue");

                    if ("0" %in% period.alpha.forecast$comp$brokerID) {
                        abline (v=(period.alpha.forecast$comp %>% filter (brokerID == 0))$log.forecast.change,
                                col="red");
                    }

                    abline(h=c(0.1, 0.9), col="purple", lty=2);
                }



                EA.vis.plotIntraday <- function (agent, lookback.length=2, plot.all=F) {

                    alpha.to.yes <- tail(agent$market$alpha.PTD$model, 2)[1];

                    aux.get <- function (bbgID, prev.name) {
                        data.intra <- EA.data3.dsm.ref$execSQL.generic.get ("live_market_equity_intraday", bbgID, order.by.dir = "ts_epoch");
                        if (nrow(data.intra) == 0) {
                            return (NULL);
                        }

                        ## filter out prev day(s)
                        data.intra <- filter (data.intra, ts_epoch >= agent$date$fwd.cal[max(1, which(agent$date$fwd.ca$Date == agent$date$today) - lookback.length - 1), "epoch.ref"]);

                        if (nrow(data.intra) == 0) {
                            return (NULL);
                        }

                        ## filter out NA
                        data.intra <- filter (data.intra, px_ask > 0 & px_bid > 0);
                        if (nrow(data.intra) == 0) {
                            return (NULL);
                        }

                        ## compute 1D return
                        data.intra$px.mid <- apply (data.intra[, c("px_ask", "px_bid")], 1, mean);
                        data.intra$return1D <- with (agent$market[[prev.name]], log(data.intra$px.mid + today.div) - log(prev.px));

                        ## drop BBGID
                        data.intra[, -1];
                    }

                    data.s <- aux.get (agent$stock$bbgID, "px.prev.stock");
                    data.i <- aux.get (agent$stock$exchange$index_BBG_ID, "px.prev.index");

                    ## merge them
                    data.m <- merge (data.s, data.i, by="ts_epoch", suffixes=c(".s", ".i"));
                    data.m <- data.m %>% arrange (ts_epoch);
                    data.m$alpha.PTD <- with(data.m, alpha.to.yes + return1D.s - agent$model$beta * return1D.i);

                    ## summary stats
                    alpha.mean <- mean(data.m$alpha.PTD);
                    alpha.sd <- sd(data.m$alpha.PTD);
                    alpha.range <- range(data.m$alpha.PTD);

                    #    dev.new(width=7.25, height=5.5, ypos=500)
                    #    dev.new(width=12, height=5.5)
                    if (plot.all) {
                        data.plot <- data.m;
                    } else {
                        data.plot <- data.m %>% filter (ts_epoch >= agent$date$live.today.epoch.open);
                    }

                    with (data.plot,
                          plot(EA.util.epoch2LocalDateTime(ts_epoch, "Hongkong"), alpha.PTD, type="b",
                               main = sprintf ("%s mean = %4.4f sd = %4.4f range: %4.4f %4.4f", agent$stock$ticker, alpha.mean, alpha.sd,
                                               alpha.range[1], alpha.range[2]),
                               ylim=range(c(alpha.PTD, tail(agent$market$alpha.PTD$model,1)))));

                    abline(h=alpha.mean, col="red");
                    abline(h=c(-1,1) * alpha.sd  + alpha.mean, lty=2, col="red");
                    abline(h=c(-2,2) * alpha.sd  + alpha.mean, lty=3, col="red");

                    ## add real current
                    abline(h=tail(agent$market$alpha.PTD$model,1), col="blue");

                    print (tail(data.m$px.mid.s,1) * exp(range(data.m$alpha.PTD)-tail(data.m$alpha.PTD,1)));
                }

                EA.vis.plotAnalyst <- function (y, brokerID, y.alpha=T, d = NULL, basis=0) {

                    brokerID <- as.character(brokerID);

                    if (is.null(d)) {
                        d <- dimnames(y$fund$data.history[[brokerID]]$data$log_forecast_change)[[3]][
                            max(which(as.numeric(dimnames(y$fund$data.history[[brokerID]]$data$log_forecast_change)[[3]]) <=
                                          y$date$today.progress))];
                    } else {
                        d <- as.character(d);

                        if (!d %in% dimnames(y$fund$data.history[[brokerID]]$data$log_forecast_change)[[3]]) {
                            d <- dimnames(y$fund$data.history[[brokerID]]$data$log_forecast_change)[[3]][
                                max(which(as.numeric(dimnames(y$fund$data.history[[brokerID]]$data$log_forecast_change)[[3]]) <= d))];
                        }
                    }

                    active.FY <- names(which(y$fund$data.history[[brokerID]]$data$log_forecast_isActive_mpd[y$model$measure, , d] == 1));

                    with (y$fund$data.history[[brokerID]]$data, {

                        v.x <- log_forecast_change[y$model$measure, active.FY, d] + basis;

                        if (y.alpha) {
                            v.y <- y$model$fit$alpha.act[active.FY];
                        } else {
                            v.y <- act_change[y$model$measure, active.FY];
                        }

                        v.range <- range(c(v.x, v.y));

                        plot (v.x, v.y, ylim=v.range, xlim=v.range, type="b",
                              main = sprintf ("%s %s, %s %s", y$stock$ticker, EA.data3.lookupBroker.name(brokerID), brokerID, d));
                        text (v.x, v.y, lab = active.FY, pos=1, col="gray", cex=0.8);

                        #        hist(v.y - v.x);
                        #        abs(v.y - v.x)/abs(v.y);
                        abline(h=seq(-50,50,5)/100, col="gray");
                        abline(v=seq(-50,50,5)/100, col="gray");
                        abline(h=0, v=0, col="dark orange");
                        abline(b=1, a=0, col="red");

                        abline(h=tail(y$market$alpha.PTD$model,1), col="purple");
                        abline(v=y$fund$all$log.forecast.change[which(y$fund$all$brokerID == brokerID)]+basis, col="blue");
                    })
                }

                EA.vis.IC.mkt <- function (ic.obj, d.current=NULL, plot.prefix="") {

                    ff.ic <- ic.obj$fit;
                    dd <- ic.obj$data;
                    d.ref <- ic.obj$d.ref;

                    if (!is.null(d.current)) {
                        if (d.current %in% d.ref) {
                            d.current.i  <- which(d.ref == d.current);
                        } else {
                            if (d.current < d.ref[1]) {
                                d.current.i  <- 1;
                            } else if (d.current > tail(d.ref,1)) {
                                d.current.i  <- length(d.ref);
                            } else {
                                d.current.i  <- c(tail(which(d.ref < d.current),1), which(d.ref > d.current)[1]);
                            }
                        }
                    }


                    aux.plot.sdx.rho <- function (i, name) {
                        x <- summary(ff.ic, pars=sprintf("d_sum[%d,%d,%d]",i, 1:(dd$N_d-1),2))$summary[,"50%"];
                        y <- summary(ff.ic, pars=sprintf("d_sum[%d,%d,%d]",i, 1:(dd$N_d-1),3))$summary[,"50%"]^2;
                        #        z <- summary(ff.ic, pars=sprintf("mkt_pred_param[%d,%d,%d]",i, 1:(dd$N_d-1), dd$N_d+1))$summary[,"50%"];

                        range.xy <- range(c(x,y));
                        #        range.xy <- range(c(x,y,z));

                        dev.new();
                        plot(x,y,  xlab="sdx", ylab="rho", xlim=range.xy, ylim=range.xy, main=paste (plot.prefix, name), type="b");
                        #        lines(x,z, type="b", col="blue");
                        abline(b=1, a=0, col="red");
                        abline(h=1:9/10, col="gray");
                        abline(v=1:20/10, col="gray");

                        if (!is.null(d.current)) {
                            points(x[d.current.i], y[d.current.i], pch=8, col="green");
                            #            points(x[d.current.i], z[d.current.i], pch=8, col="green");
                        }
                    }

                    aux.plot.beta <- function (i, name) {
                        y <- summary(ff.ic, pars=sprintf("d_sum[%d,%d,%d]",i, 1:(dd$N_d-1),3))$summary[,c("50%","25%","75%")];

                        dev.new();
                        plot(d.ref[-length(d.ref)], y[,1], ylim=range(y), xlab="progress.d", ylab="d_beta",
                             main=paste (plot.prefix, name), type="b");
                        lines(d.ref[-length(d.ref)], y[,2], type="b", lty=2);
                        lines(d.ref[-length(d.ref)], y[,3], type="b", lty=2);
                        abline(h=1, col="red");

                        if(!is.null(d.current)) {
                            abline(v=d.current, col="purple");
                        }
                    }

                    #    aux.plot.sdx.rho(1,"index_k");
                    #    aux.plot.sdx.rho(2,"alpha_k");
                    aux.plot.sdx.rho(1,"index");
                    aux.plot.sdx.rho(2,"stock");
                    #    aux.plot.beta(1,"index");
                    #    aux.plot.beta(2,"alpha");

                    return (NULL);

                    comb.beta.1 <-
                        summary(ff.ic, pars=sprintf("d_alpha_comb_beta[%d,%d]",1:(dd$N_d-1),1:(dd$N_d-1)))$summary
                    dev.new();
                    plot(d.ref[-dd$N_d], comb.beta.1[,"50%"], type="b", ylim=range(comb.beta.1[, c("25%", "50%", "75%")]),
                         main = "alpha d_beta[d.latest]");
                    sapply(c("25%","75%"), function (x) lines(d.ref[-dd$N_d], comb.beta.1[,x], type="b", lty=2));

                    dev.new();
                    plot(d.ref[-dd$N_d],
                         sqrt(sapply (1:(dd$N_d-1), function (max.obs, fit.sum) {
                             sum.i <- which(apply(dd$alpha_comb_index, 1, function (x) all(x<=max.obs)));
                             max(fit.sum[sum.i, "50%"]);
                         }, summary(ff.ic, pars="d_alpha_comb_var_reduce")$summary) -
                             summary(ff.ic, pars="d_alpha_comb_var_reduce")$summary[1:(dd$N_d-1),"50%"]),
                         type="b", ylab="sd reducation spread");
                }


                EA.vis.IC.broker.singleM.fit  <- function(fit.obj, d.current=NULL, plot.prefix="") {
                    fit.m  <- fit.obj$fit;

                    aux.plot  <- function (param.vector, title, extract.fun) {
                        data.DF <- do.call("rbind", lapply (names(fit.m),function (m, d.p) {
                            data.frame(measure=m,
                                       d.progress = d.p,
                                       y = extract.fun(fit.m[[m]], param.vector))
                        }, fit.obj$d.ref));

                        dev.new();
                        print(ggplot(data=data.DF, aes(x=d.progress, y=y, group=measure, colour=variable)) +
                                  ggtitle(title) + geom_line(aes(color=measure)) + geom_point(aes(color=measure)))
                    }

                    aux.extract.median  <- function (fit.obj, param.vector) {
                        summary(fit.obj, pars=param.vector)$summary[, "50%"];
                    }

                    aux.extract.gr0  <- function (fit.obj, param.vector) {
                        xx <- extract(fit.obj, pars=param.vector);
                        recover();
                        sapply (xx, function (x) length(which(x>0))/length(x))
                    }

                    mkt.name  <- c("index", "alpha");
                    with(fit.obj$dataSTAN[[1]], {
                        for (x in 1:2) {
                            title.list <- paste(plot.prefix, mkt.name[x], c("/ signle_con median rho", "/ single_con rho Pr > 0"));
                            aux.plot(sprintf("d_sum[%d,%d,%d]",         x,1:N_d_f,3),       title.list[1], aux.extract.median);
                            aux.plot(sprintf("d_sum[%d,%d,%d]",         x,1:N_d_f,3),       title.list[2], aux.extract.gr0);

                            #            aux.plot(sprintf("mkt_pred_param[%d,%d,%d]",x,1:N_d_f,N_d_f+2), title.list[2], aux.extract.median);
                        }
                    })

                    ## show prediction
                    p.count  <- dim(fit.obj$dataSTAN[[1]]$log_forecast)[1];

                    sapply(names(fit.obj$fit), function (m) {
                        cdf.p  <- sapply (1:p.count, function (p) {
                            param.pred  <- sprintf("mkt_pred[2,%d]",p);
                            if (summary(fit.m[[m]], pars=param.pred)$summary[,"mean"] != -10) {
                                post.pred  <- extract(fit.m[[m]], pars=c(param.pred))[[1]];
                                alpha.sum  <- summary(fit.m[[m]], pars=sprintf("alpha[%d]",p))$summary[, c("2.5%","97.5%","50%")];

                                post.pred  <- post.pred[order(post.pred)];

                                sapply(alpha.sum, function (x) length(which(post.pred<x)))/length(post.pred);
                            } else {
                                rep(NA,3);
                            }
                        })

                        dev.new();
                        plot(-1,-1, ylim=c(0,1), xlim=c(1,p.count), xlab="p", ylab="Pr(pred <= act)", main=paste (plot.prefix,m));
                        sapply(1:p.count, function (i) {
                            lines(rep(i,2), cdf.p[1:2,i], type="b"); points(i, cdf.p[3,i], pch=8, col="blue");
                        });
                        abline(h=1:9/10, col="gray");
                        abline(h=c(0.05,0.95), col="red");
                    });
                }

                EA.vis.compare <- function (fit.obj.list, param.m, plot.title="") {

                    cdf.p  <- sapply (1:nrow(param.m), function (i) {
                        post.pred  <- extract(fit.obj.list[[1]], pars=param.m[i,1])[[1]];
                        alpha.sum  <- summary(fit.obj.list[[2]], pars=param.m[i,2])$summary[, c("2.5%","97.5%","50%")];
                        post.pred  <- post.pred[order(post.pred)];
                        sapply(alpha.sum, function (x) length(which(post.pred<x)))/length(post.pred);
                    })

                    dev.new();
                    plot(-1,-1, ylim=c(0,1), xlim=c(1,nrow(param.m)), xlab="p", ylab="Pr(pred <= act)", main=plot.title);
                    sapply(1:nrow(param.m), function (i) {
                        lines(rep(i,2), cdf.p[1:2,i], type="b"); points(i, cdf.p[3,i], pch=8, col="blue");
                    });
                    abline(h=2:8/10, col="gray");
                    abline(h=c(0.1,0.9), col="red");
                }


                EA.vis.IC.broker.RevProgressMap  <- function (f.period, d.ref=0:10/10, plot.title="") {
                    f.period.len  <- length(f.period);

                    plot(-1,-1, ylim=c(1,f.period.len), xlim=c(0,1), main=plot.title);
                    abline(h=1:f.period.len, col="gray");
                    abline(v=0:10/10, col="gray");

                    abline(v=d.ref, col="purple", lty=2);

                    plot.data.l  <- sapply (1:length(f.period), function (p.i) {
                        tmp.f.p  <- f.period[[p.i]];

                        plot.data <- NULL;  ## d.progress, p.i, type[NA -> all (NA); 0 -> no change; 1-> change
                        plot.data  <- cbind(tmp.f.p[, "d.progress"], p.i, as.numeric(abs(tmp.f.p[,"delta"]) > 0.01));
                    })

                    plot.data.all <- do.call("rbind", plot.data.l);
                    points(plot.data.all[, 1:2],
                           pch = 8, col = ifelse(is.na(plot.data.all[,3]), "black", ifelse(plot.data.all[,3] == 1, "blue", "orange3")));
                }

                EA.vis.IC.broker.RevProgressMap.old  <- function (f.period, d.ref=0:10/10, plot.title="") {
                    f.period.len  <- length(f.period);

                    plot(-1,-1, ylim=c(0,f.period.len+1), xlim=c(0,1), main=plot.title);
                    abline(h=1:f.period.len, col="gray");
                    abline(v=0:10/10, col="gray");

                    abline(v=d.ref, col="purple", lty=2);


                    plot.data.l  <- sapply (1:length(f.period), function (p.i) {
                        tmp.f.p  <- f.period[[p.i]];

                        plot.data <- NULL;  ## d.progress, p.i, type[NA -> all (NA); 0 -> no change; 1-> change

                        if (nrow(tmp.f.p) > 0) {
                            val.last  <- NULL;
                            for (d.i in 1:nrow(tmp.f.p)) {
                                val.now  <- tmp.f.p[d.i, -1];

                                if (is.null(val.last)) {
                                    val.changed  <- T;
                                } else {
                                    ## val state: all finite, partial finite, all NA
                                    ## (all finite | partial finite) -> all NA                        -> changed
                                    ##  all NA                       -> (all finite | partial finite) -> changed
                                    ## (all finite | partial finite) -> (all finite | partial finite) -> compare finite
                                    if (identical(val.last, val.now)) {
                                        val.changed  <- F;
                                    } else {
                                        val.delta  <- val.now - val.last;

                                        if (all(!is.finite(val.delta))) {
                                            ## either from/to is all NA
                                            val.changed  <- T;
                                        } else {
                                            val.changed  <- any(na.omit(val.delta) !=0);
                                        }
                                    }
                                }

                                new.row  <- c(tmp.f.p[d.i, "d.progress"], p.i,
                                              ifelse (all(!is.finite(val.now)), as.numeric(NA),
                                                      ifelse(val.changed, 1, 0)));


                                if (is.null(plot.data)) {
                                    plot.data  <- new.row;
                                } else {
                                    plot.data  <- rbind(plot.data, new.row);
                                }

                                val.last  <- val.now;
                            }
                        }

                        plot.data;
                    })

                    plot.data.all <- do.call("rbind", plot.data.l);
                    points(plot.data.all[, 1:2],
                           pch = 8, col = ifelse(is.na(plot.data.all[,3]), "black", ifelse(plot.data.all[,3] == 1, "blue", "orange3")));
                }

                EA.vis.IC <- function (ic.obj, d.current=NULL, check.pred=F, plot.prefix="") {

                    ff.ic <- ic.obj$fit;
                    dd <- ic.obj$data;

                    if (!is.null(ic.obj$config) && !is.null(ic.obj$config$info.start)) {
                        info.start <- ic.obj$config$info.start;
                    } else {
                        info.start <- NULL;
                    }

                    if (!is.null(d.current)) {
                        d.current.i <- tail(which.min(abs(ic.obj$d.ref - d.current)),1);
                    } else {
                        d.current.i <- NULL;
                    }

                    dev.new();
                    plot(summary(ff.ic, pars="sd_ref_x")$summary[, "50%"], summary(ff.ic, pars="rho_f")$summary[, "50%"],
                         type="b", main=paste(plot.prefix, "info vs sd_x"))
                    abline(b=1, a=0, col="red");
                    abline(h=0:10/10, col="gray");
                    abline(v=0:20/10, col="gray");
                    points(summary(ff.ic, pars="sd_ref_x")$summary[seq(5, dd$N_d, 5), "50%"],
                           summary(ff.ic, pars="rho_f")$summary[seq(5, dd$N_d, 5), "50%"], pch=8, col="blue");

                    if (!is.null(d.current.i)) {
                        points(summary(ff.ic, pars="sd_ref_x")$summary[d.current.i, "50%"],
                               summary(ff.ic, pars="rho_f")$summary[d.current.i, "50%"], pch=8, col="purple", cex=1.2);
                    }

                    rho.pct <- apply(extract(ff.ic, pars="rho_f")[[1]], 2, function(x) length(which(x>0))/length(x));
                    dev.new();
                    plot(rho.pct, type="b", main = paste(plot.prefix, "Pr(rho_f) > 0"));
                    abline(h=0.9, col="red");
                    abline(v=seq(0, dd$N_d, 5), col="gray");
                    if (!is.null(d.current.i)) {
                        abline(v=d.current.i, col="purple");
                    }

                    dev.new();
                    plot(summary(ff.ic, pars="d_noise_sd")$summary[, "50%"], type="b",
                         ylim = range(c(0, summary(ff.ic, pars="d_noise_sd")$summary[, c("2.5%", "50%", "97.5%")])),
                         main = paste(plot.prefix, "noise structure"));
                    lines(summary(ff.ic, pars="d_noise_sd")$summary[, "2.5%"],  type="b", lty=2);
                    lines(summary(ff.ic, pars="d_noise_sd")$summary[, "97.5%"], type="b", lty=2);
                    abline(h=0, col="red");
                    abline(h=1:20/20, col="gray");
                    abline(v=seq(5, dd$N_d, 5), col="gray");
                    if (!is.null(d.current.i)) {
                        abline(v=d.current.i, col="purple");
                    }

                    dev.new();
                    hist(apply(extract(ff.ic, pars="d_noise_sd")[[1]], 1, which.max),
                         main = paste(plot.prefix, "which.max(noise)"));
                    if (!is.null(d.current.i)) {
                        abline(v=d.current.i, col="purple");
                    }

                    dev.new();
                    plot(summary(ff.ic, pars="d_noise_rho")$summary[, "50%"], ylim=c(-1,1),
                         main = paste(plot.prefix, "noise rho structure"), type="b");
                    sapply (c("2.5%", "25%", "75%", "97.5%"), function (x)
                        lines(summary(ff.ic, pars="d_noise_rho")$summary[, x], lty=2));
                    abline(h=0, col="red");
                    if (!is.null(d.current.i)) {
                        abline(v=d.current.i, col="purple");
                    }

                    dev.new();
                    tmp.y <- summary(ff.ic, pars="rho_f")$summary[, "50%"] * dd$sd_ref;

                    plot.range <- range (c(0, dd$sd_ref, summary(ff.ic, pars="d_noise_sd")$summary[, "50%"], tmp.y));
                    plot(summary(ff.ic, pars="d_noise_sd")$summary[, "50%"], tmp.y,
                         type="b", main = paste(plot.prefix, "info vs noise"), xlim=plot.range, ylim=plot.range);

                    points(summary(ff.ic, pars="d_noise_sd")$summary[seq(5, dd$N_d, 5), "50%"], tmp.y[seq(5, dd$N_d, 5)],
                           pch=8, col="blue");

                    if (!is.null(d.current.i)) {
                        points(summary(ff.ic, pars="d_noise_sd")$summary[d.current.i, "50%"], tmp.y[d.current.i],
                               pch=8, col="purple", cex=1.2);
                    }
                    abline(h=0:20/20, col="gray");
                    abline(v=0:20/20, col="gray");
                    abline(b=-1, a=dd$sd_ref, col="red");
                    abline(h = dd$sd_ref, col="red");


                    if (check.pred) {
                        pred.z <- sapply (1:dd$N_d, function (d.x) {
                            p <- sprintf("act_check[%d,%d]", d.x, 1:dd$N_p)
                            (dd$act_alpha - summary(ff.ic, pars=p)$summary[, "mean"])/summary(ff.ic, pars=p)$summary[, "sd"];
                        });

                        dev.new();
                        plot(apply (pred.z, 2, mean), type="b", main = paste(plot.prefix, "mean of predicted z"));
                        abline(v=seq(5, 20, 5), col="gray");
                        abline(h=0, col="red");

                        dev.new();
                        plot(apply(abs(pred.z), 2, function (x) length(which(x > 1.75)))/dd$N_p, type="b",
                             main = paste(plot.prefix, "% of abs(pred z) > 1.75"));
                        abline(v=seq(5, 20, 5), col="gray");
                        abline(h=0.15, col="red");
                    }
                }

                EA.vis.IC.market <- function (y, plot.stock=T, plot.alpha=T, plot.index=F, plot.beta=F,use.progress=T) {

                    ff.ic.market <- y$market$ic;

                    if (use.progress) {
                        today.progress <- y$date$today.progress;
                    } else {
                        today.progress <- y$date$today.N;
                    }	


                    N.d.min.1  <- ff.ic.market$data$N_d-1;

                    if (use.progress) {
                        plot.x.ref <- ff.ic.market$d.ref[1:N.d.min.1];
                        plot.x.range  <- c(0,1);
                    } else {
                        plot.x.ref <- 1:N.d.min.1;
                        plot.x.range  <- c(1,N.d.min.1);
                    }

                    aux.plot <- function (name) {
                        ff.list  <- ff.ic.market[[name]];
                        ts.ref  <- ff.ic.market$data[[paste("cum",name,sep="_")]];

                        N  <- ncol(ts.ref);

                        x.var      <- apply(ts.ref, 2, function(x) mean(x^2));
                        x.cor      <- cor(ts.ref);
                        x.cor.sq.m <- x.cor[N,1:(N-1)]^2;

                        x.var.x  <- x.var[1:(N-1)]/x.var[N];

                        if (use.progress) {
                            rho.N.progress <- y$date$fwd.cal[which(y$date$fwd.cal$progress.N == y$trade[[name]]$rho.N$config),"progress.d"];
                        } else {
                            rho.N.progress <- y$trade[[name]]$rho.N$config
                        }

                        aux.plot.param <- function (ff, param, y.range=NULL, y.range.force=F, col="black", main.prefix=NULL) {
                            if (grepl ("%", param)) {
                                data  <- summary(ff, pars=sprintf(param,1:N.d.min.1))$summary;
                            } else {
                                data  <- summary(ff, pars=param)$summary;
                            }

                            data  <- data[,c("50%", "2.5%", "97.5%", "25%", "75%"),drop=F];

                            if (y.range.force) {
                                y.range  <- c(max(y.range[1],min(data)),
                                              min(y.range[2],max(data)));
                            } else {
                                y.range  <- range(c(y.range,data));
                            }

                            plot(100,100,ylim=y.range, xlim=plot.x.range, main=paste(main.prefix, param));

                            lines(plot.x.ref, data[,1], col=col, type="b");
                            for (i in 2:ncol(data)) {
                                lines(plot.x.ref,data[,i], col=col, lty=2);
                            }

                            if (use.progress) {
                                abline(v=seq(0,1,0.1), col="gray");
                            } else {
                                abline(v=seq(0,plot.x.range[2],5), col="gray");
                            }
                            abline(v=today.progress, col="purple");
                            abline(v=rho.N.progress, col="orange");
                        }

                        dev.new();
                        aux.plot.param(ff.list, "rho_sq", y.range = c(0,1), main.prefix=name);
                        lines(plot.x.ref,x.cor.sq.m, col="red");
                        abline(b=1,a=0, col="gray");
                        abline(h=0:10/10, col="gray");
                        abline(v=ff.ic.market$cp[name,"d"], col="blue");

                        dev.new();
                        aux.plot.param(ff.list, "log_change_sd[%d,1]", main.prefix=name);
                        lines(plot.x.ref, sqrt(x.var[-N]), col="red");
                        abline(h=seq(0, 0.4, 0.01), col="gray");
                        abline(v=ff.ic.market$cp[name,"d"], col="blue");

                        dev.new();
                        aux.plot.param(ff.list, "Omega_delta[%d,2,1]", y.range = c(-1,1), main.prefix=name);
                        abline(h=-10:10/10, col="gray");
                        abline(h=0, col="blue");
                        abline(v=ff.ic.market$cp[name,"d"], col="blue");

                        #        dev.new();
                        #        aux.plot.param(ff.list, "fwd_mean_x", y.range = c(-2,2), y.range.force=T, main.prefix=name);
                        #        abline(h=seq(-5, 5, 0.1), col="gray");
                        #        abline(h=0, col="blue");
                        #        abline(v=ff.ic.market$cp[name,"d"], col="blue");

                        dev.new();
                        aux.plot.param(ff.list, "fwd_sd", main.prefix=name);
                        abline(h=seq(0, 1, 0.01), col="gray");
                        abline(h=0, col="blue");
                        abline(h=sqrt(mean(ts.ref[,"1"]^2)), col="red");
                        abline(v=ff.ic.market$cp[name,"d"], col="blue");

                        dev.new();
                        data <- cbind(summary(ff.list, pars="var_x")$summary[,"50%"],
                                      summary(ff.list, pars="rho_sq")$summary[,"50%"]);
                        plot(data,type="b", xlim=range(c(0, 1, data[,1])), ylim=range(c(0, 1, data[,2])), main=name);
                        points(data[seq(5, nrow(data), 5), ],           col="red");
                        abline(b=1, a=0, col="blue");
                        abline(h=0:20/10, col="gray");
                        abline(v=0:20/10, col="gray");
                        points(data[ff.ic.market$cp[name,"N"],,drop=F], col="orange", pch=8);

                        if (d.current.i <= nrow(data)) {
                            points( data[d.current.i, ,drop=F], pch=8, col="purple");
                        }
                    }

                    if (is.null(today.progress)) {
                        d.current.i  <- 1e6;
                    } else {
                        if (use.progress) {
                            d.current.i  <- which(ff.ic.market$d.ref == today.progress);
                        } else {
                            d.current.i  <- today.progress;
                        }
                    }

                    if( plot.alpha) {
                        aux.plot("alpha");
                    }
                    if (plot.stock) {
                        aux.plot("stock");
                    }
                    if(plot.index) {
                        aux.plot("index");
                    }

                    if(plot.beta) {
                        dev.new();
                        beta.sum  <- summary(ff.ic.market$beta, pars="beta_k")$summary[, c("50%", "2.5%", "97.5%", "25%", "75%")];
                        plot.beta.x  <- 0:(nrow(beta.sum)-1)/nrow(beta.sum);
                        plot(plot.beta.x, beta.sum[,"50%"], ,type="b", main="beta_k", xlim=c(0,1), ylim=range(beta.sum));
                        sapply(2:ncol(beta.sum), function (i) {
                            lines(plot.beta.x, beta.sum[,i], lty=2);
                        })
                        abline(h=ff.ic.market$beta.k, col="blue");
                        abline(v=today.progress, col="purple");
                    }
                }


                EA.vis.IC.market.s <- function (ff.ic.market, today.progress=NULL, plot.stock=T, plot.alpha=T, plot.index=F, plot.beta=F) {


                    plot.x.ref <- ff.ic.market$d.ref[1:(ff.ic.market$data$N_d-1)];

                    aux.plot <- function (name) {
                        ff.list  <- ff.ic.market[[name]];
                        ts.ref  <- ff.ic.market$data[[paste("cum",name,sep="_")]];

                        N  <- ncol(ts.ref);

                        x.var      <- apply(ts.ref, 2, function(x) mean(x^2));
                        x.cor      <- cor(ts.ref);
                        x.cor.sq.m <- x.cor[N,1:(N-1)]^2;

                        x.var.x  <- x.var[1:(N-1)]/x.var[N];


                        aux.plot.param <- function (ff, param, y.range=NULL, y.range.force=F, col="black", main.prefix=NULL) {
                            data  <- t(sapply(ff, function(x) summary(x, pars=param)$summary[1,c("50%", "2.5%", "97.5%", "25%", "75%")]));
                            N  <- nrow(data);

                            if (y.range.force) {
                                y.range  <- c(max(y.range[1],min(data)),
                                              min(y.range[2],max(data)));
                            } else {
                                y.range  <- range(c(y.range,data));
                            }

                            plot(100,100,ylim=y.range, xlim=c(0,1), main=paste(main.prefix, param));

                            lines(plot.x.ref, data[,1], col=col, type="b");
                            for (i in 2:ncol(data)) {
                                lines(plot.x.ref,data[,i], col=col, lty=2);
                            }
                            abline(v=seq(0,1,0.1), col="gray");

                            abline(v=today.progress, col="purple");
                            abline(v=ff.ic.market$ic.ok.start/ff.ic.market$data$N_d, col="orange");
                        }

                        dev.new();
                        aux.plot.param(ff.list, "rho_sq", y.range = c(0,1), main.prefix=name);
                        lines(plot.x.ref,x.cor.sq.m, col="red");
                        abline(b=1,a=0, col="gray");
                        abline(h=0:10/10, col="gray");

                        dev.new();
                        aux.plot.param(ff.list, "log_change_sd[1]", main.prefix=name);
                        lines(plot.x.ref, sqrt(x.var[-N]), col="red");
                        abline(h=seq(0, 0.4, 0.01), col="gray");

                        #        dev.new();
                        #        aux.plot.param(ff.list, "Omega_delta[2,1]", y.range = c(-1,1), main.prefix=name);
                        #        abline(h=-10:10/10, col="gray");
                        #        abline(h=0, col="blue");

                        #        dev.new();
                        #        aux.plot.param(ff.list, "fwd_mean_x", y.range = c(-2,2), y.range.force=T, main.prefix=name);
                        #        abline(h=seq(-5, 5, 0.1), col="gray");
                        #        abline(h=0, col="blue");

                        dev.new();
                        aux.plot.param(ff.list, "fwd_sd", main.prefix=name);
                        abline(h=seq(0, 1, 0.01), col="gray");
                        abline(h=0, col="blue");
                        abline(h=sqrt(mean(ts.ref[,"1"]^2)), col="red");

                        dev.new();
                        data  <- t(sapply(ff.list, function(x) summary(x, pars=c("var_x", "rho_sq"))$summary[,"50%"]));
                        plot(data,type="b", xlim=range(c(0, 1, data[,1])), ylim=range(c(0, 1, data[,2])), main=name);
                        points(data[seq(5, nrow(data), 5), ],           col="red");
                        abline(b=1, a=0, col="blue");
                        abline(h=0:20/10, col="gray");
                        abline(v=0:20/10, col="gray");

                        if (d.current.i <= nrow(data)) {
                            points( data[d.current.i, ,drop=F], pch=8, col="purple");
                        }
                    }

                    if (is.null(today.progress)) {
                        d.current.i  <- 1e6;
                    } else {
                        d.current.i  <- which(ff.ic.market$d.ref == today.progress);
                    }

                    if (plot.stock) {
                        aux.plot("stock");
                    }
                    if( plot.alpha) {
                        aux.plot("alpha");
                    }
                    if(plot.index) {
                        aux.plot("index");
                    }

                    if(plot.beta) {
                        dev.new();
                        beta.sum  <- summary(ff.ic.market$beta, pars="beta_k")$summary[, c("50%", "2.5%", "97.5%", "25%", "75%")];
                           plot.beta.x  <- 0:(nrow(beta.sum)-1)/nrow(beta.sum);
                        plot(plot.beta.x, beta.sum[,"50%"], ,type="b", main="beta_k", xlim=c(0,1), ylim=range(beta.sum));
                        sapply(2:ncol(beta.sum), function (i) {
                            lines(plot.beta.x, beta.sum[,i], lty=2);
                        })
                        abline(h=ff.ic.market$beta.k, col="blue");
                        abline(v=today.progress, col="purple");
                    }
                }
    
    


    
        formulaText <- reactive({
                paste("ticker: ", input$ticker, "Feed Source: ", input$feedSource)
        })

        # generate the formula
        y <- reactive({
                EA.agent(ticker = input$ticker, feed.source = input$feedSource)
        })

        y$init.period(20202)

        output$caption <- renderText({
                formulaText()
        })

        # generate 9 plots
        output$plots <- renderPlot({
                y$plot.IC()
        })

        })
