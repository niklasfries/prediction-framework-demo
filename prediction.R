require(glmnet)

rmse = function(x) {
    return(sqrt(mean(x^2)))
}

do_linr = function(X, y, p, train_ix, valid_ix, test_ix) {
    data = data.frame(X=X, y=y)
    mod = lm(y ~ ., data=data[train_ix,])
    p2 = predict(mod, newdata=data[test_ix,])
    p2[p2 < 0] = 0
    p2[p2 > 1] = 1
    return(list(mod=mod, p2=p2, rmse=rmse(p2 - p[test_ix])))
}

do_logr = function(X, y, p, train_ix, valid_ix, test_ix, n_boot=100) {
    data = data.frame(X=X, y=y)
    mod = glm(y ~ ., data=data[train_ix,], family=binomial(link='logit'))
    p2 = inv.logit(predict(mod, newdata=data[test_ix,]))
    return(list(mod=mod, p2=p2, rmse=rmse(p2 - p[test_ix])))
}

do_plr = function(X, y, p, train_ix, valid_ix, test_ix, lambdas=NULL) {
    # 'binomial' uses lognet which implies logit as opposed to probit
    mod = glmnet(X[train_ix,], y[train_ix], family='binomial')
    pred = predict.glmnet(mod, X[valid_ix,])
    y_rmses = apply(inv.logit(pred) - (y[valid_ix] %*% t(rep(1, dim(pred)[2]))), 2, rmse)
    p_rmses = apply(inv.logit(pred) - (p[valid_ix] %*% t(rep(1, dim(pred)[2]))), 2, rmse)
    lambdas = mod$lambda
    ix_min = which.min(y_rmses)
    lambda_min = lambdas[ix_min]
    p2 = inv.logit(predict.glmnet(mod, X[test_ix,], s=lambda_min))
    return(list(mod=mod, p2=p2, rmse=p_rmses[ix_min], rmses=p_rmses, y_rmses=y_rmses))
}

do_lasso = function(X, y, p, train_ix, valid_ix, test_ix, lambdas=NULL) {
    mod = glmnet(X[train_ix,], y[train_ix], family='gaussian')
    pred = predict.glmnet(mod, X[valid_ix,])
    pred[pred < 0] = 0
    pred[pred > 1] = 1
    y_rmses = apply(pred - (y[valid_ix] %*% t(rep(1, dim(pred)[2]))), 2, rmse)
    p_rmses = apply(pred - (p[valid_ix] %*% t(rep(1, dim(pred)[2]))), 2, rmse)

    lambdas = mod$lambda
    ix_min = which.min(y_rmses)
    lambda_min = lambdas[ix_min]
    p2 = predict.glmnet(mod, X[test_ix,], s=lambda_min)
    p2[p2 < 0] = 0
    p2[p2 > 1] = 1
    return(list(mod=mod, p2=p2, rmse=p_rmses[ix_min], rmses=p_rmses, y_rmses=y_rmses))
}

