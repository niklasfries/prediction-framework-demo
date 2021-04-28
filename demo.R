source('build_dataset.R')
source('prediction.R')

n = 3000 # Number of observations
m = 100 # Number of explanatory variables

# Build dataset, see build_dataset.R for details
dataset = build_dataset(n=n, m=m)
X = dataset$X
p = dataset$p
y = dataset$y

# Split dataset into training, validation and test part
train_ix = 1:1000
valid_ix = 1001:2000
test_ix = 2001:3000

# Predict probabilities from X and y using LinR, LASSO, LogR, PLR
linr_p = do_linr(X, y, p, train_ix, valid_ix, test_ix)$p2
lasso_p = do_lasso(X, y, p, train_ix, valid_ix, test_ix)$p2
logr_p = do_logr(X, y, p, train_ix, valid_ix, test_ix)$p2
plr_p = do_plr(X, y, p, train_ix, valid_ix, test_ix)$p2

print(sprintf('LinR RMSE: %.3f', rmse(linr_p - p[test_ix])))
print(sprintf('LASSO RMSE: %.3f', rmse(lasso_p - p[test_ix])))
print(sprintf('LogR RMSE: %.3f', rmse(logr_p - p[test_ix])))
print(sprintf('PLR RMSE: %.3f', rmse(plr_p - p[test_ix])))

