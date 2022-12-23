# Reading File for Analysis

ReebokAdidas <- read.csv("Reebok.csv", sep=",", header=T); # data is loaded to a frame named gmapps
summary(ReebokAdidas)
names(ReebokAdidas)

t.test(ReebokAdidas$UserReview ~ ReebokAdidas$Brand, alternative = "two.sided")

# Identifying the contributing factors affecting

AdidasReebokLm  <- lm(log(ReviewCount) ~ log(ProductPrice) + ProductScore + UserReview
                      + UserReview*ProductPrice+ factor(Category)
   , data=ReebokAdidas)
summary(AdidasReebokLm)


Adidas <- lm(UserReview ~ ProductScore + ReviewCount*ProductPrice+ factor(Category)
             , data=ReebokAdidas[ReebokAdidas$Brand == 'Adidas',])

Reebok <- lm(UserReview ~ ProductScore + ReviewCount*ProductPrice + factor(Category)
             , data=ReebokAdidas[ReebokAdidas$Brand == 'Reebok',])
summary(Adidas)
summary(Reebok)

nr <- summary(Adidas)$coefficients[2:4,1]-summary(Reebok)$coefficients[2:4,1]
denom <-sqrt(summary(Adidas)$coefficients[2:4,2]^2+summary(Reebok)$coefficients[2:4,2]^2)
nr
denom
tval <-nr/denom
tval
(qt(0.05, 380))


# Equation: Target:Review Count, Feature: 
cor(ReebokAdidas$ReviewCount, ReebokAdidas$ProductScore)
cor(ReebokAdidas$ReviewCount, ReebokAdidas$UserReview)
cor(ReebokAdidas$ProductScore, ReebokAdidas$UserReview)

CategoryTop <- lm(UserReview ~ ProductScore + ReviewCount*ProductPrice+ factor(Category)
             , data=ReebokAdidas[ReebokAdidas$Category == 'hiking-shoes', 'soccer-cleats'])

CategoryOpportunity <- lm(UserReview ~ ProductScore + ReviewCount*ProductPrice+ factor(Category)
                          , data=ReebokAdidas[ReebokAdidas$Brand == 'Adidas',])
# Testing for specific categories

new2Category <- ReebokAdidas[ReebokAdidas$Category=='hiking-shoes' | 
                               ReebokAdidas$Category=='soccer-cleats',]
newOtherCategory <- ReebokAdidas[
                                   ReebokAdidas$Category=='golf-shoes'|
                                 
                                   ReebokAdidas$Category=='hking-boots'|
                                   ReebokAdidas$Category=='tennis-shoes'|
                                   ReebokAdidas$Category=='football-cleats' |
                                   ReebokAdidas$Category=='track-and-field-shoes' |
                                   ReebokAdidas$Category=='cycling-shoes',]

hist(log(new2Category$ReviewCount))
hist(log(newOtherCategory$ReviewCount))

t.test( log(new2Category$ReviewCount), log(newOtherCategory$ReviewCount)
        , alternative = "greater", conf.level=0.90)

plot(log(ReebokAdidas$ReviewCount), log(ReebokAdidas$ProductPrice), main="Product Price vs Demand",)

