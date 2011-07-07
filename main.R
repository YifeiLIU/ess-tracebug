require(stats); require(graphics)
lm_test(Fertility ~ . , data = swiss)

for(i in 1:5){
    Sys.sleep(1)
}
