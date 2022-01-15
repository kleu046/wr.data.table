raw <- data.table(mtcars)

rm(def_cols_copy_dt)

load_all()

raw |> def_cols(mpg2 ~ mpg * 2)
