temp <- df %>%
  select(interception) %>%
  replace_na(2)

temp <- df %>% 
  filter(pass == 1) %>%
  select(everything()) %>%
  mutate(interception = replace_na(interception, 2))

plot(Pfit$model$air_yards, Pfit$residuals, xlab = "Yardline 100", ylab = "Model Residuals")
plot(Pfit$model$yards_after_catch, Pfit$residuals, xlab = "Yardline 100", ylab = "Model Residuals")
plot(Pfit$model$yardline_100, Pfit$residuals, xlab = "Yardline 100", ylab = "Model Residuals")
plot(Pfit$model$ydstogo, Pfit$residuals, xlab = "Yardline 100", ylab = "Model Residuals")
plot(Pfit$model$ydsnet, Pfit$residuals, xlab = "Yardline 100", ylab = "Model Residuals")

Pfit <- lm(epa ~ yards_after_catch + air_yards + as.factor(interception) + 
             as.factor(fumble) + as.factor(fumble_lost) + yardline_100 + 
             ydstogo + ydsnet + pass_touchdown + as.factor(down), data = Pdf)

summ(Pfit)
