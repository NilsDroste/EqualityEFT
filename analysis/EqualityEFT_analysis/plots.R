full_df %>% ggplot(aes(y=lnMun,x=year)) + geom_line() + facet_wrap(~state) + geom_vline(data = full_df, aes(xintercept = legislation), col="red") + geom_vline(data = full_df, aes(xintercept = enactment), col="blue")


full_gini_df %>% ggplot(aes(y=gini,x=year)) + geom_line() + facet_wrap(~state) + geom_vline(data = full_df, aes(xintercept = legislation), col="red") + geom_vline(data = full_df, aes(xintercept = enactment), col="blue")


full_df %>% ggplot(aes(y=lnTot,x=year)) + geom_line() + facet_wrap(~state) + geom_vline(data = full_df, aes(xintercept = legislation), col="red") + geom_vline(data = full_df, aes(xintercept = enactment), col="blue")

