Lending Club Data
================

## Part 1: Scatter plots

1)  A scatter plot relating the loan amount (the amount that each
    borrower requested) to the funded amount (the amount funded by
    investors)  
    To make an attractive chart, you will have to (at least) change the
    labels on the axis, the alpha values, and the background, gridlines,
    etc.

<!-- end list -->

``` r
ggplot(lc, aes(x = loan_amnt, y = funded_amnt_inv)) + 
  geom_point(alpha = 0.3, na.rm = TRUE, color = "#00AFBB") +
  xlab("Requested Loan Amount") + 
  ylab("Amount Funded by Investors") +
  ggtitle("Requested vs Funded") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = 'black'),
        plot.caption = element_text(face = "italic")) +
  labs(caption = "Source: Lending Club")
```

![](lc-visualization_files/figure-gfm/Scatter%20Plot%20-%201-1.png)<!-- -->

2)  A plot exploring the relationship between annual income (the
    borrower’s income) and loan amount (the amount that the borrower
    requests). Use the logarithmic scale.

<!-- end list -->

``` r
lc %>%
  select(annual_inc, loan_amnt) %>%
  drop_na() %>%
  ggplot(aes(x = log(annual_inc), y = log(loan_amnt))) +
  geom_point(alpha = .02, color = 'lightblue') + 
  stat_smooth(method="lm", se=FALSE) +
  xlab("Log of Annual Income") + 
  ylab(" Log of Loan Amount") + 
  ggtitle("Log Plot of Annual Income Vs Loan Amount") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = 'black'),
        plot.caption = element_text(face = "italic")) +
  labs(captions = " Source: Lending Club")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](lc-visualization_files/figure-gfm/Scatter%20plot%20-%202-1.png)<!-- -->

3)  Bubble Chart and Colors

<!-- end list -->

``` r
lc %>% 
  select(loan_amnt, int_rate, grade, dti) %>%
  drop_na() %>%
  mutate(int_rate_numeric = as.numeric(gsub("\\%", "", int_rate))) %>%
  ggplot(aes(x = dti, 
              y = as.numeric(int_rate_numeric), 
             size = loan_amnt, 
             color = grade)) + 
  geom_point(alpha=0.1) +
  xlab("Debt-to-Income") + 
  ylab("Interest Rate") + 
  ggtitle("Interest Rate vs DTI") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = 'black'),
        plot.caption = element_text(face = "italic")) +
  scale_x_continuous(breaks=c(0,10,20,30), 
                     labels = c('0','10','20','30'), 
                     limits = c(0,30)) +
  scale_y_continuous(breaks=c(5,10,15,20,25), 
                     labels = c('5','10','15','20','25'), 
                     limits = c(5,25)) +
  labs(size = "Loan Amount", 
       color = "Grade",
       caption = " Source: Lending Club")
```

![](lc-visualization_files/figure-gfm/Bubble%20Chart-1.png)<!-- -->

## Part 2: Distributions

1)  A histogram to show the distribution of the Loan Amount (the amount
    requested by the borrower). Make sure to change the border color of
    the bars and change the x and y-axis labels.

<!-- end list -->

``` r
ggplot(lc, aes(x = loan_amnt)) +
  geom_histogram(bins = 15, na.rm = TRUE, fill="#69b3a2", color="white") +
  ggtitle("Histogram of Loan Amount") +
  xlab("Loan Amount ($)") +
  ylab("Frequency") +
  theme(panel.background = element_blank() , 
        axis.line = element_line(colour = 'black'),
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(face = "italic")) +
  scale_y_continuous(expand = c(0,0)) +
  labs(caption = "Source: Lending Club")
```

![](lc-visualization_files/figure-gfm/Histogram%20-%201-1.png)<!-- -->

2)  Comparing the distribution of the loan amount by the homeownership
    status by creating a chart that shows some distribution information
    for each level of home ownership. In particular, make sure that your
    chart describes the median loan amount for each home ownership
    status.

<!-- end list -->

``` r
lc %>% 
  select(home_ownership, loan_amnt)  %>%
  ggplot(aes(x = loan_amnt, y = home_ownership)) +
  geom_boxplot(fill = 'lightpink' ) +
  xlab("Loan Amount") +
  ylab("Home Ownership") +
  ggtitle("Boxplot of Loan Amount vs Ownership") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = 'black'),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x = element_line(color = 'grey80', linetype = 3),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(face = "italic")) +
  labs(caption = "Source: Lending Club")
```

    ## Warning: Removed 3 rows containing non-finite values (stat_boxplot).

![](lc-visualization_files/figure-gfm/Boxplot-1.png)<!-- -->

## Part 3: Categorical plot

1)  A bar chart with the average Loan Amount by Investment Grade for
    2011. Ensure that readers can easily identify the Loan Grade with
    the lowest average Loan Amount.

<!-- end list -->

``` r
lc %>% 
  filter(issue_year == 2011) %>%
  select(loan_amnt,grade) %>%
  group_by(grade) %>% 
  summarise(avg_loan_amnt = mean(loan_amnt)) %>% 
  as.data.frame() %>%
  drop_na() %>%
  mutate(mincolor = ifelse(avg_loan_amnt == min(avg_loan_amnt), '1', '0')) %>%
  ggplot(aes(x = grade, y = avg_loan_amnt, fill = mincolor)) +
  geom_col(na.rm = TRUE, 
           fill = c('seagreen', 'grey70', 'grey70', 
                    'grey70', 'grey70', 'grey70', 'grey70')) +
  theme(panel.background = element_blank() , 
        axis.line = element_line(colour = 'black'), 
        legend.position = 'none',
        panel.grid.major.y = element_line(color = 'grey80', linetype = 3),
        plot.caption = element_text(face = "italic")) +
  xlab("Investment Grade") +
  ylab("Average Loan Amount ($)") + 
  ggtitle("Investment Grade Vs Loan Amount") +
  scale_y_continuous(expand = c(0,0)) +
  labs(caption = "Source: Lending Club")
```

![](lc-visualization_files/figure-gfm/Bar%20Chart%20-%201-1.png)<!-- -->

2)  The average loan amount by investment grade for mortgage holders
    vs. renters, in 2011.

<!-- end list -->

``` r
lc %>% 
  filter(issue_year == 2011, home_ownership =='RENT' | home_ownership == 'MORTGAGE') %>%
  select(loan_amnt,grade,home_ownership) %>%
  group_by(grade, home_ownership) %>% 
  summarise(avg_loan_amnt = mean(loan_amnt)) %>% 
  as.data.frame() %>%
  drop_na() %>%
  ggplot(aes(x = grade, y = avg_loan_amnt, fill = home_ownership)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = 'black'),
        plot.caption = element_text(face = "italic")) +
  xlab("Investment Grade") +
  ylab("Average Loan Amount ($)") +
  ggtitle("Average Loan Amount by Investment Grade") +
  labs(fill = "Home Ownership", 
       caption = "Source: Lending Club") +
  scale_fill_manual(values = c(MORTGAGE = "#117A65", RENT = "#AEB6BF")) +
  scale_y_continuous(expand = c(0,0))
```

![](lc-visualization_files/figure-gfm/Bar%20Chart%20-%202-1.png)<!-- -->

3)  The total loan amount by investment grade in 2011. Break each year
    into mortgage holders vs. all others.

<!-- end list -->

``` r
lc %>% 
  filter(issue_year == 2011) %>%
  select(loan_amnt,grade,home_ownership) %>%
  mutate(home_ownership = replace(home_ownership, home_ownership!= 'MORTGAGE', 'OTHER')) %>%
  group_by(grade, home_ownership) %>% 
  summarise(total_loan_amnt = sum(loan_amnt)) %>%
  drop_na() %>%
  ggplot(aes(x = grade, y = total_loan_amnt/1000000, fill = home_ownership)) +
  geom_bar(stat = 'identity') +
  theme(panel.background = element_blank() , 
        axis.line = element_line(colour = 'black'),
        panel.grid.major.y = element_line(color = 'grey80', linetype = 3),
        plot.caption = element_text(face = "italic")) +
  xlab("Investment Grade") +
  ylab("Total Loan Amount (Million $)") +
  ggtitle("Total Loan Amount by Investment Grade for Mortgage vs All") +
  labs(fill = "Home Ownership", caption = "Source: Lending Club") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values= c(MORTGAGE = "#5DADE2", OTHER = "#A6ACAF"))
```

![](lc-visualization_files/figure-gfm/Stacked%20Bar%20Chart-1.png)<!-- -->

## Part 4: More Bar Charts

1)  Pyramid Chart Using patchwork to create a pyramid chart. Place a
    label for the LoanGrade in the center, and display the number of
    loans, organized by the purpose of the debt(group debt consolidation
    and credit cards together versus all other loan
purposes)

<!-- end list -->

``` r
pyramid_data <- lc%>%mutate(debt_purpose=ifelse(purpose%in%c('credit_card','debt_consolidation'),"Debt","Other"))%>%
  group_by(grade, debt_purpose)%>%
  summarise(numberOfLoans =n())%>%
  pivot_wider(names_from = debt_purpose,values_from = numberOfLoans)%>%
  mutate(Other =-Other)%>%
  pivot_longer(Debt:Other, names_to="loan_purpose",values_to="n")%>%
  na.omit()%>% 
  ungroup()%>%
  mutate(grade =fct_relevel(as.factor(grade),
                            levels=rev(levels(as.factor(grade)))))
```

    ## Warning: Outer names are only allowed for unnamed scalar atomic inputs

``` r
debt_chart <- pyramid_data %>% 
  filter(loan_purpose=='Debt') %>% 
  ggplot(aes(y=grade, x=n)) + 
  geom_col(fill="royalblue") + 
  geom_vline(xintercept = 0) + 
  labs(x="Debt") + 
  theme(axis.title.y = element_blank(),
        axis.title.x =element_text(color="royalblue", face="bold"),
        axis.ticks =element_blank(),
        panel.grid.major.x =element_line(color="grey90", linetype=2),
        panel.background =element_rect(fill="white"),
        axis.text.y =element_text(hjust=0.5,face="bold")) + 
  scale_x_continuous(breaks=c(0,1000,2000,3000,4000,5000,6000,7000),
                     labels=c("0","1K","2K","3K","4K","5K","6K","7K"),
                     limits =c(0,7500),position = "top")

other_chart <- pyramid_data %>% 
  filter(loan_purpose=='Other') %>% 
  ggplot(aes(y=grade, x=n)) + 
  geom_col(fill="grey70") + 
  geom_vline(xintercept = 0) + 
  labs(x="Other Purposes") + 
  theme(axis.title.y =element_blank(),
        axis.title.x =element_text(color="grey70", face="bold"),
        axis.text.y =element_blank(),
        axis.ticks =element_blank(),
        panel.grid.major.x =element_line(color="grey90", linetype=2),
        panel.background=element_rect(fill="white")) + 
  scale_x_continuous(breaks=c(0,-1000,-2000,-3000,-4000,-5000,-6000,-7000),
                     labels=c("0","1K","2K","3K","4K","5K","6K","7K"),
                     limits =c(-7500,0),
                     position="top")

other_chart + debt_chart + 
  plot_annotation(title ='Lending Club Loans',
                  subtitle ='Loans to pay down or consolidate debt are less likely to be Grade A loans than loans for\nother purposes.',
                  caption ='Source: Lending Club',
                  theme =theme(plot.title =element_text(size = 14),
                               plot.subtitle=element_text(size=12, face="italic"),
                               plot.caption =element_text(face="italic")))
```

![](lc-visualization_files/figure-gfm/Pyramid%20Chart-1.png)<!-- -->

## Part 5: Time Series

1)  Multiple Line Chart: Creating a line chart of the average loan
    amount in each of the loangrades over time. Highlighting loan grade
    A and keep the other shades light grey.

<!-- end list -->

``` r
#fix the issue date
lc$issue_date = as.Date(paste(lc$issue_d,"-01",sep=""), format="%y-%b-%d")

lc %>% 
  select(grade, loan_amnt, issue_date) %>%
  drop_na() %>%
  group_by(issue_date, grade) %>%
  summarise(average_loan_amnt = mean(loan_amnt)) %>%
  mutate(Grade = ifelse(grade == 'A', 'A', 'Others')) %>%
  ggplot(aes(x = issue_date, y = average_loan_amnt, group = grade, color = Grade)) +
  geom_line() +
  scale_color_manual(values=c("steelblue", "lightgrey")) +
  theme(panel.background = element_blank() , axis.line = element_line(colour = 'black'),
        plot.caption = element_text(face = "italic")) +
  xlab("Issue Date") +
  ylab("Average Loan Amount") + 
  ggtitle("Average Loan Amount Over the years for grade A vs Others") +
  labs(caption = "Source: Lending Club Data")
```

![](lc-visualization_files/figure-gfm/Line%20Chart-1.png)<!-- -->

2)  Area Chart: An area chart showing the total number of loans for
    debt-relatedcategories (credit cards and debt consolidation), versus
    all other types over time.

<!-- end list -->

``` r
lc %>% 
  select(purpose,issue_date) %>%
  drop_na() %>%
  mutate(color_purpose = ifelse(purpose == 'credit_card' | purpose == 'debt_consolidation', 'Debt-Related', 'Others')) %>%
  group_by(issue_date, color_purpose) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = issue_date, y = count, fill = color_purpose)) +
  geom_area(alpha = .5, size = .5, color= 'white') +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = 'black')) +
  xlab("Issue Date") +
  ylab("Number of Loans") + 
  ggtitle("Number of Loans issued for Debt vs Non-Debt Purposes") +
  labs(fill = "Purpose",
       caption = "Source: Lending Club Data") +
  scale_fill_manual(values=c("steelblue", "lightgrey")) +
  theme(plot.caption = element_text(face = "italic"))
```

![](lc-visualization_files/figure-gfm/Area%20Chart-1.png)<!-- -->
