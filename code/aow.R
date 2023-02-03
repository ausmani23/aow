########################################################
#########################################################

#clear workspace
rm(list=ls())

#load packages
require(stringr)
require(plyr)
require(dplyr)
require(zoo)
require(data.table)
require(tidyr)
require(ggplot2)
require(rprojroot)

homedir<-find_root(
  criterion=has_file('aow.RProj')
)
datadir<-file.path(
  homedir,"data"
)
codedir<-file.path(
  homedir,"code"
)
outputdir<-file.path(
  homedir,"output"
)
filesdir<-file.path(
  homedir,'files'
)

#helper function
suma = function(x) if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE)

#calculations list 
calcs_list <- list()
#all calculations mentioned in the paper are stored in this list
#after running the file, these calculations are saved as a .txt file in '/output'

#########################################################
#########################################################

#POLICE KILLINGS

#most reliable source
require(rvest)
url<-'https://www.prisonpolicy.org/blog/2020/06/05/policekillings/'
tmptab<-read_html(url) %>%
  html_nodes('table') %>%
  html_table
tmpdf<-tmptab[[1]]
names(tmpdf)<-c(
  'countryname',
  'policekillings',
  'population',
  'policekillings_percapita',
  'source',
  'year',
  'popsource'
)
tmpdf$policekillings <- 
  str_replace(tmpdf$policekillings,"\\,","") %>% as.numeric
sum(tmpdf$policekillings[tmpdf$countryname!='United States'])

usa<-tmpdf$countryname=='United States'

calcs_list[['USA/Other Developed Countries Police Killings Ratio']]<-
  tmpdf$policekillings_percapita[usa]/
  median(tmpdf$policekillings_percapita[!usa])

#########################################################
#########################################################

#LIFE AT THE BOTTOM, US VS NORWAY

#load lane kenworthy's dataset
#this dataset accompanies the book, Social Democratic Capitalism
#https://lanekenworthy.files.wordpress.com/2019/10/sdc-data.xlsx

setwd(datadir); dir()
require(readxl)
thisfile<-'sdc-data.xlsx'
tmpdf<-read_xlsx(
  thisfile,
  sheet='Fig 2.5'
)
tmpdf2<-read_xlsx(
  thisfile,
  sheet='Fig 2.6'
)
tmpdf<-merge(tmpdf,tmpdf2)

nor<-tmpdf$countryabbr_nordic=='*Nor*'
usa<-tmpdf$countryabbr_nordic=='US'
tmpdf$p10income_2010to2016[nor]
tmpdf$p10income_2010to2016[nor]/tmpdf$p10income_2010to2016[usa]

tmpdf$relativepoverty_2010to2016[nor]
tmpdf$relativepoverty_2010to2016[nor]/tmpdf$relativepoverty_2010to2016[usa]

#########################################################
#########################################################

#EMPLOYMENT RATES

#these data come from IPUMS Census Microdata
#Steven Ruggles, Sarah Flood, Ronald Goeken, Megan Schouweiler and Matthew Sobek. 
#IPUMS USA: Version 12.0 [dataset]. Minneapolis, MN: IPUMS, 2022. 
#https://doi.org/10.18128/D010.V12.0

setwd(datadir); dir()
sumdfs<-readRDS('sumdfs.RDS')
empdf<-lapply(sumdfs,function(x) x$emp_f) %>% 
  rbind.fill
empdf<-data.table(empdf)

empdf[ ind_f%in%c(1), sector_f := 1] #agriculture
empdf[ ind_f%in%c(2,3,4,5), sector_f := 2] #industry
empdf[ ind_f%in%c(6,7,8,9), sector_f := 3] #services
empdf[ ind_f%in%c(10), sector_f := 4] #govt

empdf[ ind_f%in%c(1), sector2_f := 1] #agriculture
empdf[ ind_f%in%c(2,3,4,5), sector2_f := 2] #industry
empdf[ ind_f%in%c(6,7,8,9), sector2_f := 3] #services
empdf[ ind_f%in%c(10), sector2_f := 3] #govt

#trends by race and gender 
plotdf <- empdf[
  ageg_f%in%c(3,4,5) & 
    race_f%in%c(1,2) &
    !is.na(emp_f)
  ,
  .(
    jobschoolless = 100 - 100 * suma(schoolemp_f)/sum(pop)
  )
  ,
  by=c(
    'race_f',
    'sex_f',
    'year'
  )
]

tmplevels<-c(1,2)
tmplabels<-c("White","Black")
plotdf$race_f<-factor(
  plotdf$race_f,
  tmplevels,
  tmplabels
)

tmplevels<-c(1,2)
tmplabels<-c("Men","Women")
plotdf$sex_f<-factor(
  plotdf$sex_f,
  tmplevels,
  tmplabels
)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=year,
    y=jobschoolless,
    linetype=race_f
  )
) +
  geom_line(
    size=0.75
  ) + 
  facet_wrap( ~ sex_f) +
  scale_linetype(
    name=''
  ) +
  theme_bw() +
  xlab("") +
  ylab("% Neither in Job Nor School\n") +
  theme(
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1,'cm')
    #plot.margin=unit(c(1,1,1,1), "cm")
  )
setwd(outputdir)
ggsave(
  'fig_jobs_gender.png',
  g.tmp,
  width=8/1.25,
  height=4/1.25
)

#trends by race and skill (men)

plotdf<-empdf[
  ageg_f%in%c(3,4,5) &
    race_f%in%c(1,2) &
    sex_f%in%c(1) &
    ed_p%in%c(1,3) &
    !is.na(emp_f) & 
    !is.na(ed_p)
  ,
  .(
    jobschoolless = 100 - 100 * suma(schoolemp_f)/sum(pop)
  ),
  by=c(
    'race_f',
    'ed_p',
    'year'
  )
]


tmplevels<-c(1,2,3)
tmplabels<-c(
  "Low-Skill",
  "Middle Skill",
  "High-Skill"
)
plotdf$ed_p<-factor(
  plotdf$ed_p,
  tmplevels,
  tmplabels
)

tmplevels<-c(1,2)
tmplabels<-c("White","Black")
plotdf$race_f<-factor(
  plotdf$race_f,
  tmplevels,
  tmplabels
)

g.tmp <-ggplot(
  plotdf,
  aes(
    x=year,
    y=jobschoolless,
    linetype=race_f
  )
) + 
  geom_line() +
  scale_linetype(
    name=''
  ) +
  facet_wrap(
    ~ ed_p
  ) +  
  theme_bw() +
  xlab("") +
  ylab("% Neither in Job Nor School\n") +
  theme(
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1,'cm')
    #plot.margin=unit(c(1,1,1,1), "cm")
  )
setwd(outputdir)
ggsave(
  'fig_jobs_skill.png',
  g.tmp,
  width=8/1.25,
  height=4/1.25
)

########################################################
########################################################

#load the Survey of Consumer Finances (2019)
#https://www.federalreserve.gov/econres/files/scfp2019s.zip

setwd(datadir); dir()
require(haven)
fulldf<-read_dta('rscfp2019.dta') %>%
  data.table
names(fulldf)

oldnames<-c(
  'networth',
  'income',
  'racecl4',
  'edcl',
  'wgt',
  'age'
)
newnames<-c(
  'networth',
  'income',
  'race',
  'educ',
  'weight',
  'age'
)

fulldf <- fulldf[,oldnames,with=F]
names(fulldf)<-newnames

#income and net worth by raceXclass
require(spatstat)

#this is the racial gap
tmpdf<-fulldf[
  race%in%c(1,2)
  ,
  .(networth_median=weighted.median(networth,weight))
  ,
  by=c('race')
]
tmpdf$i<-1
tmpdf<-spread(tmpdf,race,networth_median)
calcs_list[['White/Black Wealth Gap']] <- tmpdf$`1`/tmpdf$`2` #about 7.85

#this is the education gap, amongst black people
#college vs. high school
tmpdf<-fulldf[
  race%in%c(2) & 
    educ%in%c(1,4)
  ,
  .(networth_median=weighted.median(networth,weight))
  ,
  by=c('race','educ')
]
tmpdf<-spread(tmpdf,educ,networth_median)
calcs_list[['College/Dropout Black Wealth Gap']] <- tmpdf$`4`/tmpdf$`1` #about 10.3

########################################################
########################################################

#load the Current Population Survey data
#these are the 2021 edition
#Sarah Flood, Miriam King, Renae Rodgers, Steven Ruggles, J. Robert Warren and Michael Westberry.
#Integrated Public Use Microdata Series, Current Population Survey: Version 10.0 [dataset]. 
#Minneapolis, MN: IPUMS, 2022. https://doi.org/10.18128/D030.V10.0

#helper funcitons
isodd<-function(x) x%%2!=0
iseven<-function(x) x%%2==0
genfw<-function(x) { #takes columns you want, and generates fwf request  
  #format has to be startcol,endcol,startcol,endcol,startcol,endcol...  
  fwf<-c()  
  for (i in 1:length(x)) {
    if (i==1) 
      fwf<-append(fwf,-(x[1]-1)) 
    if (isodd(i) & i>1) {
      if ((x[i]-x[i-1])>1) 
        fwf<-append(fwf,-(x[i]-x[i-1])+1)
    }
    if (iseven(i)) 
      fwf<-append(fwf,x[i]-x[i-1]+1)
  }
  if (fwf[1]==0) fwf<-fwf[-1] #if we're getting first col, need to do this 
  return(fwf)
}

setwd(datadir); dir()
tmpfname<-"cps_00015"

#get info from the codebook
setwd(datadir); dir()
tmp<-readLines(paste0(tmpfname,".txt"))
stline<-str_detect(tmp,"YEAR") %>% which %>% min
endline<-str_detect(tmp,"UNION") %>% which %>% min
if(!is.finite(stline) | !is.finite(endline))
  stop('var missing')

tmp<-tmp[stline:endline]
tmp<-str_extract(tmp,"(.*?)\\sX\\s")
colnames<-str_extract(tmp,"[A-Z0-9]+") %>% 
  tolower
locs<-str_extract(tmp,"(H|P)\\s+[0-9]+(\\-[0-9]+)?")
locs<-str_replace(locs,"(H|P)\\s+","")
stloc<-str_extract(locs,"^[0-9]+") %>%
  as.numeric
endloc<-str_extract(locs,"[0-9]+$") %>%
  as.numeric

#order stloc
neworder<-order(stloc)
colnames<-colnames[neworder]
stloc<-stloc[neworder]
endloc<-endloc[neworder]

#need to generate fwf format for cols
tmpmatrix<-matrix(
  c(stloc,endloc),
  ncol=2
) 
tmpfwf<-c(t(tmpmatrix))
widths<-genfw(tmpfwf)
types<-rep("i",length(colnames)) %>%
  paste0(collapse="")
types<-rep("integer",length(colnames))


#load the dataset
require(LaF)
raw<-laf_open_fwf(
  filename=paste0(tmpfname,".dat"),
  column_types=types,
  column_names=colnames,
  column_widths=widths
)
cpsdf <- data.table(raw[])
cpsdf$myid<-1:nrow(cpsdf)

#recodings
#agegroups 
cpsdf[ age< 15, ageg_f := 1] 
cpsdf[ age>=15 & age<18 , ageg_f := 2]
cpsdf[ age>=18 & age<24 , ageg_f := 3] 
cpsdf[ age>=24 & age<35 , ageg_f := 4]
cpsdf[ age>=35 & age<50 , ageg_f := 5]
cpsdf[ age>=50 & age<65 , ageg_f := 6]
cpsdf[ age>=65 , ageg_f :=7]

#sex
cpsdf[ sex==1 , sex_f := 1] 
cpsdf[ sex==2 , sex_f := 2] 

#educ
cpsdf[ educ%in%c(2:72), ed_f:= 1] #less than HS
cpsdf[ educ %in%c(73), ed_f := 2] #HS grad
cpsdf[ educ %in%c(80:110), ed_f :=3] #some college
cpsdf[ educ %in%c(111:125), ed_f :=4] #college grad

#race/hisp
cpsdf[ , race_f := 4] #make everyone other
cpsdf[ race==100 & hispan==0 , race_f := 1] #whites
cpsdf[ race==200 & hispan==0, race_f := 2]
cpsdf[ hispan!=0, race_f:=3]

#emp
cpsdf[ empstat%in%c(1:12,33), emp_f := 1] #employed/armed forces
cpsdf[ empstat%in%c(20:22,31,32,34,35,36) , emp_f := 0] #nilf,unempoyed

#union member/coverage
cpsdf[ union==0, union_f:= NA_integer_ ]
cpsdf[ union==1, union_f:=0  ]
cpsdf[ union%in%c(2,3), union_f:= 1]
weighted.mean(cpsdf$union_f,cpsdf$asecwt,na.rm=T) #11%

#fix topcoding of income variables
incvars<-names(cpsdf)[str_detect(names(cpsdf),'inc')]
for(v in incvars) {
  cpsdf[[v]][cpsdf[[v]]%in%c(999999999,99999999)]<-NA_integer_
}

#fix weights
cpsdf$asecwt <- cpsdf$asecwt/10^4

#summarize earnings
cpsdf[
  ,
  earnings:=incwage + incbus + incfarm + incint + incdivid + incrent
]

#all men
tmpdf1<-cpsdf[
  emp_f==1 & 
    ageg_f%in%c(3,4,5) & 
    sex_f==1
  ,
  .(
    earnings_avg=weighted.mean(earnings,asecwt),
    earnings_median=weighted.median(earnings,asecwt),
    earnings_20th=weighted.quantile(earnings,asecwt,p=0.20)
  )
]
tmpdf1$group<-'all men'

#arnings for all low-skilled men
tmpdf2<-cpsdf[
  ed_f==1 & 
    emp_f==1 & 
    ageg_f%in%c(3,4,5) & 
    sex_f==1
  ,
  .(
    earnings_avg=weighted.mean(earnings,asecwt),
    earnings_median=weighted.median(earnings,asecwt),
    earnings_20th=weighted.quantile(earnings,asecwt,p=0.20)
  )
]
tmpdf2$group<-'all low-skilled men'

#arnings for low-skilled Black men
tmpdf3<-cpsdf[
  ed_f==1 & 
    race_f==2 &
    emp_f==1 & 
    ageg_f%in%c(3,4,5) & 
    sex_f==1
  ,
  .(
    earnings_avg=weighted.mean(earnings,asecwt),
    earnings_median=weighted.median(earnings,asecwt),
    earnings_20th=weighted.quantile(earnings,asecwt,p=0.20)
  )
]
tmpdf3$group<-'all low-skilled black men'


tmpdf<-rbind.fill(
  tmpdf1,
  tmpdf2,
  tmpdf3
)
tmpdf


#union membership by race, education
calcs_list[['Union Density']] <- 
  100 * weighted.mean(cpsdf$union_f,cpsdf$asecwt,na.rm=T)
calcs_list[['Union Density (<HS)']] <- 
  100 * weighted.mean(cpsdf$union_f[cpsdf$ed_f==1],cpsdf$asecwt[cpsdf$ed_f==1],na.rm=T)
tmpdf<-cpsdf[
  ed_f%in%c(1) &
  emp_f==1 & 
    ageg_f%in%c(3,4,5) &
    race_f%in%c(1,2) 
  ,
  .(
    100 * weighted.mean(union_f,asecwt,na.rm=T)
  )
  ,
  by=c(
    'race_f',
    'ed_f'#,
    #'sex_f'
  )
]
setkey(tmpdf,race_f,ed_f)
calcs_list[['Union Density (Black, <HS)']] <- unlist(tmpdf[1,3])


#summary of income and public support
#by wage quantiles

#look only at eligible wage earners who reside in the
#in the poorest 20% of households
require(spatstat)
tmpdf<-cpsdf
tmpf2<-ewcdf(tmpdf$hhincome,tmpdf$asecwth)
tmpdf$hhincome_q <- round(100 * tmpf2(tmpdf$hhincome))
tmpdf<-tmpdf[hhincome_q<20 & !is.na(incwage)]

#indicate those 
supportvars<-c(
  "incss",
  "incwelfr",
  "incretir",
  "incssi",
  "incunemp",
  "incwkcom",
  "incvet",
  "incsurv",
  "incdisab"
)
tmpdf$zerosupport<-apply(tmpdf[,supportvars,with=F],1,function(x) sum(x==0)==length(x))

plotdf <- tmpdf[
  race_f%in%c(1,2) & 
    ageg_f%in%c(3,4,5)
  ,
  .(
    earnings=weighted.mean(incwage,asecwt),
    income=weighted.mean(
      incwage + 
        incbus + 
        incfarm + 
        incint +
        incdivid + 
        incrent +
        inceduc + 
        incchild + 
        #incalim + 
        incasist +
        incother + 
        incrann + 
        incpens
      ,asecwt),
    support=weighted.mean(
      incss + 
        incwelfr + 
        incretir + 
        incssi + 
        incunemp + 
        incwkcom + 
        incvet + 
        incsurv + 
        incdisab
      ,asecwt),
    taxcredits=weighted.mean(
      eitcred + 
        ctccrd + 
        actccrd,
      asecwt),
    pct_zerosupport=100 * sum(
      zerosupport*asecwt
    )/sum(asecwt)
  )
  ,
  by=c(
    'race_f',
    'sex_f'
  )
]
setkey(plotdf,race_f,sex_f)
plotdf$pct_earnings_support <- 100 * (plotdf$support + plotdf$taxcredit)/(plotdf$earnings + plotdf$taxcredit)
plotdf$earnings_support <- plotdf$support + plotdf$taxcredit

plotdf$race_f <- factor(
  plotdf$race_f,
  levels=c(1,2),
  labels=c('White','Black')
)
plotdf$sex_f <- factor(
  plotdf$sex_f,
  levels=c(1,2),
  labels=c('M','F')
)
# # plotdf$ed_f <- factor(
# #   plotdf$ed_f,
# #   levels=c(1,4),
# #   labels=c('<HS','>=College')
# # )
# plotdf$quantile <- factor(
#   plotdf$quantile,
#   levels=c('Poor','Other','Rich')
# )
plotdf$id<-paste(
  plotdf$race_f,
  plotdf$sex_f,
  sep=', '
)
tmporder<-order(plotdf$earnings_support)
tmplevels<-plotdf$id[tmporder]
plotdf$id<-factor(plotdf$id,tmplevels)

plotdf$color<-'darkgrey'
plotdf$color[plotdf$race_f=='Black' & plotdf$sex_f=='M']<-'red'
tmpcolors<-c('darkgrey','red')
names(tmpcolors)<-levels(plotdf$color)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=id,
    y=earnings_support,
    fill=color
  )
) +
  geom_bar(
    stat='identity',
    width=0.5,
    color='black'
  ) +
  scale_fill_manual(
    guide='none',
    values=tmpcolors
  ) +
  coord_flip() +
  theme_bw() +
  xlab("") +
  scale_y_continuous(
    breaks=c(0,2000,4000),
    labels=c('$0','$2,000','$4,000')
  ) +
  ylab("\nAverage Level of Annual Income Support")

setwd(outputdir)
ggsave(
  'fig_support.png',
  g.tmp,
  width=8/1.5,
  height=3/1.5
)

########################################################
########################################################

#this is a simple simulation of how returns_punishment are dictated by 
#PLE and simple decomposition of punishment into components

returns_punishment <- function(
    returns_lawabidinglife,
    pcaught,
    returns_crime
) {
  #returns_lawabidinglife have to be higher than lawdefying (the PLE)
  #assume some kidn of fixed constant, just for illustration
  returns_lawdefyinglife <- 0.95 * returns_lawabidinglife
  #this is equation for returns to punishment (simple decomposition)
  #returns_lawdefyinglife/pcaught - (returns_crime*(1-pcaught)/(pcaught))
  returns_lawdefyinglife/pcaught - returns_crime/pcaught + returns_crime
}

tmpdf<-expand.grid(
  returns_lawabidinglife = seq(0,10,length.out=51),
  pcaught=seq(0,1,length.out=11),
  returns_crime=seq(0,10,length.out=51)
)
tmpdf$i<-1:nrow(tmpdf)
tmpdf$returns_punishment <- sapply(tmpdf$i,function(i) {
  thisrow<-tmpdf[i,]
  returns_punishment(
    thisrow$returns_lawabidinglife,
    thisrow$pcaught,
    thisrow$returns_crime
  )
})

#this one depends on the value of the others
#easily verifiable by tkaing the derivative.. 
tmp<-tmpdf$returns_lawabidinglife==5 &
  tmpdf$returns_crime==10
plotdf<-tmpdf[tmp,]

#PLE doesn't have any clear implications 
#for what happens to E(returns|punishment)
#as p(caught) goes up, only in reverse
#so reverse p(caught)
#plotdf$pcaught <- -1 * plotdf$pcaught
g.tmp<-ggplot(
  plotdf,
  aes(
    x=pcaught,
    y=returns_punishment
  )
) + 
  geom_line(
    arrow=arrow(ends='first'),
    size=1
  ) +
  theme_bw() +
  xlab('\np(caught)') +
  ylab('E(returns|punishment)\n') +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
setwd(outputdir)
ggsave(
  'fig_dpcaught.png',
  g.tmp,
  width=9/2,
  height=6/2
)

#this one doesn't depend on value of returns to crime, 
#so we can just choose an arbitrary number
tmp<-tmpdf$pcaught==c(0.5) &
  tmpdf$returns_crime==median(tmpdf$returns_crime) 
plotdf<-tmpdf[tmp,]
g.tmp<-ggplot(
  plotdf,
  aes(
    x=returns_lawabidinglife,
    y=returns_punishment
  )
) + 
  geom_line(
    arrow=arrow(ends='first'),
    size=1
  ) +
  theme_bw() +
  xlab('\nE(returns|law-abiding life)') +
  ylab('E(returns|punishment)\n') + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
setwd(outputdir)
ggsave(
  'fig_dlawabiding.png',
  g.tmp,
  width=9/2,
  height=6/2
)

#this doesn't depend on the value of returns to LAL, 
#so pick an arbitrary value
tmp<-tmpdf$pcaught==median(tmpdf$pcaught) &
  tmpdf$returns_lawabidinglife==median(tmpdf$returns_lawabidinglife) 
plotdf<-tmpdf[tmp,]
g.tmp <- ggplot(
  plotdf,
  aes(
    x=returns_crime,
    y=returns_punishment
  )
) + 
  geom_line(
    arrow=arrow(ends='last'),
    size=1
  ) +
  theme_bw() +
  xlab('\nE(returns|crime)') +
  ylab('E(returns|punishment)\n') + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
setwd(outputdir)
ggsave(
  'fig_dcrime.png',
  g.tmp,
  width=9/2,
  height=6/2
)

########################################################
########################################################

#this is a simple graph to illustrate theory of crime

tmpdf<-data.frame(
  percentile=0:100
)
tmpdf$returns_law <- 
  log(tmpdf$percentile + 1) + 0
tmpdf$returns_crime <- 
  -1 * log( 0.01 * tmpdf$percentile + 1) + 2.5

#for shading
shadedf<-tmpdf[tmpdf$returns_crime>tmpdf$returns_law,]

#for plotting
plotdf<-pivot_longer(
  tmpdf,
  cols = c('returns_law','returns_crime'),
  names_to = "var",
  values_to = "val"
)

plotdf$var<-factor(
  plotdf$var,
  levels=c('returns_crime','returns_law'),
  labels=c('Law-Defying','Law-Abiding')
)

tmpcolors<-c('red','blue') 
names(tmpcolors) <- names(plotdf$var)

g.tmp <- ggplot(
  plotdf
) +
  geom_line(
    aes(
      x=percentile,
      y=val,
      color=var
      ),
    size=1
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  geom_ribbon(
    data=shadedf,
    aes(
      x=percentile,
      ymin=returns_law,
      ymax=returns_crime
    ),
    fill='darkgreen',
    alpha=0.4
  ) + 
  xlab("\nSocial Stratum") +
  ylab("Average Returns\n") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'top',
    legend.direction = 'horizontal'
  )

setwd(outputdir)
ggsave(
  'fig_theoryofcrime.png',
  g.tmp,
  width=9/2,
  height=6/2
)


#########################################################
#########################################################

#these data are OECD spending data
#downloaded from https://data.oecd.org/gga/general-government-spending.htm

#compare public order spending
#with spending on social protection,
#optionally including also health/education

setwd(datadir); dir()
spdf<-fread(
  'DP_LIVE_31012023154731621.csv'
)
names(spdf)<-tolower(names(spdf))
names(spdf)[1]<-c("country")
spdf$`flag codes`<-
  spdf$indicator<-
  spdf$frequency<-NULL

#we can put all spending in thousands of dollars per capita
#if we use gdp_capita and population

#gdp/capita
tmp<-spdf$measure=='THND_USD_CAP' &
  spdf$time=='2020'
tmpdf<-spdf[tmp,] #set aside
spdf<-spdf[!tmp,] #remove from spdf
names(tmpdf)[names(tmpdf)=="value"]<-"gdp_capita"
tmpdf$measure<-tmpdf$subject<-NULL
spdf<-merge(
  spdf,
  tmpdf
)

#population
#downloaded from: https://data.oecd.org/pop/population.htm
setwd(datadir); dir()
popdf<-fread(
  'DP_LIVE_31012023155513737.csv'
)
names(popdf)<-tolower(names(popdf))
names(popdf)[1]<-c("country")
popdf<-popdf[
  indicator=='POP' &
    subject=='TOT' & 
    measure=='MLN_PER' & 
    frequency=='A' & 
    time=='2020',
  c('country','time','value')
]
names(popdf)[names(popdf)=='value']<-'population'
spdf<-merge(
  spdf,
  popdf,
  by=c('country','time'),
)

#convert to thousands of dollars
#gdp/pop * pop*10^6 * pc_gdp/100
spdf$gdp <- (spdf$gdp_capita * (spdf$population * 10^6)) 
spdf$value_dollars <- spdf$value/100 * spdf$gdp
spdf$value_dollarspc <- spdf$value_dollars/(spdf$population*10^6)

#this is a set of country codes,
#to convert three-letter codes to countrynames
setwd(datadir); dir()
tmpdf<-fread(
  '34107835.csv',
  skip=1
)
tmpdf$V3<-tmpdf$V4<-NULL
names(tmpdf)<-c("country","countryname")
spdf<-merge(
  spdf,
  tmpdf,
  by='country',
  all.x=T
)
spdf$advanced<-F
tmp<-spdf$country%in%c(
  "AUT",
  "AUS",
  "BEL",
  "CHE",
  "DEU",
  "DNK",
  "ESP",
  "FIN",
  "FRA",
  "GBR",
  "ITA",
  "JPN",
  #"LUX", #cut b/c small population
  "NLD",
  "NOR",
  "SWE",
  "USA"
)
spdf$advanced[tmp]<-T
spdf<-data.table(spdf)

#for each country,
#calculate the ratios
spdf$subject %>% unique 
tmpsubjects<-c("TOT","PUBORD","SOCPROT","HEALTH","EDU")
tmpdf<-spdf[
    subject%in%tmpsubjects &
    advanced
  ,
]
tmpdf$measure<-NULL
tmpdf$gdp_capita<-
  tmpdf$population<-
  tmpdf$gdp<-
  tmpdf$value_dollars<-NULL
tmpdf<-gather(
  tmpdf,
  var,
  val,
  value:value_dollarspc,
)
tmpdf<-spread(
  tmpdf,
  subject,
  val,
) %>% data.table

#get ratios
names(tmpdf)<-tolower(names(tmpdf))
tmpdf$ratio_sp<-
  (tmpdf$edu + tmpdf$health + tmpdf$socprot)/
  tmpdf$pubord
tmpdf$ratio_s2p<-(tmpdf$socprot/tmpdf$pubord)
tmpdf$ratio_s1p<-(tmpdf$edu + tmpdf$health)/tmpdf$pubord

#make plot
plotdf<-tmpdf[tmpdf$var=='value_dollarspc',]

#calcs
calcs_list[['US Spending on Public Order vs. Others']] <- 
  100 * (plotdf$pubord[plotdf$country=='USA']/
  mean(plotdf$pubord[plotdf$country!='USA']) - 1)

calcs_list[['Average Ratio in Developed Countries']] <- 
  mean(plotdf$ratio_sp)
median(plotdf$ratio_sp)
calcs_list[['Ratio in USA']] <- plotdf$ratio_sp[plotdf$country=='USA']
calcs_list[['Ratio in Denmark']] <- plotdf$ratio_sp[plotdf$countryname=='Denmark']


# DEPRECATED (we don't use this graph)
# tmporder<-order(plotdf$ratio_sp)
# tmplevels<-plotdf$countryname[tmporder]
# plotdf$countryname<-factor(
#   plotdf$countryname,
#   tmplevels
# )
# plotdf$usa<-plotdf$country=="USA"
# tmpcolors<-c("red","grey")
# names(tmpcolors)<-c(T,F)
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=countryname,
#     y=ratio_sp,
#     fill=usa
#   )
# ) +
#   geom_hline(
#     yintercept=mean(plotdf$ratio_sp),
#     linetype='dashed',
#     alpha=0.5
#   ) +
#   geom_bar(
#     stat='identity',
#     color='black',
#     width=0.7
#   ) +
#   scale_fill_manual(
#     guide='none',
#     values=tmpcolors
#   ) +
#   xlab("") +
#   ylab("\nSocial Spending / Penal Spending") +
#   coord_flip() +
#   theme_bw() 
# setwd(outputdir)
# ggsave(
#   filename='fig_spvspspending.png',
#   plot=g.tmp,
#   width=6,
#   height=6
# )

#calculations for text
#to calculate what USA spends
#we use these numbers and a measure of USA gdp in 2020
usapop_2020 <- 331.449520 * 10^6 #https://www.census.gov/quickfacts/fact/table/US/PST045222
usagdp_2020 <- 20.93 * 10^12 #https://www.bea.gov/news/2021/gross-domestic-product-4th-quarter-and-year-2020-advance-estimate

#pct spent on public order
puborderpct_usa_2020 <-
  (tmpdf$pubord[tmpdf$country=='USA' & tmpdf$var=='value'])/100
#pct spend on social protection, education and health
socspendpct_usa_2020 <- 
  (tmpdf$socprot[tmpdf$country=='USA' & tmpdf$var=='value'] +
  tmpdf$edu[tmpdf$country=='USA' & tmpdf$var=='value'] + 
  tmpdf$health[tmpdf$country=='USA' & tmpdf$var=='value'])/100

#$ amount spent (in billions)
puborder_usa_2020 <- (puborderpct_usa_2020 * usagdp_2020)/10^9
calcs_list[['USA Spending on Public Order (billiions)']] <- puborder_usa_2020 
socspend_usa_2020 <- (socspendpct_usa_2020 * usagdp_2020)/10^9
calcs_list[['USA Spending on Social Protection, Education and Health (billions)']] <- socspend_usa_2020

#how does the US rank relative to other countries
tmpdf$sp<-tmpdf$edu + tmpdf$health + tmpdf$socprot
mydf<-tmpdf[var=='value',]
mydf[order(mydf$sp),c('countryname','sp')]
mydf[order(mydf$socprot),c('countryname','socprot')]

#amount that diverting to social spending would increase social spending
calcs_list[['What Would Reinvestment do to Social Spending? (% increase)']] <- 
  100 * (( (puborder_usa_2020 + socspend_usa_2020)/socspend_usa_2020) -1)

#amount that distributing puborder spending would do
#if we gave universal grant to each household
usahh_2020 <- 124010992 #https://www.census.gov/quickfacts/fact/table/US/HSD410221
calcs_list[['If Distributed as a Universal Grant?']] <- 
  10^9 * puborder_usa_2020/usahh_2020
#if we gave as targeted grant to each black familiy below the poverty line
blackhhpoverty_2020 <- 1795 * 10^3 #table 4 at https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-people.html
calcs_list[['If Distributed as a Hyper-Targeted Grant?']] <- 
  10^9 * puborder_usa_2020/blackhhpoverty_2020

#how much would it cost to match Nordic's level of social spending?
tmp<-tmpdf$countryname%in%c('Norway','Sweden','Denmark','Finland')
socspendpct_nordic_2020 <- mean(
  (tmpdf$socprot[tmp & tmpdf$var=='value'] +
    tmpdf$edu[tmp & tmpdf$var=='value'] + 
    tmpdf$health[tmp & tmpdf$var=='value'])/100
)
usagap <- socspendpct_nordic_2020 - socspendpct_usa_2020
calcs_list[['How Much Would Reinvestment Close the Gap to Nordics?']]<- 
  usagap/puborderpct_usa_2020
calcs_list[['How Much Would Reinvestment Close the Gap to Nordics? (%)']]<- 
  100 * 1/(usagap/puborderpct_usa_2020)

#make a quick plot of this
plotdf<-data.frame(
  x=c('Social','Penal'),
  y=c(socspend_usa_2020,puborder_usa_2020)
)
plotdf$x<-factor(plotdf$x)

# blues<-hcl(
#   h=seq(210,240,length=3),
#   l=65,
#   c=100
# )
# reds<-hcl(
#   h=seq(0,60,length=6),
#   l=65,
#   c=100
# )
tmpcolors<-c('#FF6C91','#00B4F0')
names(tmpcolors)<-levels(plotdf$x)
g.tmp<-ggplot(
  plotdf,
  aes(
    x=x,
    y=y/10^3,
    fill=x
  )
) +
  geom_bar(
    stat='identity',
    width=0.3,
    color='black'
  ) +
  scale_fill_manual(
    guide='none',
    values=tmpcolors
  ) +
  coord_flip() +
  theme_bw() + 
  xlab("") + 
  ylab("\nUS Government Spending (in trillions)")

setwd(outputdir)
ggsave(
  filename='fig_govspending.png',
  plot=g.tmp,
  width=6,
  height=2
)

#########################################################
#########################################################

#write out calculations
setwd(outputdir); dir()
write('Calculations for Abolition of What?','calculations.txt')
write('###','calculations.txt',append=T)
lapply(
  seq_along(calcs_list),
  function(i) {
    print(i)
    write(names(calcs_list)[i],'calculations.txt',append=T)
    write(calcs_list[[i]],'calculations.txt',append=T)
    write("###",'calculations.txt',append=T)
  }
)
