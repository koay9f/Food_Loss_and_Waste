#!/usr/bin/env python
# coding: utf-8

# In[5]:


import math
import pandas as pd
import os
import random
import numpy
#Reliability,Temporal correlation ,Geographical correlation, Further technological correlation,Completeness.Sample size
Pedree_matrix=[[1,1.05,1.10,1.20,1.5],[1,1.03,1.10,1.20,1.5],
               [1,1.01,1.02,1.05,1.1],[1,1.1,1.2,1.5,2],[1,1.02,1.05,1.10,1.2],
               [1,1.02,1.02,1.1,1.2]]
Basic_f=1.05
def GSD(pedigree_score):
    return math.exp(math.sqrt(sum(math.log(Pedree_matrix[i][pedigree_score[i]-1])**2 
                                  for i in range(len(pedigree_score)))+math.log(Basic_f)**2))
        
def CV(pedigree_score):
    return math.sqrt(math.exp(math.log(GSD(pedigree_score))**2)-1)
def LU_UB(pedigree_score,mu):
    b=(mu*2*CV(pedigree_score)*math.log(3)+mu*2)/2
    a=mu*2-b
    return (a,b)
Food_comodity=['Grain','Fruit','Vege','Fish','Nut','Sugar','Oil','Milk','P&M','Egg']
path='/Users/wenquandong/OneDrive/FLW uncertainty'
data=os.path.join(path,'FLW uncertainty analysis.xlsx')
onfarm_unhar=pd.read_excel(data,sheet_name='Onfarm Unharvest')
On_farm_p={i:onfarm_unhar[i][0] for i in Food_comodity}
f_unharvested={i:onfarm_unhar[i][1] for i in Food_comodity}
f_unharvested_pedigree_score={i:[int(onfarm_unhar[i][j]) for j in range(2,len(onfarm_unhar[i]))]  for i in Food_comodity}
gsd_onfarm_unhar={i:GSD(f_unharvested_pedigree_score[i]) for i in Food_comodity}
cv_onfarm_unhar={i:CV(f_unharvested_pedigree_score[i]) for i in Food_comodity}
lu_ub_onfarm_unhar_f={i:LU_UB(f_unharvested_pedigree_score[i],f_unharvested[i]) for i in Food_comodity}
def onfarm_unharw(commodity,factor):
    if commodity=='Grain':
        return (On_farm_p[commodity]*factor)
    else:
        return (On_farm_p[commodity]/(1-factor)*factor)
unsoldc_w_f=['Sugar','Oil','Milk','P&M','Egg'] #unsold commodity with factor
onfarm_unsold=pd.read_excel(data,sheet_name='Onfarm Unsold')
f_unsold={i:onfarm_unsold[i][1] for i in unsoldc_w_f}
f_unsold_pedigree_score={i:[int(onfarm_unsold[i][j]) for j in range(2,len(onfarm_unsold[i]))]  for i in unsoldc_w_f}
gsd_onfarm_unsold={i:GSD(f_unsold_pedigree_score[i]) for i in unsoldc_w_f}
cv_onfarm_unsold={i:CV(f_unsold_pedigree_score[i]) for i in unsoldc_w_f}
lu_ub_onfarm_unsold_f={i:LU_UB(f_unsold_pedigree_score[i],f_unsold[i]) for i in unsoldc_w_f}
def onfarm_unsold(commodity,factor):
    if commodity=='Sugar':
        return ((On_farm_p[commodity]-1.74-5.850515521)*factor)
    elif commodity=='Milk':
        return ((On_farm_p[commodity]-3.78471965501585-93/2204.623)*factor)
    else:
        return (On_farm_p[commodity]*factor)
########FLW analysis to USDA factors####
USDA_uncertainty=pd.read_excel(data,sheet_name='USDA LAFA uncertainty')
f_primary_pediscore=[USDA_uncertainty['Loss Factor at Primary Level'][i] for i in range(6)]
f_consumer_pediscore=[USDA_uncertainty['Loss Factor at Consumer level'][i] for i in range(6)]
f_retail_pediscore=[USDA_uncertainty['Loss Factor at Retail Level'][i] for i in range(6)]
gsd_f_primary=GSD(f_primary_pediscore) 
gsd_f_consumer=GSD(f_consumer_pediscore) 
gsd_f_retail=GSD(f_retail_pediscore) 
cv_f_primary=CV(f_primary_pediscore) 
cv_f_consumer=CV(f_consumer_pediscore) 
cv_f_retail=CV(f_retail_pediscore)
commodity_wo_primary=['Sugar','Oil','Milk','P&M','Egg']
commodity_w_primary=['Grain','Fruit','Vege','Fish','Nut']
USDA_data1=pd.read_excel(data,sheet_name='USDA LAFA data 1')
USDA_data2=pd.read_excel(data,sheet_name='USDA LAFA data 2')
retail_weight={i: USDA_data1[i][0] if i in USDA_data1 else USDA_data2[i][1]  for i in Food_comodity }
f_retail={i: USDA_data1[i][1] if i in USDA_data1 else USDA_data2[i][2]  for i in Food_comodity }
f_consumer={i: USDA_data1[i][2] if i in USDA_data1 else USDA_data2[i][3]  for i in Food_comodity }
f_primary={i:USDA_data2[i][0] for i in commodity_w_primary}
lu_ub_retail={i:LU_UB(f_retail_pediscore,f_retail[i]) for i in Food_comodity}
lu_ub_consumer={i:LU_UB(f_consumer_pediscore,f_consumer[i]) for i in Food_comodity}
lu_ub_primary={i:LU_UB(f_primary_pediscore,f_primary[i]) for i in commodity_w_primary}
lu_ub_primary['Fish']=(0.546099,0.553)
lu_ub_primary['Fruit']=(0.14155028172767022,0.21017282254315154)
def retail_loss(commodity,factor):
    return (retail_weight[commodity]*factor)
def consumer_loss(commodity,f_retail,f_consumer):
    return (retail_weight[commodity]*(1-f_retail)*f_consumer)
####distribution factor
dis_uncertainty=pd.read_excel(data,sheet_name='Distribution')
f_dis_pediscore=[dis_uncertainty['score'][i] for i in range(6)]
gsd_f_dis=GSD(f_dis_pediscore) 
cv_f_dis=CV(f_dis_pediscore) 
f_dis={i: 0.05  for i in Food_comodity }
#f_dis['Fish']=0.052631579



lu_ub_dis={i:LU_UB(f_dis_pediscore,f_dis[i]) for i in Food_comodity}  
def dis_loss(commodity,factor):
    return retail_weight[commodity]/(1-factor)*factor
#####start calculation confidence interval 
on_farm_waste_unhar={i:[] for i in Food_comodity}
on_farm_waste_unsold={i:[] for i in Food_comodity}
manufacture_waste={i:[] for i in Food_comodity}


dis_waste={i:[] for i in Food_comodity}
retail_waste={i:[] for i in Food_comodity}
consumer_waste={i:[] for i in Food_comodity}
Med={i:[] for i in Food_comodity}
Net={i:[] for i in Food_comodity}
R={i:[] for i in Food_comodity}
Pre={i:[] for i in Food_comodity}
Primary={i:[] for i in commodity_w_primary}
for iteration in range(5000):
    #random.seed(iteration)
    nf_unhar={i:random.uniform(lu_ub_onfarm_unhar_f[i][0],lu_ub_onfarm_unhar_f[i][1]) for i in Food_comodity}
    unharvested={i: onfarm_unharw(i,nf_unhar[i]) for i in Food_comodity}
    
    nf_unsold={i:random.uniform(lu_ub_onfarm_unsold_f[i][0],lu_ub_onfarm_unsold_f[i][1]) for i in unsoldc_w_f}
    unsold_withf={i: onfarm_unsold(i,nf_unsold[i]) for i in unsoldc_w_f} ###unsold calculated with factor
    other_data=pd.read_excel(data,sheet_name='Other Data')
    other_onfarm_reduction={i:other_data[i][0] for i in Food_comodity}
    other_market_reduction={i:other_data[i][1] for i in Food_comodity}
    netproduction={i:On_farm_p[i]-unsold_withf[i]- other_onfarm_reduction[i] if i in unsoldc_w_f 
                   else On_farm_p[i]- other_onfarm_reduction[i]
                   for i in Food_comodity}
    nf_retail={i:random.uniform(lu_ub_retail[i][0],lu_ub_retail[i][1]) for i in Food_comodity}
    nf_consumer={i:random.uniform(lu_ub_consumer[i][0],lu_ub_consumer[i][1]) for i in Food_comodity}
    retail_l={i: retail_loss(i,nf_retail[i]) for i in Food_comodity}#
    consumerwieght={i:retail_weight[i]-retail_l[i] for i in Food_comodity}
    consumer_l={i: consumer_loss(i,nf_retail[i],nf_consumer[i]) for i in Food_comodity}
    consumedwieght={i:retail_weight[i]-retail_l[i]-consumer_l[i] for i in Food_comodity}
    nf_dis={i: random.uniform(lu_ub_dis[i][0],lu_ub_dis[i][1]) for i in Food_comodity}
    dis_l={i: dis_loss(i,nf_dis[i]) for i in Food_comodity}
    Pre_dis={i: dis_l[i]+ retail_weight[i] for i in Food_comodity}
    foodmanufactured={i: Pre_dis[i]+ other_market_reduction[i] for i in Food_comodity}
    nf_primary={i:random.uniform(lu_ub_primary[i][0],lu_ub_primary[i][1]) for i in commodity_w_primary}
    
    primary_weight={i:foodmanufactured[i]/(1-nf_primary[i]) for i in commodity_w_primary}
    manufacturedloss={i: netproduction[i]-foodmanufactured[i] if i in commodity_wo_primary 
                      else primary_weight[i]-foodmanufactured[i] for i in Food_comodity}
    unsold_withoutf={i:netproduction[i]-primary_weight[i] for i in commodity_w_primary}
    for i in Food_comodity:
        on_farm_waste_unhar[i].append(unharvested[i])
        manufacture_waste[i].append(manufacturedloss[i])
        dis_waste[i].append(dis_l[i])
        retail_waste[i].append(retail_l[i])
        consumer_waste[i].append(consumer_l[i])
        if i in commodity_w_primary:
            on_farm_waste_unsold[i].append(unsold_withoutf[i])
        else:
            on_farm_waste_unsold[i].append(unsold_withf[i])
        if i in Primary:
            Primary[i].append(primary_weight[i])
        Med[i].append(foodmanufactured[i])
        Net[i].append(netproduction[i])
        Pre[i].append(Pre_dis[i])
    #print (netproduction['Fish'],primary_weight['Fish'],foodmanufactured['Fish'],retail_weight['Fish'],nf_primary['Fish'])
import xlwt 
b = xlwt.Workbook()
sheet=b.add_sheet('unsold')
print ('unsold')
index=0
for i in lu_ub_onfarm_unsold_f:

    #data=on_farm_waste_unsold[i]
    sheet.write(index,0,i)
    #sheet.write(index,0,i)
    sheet.write(index,1,lu_ub_onfarm_unsold_f[i][0])
    sheet.write(index,2,lu_ub_onfarm_unsold_f[i][1])
    #interval=st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data))
    #sheet.write(index,2,interval[0])
    #sheet.write(index,3,interval[1])
    index+=1
    #print (i,numpy.mean(on_farm_waste_unsold[i]),
          #st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data)))
sheet=b.add_sheet('unharvest')
#print ('unsold')
index=0
print ('unharvest')
for i in lu_ub_onfarm_unhar_f:

    #data=on_farm_waste_unsold[i]
    sheet.write(index,0,i)
    #sheet.write(index,0,i)
    sheet.write(index,1,lu_ub_onfarm_unhar_f[i][0])
    sheet.write(index,2,lu_ub_onfarm_unhar_f[i][1])
    #interval=st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data))
    #sheet.write(index,2,interval[0])
    #sheet.write(index,3,interval[1])
    index+=1
    #print (i,numpy.mean(data),
          # st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data)))
sheet=b.add_sheet('manufacturing loss')
#print ('unsold')
index=0
print ('manufacturing loss')###
for i in lu_ub_primary:

    #data=on_farm_waste_unsold[i]
    sheet.write(index,0,i)
    #sheet.write(index,0,i)
    sheet.write(index,1,lu_ub_primary[i][0])
    sheet.write(index,2,lu_ub_primary[i][1])
    #interval=st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data))
    #sheet.write(index,2,interval[0])
    #sheet.write(index,3,interval[1])
    index+=1
sheet=b.add_sheet('dis_waste')
#print ('unsold')
index=0
print ('dis_waste')###
for i in lu_ub_dis:

    #data=on_farm_waste_unsold[i]
    sheet.write(index,0,i)
    #sheet.write(index,0,i)
    sheet.write(index,1,lu_ub_dis[i][0])
    sheet.write(index,2,lu_ub_dis[i][1])
    #interval=st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data))
    #sheet.write(index,2,interval[0])
    #sheet.write(index,3,interval[1])
    index+=1
sheet=b.add_sheet('retail_waste')
#print ('unsold')
index=0
print ('retail_waste')###
for i in lu_ub_retail:

    #data=on_farm_waste_unsold[i]
    sheet.write(index,0,i)
    #sheet.write(index,0,i)
    sheet.write(index,1,lu_ub_retail[i][0])
    sheet.write(index,2,lu_ub_retail[i][1])
    #interval=st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data))
    #sheet.write(index,2,interval[0])
    #sheet.write(index,3,interval[1])
    index+=1
print ('consumer_waste')###
sheet=b.add_sheet('consumer_waste')
#print ('unsold')
index=0
for i in lu_ub_consumer:

    #data=on_farm_waste_unsold[i]
    sheet.write(index,0,i)
    #sheet.write(index,0,i)
    sheet.write(index,1,lu_ub_consumer[i][0])
    sheet.write(index,2,lu_ub_consumer[i][1])
    index+=1
b.save('uncertainty result 3011.xls')  


# In[2]:


import xlwt 
b = xlwt.Workbook()
sheet=b.add_sheet('unsold')
print ('unsold')
index=0
for i in Food_comodity:
   

    data=on_farm_waste_unsold[i]
    sheet.write(index,0,i)
    #sheet.write(index,0,i)
    sheet.write(index,1,numpy.mean(data))
    interval=st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data))
    sheet.write(index,2,interval[0])
    sheet.write(index,3,interval[1])
    index+=1
    print (i,numpy.mean(on_farm_waste_unsold[i]),
           st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data)))
sheet=b.add_sheet('unharvest')
#print ('unsold')
index=0
print ('unharvest')
for i in Food_comodity:
    data=on_farm_waste_unhar[i]
    sheet.write(index,0,i)
    #sheet.write(index,0,i)
    sheet.write(index,1,numpy.mean(data))
    interval=st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data))
    sheet.write(index,2,interval[0])
    sheet.write(index,3,interval[1])
    index+=1
    print (i,numpy.mean(data),
           st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data)))
sheet=b.add_sheet('manufacturing loss')
#print ('unsold')
index=0
print ('manufacturing loss')###
for i in Food_comodity:
    data= manufacture_waste[i]
    sheet.write(index,0,i)
    #sheet.write(index,0,i)
    sheet.write(index,1,numpy.mean(data))
    interval=st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data))
    sheet.write(index,2,interval[0])
    sheet.write(index,3,interval[1])
    index+=1
    print (i,numpy.mean(data),
           st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data)))
sheet=b.add_sheet('dis_waste')
#print ('unsold')
index=0
print ('dis_waste')###
for i in Food_comodity:
    data= dis_waste[i]
    sheet.write(index,0,i)
    #sheet.write(index,0,i)
    sheet.write(index,1,numpy.mean(data))
    interval=st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data))
    sheet.write(index,2,interval[0])
    sheet.write(index,3,interval[1])
    index+=1
    print (i,numpy.mean(data),
           st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data)))
sheet=b.add_sheet('retail_waste')
#print ('unsold')
index=0
print ('retail_waste')###
for i in Food_comodity:
    data= retail_waste[i]
    sheet.write(index,0,i)
    #sheet.write(index,0,i)
    sheet.write(index,1,numpy.mean(data))
    interval=st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data))
    sheet.write(index,2,interval[0])
    sheet.write(index,3,interval[1])
    index+=1
    print (i,numpy.mean(data),
           st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data)))
print ('consumer_waste')###
sheet=b.add_sheet('consumer_waste')
#print ('unsold')
index=0
for i in Food_comodity:
    data= consumer_waste[i]
    sheet.write(index,0,i)
    #sheet.write(index,0,i)
    sheet.write(index,1,numpy.mean(data))
    interval=st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data))
    sheet.write(index,2,interval[0])
    sheet.write(index,3,interval[1])
    index+=1
    print (i,numpy.mean(data),
           st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data)))
b.save('uncertainty result 3011.xls')  


# In[340]:


####calculate the confidence interval 95%
import scipy.stats as st
####on_farm stage
for i in Food_comodity:
    data=on_farm_waste_unsold[i]
    print (i,numpy.mean(data),st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data)) )
for i in Food_comodity:
    data=on_farm_waste_unhar[i]
    print (i,st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data)) )
for i in Food_comodity:
    data=manufacture_waste[i]
    print (i,st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data)) )
for i in Food_comodity:
    data=dis_waste[i]
    print (i,st.t.interval(alpha=0.95, df=len(data)-1, loc=numpy.mean(data), scale=st.sem(data)) )

