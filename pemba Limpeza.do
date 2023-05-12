clear all
set more off


  
	
	  global master   "C:\Users\MUVA\OneDrive\Projectos Rstudio\2023\app_susamati\pemba"
	  

//"C:\Users\Dercio\OneDrive\Projectos Rstudio\2023\app_susamati\pemba\Sanear pemba v3\Saneamento_pemba.dta"
	 
	*----------------------------------------------------------------------	
		//global produtores "$master/01 Sessoes com produtores"
		//global comunitaria  "$master/02 Sessoes comunitarias"
		//global parceiros "$master/03 Sessoes com parceiros"  
	*----------------------------------------------------------------------	
	**----------------------------------------------------------------------	
		 
	*----------------------------------------------------------------------	
	*----------------------------------------------------------------------	

	*----------------------------------------------------------------------	
 
	 ***Criando ficheiros temporarios 
	 *----------------------------------
	 tempfile gorogonsa chimanimani G1 G2 G3 G4 G5 CH1 CH2 CH3 CH4 CH5 CH6 CH7 ///
	 CH8 geral GS1 GS2 GS3 GS4 GS5 GS6 GS7 GS8 CHS1 CHS2 CHS3 CHS4 CHS5 CHS6 CHS7 CHS8 geral 1 2 3 4 5 6 7 8 9
	 *----------------------------------
	 
	 use "$master/Sanear pemba v1\Saneamento_pemba.dta", clear
	 gen versao=1
	 decode Inqueridor , generate(newvar)
	 drop Inqueridor
	 ren  newvar Inqueridor
     save `1', replace 
	 clear 
	 
	 use "$master/Sanear pemba v2\Saneamento_pemba.dta", clear
	 gen versao=2
	 decode Inqueridor , generate(newvar)
	 drop Inqueridor
	 ren  newvar Inqueridor
     save `2', replace 
	 clear 
	 
	 use "$master/Sanear pemba v3\Saneamento_pemba.dta", clear
	 gen versao=3
	 decode Inqueridor , generate(newvar)
	 drop Inqueridor
	 ren  newvar Inqueridor
     save `3', replace 
	 
	 merge m:m interview__id using `1' 
	 drop _merge
	 merge m:m interview__id using `2'
	 
	 replace date="" if date=="##N/A##"
     drop _merge
	 
	 replace Inqueridor="Kelvin  Vanhiua" if Inqueridor=="Tabu Magaga"
	 
	 drop interview__key interview__id
	 
	 gen data = substr(date, 1, 10)
	 destring data, replace
	 //drop if consentimento==.

     replace nr_telefones=0 if nr_telefones==999
	 replace nr_smartphone=0 if nr_smartphone==999
	 replace nr_analogico=0 if nr_analogico==999
	 
	 
	 gen membros_familia= adultos_nr + nr_criancas
	 gen nrmembros_familia=""
	 replace nrmembros_familia="[1;3]" if membros_familia<4
	 replace nrmembros_familia="[4;6]" if membros_familia>=4 & membros_familia<7
	 replace nrmembros_familia="[7;9]" if membros_familia>=7 & membros_familia<10
	 replace nrmembros_familia="[10;12]" if membros_familia>=10 & membros_familia<13
	 replace nrmembros_familia="[13;15]" if membros_familia>=13 & membros_familia<20
 
      
	 
	 order date consentimento nome idade sexo tempo_cidade data   Inqueridor     membros_familia ///
	 nrmembros_familia nr_adultos_feminino adultos_nr nr_adultos_masculino ///
	 nr_criancas nr_criancas_feminino nr_criancas_masculino chefe_agregado ///
	 idade_chefe sexo_chefe residencia_local chao_casa chao_especifique parede ///
	 casa_cobertura material_cobertura fonte_agua especifique_fonte_agua ///
	 fonte_cozinha fonte_energia aparelhos_funcionais__1 aparelhos_funcionais__2 ///
	 aparelhos_funcionais__3 aparelhos_funcionais__4 aparelhos_funcionais__5 ///
	 aparelhos_funcionais__6 aparelhos_funcionais__7 aparelhos_funcionais__8 ///
	 aparelhos_funcionais__9 aparelhos_funcionais__10 aparelhos_funcionais__11 ///
	 aparelhos_funcionais__12 aparelhos_funcionais__13 nr_telefones nr_smartphone ///
	 nr_analogico rendimento_familia rendimento_tipo rendimento_faixa
	 
	 //idade de chefe da damilia
	 replace idade_chefe =40 if idade_chefe==4
	 gen int_idadeChefe=""
	 replace int_idadeChefe="[20;39]" if idade_chefe<40
	 replace int_idadeChefe="[40;59]" if idade_chefe>=40 & idade_chefe<60
	 replace int_idadeChefe="[60;79]" if idade_chefe>=60 & idade_chefe<80
	 replace int_idadeChefe="[80;100]" if idade_chefe>=80 & idade_chefe<100
	 
	 decode tipo_casa_banho, gen(tipo_casa_banho1)
	 replace tipo_casa_banho1="Sem Latrina" if tipo_casa_banho1=="" | tipo_casa_banho1==".a"
	 
	 replace GPS_dentro__Latitude=Gps_fora__Latitude if GPS_dentro__Latitude==.
	 replace GPS_dentro__Longitude=Gps_fora__Longitude if GPS_dentro__Longitude==.
	 replace GPS_dentro__Latitude=Gps_fora__Latitude if GPS_dentro__Latitude==.a
	 replace GPS_dentro__Longitude=Gps_fora__Longitude if GPS_dentro__Longitude==.a
	 
	 label define rendimento_tipo 1 "Trabalham" 2 "Fazem biscato" 3 "Tem negocio" 5 "outro especificar" 6 "Machamba" 7 "Machamba", replace
	 replace rendimento_tipo=6 if rendimento_tipo==7
	 drop if Tem_latrina==. | Tem_latrina==.a
	 
	 drop tipo_casa_banho1
     decode tipo_casa_banho, generate(tipo_casa_banho1)
     replace tipo_casa_banho1="Sem Lantrina" if tipo_casa_banho1==""
	 replace rendimento_tipo=3 if rendimento_tipo==4
	 
	 replace quanto_contribuir=0 if quanto_contribuir==.a
	 
	 gen faixa_contribuir=""
	 replace faixa_contribuir="0" if quanto_contribuir==.
	 replace faixa_contribuir="0" if quanto_contribuir==.a 
	 replace faixa_contribuir="0" if quanto_contribuir==0
	 replace faixa_contribuir="[1-100 Meticais]" if quanto_contribuir>0 & quanto_contribuir<=100
	 replace faixa_contribuir="[100-500 Meticais]" if quanto_contribuir>=100 & quanto_contribuir<500
	 replace faixa_contribuir="[500 - 1000 Meticais]" if quanto_contribuir>=500 & quanto_contribuir<1000
	 replace faixa_contribuir="[1000 - 2000 Meticais]" if quanto_contribuir>=1000 & quanto_contribuir<2000
	 replace faixa_contribuir="[2000 - 4000 Meticais]" if quanto_contribuir>=2000 & quanto_contribuir<4000
	 replace faixa_contribuir="[4000 - 6000 Meticais]" if quanto_contribuir>=4000 & quanto_contribuir<6000
	 replace faixa_contribuir="[6000 - 8000 Meticais]" if quanto_contribuir>=6000 & quanto_contribuir!=.

     replace tem_condicoes=2 if tem_condicoes==. | tem_condicoes==.a
	  
	 
     replace disponivel_casa_banho=. if disponivel_casa_banho==.a


	 export excel using "$master/pemba_clear.xls", firstrow(variables) replace
	 export excel using "C:\Users\MUVA\OneDrive\Projectos Rstudio\2023\Susamati\data\pemba_clear.xls", firstrow(variables) replace
	 export excel using "C:\Users\MUVA\OneDrive\Projectos Rstudio\2023\chibue\data\pemba_clear.xls", firstrow(variables) replace
	 
	 
	 

	 ex
