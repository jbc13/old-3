# Definições dos Testes de QC
Jonas Corrêa (graduando) e Jônatan Tatsch (orientador)  
`r format(Sys.time(), '%d %B, %Y')`  

# Convenções dos testes de controle de qualidade

Cada teste de controle de qualidade (QC) será referenciado por um número (seguido a descrição do texto), o dado será considerado suspeito quando os testes abaixo forem válidos:

- `QC 1:` Teste do Intervalo de Variação (TIV) 

    - `1a:` Baseado em limites estabelecidos pelo intervalo de medida instrumental

        - $T_{avg} < -50 °C$ ou $T_{avg} > 60 °C$ ;

             Onde:
         
               $T_{avg}=(T_{max}+T_{min}) ÷ 2$

    - `1b:` Baseado no limites máximos e mínimos absolutos históricos da estação climatológica (EMC) mas próxima da EMA

         - $T_{avg} < T_{low, j, m}$ ou $T_{avg} > T_{high, j, m}$ ;

             Onde:

               $j$: estação climatológica mais próxima da EMA

               $m = 1,...,12$ (mês do ano)


- `QC 2:` Teste de Persistência Temporal (TPT) 

    - Valores consecutivos repetidos

        - $T_{avg} (h) = T_{avg} (h+1) = T_{avg} (h+2) = ... = T_{avg} (h+n)$

             Onde:

               $n = 1h, 2h, 3h, 4h, 5h,...$

- ` QC 3:` Teste de Consistência Interna (TCI)

      - `3a:`
          - $T_{min}(h) ≥ T_{max}(h)$

      - `3b:`
          - $T_{inst}(h) < T_{min}(h)$ ou $T_{inst}(h) > T_{max}(h)$

      - `3c_filter:`
      
         Testes para filtragem dos dados válidos de $T_{d\_avg}$ para uso deles no teste 3c
      
          - $T_{d\_min}(h) ≥ T_{d\_max}(h)$
          - $T_{d\_inst}(h) < T_{d\_min}(h)$ ou $T_{d\_inst}(h) > T_{d\_max}(h)$

      - `3c:`
          - $T_{avg}(h) < T_{d\_avg\_f}(h)$

             Onde:

               Para poder fazer essa comparação com $T_{avg}(h)$, foram usados somente os dados válidos de $T_{d\_avg}(h)$ aprovados após o filtro do rmd _3c_filter_, os demais dados considerados como suspeitos receberam NA em suas respectivas horas, onde:

               $T_{d\_avg}=(T_{d\_max}+T_{d\_min}) ÷ 2$

               $T_{d\_avg\_f} = T_{d\_avg} + filtro$

      - `3d:`
          - $T_{avg}(d) < T_{min}(d)$ ou $T_{avg}(d) > T_{max}(d)$

             Onde:

               $T_{avg}(d) = sum(T_{avg}(h)) ÷ 24$

               $T_{min}(d) = min(T_{min}(h))$

               $T_{max}(d) = max(T_{max}(h))$

      - `3e:`
          - $T_{max}(d) < T_{min}(d-1)$

      - `3f:`
          - $T_{min}(d) ≥ T_{max}(d-1)$


- `QC 4:` Teste Consistência Temporal / taxa de variação horária (TCT)

    - `4a:`
          - $|T_{avg}(h) - T_{avg}(h-dt)| > T_{tol} (dt)$

             Onde:

               $dt =$ 1h, 2h, 3h, 6h e 12h

               $T_{tol} (dt) =$ 4°C, 7°C, 9°C, 15°C e 25°C

    - `4b:` Teste de QC *Valente & Tatsch (2017)*


- `QC 5:` Teste Consistência Espacial (TCE)


- `QC 6:` Teste Homogeneidade Temporal (THT)

# Especificação dos sensores das EMAs do INMET.

| Variável               | Sensor     | Acurácia                                | Intervalo de medida | resolução    | altura acima do solo |
|------------------------|------------|-----------------------------------------|---------------------|--------------|----------------------|
| Temperatura do ar      | Pt100      | $\pm0,08$                               | -50°C/60°C            | 0,1°C ?      | 2 m                  |
| Umidade relativa do ar | Humicap180 | $\pm2\%$ (0/90 %), $\pm3\%$ (90/100%)  | 0,8/100 \%          | 0,1% ?       | 2 m                  |
| Precipitação           | QMR102     | < 24 mm/h ($\pm 1\%$), < 120 mm/h ($\pm5\%$) | 0/120 mm/h          | 0,2mm /pulso | 1,5 m                |



