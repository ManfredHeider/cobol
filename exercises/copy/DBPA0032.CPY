      *   Mainframe Express 3.1  6.018.0057
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-65)
      *   MapSet Name   BPA0032
      *   Date Created  03/07/2018
      *   Time Created  14:25:49

      *  Input Data For Map BPA0032
         01 BPA0032I.
            03 FILLER                         PIC X(12).
            03 EGESL                          PIC S9(4) COMP.
            03 EGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 EVGESL                         PIC S9(4) COMP.
            03 EVGESF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EVGESI                         PIC X(1).
            03 AVGESBML                       PIC S9(4) COMP.
            03 AVGESBMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVGESBMI                       PIC X(4).
            03 ASEIFBL                        PIC S9(4) COMP.
            03 ASEIFBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIFBI                        PIC X(7).
            03 ESEIAKTL                       PIC S9(4) COMP.
            03 ESEIAKTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESEIAKTI                       PIC X(3).
            03 ASEISTRL                       PIC S9(4) COMP.
            03 ASEISTRF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEISTRI                       PIC X(1).
            03 ASEIGESL                       PIC S9(4) COMP.
            03 ASEIGESF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIGESI                       PIC X(3).
            03 EABRVTTL                       PIC S9(4) COMP.
            03 EABRVTTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EABRVTTI                       PIC X(2).
            03 EABRVMML                       PIC S9(4) COMP.
            03 EABRVMMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EABRVMMI                       PIC X(2).
            03 EABRVJJL                       PIC S9(4) COMP.
            03 EABRVJJF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EABRVJJI                       PIC X(4).
            03 EABRBTTL                       PIC S9(4) COMP.
            03 EABRBTTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EABRBTTI                       PIC X(2).
            03 EABRBMML                       PIC S9(4) COMP.
            03 EABRBMMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EABRBMMI                       PIC X(2).
            03 EABRBJJL                       PIC S9(4) COMP.
            03 EABRBJJF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EABRBJJI                       PIC X(4).
            03 EPRODL                         PIC S9(4) COMP.
            03 EPRODF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPRODI                         PIC X(3).
            03 AWAEPBKL                       PIC S9(4) COMP.
            03 AWAEPBKF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AWAEPBKI                       PIC X(4).
            03 DFHG001I                       OCCURS 11 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 AGESL                          PIC S9(4) COMP.
               05 AGESF                          PIC X.
               05 FILLER                         PIC X(1).
               05 AGESI                          PIC X(2).
               05 AGESTBML                       PIC S9(4) COMP.
               05 AGESTBMF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AGESTBMI                       PIC X(10).
               05 AABRDAVL                       PIC S9(4) COMP.
               05 AABRDAVF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AABRDAVI                       PIC X(10).
               05 AABRDABL                       PIC S9(4) COMP.
               05 AABRDABF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AABRDABI                       PIC X(10).
               05 ASPROVBL                       PIC S9(4) COMP.
               05 ASPROVBF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ASPROVBI                       PIC X(15).
               05 AANZFL                         PIC S9(4) COMP.
               05 AANZFF                         PIC X.
               05 FILLER                         PIC X(1).
               05 AANZFI                         PIC X(7).
               05 AANZZL                         PIC S9(4) COMP.
               05 AANZZF                         PIC X.
               05 FILLER                         PIC X(1).
               05 AANZZI                         PIC X(9).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BPA0032
         01 BPA0032O REDEFINES BPA0032I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 EGESA                          PIC X.
            03 EGESH                          PIC X.
            03 EGESO                          PIC X(2).
            03 FILLER                         PIC X(2).
            03 AGESBMA                        PIC X.
            03 AGESBMH                        PIC X.
            03 AGESBMO                        PIC X(10).
            03 FILLER                         PIC X(2).
            03 EVGESA                         PIC X.
            03 EVGESH                         PIC X.
            03 EVGESO                         PIC X(1).
            03 FILLER                         PIC X(2).
            03 AVGESBMA                       PIC X.
            03 AVGESBMH                       PIC X.
            03 AVGESBMO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 ASEIFBA                        PIC X.
            03 ASEIFBH                        PIC X.
            03 ASEIFBO                        PIC X(7).
            03 FILLER                         PIC X(2).
            03 ESEIAKTA                       PIC X.
            03 ESEIAKTH                       PIC X.
            03 ESEIAKTO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 ASEISTRA                       PIC X.
            03 ASEISTRH                       PIC X.
            03 ASEISTRO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 ASEIGESA                       PIC X.
            03 ASEIGESH                       PIC X.
            03 ASEIGESO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 EABRVTTA                       PIC X.
            03 EABRVTTH                       PIC X.
            03 EABRVTTO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EABRVMMA                       PIC X.
            03 EABRVMMH                       PIC X.
            03 EABRVMMO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EABRVJJA                       PIC X.
            03 EABRVJJH                       PIC X.
            03 EABRVJJO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 EABRBTTA                       PIC X.
            03 EABRBTTH                       PIC X.
            03 EABRBTTO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EABRBMMA                       PIC X.
            03 EABRBMMH                       PIC X.
            03 EABRBMMO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EABRBJJA                       PIC X.
            03 EABRBJJH                       PIC X.
            03 EABRBJJO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 EPRODA                         PIC X.
            03 EPRODH                         PIC X.
            03 EPRODO                         PIC X(3).
            03 FILLER                         PIC X(2).
            03 AWAEPBKA                       PIC X.
            03 AWAEPBKH                       PIC X.
            03 AWAEPBKO                       PIC X(4).
            03 DFHG001O OCCURS 11.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 AGESA                          PIC X.
               05 AGESH                          PIC X.
               05 AGESO                          PIC X(2).
               05 FILLER                         PIC X(2).
               05 AGESTBMA                       PIC X.
               05 AGESTBMH                       PIC X.
               05 AGESTBMO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AABRDAVA                       PIC X.
               05 AABRDAVH                       PIC X.
               05 AABRDAVO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AABRDABA                       PIC X.
               05 AABRDABH                       PIC X.
               05 AABRDABO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 ASPROVBA                       PIC X.
               05 ASPROVBH                       PIC X.
               05 ASPROVBO                       PIC X(15).
               05 FILLER                         PIC X(2).
               05 AANZFA                         PIC X.
               05 AANZFH                         PIC X.
               05 AANZFO                         PIC X(7).
               05 FILLER                         PIC X(2).
               05 AANZZA                         PIC X.
               05 AANZZH                         PIC X.
               05 AANZZO                         PIC X(9).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).


