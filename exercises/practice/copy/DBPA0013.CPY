      *   Mainframe Express 3.1  6.018.0057
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-65)
      *   MapSet Name   BPA0013
      *   Date Created  03/07/2018
      *   Time Created  14:25:58

      *  Input Data For Map BPA0013
         01 BPA0013I.
            03 FILLER                         PIC X(12).
            03 EGESL                          PIC S9(4) COMP.
            03 EGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 ESDATTTL                       PIC S9(4) COMP.
            03 ESDATTTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESDATTTI                       PIC X(2).
            03 ESDATMML                       PIC S9(4) COMP.
            03 ESDATMMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESDATMMI                       PIC X(2).
            03 ESDATJJL                       PIC S9(4) COMP.
            03 ESDATJJF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESDATJJI                       PIC XXXX.
            03 ASEIFBL                        PIC S9(4) COMP.
            03 ASEIFBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIFBI                        PIC X(6).
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
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 DFHG001I                       OCCURS 12 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 EPRODSLL                       PIC S9(4) COMP.
               05 EPRODSLF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EPRODSLI                       PIC X(3).
               05 EPRODBLL                       PIC S9(4) COMP.
               05 EPRODBLF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EPRODBLI                       PIC X(30).
               05 EPRODBKL                       PIC S9(4) COMP.
               05 EPRODBKF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EPRODBKI                       PIC X(10).
               05 EGABTTL                        PIC S9(4) COMP.
               05 EGABTTF                        PIC X.
               05 FILLER                         PIC X(1).
               05 EGABTTI                        PIC X(2).
               05 EGABMML                        PIC S9(4) COMP.
               05 EGABMMF                        PIC X.
               05 FILLER                         PIC X(1).
               05 EGABMMI                        PIC X(2).
               05 EGABJJL                        PIC S9(4) COMP.
               05 EGABJJF                        PIC X.
               05 FILLER                         PIC X(1).
               05 EGABJJI                        PIC X(4).
               05 EGBISTTL                       PIC S9(4) COMP.
               05 EGBISTTF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EGBISTTI                       PIC X(2).
               05 EGBISMML                       PIC S9(4) COMP.
               05 EGBISMMF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EGBISMMI                       PIC X(2).
               05 EGBISJJL                       PIC S9(4) COMP.
               05 EGBISJJF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EGBISJJI                       PIC X(4).
               05 AVGESL                         PIC S9(4) COMP.
               05 AVGESF                         PIC X.
               05 FILLER                         PIC X(1).
               05 AVGESI                         PIC X(9).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(78).

      *  Output Data For Map BPA0013
         01 BPA0013O REDEFINES BPA0013I.
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
            03 ESDATTTA                       PIC X.
            03 ESDATTTH                       PIC X.
            03 ESDATTTO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 ESDATMMA                       PIC X.
            03 ESDATMMH                       PIC X.
            03 ESDATMMO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 ESDATJJA                       PIC X.
            03 ESDATJJH                       PIC X.
            03 ESDATJJO                       PIC XXXX.
            03 FILLER                         PIC X(2).
            03 ASEIFBA                        PIC X.
            03 ASEIFBH                        PIC X.
            03 ASEIFBO                        PIC X(6).
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
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 DFHG001O OCCURS 12.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 EPRODSLA                       PIC X.
               05 EPRODSLH                       PIC X.
               05 EPRODSLO                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 EPRODBLA                       PIC X.
               05 EPRODBLH                       PIC X.
               05 EPRODBLO                       PIC X(30).
               05 FILLER                         PIC X(2).
               05 EPRODBKA                       PIC X.
               05 EPRODBKH                       PIC X.
               05 EPRODBKO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 EGABTTA                        PIC X.
               05 EGABTTH                        PIC X.
               05 EGABTTO                        PIC X(2).
               05 FILLER                         PIC X(2).
               05 EGABMMA                        PIC X.
               05 EGABMMH                        PIC X.
               05 EGABMMO                        PIC X(2).
               05 FILLER                         PIC X(2).
               05 EGABJJA                        PIC X.
               05 EGABJJH                        PIC X.
               05 EGABJJO                        PIC X(4).
               05 FILLER                         PIC X(2).
               05 EGBISTTA                       PIC X.
               05 EGBISTTH                       PIC X.
               05 EGBISTTO                       PIC X(2).
               05 FILLER                         PIC X(2).
               05 EGBISMMA                       PIC X.
               05 EGBISMMH                       PIC X.
               05 EGBISMMO                       PIC X(2).
               05 FILLER                         PIC X(2).
               05 EGBISJJA                       PIC X.
               05 EGBISJJH                       PIC X.
               05 EGBISJJO                       PIC X(4).
               05 FILLER                         PIC X(2).
               05 AVGESA                         PIC X.
               05 AVGESH                         PIC X.
               05 AVGESO                         PIC X(9).
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(78).


