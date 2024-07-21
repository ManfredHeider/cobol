      *   Micro Focus Mainframe Express  3.0.75
      *   Micro Focus BMS Screen Painter (ver COBC_1-1-37-06)
      *   MapSet Name   BPA0048
      *   Date Created  06/27/2006
      *   Time Created  15:58:53

      *  Input Data For Map BPA0048
         01 BPA0048I.
            03 FILLER                         PIC X(12).
            03 EGESL                          PIC S9(4) COMP.
            03 EGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 EDPGVL                         PIC S9(4) COMP.
            03 EDPGVF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EDPGVI                         PIC X(4).
            03 ADPGVBML                       PIC S9(4) COMP.
            03 ADPGVBMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ADPGVBMI                       PIC X(10).
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
            03 AFCSFBL                        PIC S9(4) COMP.
            03 AFCSFBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCSFBI                        PIC X(5).
            03 EFCSL                          PIC S9(4) COMP.
            03 EFCSF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EFCSI                          PIC X(3).
            03 AFCSANZL                       PIC S9(4) COMP.
            03 AFCSANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCSANZI                       PIC X(12).
            03 EPGVL                          PIC S9(4) COMP.
            03 EPGVF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EPGVI                          PIC X(4).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 DFHG001I                       OCCURS 12 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 EDPGVTL                        PIC S9(4) COMP.
               05 EDPGVTF                        PIC X.
               05 FILLER                         PIC X(1).
               05 EDPGVTI                        PIC X(4).
               05 EPGVTL                         PIC S9(4) COMP.
               05 EPGVTF                         PIC X.
               05 FILLER                         PIC X(1).
               05 EPGVTI                         PIC X(4).
               05 APPGVBLL                       PIC S9(4) COMP.
               05 APPGVBLF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APPGVBLI                       PIC X(42).
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
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(79).

      *  Output Data For Map BPA0048
         01 BPA0048O REDEFINES BPA0048I.
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
            03 EDPGVA                         PIC X.
            03 EDPGVH                         PIC X.
            03 EDPGVO                         PIC X(4).
            03 FILLER                         PIC X(2).
            03 ADPGVBMA                       PIC X.
            03 ADPGVBMH                       PIC X.
            03 ADPGVBMO                       PIC X(10).
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
            03 AFCSFBA                        PIC X.
            03 AFCSFBH                        PIC X.
            03 AFCSFBO                        PIC X(5).
            03 FILLER                         PIC X(2).
            03 EFCSA                          PIC X.
            03 EFCSH                          PIC X.
            03 EFCSO                          PIC X(3).
            03 FILLER                         PIC X(2).
            03 AFCSANZA                       PIC X.
            03 AFCSANZH                       PIC X.
            03 AFCSANZO                       PIC X(12).
            03 FILLER                         PIC X(2).
            03 EPGVA                          PIC X.
            03 EPGVH                          PIC X.
            03 EPGVO                          PIC X(4).
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
               05 EDPGVTA                        PIC X.
               05 EDPGVTH                        PIC X.
               05 EDPGVTO                        PIC X(4).
               05 FILLER                         PIC X(2).
               05 EPGVTA                         PIC X.
               05 EPGVTH                         PIC X.
               05 EPGVTO                         PIC X(4).
               05 FILLER                         PIC X(2).
               05 APPGVBLA                       PIC X.
               05 APPGVBLH                       PIC X.
               05 APPGVBLO                       PIC X(42).
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
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(79).

