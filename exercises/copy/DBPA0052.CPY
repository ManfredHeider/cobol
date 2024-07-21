      *   Mainframe Express 3.1  6.013.0041
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-57)
      *   MapSet Name   BPA0052
      *   Date Created  08/25/2015
      *   Time Created  10:11:20

      *  Input Data For Map BPA0052
         01 BPA0052I.
            03 FILLER                         PIC X(12).
            03 EGESL                          PIC S9(4) COMP.
            03 EGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 EVNRL                          PIC S9(4) COMP.
            03 EVNRF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EVNRI                          PIC X(15).
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
            03 ANAMEFBL                       PIC S9(4) COMP.
            03 ANAMEFBF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ANAMEFBI                       PIC X(5).
            03 ANAMEL                         PIC S9(4) COMP.
            03 ANAMEF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ANAMEI                         PIC X(25).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 DFHG001I                       OCCURS 11 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 AZINFOL                        PIC S9(4) COMP.
               05 AZINFOF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AZINFOI                        PIC X(2).
               05 AFAEVONL                       PIC S9(4) COMP.
               05 AFAEVONF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AFAEVONI                       PIC X(10).
               05 EFOLGNRL                       PIC S9(4) COMP.
               05 EFOLGNRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EFOLGNRI                       PIC X(3).
               05 APGVL                          PIC S9(4) COMP.
               05 APGVF                          PIC X.
               05 FILLER                         PIC X(1).
               05 APGVI                          PIC X(4).
               05 AGESBTRL                       PIC S9(4) COMP.
               05 AGESBTRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AGESBTRI                       PIC X(13).
               05 AUSTBTGL                       PIC S9(4) COMP.
               05 AUSTBTGF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AUSTBTGI                       PIC X(13).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(79).

      *  Output Data For Map BPA0052
         01 BPA0052O REDEFINES BPA0052I.
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
            03 EVNRA                          PIC X.
            03 EVNRH                          PIC X.
            03 EVNRO                          PIC X(15).
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
            03 ANAMEFBA                       PIC X.
            03 ANAMEFBH                       PIC X.
            03 ANAMEFBO                       PIC X(5).
            03 FILLER                         PIC X(2).
            03 ANAMEA                         PIC X.
            03 ANAMEH                         PIC X.
            03 ANAMEO                         PIC X(25).
            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 DFHG001O OCCURS 11.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 AZINFOA                        PIC X.
               05 AZINFOH                        PIC X.
               05 AZINFOO                        PIC X(2).
               05 FILLER                         PIC X(2).
               05 AFAEVONA                       PIC X.
               05 AFAEVONH                       PIC X.
               05 AFAEVONO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 EFOLGNRA                       PIC X.
               05 EFOLGNRH                       PIC X.
               05 EFOLGNRO                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 APGVA                          PIC X.
               05 APGVH                          PIC X.
               05 APGVO                          PIC X(4).
               05 FILLER                         PIC X(2).
               05 AGESBTRA                       PIC X.
               05 AGESBTRH                       PIC X.
               05 AGESBTRO                       PIC X(13).
               05 FILLER                         PIC X(2).
               05 AUSTBTGA                       PIC X.
               05 AUSTBTGH                       PIC X.
               05 AUSTBTGO                       PIC X(13).
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(79).


