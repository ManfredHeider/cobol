      *   Mainframe Express 3.1  6.018.0057
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-65)
      *   MapSet Name   BPA0070
      *   Date Created  08/14/2018
      *   Time Created  11:24:45

      *  Input Data For Map BPA0070
         01 BPA0070I.
            03 FILLER                         PIC X(12).
            03 EPRODL                         PIC S9(4) COMP.
            03 EPRODF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPRODI                         PIC X(3).
            03 EPGVL                          PIC S9(4) COMP.
            03 EPGVF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EPGVI                          PIC X(4).
            03 APGVBML                        PIC S9(4) COMP.
            03 APGVBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 APGVBMI                        PIC X(30).
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
            03 DFHG001I                       OCCURS 11 TIMES.
               05 APGVL                          PIC S9(4) COMP.
               05 APGVF                          PIC X.
               05 FILLER                         PIC X(1).
               05 APGVI                          PIC X(4).
               05 APRODL                         PIC S9(4) COMP.
               05 APRODF                         PIC X.
               05 FILLER                         PIC X(1).
               05 APRODI                         PIC X(3).
               05 APRODBLL                       PIC S9(4) COMP.
               05 APRODBLF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODBLI                       PIC X(50).
               05 AGUELABL                       PIC S9(4) COMP.
               05 AGUELABF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AGUELABI                       PIC X(10).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(79).

      *  Output Data For Map BPA0070
         01 BPA0070O REDEFINES BPA0070I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 EPRODA                         PIC X.
            03 EPRODH                         PIC X.
            03 EPRODO                         PIC X(3).
            03 FILLER                         PIC X(2).
            03 EPGVA                          PIC X.
            03 EPGVH                          PIC X.
            03 EPGVO                          PIC X(4).
            03 FILLER                         PIC X(2).
            03 APGVBMA                        PIC X.
            03 APGVBMH                        PIC X.
            03 APGVBMO                        PIC X(30).
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
            03 DFHG001O OCCURS 11.
               05 FILLER                         PIC X(2).
               05 APGVA                          PIC X.
               05 APGVH                          PIC X.
               05 APGVO                          PIC X(4).
               05 FILLER                         PIC X(2).
               05 APRODA                         PIC X.
               05 APRODH                         PIC X.
               05 APRODO                         PIC X(3).
               05 FILLER                         PIC X(2).
               05 APRODBLA                       PIC X.
               05 APRODBLH                       PIC X.
               05 APRODBLO                       PIC X(50).
               05 FILLER                         PIC X(2).
               05 AGUELABA                       PIC X.
               05 AGUELABH                       PIC X.
               05 AGUELABO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(79).


