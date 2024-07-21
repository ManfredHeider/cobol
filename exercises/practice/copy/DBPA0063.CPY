      *   Mainframe Express 3.1  6.013.0041
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-57)
      *   MapSet Name   BPA0063
      *   Date Created  08/11/2015
      *   Time Created  09:04:47

      *  Input Data For Map BPA0063
         01 BPA0063I.
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
            03 ANAMEL                         PIC S9(4) COMP.
            03 ANAMEF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ANAMEI                         PIC X(25).
            03 AFBPINL                        PIC S9(4) COMP.
            03 AFBPINF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFBPINI                        PIC X(4).
            03 APINL                          PIC S9(4) COMP.
            03 APINF                          PIC X.
            03 FILLER                         PIC X(1).
            03 APINI                          PIC X(9).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 AWAEPBL                        PIC S9(4) COMP.
            03 AWAEPBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AWAEPBI                        PIC X(4).
            03 DFHG001I                       OCCURS 11 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 AZINFOL                        PIC S9(4) COMP.
               05 AZINFOF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AZINFOI                        PIC X(2).
               05 AVSTATL                        PIC S9(4) COMP.
               05 AVSTATF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AVSTATI                        PIC X(4).
               05 APRODS1L                       PIC S9(4) COMP.
               05 APRODS1F                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODS1I                       PIC X(3).
               05 ANPROVL                        PIC S9(4) COMP.
               05 ANPROVF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ANPROVI                        PIC X(15).
               05 APROVBL                        PIC S9(4) COMP.
               05 APROVBF                        PIC X.
               05 FILLER                         PIC X(1).
               05 APROVBI                        PIC X(15).
               05 AFAEVDAL                       PIC S9(4) COMP.
               05 AFAEVDAF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AFAEVDAI                       PIC X(10).
               05 AABREDAL                       PIC S9(4) COMP.
               05 AABREDAF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AABREDAI                       PIC X(10).
               05 AHKFTABL                       PIC S9(4) COMP.
               05 AHKFTABF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AHKFTABI                       PIC X(3).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BPA0063
         01 BPA0063O REDEFINES BPA0063I.
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
            03 ANAMEA                         PIC X.
            03 ANAMEH                         PIC X.
            03 ANAMEO                         PIC X(25).
            03 FILLER                         PIC X(2).
            03 AFBPINA                        PIC X.
            03 AFBPINH                        PIC X.
            03 AFBPINO                        PIC X(4).
            03 FILLER                         PIC X(2).
            03 APINA                          PIC X.
            03 APINH                          PIC X.
            03 APINO                          PIC X(9).
            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 FILLER                         PIC X(2).
            03 AWAEPBA                        PIC X.
            03 AWAEPBH                        PIC X.
            03 AWAEPBO                        PIC X(4).
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
               05 AVSTATA                        PIC X.
               05 AVSTATH                        PIC X.
               05 AVSTATO                        PIC X(4).
               05 FILLER                         PIC X(2).
               05 APRODS1A                       PIC X.
               05 APRODS1H                       PIC X.
               05 APRODS1O                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 ANPROVA                        PIC X.
               05 ANPROVH                        PIC X.
               05 ANPROVO                        PIC X(15).
               05 FILLER                         PIC X(2).
               05 APROVBA                        PIC X.
               05 APROVBH                        PIC X.
               05 APROVBO                        PIC X(15).
               05 FILLER                         PIC X(2).
               05 AFAEVDAA                       PIC X.
               05 AFAEVDAH                       PIC X.
               05 AFAEVDAO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AABREDAA                       PIC X.
               05 AABREDAH                       PIC X.
               05 AABREDAO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AHKFTABA                       PIC X.
               05 AHKFTABH                       PIC X.
               05 AHKFTABO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).


