      *   Micro Focus Enterprise Developer for Eclipse  5.0.00256
      *   Micro Focus BMS Screen Painter (ver BMSIDE_5-0-01)
      *   MapSet Name   BPA0022
      *   Date Created  02/27/2020
      *   Time Created  14:36:50

      *  Input Data For Map BPA0022
         01 BPA0022I.
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
            03 AFCSANZI                       PIC X(20).
            03 APINL                          PIC S9(4) COMP.
            03 APINF                          PIC X.
            03 FILLER                         PIC X(1).
            03 APINI                          PIC X(9).
            03 AWAEBETL                       PIC S9(4) COMP.
            03 AWAEBETF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AWAEBETI                       PIC X(3).
            03 DFHG001I                       OCCURS 11 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 APRODSLL                       PIC S9(4) COMP.
               05 APRODSLF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODSLI                       PIC X(3).
               05 AORDNRL                        PIC S9(4) COMP.
               05 AORDNRF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AORDNRI                        PIC X(2).
               05 APTARTL                        PIC S9(4) COMP.
               05 APTARTF                        PIC X.
               05 FILLER                         PIC X(1).
               05 APTARTI                        PIC X(4).
               05 AGESVNRL                       PIC S9(4) COMP.
               05 AGESVNRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AGESVNRI                       PIC X(2).
               05 AVVNRL                         PIC S9(4) COMP.
               05 AVVNRF                         PIC X.
               05 FILLER                         PIC X(1).
               05 AVVNRI                         PIC X(15).
               05 APFLBEGL                       PIC S9(4) COMP.
               05 APFLBEGF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APFLBEGI                       PIC X(10).
               05 AKDBETRL                       PIC S9(4) COMP.
               05 AKDBETRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKDBETRI                       PIC X(7).
               05 ABEWBTGL                       PIC S9(4) COMP.
               05 ABEWBTGF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABEWBTGI                       PIC X(14).
               05 AEEENTL                        PIC S9(4) COMP.
               05 AEEENTF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AEEENTI                        PIC X(2).
               05 APESKZL                        PIC S9(4) COMP.
               05 APESKZF                        PIC X.
               05 FILLER                         PIC X(1).
               05 APESKZI                        PIC X(1).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BPA0022
         01 BPA0022O REDEFINES BPA0022I.
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
            03 AFCSANZO                       PIC X(20).
            03 FILLER                         PIC X(2).
            03 APINA                          PIC X.
            03 APINH                          PIC X.
            03 APINO                          PIC X(9).
            03 FILLER                         PIC X(2).
            03 AWAEBETA                       PIC X.
            03 AWAEBETH                       PIC X.
            03 AWAEBETO                       PIC X(3).
            03 DFHG001O OCCURS 11.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 APRODSLA                       PIC X.
               05 APRODSLH                       PIC X.
               05 APRODSLO                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 AORDNRA                        PIC X.
               05 AORDNRH                        PIC X.
               05 AORDNRO                        PIC X(2).
               05 FILLER                         PIC X(2).
               05 APTARTA                        PIC X.
               05 APTARTH                        PIC X.
               05 APTARTO                        PIC X(4).
               05 FILLER                         PIC X(2).
               05 AGESVNRA                       PIC X.
               05 AGESVNRH                       PIC X.
               05 AGESVNRO                       PIC X(2).
               05 FILLER                         PIC X(2).
               05 AVVNRA                         PIC X.
               05 AVVNRH                         PIC X.
               05 AVVNRO                         PIC X(15).
               05 FILLER                         PIC X(2).
               05 APFLBEGA                       PIC X.
               05 APFLBEGH                       PIC X.
               05 APFLBEGO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AKDBETRA                       PIC X.
               05 AKDBETRH                       PIC X.
               05 AKDBETRO                       PIC X(7).
               05 FILLER                         PIC X(2).
               05 ABEWBTGA                       PIC X.
               05 ABEWBTGH                       PIC X.
               05 ABEWBTGO                       PIC X(14).
               05 FILLER                         PIC X(2).
               05 AEEENTA                        PIC X.
               05 AEEENTH                        PIC X.
               05 AEEENTO                        PIC X(2).
               05 FILLER                         PIC X(2).
               05 APESKZA                        PIC X.
               05 APESKZH                        PIC X.
               05 APESKZO                        PIC X(1).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).


