      *   Micro Focus BMS Screen Painter (ver MFER25)
      *   MapSet Name   BPA0055
      *   Date Created  11/04/2004
      *   Time Created  11:32:46

      *  Input Data For Map BPA0055
         01 BPA0055I.
            03 FILLER                         PIC X(12).
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
            03 AKUNDEL                        PIC S9(4) COMP.
            03 AKUNDEF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AKUNDEI                        PIC X(25).
            03 DFHG001I                       OCCURS 7 TIMES.
               05 ATTABL                         PIC S9(4) COMP.
               05 ATTABF                         PIC X.
               05 FILLER                         PIC X(1).
               05 ATTABI                         PIC X(2).
               05 AMMABL                         PIC S9(4) COMP.
               05 AMMABF                         PIC X.
               05 FILLER                         PIC X(1).
               05 AMMABI                         PIC X(2).
               05 AJJABL                         PIC S9(4) COMP.
               05 AJJABF                         PIC X.
               05 FILLER                         PIC X(1).
               05 AJJABI                         PIC X(4).
               05 AKDBETRL                       PIC S9(4) COMP.
               05 AKDBETRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKDBETRI                       PIC X(7).
               05 ABDATL                         PIC S9(4) COMP.
               05 ABDATF                         PIC X.
               05 FILLER                         PIC X(1).
               05 ABDATI                         PIC X(10).
               05 AUSERL                         PIC S9(4) COMP.
               05 AUSERF                         PIC X.
               05 FILLER                         PIC X(1).
               05 AUSERI                         PIC X(8).
            03 EGESL                          PIC S9(4) COMP.
            03 EGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EGESI                          PIC X(2).
            03 EVNRL                          PIC S9(4) COMP.
            03 EVNRF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EVNRI                          PIC X(15).
            03 KDTEXTL                        PIC S9(4) COMP.
            03 KDTEXTF                        PIC X.
            03 FILLER                         PIC X(1).
            03 KDTEXTI                        PIC X(19).
            03 EVBNRL                         PIC S9(4) COMP.
            03 EVBNRF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EVBNRI                         PIC X(7).
            03 EVBPZL                         PIC S9(4) COMP.
            03 EVBPZF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EVBPZI                         PIC X(1).
            03 GTTEXTL                        PIC S9(4) COMP.
            03 GTTEXTF                        PIC X.
            03 FILLER                         PIC X(1).
            03 GTTEXTI                        PIC X(9).
            03 ETTL                           PIC S9(4) COMP.
            03 ETTF                           PIC X.
            03 FILLER                         PIC X(1).
            03 ETTI                           PIC X(2).
            03 EMML                           PIC S9(4) COMP.
            03 EMMF                           PIC X.
            03 FILLER                         PIC X(1).
            03 EMMI                           PIC X(2).
            03 EJJJJL                         PIC S9(4) COMP.
            03 EJJJJF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EJJJJI                         PIC X(4).
            03 EAUSWL                         PIC S9(4) COMP.
            03 EAUSWF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EAUSWI                         PIC X(1).
            03 AWTEXTL                        PIC S9(4) COMP.
            03 AWTEXTF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AWTEXTI                        PIC X(24).

      *  Output Data For Map BPA0055
         01 BPA0055O REDEFINES BPA0055I.
            03 FILLER                         PIC X(12).
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
            03 AKUNDEA                        PIC X.
            03 AKUNDEH                        PIC X.
            03 AKUNDEO                        PIC X(25).
            03 DFHG001O OCCURS 7.
               05 FILLER                         PIC X(2).
               05 ATTABA                         PIC X.
               05 ATTABH                         PIC X.
               05 ATTABO                         PIC X(2).
               05 FILLER                         PIC X(2).
               05 AMMABA                         PIC X.
               05 AMMABH                         PIC X.
               05 AMMABO                         PIC X(2).
               05 FILLER                         PIC X(2).
               05 AJJABA                         PIC X.
               05 AJJABH                         PIC X.
               05 AJJABO                         PIC X(4).
               05 FILLER                         PIC X(2).
               05 AKDBETRA                       PIC X.
               05 AKDBETRH                       PIC X.
               05 AKDBETRO                       PIC X(7).
               05 FILLER                         PIC X(2).
               05 ABDATA                         PIC X.
               05 ABDATH                         PIC X.
               05 ABDATO                         PIC X(10).
               05 FILLER                         PIC X(2).
               05 AUSERA                         PIC X.
               05 AUSERH                         PIC X.
               05 AUSERO                         PIC X(8).
            03 FILLER                         PIC X(2).
            03 EGESA                          PIC X.
            03 EGESH                          PIC X.
            03 EGESO                          PIC X(2).
            03 FILLER                         PIC X(2).
            03 EVNRA                          PIC X.
            03 EVNRH                          PIC X.
            03 EVNRO                          PIC X(15).
            03 FILLER                         PIC X(2).
            03 KDTEXTA                        PIC X.
            03 KDTEXTH                        PIC X.
            03 KDTEXTO                        PIC X(19).
            03 FILLER                         PIC X(2).
            03 EVBNRA                         PIC X.
            03 EVBNRH                         PIC X.
            03 EVBNRO                         PIC X(7).
            03 FILLER                         PIC X(2).
            03 EVBPZA                         PIC X.
            03 EVBPZH                         PIC X.
            03 EVBPZO                         PIC X(1).
            03 FILLER                         PIC X(2).
            03 GTTEXTA                        PIC X.
            03 GTTEXTH                        PIC X.
            03 GTTEXTO                        PIC X(9).
            03 FILLER                         PIC X(2).
            03 ETTA                           PIC X.
            03 ETTH                           PIC X.
            03 ETTO                           PIC X(2).
            03 FILLER                         PIC X(2).
            03 EMMA                           PIC X.
            03 EMMH                           PIC X.
            03 EMMO                           PIC X(2).
            03 FILLER                         PIC X(2).
            03 EJJJJA                         PIC X.
            03 EJJJJH                         PIC X.
            03 EJJJJO                         PIC X(4).
            03 FILLER                         PIC X(2).
            03 EAUSWA                         PIC X.
            03 EAUSWH                         PIC X.
            03 EAUSWO                         PIC X(1).
            03 FILLER                         PIC X(2).
            03 AWTEXTA                        PIC X.
            03 AWTEXTH                        PIC X.
            03 AWTEXTO                        PIC X(24).

