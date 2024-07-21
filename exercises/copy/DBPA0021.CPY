      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0021.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DBPA0021.CPv  $
      *
      *   Rev 2.3   04 Jun 2007 15:56:24   t13ae46
      *FPA00906: Anpassungen im PO13 und PO14 zur Ablösung PEW
      *
      *   Rev 2.0   Feb 27 1999 14:26:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   22 Jun 1998 08:52:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   Micro Focus Mainframe Express  3.0.75
      *   Micro Focus BMS Screen Painter (ver COBC_1-1-37-53)
      *   MapSet Name   BPA0021
      *   Date Created  06/04/2007
      *   Time Created  15:38:37

      *  Input Data For Map BPA0021
         01 BPA0021I.
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
            03 ESEIAKTI                       PIC X(2).
            03 ASEISTRL                       PIC S9(4) COMP.
            03 ASEISTRF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEISTRI                       PIC X(1).
            03 ASEIGESL                       PIC S9(4) COMP.
            03 ASEIGESF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIGESI                       PIC X(2).
            03 ANAMEL                         PIC S9(4) COMP.
            03 ANAMEF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ANAMEI                         PIC X(25).
            03 DFHG001I                       OCCURS 4 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 AVNRL                          PIC S9(4) COMP.
               05 AVNRF                          PIC X.
               05 FILLER                         PIC X(1).
               05 AVNRI                          PIC X(15).
               05 AVERSL                         PIC S9(4) COMP.
               05 AVERSF                         PIC X.
               05 FILLER                         PIC X(1).
               05 AVERSI                         PIC X(2).
               05 AAVERML                        PIC S9(4) COMP.
               05 AAVERMF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AAVERMI                        PIC X(7).
               05 AGUELABL                       PIC S9(4) COMP.
               05 AGUELABF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AGUELABI                       PIC X(10).
               05 AGUELBIL                       PIC S9(4) COMP.
               05 AGUELBIF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AGUELBII                       PIC X(10).
               05 AAENVONL                       PIC S9(4) COMP.
               05 AAENVONF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AAENVONI                       PIC X(8).
               05 AAENAML                        PIC S9(4) COMP.
               05 AAENAMF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AAENAMI                        PIC X(10).
            03 DFHG002I                       OCCURS 5 TIMES.
               05 EFCL                           PIC S9(4) COMP.
               05 EFCF                           PIC X.
               05 FILLER                         PIC X(1).
               05 EFCI                           PIC X(1).
               05 APINL                          PIC S9(4) COMP.
               05 APINF                          PIC X.
               05 FILLER                         PIC X(1).
               05 APINI                          PIC X(9).
               05 APEWKZL                        PIC S9(4) COMP.
               05 APEWKZF                        PIC X.
               05 FILLER                         PIC X(1).
               05 APEWKZI                        PIC X(1).
               05 AKDBETRL                       PIC S9(4) COMP.
               05 AKDBETRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKDBETRI                       PIC X(7).
               05 AGUEABL                        PIC S9(4) COMP.
               05 AGUEABF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AGUEABI                        PIC X(10).
               05 AAEVONL                        PIC S9(4) COMP.
               05 AAEVONF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AAEVONI                        PIC X(8).
               05 AAEAML                         PIC S9(4) COMP.
               05 AAEAMF                         PIC X.
               05 FILLER                         PIC X(1).
               05 AAEAMI                         PIC X(10).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BPA0021
         01 BPA0021O REDEFINES BPA0021I.
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
            03 ESEIAKTO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 ASEISTRA                       PIC X.
            03 ASEISTRH                       PIC X.
            03 ASEISTRO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 ASEIGESA                       PIC X.
            03 ASEIGESH                       PIC X.
            03 ASEIGESO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 ANAMEA                         PIC X.
            03 ANAMEH                         PIC X.
            03 ANAMEO                         PIC X(25).
            03 DFHG001O OCCURS 4.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 AVNRA                          PIC X.
               05 AVNRH                          PIC X.
               05 AVNRO                          PIC X(15).
               05 FILLER                         PIC X(2).
               05 AVERSA                         PIC X.
               05 AVERSH                         PIC X.
               05 AVERSO                         PIC X(2).
               05 FILLER                         PIC X(2).
               05 AAVERMA                        PIC X.
               05 AAVERMH                        PIC X.
               05 AAVERMO                        PIC X(7).
               05 FILLER                         PIC X(2).
               05 AGUELABA                       PIC X.
               05 AGUELABH                       PIC X.
               05 AGUELABO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AGUELBIA                       PIC X.
               05 AGUELBIH                       PIC X.
               05 AGUELBIO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AAENVONA                       PIC X.
               05 AAENVONH                       PIC X.
               05 AAENVONO                       PIC X(8).
               05 FILLER                         PIC X(2).
               05 AAENAMA                        PIC X.
               05 AAENAMH                        PIC X.
               05 AAENAMO                        PIC X(10).
            03 DFHG002O OCCURS 5.
               05 FILLER                         PIC X(2).
               05 EFCA                           PIC X.
               05 EFCH                           PIC X.
               05 EFCO                           PIC X(1).
               05 FILLER                         PIC X(2).
               05 APINA                          PIC X.
               05 APINH                          PIC X.
               05 APINO                          PIC X(9).
               05 FILLER                         PIC X(2).
               05 APEWKZA                        PIC X.
               05 APEWKZH                        PIC X.
               05 APEWKZO                        PIC X(1).
               05 FILLER                         PIC X(2).
               05 AKDBETRA                       PIC X.
               05 AKDBETRH                       PIC X.
               05 AKDBETRO                       PIC X(7).
               05 FILLER                         PIC X(2).
               05 AGUEABA                        PIC X.
               05 AGUEABH                        PIC X.
               05 AGUEABO                        PIC X(10).
               05 FILLER                         PIC X(2).
               05 AAEVONA                        PIC X.
               05 AAEVONH                        PIC X.
               05 AAEVONO                        PIC X(8).
               05 FILLER                         PIC X(2).
               05 AAEAMA                         PIC X.
               05 AAEAMH                         PIC X.
               05 AAEAMO                         PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).

