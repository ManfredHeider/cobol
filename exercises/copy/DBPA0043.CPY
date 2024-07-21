      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0043.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DBPA0043.CPv  $
      *
      *   Rev 2.3   23 Aug 2007 11:29:38   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.2   29 Sep 2006 09:33:32   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Mar 30 1999 14:50:22   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 2.0   Feb 27 1999 14:27:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   22 Jun 1998 08:54:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   Micro Focus Mainframe Express  3.0.75
      *   Micro Focus BMS Screen Painter (ver COBC_1-1-37-53)
      *   MapSet Name   BPA0043
      *   Date Created  08/23/2007
      *   Time Created  10:59:25

      *  Input Data For Map BPA0043
         01 BPA0043I.
            03 FILLER                         PIC X(12).
            03 AGESL                          PIC S9(4) COMP.
            03 AGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 AGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 AVNRL                          PIC S9(4) COMP.
            03 AVNRF                          PIC X.
            03 FILLER                         PIC X(1).
            03 AVNRI                          PIC X(15).
            03 APINL                          PIC S9(4) COMP.
            03 APINF                          PIC X.
            03 FILLER                         PIC X(1).
            03 APINI                          PIC X(9).
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
            03 AAVERML                        PIC S9(4) COMP.
            03 AAVERMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AAVERMI                        PIC X(7).
            03 AAVMPZL                        PIC S9(4) COMP.
            03 AAVMPZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AAVMPZI                        PIC X(1).
            03 AFAEVONL                       PIC S9(4) COMP.
            03 AFAEVONF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFAEVONI                       PIC X(10).
            03 AFAEBISL                       PIC S9(4) COMP.
            03 AFAEBISF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFAEBISI                       PIC X(10).
            03 AZEILEI                        OCCURS 9 TIMES.
               05 AVVNRL                         PIC S9(4) COMP.
               05 AVVNRF                         PIC X.
               05 FILLER                         PIC X(1).
               05 AVVNRI                         PIC X(15).
               05 AKDBETRL                       PIC S9(4) COMP.
               05 AKDBETRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKDBETRI                       PIC X(7).
               05 APRODSLL                       PIC S9(4) COMP.
               05 APRODSLF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODSLI                       PIC X(3).
               05 ALAUFZL                        PIC S9(4) COMP.
               05 ALAUFZF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ALAUFZI                        PIC X(3).
               05 AMINLZL                        PIC S9(4) COMP.
               05 AMINLZF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AMINLZI                        PIC X(3).
               05 AJNPEVSL                       PIC S9(4) COMP.
               05 AJNPEVSF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AJNPEVSI                       PIC X(13).
               05 AJNPVNRL                       PIC S9(4) COMP.
               05 AJNPVNRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AJNPVNRI                       PIC X(13).
               05 AERHJNPL                       PIC S9(4) COMP.
               05 AERHJNPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AERHJNPI                       PIC X(6).
               05 EEEENTL                        PIC S9(4) COMP.
               05 EEEENTF                        PIC X.
               05 FILLER                         PIC X(1).
               05 EEEENTI                        PIC X(2).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BPA0043
         01 BPA0043O REDEFINES BPA0043I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 AGESA                          PIC X.
            03 AGESH                          PIC X.
            03 AGESO                          PIC X(2).
            03 FILLER                         PIC X(2).
            03 AGESBMA                        PIC X.
            03 AGESBMH                        PIC X.
            03 AGESBMO                        PIC X(10).
            03 FILLER                         PIC X(2).
            03 AVNRA                          PIC X.
            03 AVNRH                          PIC X.
            03 AVNRO                          PIC X(15).
            03 FILLER                         PIC X(2).
            03 APINA                          PIC X.
            03 APINH                          PIC X.
            03 APINO                          PIC X(9).
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
            03 AAVERMA                        PIC X.
            03 AAVERMH                        PIC X.
            03 AAVERMO                        PIC X(7).
            03 FILLER                         PIC X(2).
            03 AAVMPZA                        PIC X.
            03 AAVMPZH                        PIC X.
            03 AAVMPZO                        PIC X(1).
            03 FILLER                         PIC X(2).
            03 AFAEVONA                       PIC X.
            03 AFAEVONH                       PIC X.
            03 AFAEVONO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFAEBISA                       PIC X.
            03 AFAEBISH                       PIC X.
            03 AFAEBISO                       PIC X(10).
            03 AZEILEO OCCURS 9.
               05 FILLER                         PIC X(2).
               05 AVVNRA                         PIC X.
               05 AVVNRH                         PIC X.
               05 AVVNRO                         PIC X(15).
               05 FILLER                         PIC X(2).
               05 AKDBETRA                       PIC X.
               05 AKDBETRH                       PIC X.
               05 AKDBETRO                       PIC X(7).
               05 FILLER                         PIC X(2).
               05 APRODSLA                       PIC X.
               05 APRODSLH                       PIC X.
               05 APRODSLO                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 ALAUFZA                        PIC X.
               05 ALAUFZH                        PIC X.
               05 ALAUFZO                        PIC X(3).
               05 FILLER                         PIC X(2).
               05 AMINLZA                        PIC X.
               05 AMINLZH                        PIC X.
               05 AMINLZO                        PIC X(3).
               05 FILLER                         PIC X(2).
               05 AJNPEVSA                       PIC X.
               05 AJNPEVSH                       PIC X.
               05 AJNPEVSO                       PIC X(13).
               05 FILLER                         PIC X(2).
               05 AJNPVNRA                       PIC X.
               05 AJNPVNRH                       PIC X.
               05 AJNPVNRO                       PIC X(13).
               05 FILLER                         PIC X(2).
               05 AERHJNPA                       PIC X.
               05 AERHJNPH                       PIC X.
               05 AERHJNPO                       PIC X(6).
               05 FILLER                         PIC X(2).
               05 EEEENTA                        PIC X.
               05 EEEENTH                        PIC X.
               05 EEEENTO                        PIC X(2).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).

