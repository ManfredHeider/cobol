      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   dbpa0041.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\dbpa0041.cpv  $
      *
      *   Rev 2.1   Apr 27 1999 13:24:58   T01JM2S
      *NT-Umstellung
      *
      *   Rev 2.0   Feb 27 1999 14:27:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   22 Jun 1998 08:54:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   Micro Focus BMS Screen Painter (ver MFBMW 1.1.14)
      *   MapSet Name   BPA0041
      *   Date Created  04/23/1999
      *   Time Created  14:19:24

      *  Input Data For Map BPA0041
         01 BPA0041I.
            03 FILLER                         PIC X(12).
            03 APOBJBKL                       PIC S9(4) COMP.
            03 APOBJBKF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APOBJBKI                       PIC X(4).
            03 APAUSL                         PIC S9(4) COMP.
            03 APAUSF                         PIC X.
            03 FILLER                         PIC X(1).
            03 APAUSI                         PIC X(3).
            03 APAUSBML                       PIC S9(4) COMP.
            03 APAUSBMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APAUSBMI                       PIC X(10).
            03 AERSTDAL                       PIC S9(4) COMP.
            03 AERSTDAF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AERSTDAI                       PIC X(10).
            03 AKENNL                         PIC S9(4) COMP.
            03 AKENNF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AKENNI                         PIC X(8).
            03 APARTBLL                       PIC S9(4) COMP.
            03 APARTBLF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APARTBLI                       PIC X(40).
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
            03 ATEXTD                         OCCURS 14 TIMES.
               05 ATEXTL                         PIC S9(4) COMP.
               05 ATEXTF                         PIC X.
               05 FILLER                         PIC X(1).
               05 ATEXTI                         PIC X(79).

      *  Output Data For Map BPA0041
         01 BPA0041O REDEFINES BPA0041I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 APOBJBKA                       PIC X.
            03 APOBJBKH                       PIC X.
            03 APOBJBKO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 APAUSA                         PIC X.
            03 APAUSH                         PIC X.
            03 APAUSO                         PIC X(3).
            03 FILLER                         PIC X(2).
            03 APAUSBMA                       PIC X.
            03 APAUSBMH                       PIC X.
            03 APAUSBMO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AERSTDAA                       PIC X.
            03 AERSTDAH                       PIC X.
            03 AERSTDAO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AKENNA                         PIC X.
            03 AKENNH                         PIC X.
            03 AKENNO                         PIC X(8).
            03 FILLER                         PIC X(2).
            03 APARTBLA                       PIC X.
            03 APARTBLH                       PIC X.
            03 APARTBLO                       PIC X(40).
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
            03 DFHMS1 OCCURS 14.
               05 FILLER                         PIC X(2).
               05 ATEXTA                         PIC X.
               05 ATEXTH                         PIC X.
               05 ATEXTO                         PIC X(79).

