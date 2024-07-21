      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0016.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DBPA0016.CPv  $
      *
      *   Rev 2.1   23 Aug 2007 10:38:20   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.0   Feb 27 1999 14:26:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 08:56:36   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Nov 1997 17:26:36   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   Micro Focus Mainframe Express  3.0.75
      *   Micro Focus BMS Screen Painter (ver COBC_1-1-37-53)
      *   MapSet Name   BPA0016
      *   Date Created  08/23/2007
      *   Time Created  10:26:41

      *  Input Data For Map BPA0016
         01 BPA0016I.
            03 FILLER                         PIC X(12).
            03 AGESL                          PIC S9(4) COMP.
            03 AGESF                          PIC X.
            03 FILLER REDEFINES AGESF.
               05 AGESA                          PIC X.
            03 FILLER                         PIC X(1).
            03 AGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER REDEFINES AGESBMF.
               05 AGESBMA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 EVNRL                          PIC S9(4) COMP.
            03 EVNRF                          PIC X.
            03 FILLER REDEFINES EVNRF.
               05 EVNRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EVNRI                          PIC X(15).
            03 EPINL                          PIC S9(4) COMP.
            03 EPINF                          PIC X.
            03 FILLER REDEFINES EPINF.
               05 EPINA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EPINI                          PIC X(9).
            03 EPINPZL                        PIC S9(4) COMP.
            03 EPINPZF                        PIC X.
            03 FILLER REDEFINES EPINPZF.
               05 EPINPZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EPINPZI                        PIC X(2).
            03 EFAETTL                        PIC S9(4) COMP.
            03 EFAETTF                        PIC X.
            03 FILLER REDEFINES EFAETTF.
               05 EFAETTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EFAETTI                        PIC X(2).
            03 EFAEMML                        PIC S9(4) COMP.
            03 EFAEMMF                        PIC X.
            03 FILLER REDEFINES EFAEMMF.
               05 EFAEMMA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EFAEMMI                        PIC X(2).
            03 EFAEJJL                        PIC S9(4) COMP.
            03 EFAEJJF                        PIC X.
            03 FILLER REDEFINES EFAEJJF.
               05 EFAEJJA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EFAEJJI                        PIC X(4).
            03 EPGVAMVL                       PIC S9(4) COMP.
            03 EPGVAMVF                       PIC X.
            03 FILLER REDEFINES EPGVAMVF.
               05 EPGVAMVA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EPGVAMVI                       PIC X(4).

      *  Output Data For Map BPA0016
         01 BPA0016O REDEFINES BPA0016I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 AGESH                          PIC X.
            03 AGESO                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 AGESBMH                        PIC X.
            03 AGESBMO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EVNRH                          PIC X.
            03 EVNRO                          PIC X(15).
            03 FILLER                         PIC X(3).
            03 EPINH                          PIC X.
            03 EPINO                          PIC X(9).
            03 FILLER                         PIC X(3).
            03 EPINPZH                        PIC X.
            03 EPINPZO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EFAETTH                        PIC X.
            03 EFAETTO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EFAEMMH                        PIC X.
            03 EFAEMMO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EFAEJJH                        PIC X.
            03 EFAEJJO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 EPGVAMVH                       PIC X.
            03 EPGVAMVO                       PIC X(4).

