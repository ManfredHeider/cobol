      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0015.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DBPA0015.CPv  $
      *
      *   Rev 2.2   Apr 19 2006 09:45:24   t13ae46
      *APA00449: Kontrolle manueller Datensätze (Anforderung KPMG a
      *n Daisy)
      *
      *   Rev 2.1   Nov 25 1999 09:03:40   T13EX21
      *APA00063 - Verlängerung Feld VSTTXT
      *
      *   Rev 2.0   Feb 27 1999 14:26:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 08:55:30   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Nov 1997 17:26:30   P13PVCS
      *Initial revision.
      *
      *   Micro Focus BMS Screen Painter (ver MFER25)
      *   MapSet Name   BPA0015
      *   Date Created  04/12/2006
      *   Time Created  09:17:34
      *   Micro Focus BMS Screen Painter (ver MFER25)
      *   MapSet Name   BPA0015
      *   Date Created  04/19/2006
      *   Time Created  09:08:59

      *  Input Data For Map BPA0015
         01 BPA0015I.
            03 FILLER                         PIC X(12).
            03 EFCSL                          PIC S9(4) COMP.
            03 EFCSF                          PIC X.
            03 FILLER REDEFINES EFCSF.
               05 EFCSA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EFCSI                          PIC X(3).
            03 AFCSANZL                       PIC S9(4) COMP.
            03 AFCSANZF                       PIC X.
            03 FILLER REDEFINES AFCSANZF.
               05 AFCSANZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCSANZI                       PIC X(57).
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
            03 ADTAARTL                       PIC S9(4) COMP.
            03 ADTAARTF                       PIC X.
            03 FILLER REDEFINES ADTAARTF.
               05 ADTAARTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ADTAARTI                       PIC X(3).
            03 ADARTNLL                       PIC S9(4) COMP.
            03 ADARTNLF                       PIC X.
            03 FILLER REDEFINES ADARTNLF.
               05 ADARTNLA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ADARTNLI                       PIC X(10).
            03 AVSTATL                        PIC S9(4) COMP.
            03 AVSTATF                        PIC X.
            03 FILLER REDEFINES AVSTATF.
               05 AVSTATA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVSTATI                        PIC X(4).
            03 AVSTEXTL                       PIC S9(4) COMP.
            03 AVSTEXTF                       PIC X.
            03 FILLER REDEFINES AVSTEXTF.
               05 AVSTEXTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVSTEXTI                       PIC X(48).
            03 AABREDAL                       PIC S9(4) COMP.
            03 AABREDAF                       PIC X.
            03 FILLER REDEFINES AABREDAF.
               05 AABREDAA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AABREDAI                       PIC X(10).
            03 AUEBDATL                       PIC S9(4) COMP.
            03 AUEBDATF                       PIC X.
            03 FILLER REDEFINES AUEBDATF.
               05 AUEBDATA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AUEBDATI                       PIC X(10).
            03 ANZSAOFL                       PIC S9(4) COMP.
            03 ANZSAOFF                       PIC X.
            03 FILLER REDEFINES ANZSAOFF.
               05 ANZSAOFA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ANZSAOFI                       PIC X(9).
            03 ANZSAMFL                       PIC S9(4) COMP.
            03 ANZSAMFF                       PIC X.
            03 FILLER REDEFINES ANZSAMFF.
               05 ANZSAMFA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ANZSAMFI                       PIC X(9).
            03 APROVOFL                       PIC S9(4) COMP.
            03 APROVOFF                       PIC X.
            03 FILLER REDEFINES APROVOFF.
               05 APROVOFA                       PIC X.
            03 FILLER                         PIC X(1).
            03 APROVOFI                       PIC X(13).
            03 APROVMFL                       PIC S9(4) COMP.
            03 APROVMFF                       PIC X.
            03 FILLER REDEFINES APROVMFF.
               05 APROVMFA                       PIC X.
            03 FILLER                         PIC X(1).
            03 APROVMFI                       PIC X(13).
            03 AERSTELL                       PIC S9(4) COMP.
            03 AERSTELF                       PIC X.
            03 FILLER REDEFINES AERSTELF.
               05 AERSTELA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AERSTELI                       PIC X(8).
            03 AERSDATL                       PIC S9(4) COMP.
            03 AERSDATF                       PIC X.
            03 FILLER REDEFINES AERSDATF.
               05 AERSDATA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AERSDATI                       PIC X(10).
            03 APVWFREL                       PIC S9(4) COMP.
            03 APVWFREF                       PIC X.
            03 FILLER REDEFINES APVWFREF.
               05 APVWFREA                       PIC X.
            03 FILLER                         PIC X(1).
            03 APVWFREI                       PIC X(8).
            03 APVWDATL                       PIC S9(4) COMP.
            03 APVWDATF                       PIC X.
            03 FILLER REDEFINES APVWDATF.
               05 APVWDATA                       PIC X.
            03 FILLER                         PIC X(1).
            03 APVWDATI                       PIC X(10).

      *  Output Data For Map BPA0015
         01 BPA0015O REDEFINES BPA0015I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 EFCSH                          PIC X.
            03 EFCSO                          PIC X(3).
            03 FILLER                         PIC X(3).
            03 AFCSANZH                       PIC X.
            03 AFCSANZO                       PIC X(57).
            03 FILLER                         PIC X(3).
            03 AGESH                          PIC X.
            03 AGESO                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 AGESBMH                        PIC X.
            03 AGESBMO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 ADTAARTH                       PIC X.
            03 ADTAARTO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 ADARTNLH                       PIC X.
            03 ADARTNLO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AVSTATH                        PIC X.
            03 AVSTATO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AVSTEXTH                       PIC X.
            03 AVSTEXTO                       PIC X(48).
            03 FILLER                         PIC X(3).
            03 AABREDAH                       PIC X.
            03 AABREDAO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AUEBDATH                       PIC X.
            03 AUEBDATO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 ANZSAOFH                       PIC X.
            03 ANZSAOFO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 ANZSAMFH                       PIC X.
            03 ANZSAMFO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 APROVOFH                       PIC X.
            03 APROVOFO                       PIC X(13).
            03 FILLER                         PIC X(3).
            03 APROVMFH                       PIC X.
            03 APROVMFO                       PIC X(13).
            03 FILLER                         PIC X(3).
            03 AERSTELH                       PIC X.
            03 AERSTELO                       PIC X(8).
            03 FILLER                         PIC X(3).
            03 AERSDATH                       PIC X.
            03 AERSDATO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 APVWFREH                       PIC X.
            03 APVWFREO                       PIC X(8).
            03 FILLER                         PIC X(3).
            03 APVWDATH                       PIC X.
            03 APVWDATO                       PIC X(10).

