      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0011.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DBPA0011.CPv  $
      *
      *   Rev 2.2   23 Aug 2007 10:24:12   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   REV 2.1   FEB 22 2000 08:41:00   T13AE18
      *APA00080
      *
      *   REV 2.0   FEB 27 1999 14:26:52   P13SP20
      *NT-UMSTELLUNG
      *
      *   REV 1.1   11 NOV 1998 08:53:10   T01JM2S
      *PEG-PRODUKTIONSÜBERGABE
      *
      *   REV 1.0   05 NOV 1997 17:26:14   P13PVCS
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      *   Micro Focus Mainframe Express  3.0.75
      *   Micro Focus BMS Screen Painter (ver COBC_1-1-37-53)
      *   MapSet Name   BPA0011
      *   Date Created  08/23/2007
      *   Time Created  10:20:27

      *  Input Data For Map BPA0011
         01 BPA0011I.
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
            03 AVERSL                         PIC S9(4) COMP.
            03 AVERSF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVERSI                         PIC X(2).
            03 AVSTATL                        PIC S9(4) COMP.
            03 AVSTATF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVSTATI                        PIC X(4).
            03 AVSTEXTL                       PIC S9(4) COMP.
            03 AVSTEXTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVSTEXTI                       PIC X(40).
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
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 ASPUEB1L                       PIC S9(4) COMP.
            03 ASPUEB1F                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASPUEB1I                       PIC X(75).
            03 ASPUEB2L                       PIC S9(4) COMP.
            03 ASPUEB2F                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASPUEB2I                       PIC X(75).
            03 DFHG001I                       OCCURS 11 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 AZEILEL                        PIC S9(4) COMP.
               05 AZEILEF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AZEILEI                        PIC X(74).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BPA0011
         01 BPA0011O REDEFINES BPA0011I.
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
            03 AVERSA                         PIC X.
            03 AVERSH                         PIC X.
            03 AVERSO                         PIC X(2).
            03 FILLER                         PIC X(2).
            03 AVSTATA                        PIC X.
            03 AVSTATH                        PIC X.
            03 AVSTATO                        PIC X(4).
            03 FILLER                         PIC X(2).
            03 AVSTEXTA                       PIC X.
            03 AVSTEXTH                       PIC X.
            03 AVSTEXTO                       PIC X(40).
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
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 FILLER                         PIC X(2).
            03 ASPUEB1A                       PIC X.
            03 ASPUEB1H                       PIC X.
            03 ASPUEB1O                       PIC X(75).
            03 FILLER                         PIC X(2).
            03 ASPUEB2A                       PIC X.
            03 ASPUEB2H                       PIC X.
            03 ASPUEB2O                       PIC X(75).
            03 DFHG001O OCCURS 11.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 AZEILEA                        PIC X.
               05 AZEILEH                        PIC X.
               05 AZEILEO                        PIC X(74).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).

