      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0009.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DBPA0009.CPv  $
      *
      *   Rev 2.9   Sep 27 2016 09:29:46   t13ae46
      *APA02280: Dialoganpassungen PG14
      *
      *    Rev 2.8   Jul 29 2015 16:03:04   t13ae46
      * APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      * e (PG11, PG12, PG14, PF11)
      *
      *    Rev 2.7   Dec 17 2013 10:46:36   t13ex67
      * APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      * g): PG14
      *
      *   Rev 2.6   Jan 07 2011 11:29:06   t13ae23
      *APA01129: Selektion PG14 erweitern
      *
      *   Rev 2.5   29 Sep 2006 09:34:52   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Feb 27 1999 14:26:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 08:50:58   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Nov 1997 17:26:04   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      *   Mainframe Express 3.1  6.013.0041
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-57)
      *   MapSet Name   BPA0009
      *   Date Created  09/27/2016
      *   Time Created  09:24:49

      *  Input Data For Map BPA0009
         01 BPA0009I.
            03 FILLER                         PIC X(12).
            03 AGESL                          PIC S9(4) COMP.
            03 AGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 AGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 ASEIFBL                        PIC S9(4) COMP.
            03 ASEIFBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIFBI                        PIC X(6).
            03 ESEIAKTL                       PIC S9(4) COMP.
            03 ESEIAKTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESEIAKTI                       PIC X(4).
            03 ASEISTRL                       PIC S9(4) COMP.
            03 ASEISTRF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEISTRI                       PIC X(1).
            03 ASEIGESL                       PIC S9(4) COMP.
            03 ASEIGESF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIGESI                       PIC X(4).
            03 AUEBDATL                       PIC S9(4) COMP.
            03 AUEBDATF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AUEBDATI                       PIC X(10).
            03 EFCSL                          PIC S9(4) COMP.
            03 EFCSF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EFCSI                          PIC X(3).
            03 AFCSANZL                       PIC S9(4) COMP.
            03 AFCSANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCSANZI                       PIC X(17).
            03 EPRODL                         PIC S9(4) COMP.
            03 EPRODF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPRODI                         PIC X(3).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 AFABFVL                        PIC S9(4) COMP.
            03 AFABFVF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFABFVI                        PIC X(10).
            03 AFABFBL                        PIC S9(4) COMP.
            03 AFABFBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFABFBI                        PIC X(10).
            03 APWAEBKL                       PIC S9(4) COMP.
            03 APWAEBKF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APWAEBKI                       PIC X(4).
            03 DFHG001I                       OCCURS 11 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 APLUSL                         PIC S9(4) COMP.
               05 APLUSF                         PIC X.
               05 FILLER                         PIC X(1).
               05 APLUSI                         PIC X(1).
               05 AVNRL                          PIC S9(4) COMP.
               05 AVNRF                          PIC X.
               05 FILLER                         PIC X(1).
               05 AVNRI                          PIC X(16).
               05 AVSTATL                        PIC S9(4) COMP.
               05 AVSTATF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AVSTATI                        PIC X(4).
               05 APRODS1L                       PIC S9(4) COMP.
               05 APRODS1F                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODS1I                       PIC X(3).
               05 APRODS2L                       PIC S9(4) COMP.
               05 APRODS2F                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODS2I                       PIC X(3).
               05 APRODS3L                       PIC S9(4) COMP.
               05 APRODS3F                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODS3I                       PIC X(3).
               05 APLUSPSL                       PIC S9(4) COMP.
               05 APLUSPSF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APLUSPSI                       PIC X(1).
               05 APROVBL                        PIC S9(4) COMP.
               05 APROVBF                        PIC X.
               05 FILLER                         PIC X(1).
               05 APROVBI                        PIC X(15).
               05 AFAEVDAL                       PIC S9(4) COMP.
               05 AFAEVDAF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AFAEVDAI                       PIC X(10).
               05 AFAEBDAL                       PIC S9(4) COMP.
               05 AFAEBDAF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AFAEBDAI                       PIC X(10).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BPA0009
         01 BPA0009O REDEFINES BPA0009I.
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
            03 ASEIFBA                        PIC X.
            03 ASEIFBH                        PIC X.
            03 ASEIFBO                        PIC X(6).
            03 FILLER                         PIC X(2).
            03 ESEIAKTA                       PIC X.
            03 ESEIAKTH                       PIC X.
            03 ESEIAKTO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 ASEISTRA                       PIC X.
            03 ASEISTRH                       PIC X.
            03 ASEISTRO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 ASEIGESA                       PIC X.
            03 ASEIGESH                       PIC X.
            03 ASEIGESO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 AUEBDATA                       PIC X.
            03 AUEBDATH                       PIC X.
            03 AUEBDATO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 EFCSA                          PIC X.
            03 EFCSH                          PIC X.
            03 EFCSO                          PIC X(3).
            03 FILLER                         PIC X(2).
            03 AFCSANZA                       PIC X.
            03 AFCSANZH                       PIC X.
            03 AFCSANZO                       PIC X(17).
            03 FILLER                         PIC X(2).
            03 EPRODA                         PIC X.
            03 EPRODH                         PIC X.
            03 EPRODO                         PIC X(3).
            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 FILLER                         PIC X(2).
            03 AFABFVA                        PIC X.
            03 AFABFVH                        PIC X.
            03 AFABFVO                        PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFABFBA                        PIC X.
            03 AFABFBH                        PIC X.
            03 AFABFBO                        PIC X(10).
            03 FILLER                         PIC X(2).
            03 APWAEBKA                       PIC X.
            03 APWAEBKH                       PIC X.
            03 APWAEBKO                       PIC X(4).
            03 DFHG001O OCCURS 11.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 APLUSA                         PIC X.
               05 APLUSH                         PIC X.
               05 APLUSO                         PIC X(1).
               05 FILLER                         PIC X(2).
               05 AVNRA                          PIC X.
               05 AVNRH                          PIC X.
               05 AVNRO                          PIC X(16).
               05 FILLER                         PIC X(2).
               05 AVSTATA                        PIC X.
               05 AVSTATH                        PIC X.
               05 AVSTATO                        PIC X(4).
               05 FILLER                         PIC X(2).
               05 APRODS1A                       PIC X.
               05 APRODS1H                       PIC X.
               05 APRODS1O                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 APRODS2A                       PIC X.
               05 APRODS2H                       PIC X.
               05 APRODS2O                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 APRODS3A                       PIC X.
               05 APRODS3H                       PIC X.
               05 APRODS3O                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 APLUSPSA                       PIC X.
               05 APLUSPSH                       PIC X.
               05 APLUSPSO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 APROVBA                        PIC X.
               05 APROVBH                        PIC X.
               05 APROVBO                        PIC X(15).
               05 FILLER                         PIC X(2).
               05 AFAEVDAA                       PIC X.
               05 AFAEVDAH                       PIC X.
               05 AFAEVDAO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AFAEBDAA                       PIC X.
               05 AFAEBDAH                       PIC X.
               05 AFAEBDAO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).


