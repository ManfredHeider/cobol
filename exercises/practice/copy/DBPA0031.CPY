      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0031.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DBPA0031.CPv  $
      *
      *   Rev 2.2   29 Sep 2006 09:31:30   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 1.0   09 Jun 1998 16:28:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   Micro Focus Mainframe Express  3.0.75
      *   Micro Focus BMS Screen Painter (ver COBC_1-1-37-06)
      *   MapSet Name   BPA0031
      *   Date Created  08/21/2006
      *   Time Created  11:19:15

      *  Input Data For Map BPA0031
         01 BPA0031I.
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
            03 ANAMEFBL                       PIC S9(4) COMP.
            03 ANAMEFBF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ANAMEFBI                       PIC X(5).
            03 ANAMEL                         PIC S9(4) COMP.
            03 ANAMEF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ANAMEI                         PIC X(25).
            03 AANZPROL                       PIC S9(4) COMP.
            03 AANZPROF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AANZPROI                       PIC X(2).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 ALZFB1L                        PIC S9(4) COMP.
            03 ALZFB1F                        PIC X.
            03 FILLER                         PIC X(1).
            03 ALZFB1I                        PIC X(6).
            03 AABLFB1L                       PIC S9(4) COMP.
            03 AABLFB1F                       PIC X.
            03 FILLER                         PIC X(1).
            03 AABLFB1I                       PIC X(11).
            03 AJNPFB1L                       PIC S9(4) COMP.
            03 AJNPFB1F                       PIC X.
            03 FILLER                         PIC X(1).
            03 AJNPFB1I                       PIC X(15).
            03 AWHRFB1L                       PIC S9(4) COMP.
            03 AWHRFB1F                       PIC X.
            03 FILLER                         PIC X(1).
            03 AWHRFB1I                       PIC X(7).
            03 ALZFB2L                        PIC S9(4) COMP.
            03 ALZFB2F                        PIC X.
            03 FILLER                         PIC X(1).
            03 ALZFB2I                        PIC X(6).
            03 AABLFB2L                       PIC S9(4) COMP.
            03 AABLFB2F                       PIC X.
            03 FILLER                         PIC X(1).
            03 AABLFB2I                       PIC X(11).
            03 AJNPFB2L                       PIC S9(4) COMP.
            03 AJNPFB2F                       PIC X.
            03 FILLER                         PIC X(1).
            03 AJNPFB2I                       PIC X(15).
            03 AWHGFB2L                       PIC S9(4) COMP.
            03 AWHGFB2F                       PIC X.
            03 FILLER                         PIC X(1).
            03 AWHGFB2I                       PIC X(7).
            03 DFHG001I                       OCCURS 11 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 APRODSLL                       PIC S9(4) COMP.
               05 APRODSLF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODSLI                       PIC X(3).
               05 APRODBML                       PIC S9(4) COMP.
               05 APRODBMF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODBMI                       PIC X(10).
               05 AGUELABL                       PIC S9(4) COMP.
               05 AGUELABF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AGUELABI                       PIC X(10).
               05 ALZL                           PIC S9(4) COMP.
               05 ALZF                           PIC X.
               05 FILLER                         PIC X(1).
               05 ALZI                           PIC X(4).
               05 AABLAUFL                       PIC S9(4) COMP.
               05 AABLAUFF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AABLAUFI                       PIC X(10).
               05 AJNETPRL                       PIC S9(4) COMP.
               05 AJNETPRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AJNETPRI                       PIC X(15).
               05 AWAEHRGL                       PIC S9(4) COMP.
               05 AWAEHRGF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AWAEHRGI                       PIC X(4).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(79).

      *  Output Data For Map BPA0031
         01 BPA0031O REDEFINES BPA0031I.
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
            03 ANAMEFBA                       PIC X.
            03 ANAMEFBH                       PIC X.
            03 ANAMEFBO                       PIC X(5).
            03 FILLER                         PIC X(2).
            03 ANAMEA                         PIC X.
            03 ANAMEH                         PIC X.
            03 ANAMEO                         PIC X(25).
            03 FILLER                         PIC X(2).
            03 AANZPROA                       PIC X.
            03 AANZPROH                       PIC X.
            03 AANZPROO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 FILLER                         PIC X(2).
            03 ALZFB1A                        PIC X.
            03 ALZFB1H                        PIC X.
            03 ALZFB1O                        PIC X(6).
            03 FILLER                         PIC X(2).
            03 AABLFB1A                       PIC X.
            03 AABLFB1H                       PIC X.
            03 AABLFB1O                       PIC X(11).
            03 FILLER                         PIC X(2).
            03 AJNPFB1A                       PIC X.
            03 AJNPFB1H                       PIC X.
            03 AJNPFB1O                       PIC X(15).
            03 FILLER                         PIC X(2).
            03 AWHRFB1A                       PIC X.
            03 AWHRFB1H                       PIC X.
            03 AWHRFB1O                       PIC X(7).
            03 FILLER                         PIC X(2).
            03 ALZFB2A                        PIC X.
            03 ALZFB2H                        PIC X.
            03 ALZFB2O                        PIC X(6).
            03 FILLER                         PIC X(2).
            03 AABLFB2A                       PIC X.
            03 AABLFB2H                       PIC X.
            03 AABLFB2O                       PIC X(11).
            03 FILLER                         PIC X(2).
            03 AJNPFB2A                       PIC X.
            03 AJNPFB2H                       PIC X.
            03 AJNPFB2O                       PIC X(15).
            03 FILLER                         PIC X(2).
            03 AWHGFB2A                       PIC X.
            03 AWHGFB2H                       PIC X.
            03 AWHGFB2O                       PIC X(7).
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
               05 APRODBMA                       PIC X.
               05 APRODBMH                       PIC X.
               05 APRODBMO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AGUELABA                       PIC X.
               05 AGUELABH                       PIC X.
               05 AGUELABO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 ALZA                           PIC X.
               05 ALZH                           PIC X.
               05 ALZO                           PIC X(4).
               05 FILLER                         PIC X(2).
               05 AABLAUFA                       PIC X.
               05 AABLAUFH                       PIC X.
               05 AABLAUFO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AJNETPRA                       PIC X.
               05 AJNETPRH                       PIC X.
               05 AJNETPRO                       PIC X(15).
               05 FILLER                         PIC X(2).
               05 AWAEHRGA                       PIC X.
               05 AWAEHRGH                       PIC X.
               05 AWAEHRGO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(79).

