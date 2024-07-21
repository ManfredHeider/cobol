      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0020.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DBPA0020.CPv  $
      *
      *   Rev 2.6   17 Oct 2007 11:01:26   t13ae46
      *FPA00952: Verhindern Abbruch in PO16
      *
      *   REV 2.4   23 AUG 2007 10:51:58   T13AE46
      *APA00639: ERWEITERUNG DER PIN AUF 9 STELLEN
      *
      *   REV 2.3   29 SEP 2006 09:36:10   T13AE46
      *APA00452: ERWEITERUNG DES PS AUF 3 STELLEN
      *
      *   REV 2.0   FEB 27 1999 14:26:56   P13SP20
      *NT-UMSTELLUNG
      *
      *   REV 1.0   05 NOV 1997 17:26:56   P13PVCS
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      *   Micro Focus Mainframe Express  3.0.75
      *   Micro Focus BMS Screen Painter (ver COBC_1-1-37-53)
      *   MapSet Name   BPA0020
      *   Date Created  10/17/2007
      *   Time Created  10:59:07

      *  Input Data For Map BPA0020
         01 BPA0020I.
            03 FILLER                         PIC X(12).
            03 EGESL                          PIC S9(4) COMP.
            03 EGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 EPINL                          PIC S9(4) COMP.
            03 EPINF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EPINI                          PIC X(9).
            03 APINPRZL                       PIC S9(4) COMP.
            03 APINPRZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APINPRZI                       PIC X(2).
            03 EVBNRL                         PIC S9(4) COMP.
            03 EVBNRF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EVBNRI                         PIC X(7).
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
            03 ENAMEL                         PIC S9(4) COMP.
            03 ENAMEF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ENAMEI                         PIC X(25).
            03 EVNAMEL                        PIC S9(4) COMP.
            03 EVNAMEF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVNAMEI                        PIC X(20).
            03 EPRODSLL                       PIC S9(4) COMP.
            03 EPRODSLF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EPRODSLI                       PIC X(3).
            03 EVONTTL                        PIC S9(4) COMP.
            03 EVONTTF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVONTTI                        PIC X(2).
            03 EVONMML                        PIC S9(4) COMP.
            03 EVONMMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVONMMI                        PIC X(2).
            03 EVONJJL                        PIC S9(4) COMP.
            03 EVONJJF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVONJJI                        PIC X(4).
            03 EBISTTL                        PIC S9(4) COMP.
            03 EBISTTF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBISTTI                        PIC X(2).
            03 EBISMML                        PIC S9(4) COMP.
            03 EBISMMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBISMMI                        PIC X(2).
            03 EBISJJL                        PIC S9(4) COMP.
            03 EBISJJF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBISJJI                        PIC X(4).
            03 DFHG001I                       OCCURS 10 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 AVNRL                          PIC S9(4) COMP.
               05 AVNRF                          PIC X.
               05 FILLER                         PIC X(1).
               05 AVNRI                          PIC X(15).
               05 APINL                          PIC S9(4) COMP.
               05 APINF                          PIC X.
               05 FILLER                         PIC X(1).
               05 APINI                          PIC X(9).
               05 ANAMEL                         PIC S9(4) COMP.
               05 ANAMEF                         PIC X.
               05 FILLER                         PIC X(1).
               05 ANAMEI                         PIC X(18).
               05 AKDBETRL                       PIC S9(4) COMP.
               05 AKDBETRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKDBETRI                       PIC X(7).
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
               05 APLUSL                         PIC S9(4) COMP.
               05 APLUSF                         PIC X.
               05 FILLER                         PIC X(1).
               05 APLUSI                         PIC X(1).
               05 AVBEGDAL                       PIC S9(4) COMP.
               05 AVBEGDAF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AVBEGDAI                       PIC X(10).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BPA0020
         01 BPA0020O REDEFINES BPA0020I.
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
            03 EPINA                          PIC X.
            03 EPINH                          PIC X.
            03 EPINO                          PIC X(9).
            03 FILLER                         PIC X(2).
            03 APINPRZA                       PIC X.
            03 APINPRZH                       PIC X.
            03 APINPRZO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EVBNRA                         PIC X.
            03 EVBNRH                         PIC X.
            03 EVBNRO                         PIC X(7).
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
            03 ENAMEA                         PIC X.
            03 ENAMEH                         PIC X.
            03 ENAMEO                         PIC X(25).
            03 FILLER                         PIC X(2).
            03 EVNAMEA                        PIC X.
            03 EVNAMEH                        PIC X.
            03 EVNAMEO                        PIC X(20).
            03 FILLER                         PIC X(2).
            03 EPRODSLA                       PIC X.
            03 EPRODSLH                       PIC X.
            03 EPRODSLO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 EVONTTA                        PIC X.
            03 EVONTTH                        PIC X.
            03 EVONTTO                        PIC X(2).
            03 FILLER                         PIC X(2).
            03 EVONMMA                        PIC X.
            03 EVONMMH                        PIC X.
            03 EVONMMO                        PIC X(2).
            03 FILLER                         PIC X(2).
            03 EVONJJA                        PIC X.
            03 EVONJJH                        PIC X.
            03 EVONJJO                        PIC X(4).
            03 FILLER                         PIC X(2).
            03 EBISTTA                        PIC X.
            03 EBISTTH                        PIC X.
            03 EBISTTO                        PIC X(2).
            03 FILLER                         PIC X(2).
            03 EBISMMA                        PIC X.
            03 EBISMMH                        PIC X.
            03 EBISMMO                        PIC X(2).
            03 FILLER                         PIC X(2).
            03 EBISJJA                        PIC X.
            03 EBISJJH                        PIC X.
            03 EBISJJO                        PIC X(4).
            03 DFHG001O OCCURS 10.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 AVNRA                          PIC X.
               05 AVNRH                          PIC X.
               05 AVNRO                          PIC X(15).
               05 FILLER                         PIC X(2).
               05 APINA                          PIC X.
               05 APINH                          PIC X.
               05 APINO                          PIC X(9).
               05 FILLER                         PIC X(2).
               05 ANAMEA                         PIC X.
               05 ANAMEH                         PIC X.
               05 ANAMEO                         PIC X(18).
               05 FILLER                         PIC X(2).
               05 AKDBETRA                       PIC X.
               05 AKDBETRH                       PIC X.
               05 AKDBETRO                       PIC X(7).
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
               05 APLUSA                         PIC X.
               05 APLUSH                         PIC X.
               05 APLUSO                         PIC X(1).
               05 FILLER                         PIC X(2).
               05 AVBEGDAA                       PIC X.
               05 AVBEGDAH                       PIC X.
               05 AVBEGDAO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).

