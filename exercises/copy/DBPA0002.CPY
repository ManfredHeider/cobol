      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0002.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DBPA0002.CPv  $
      *
      *   Rev 2.4   Jan 23 2007 14:36:08   t13ex44
      *FPA00886: Korrektur PW12
      *
      *   Rev 2.3   Nov 27 2006 14:39:26   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   21 Mar 2001 15:28:46   T13AE23
      *AAN00292 - PEW-Banken  VNR auf 15 Stellen 
      *                    vergrößert -
      *
      *   Rev 2.1   14 Nov 2000 14:42:46   T13AE34
      *AAN00285-PEW Hypo
      *
      *   Rev 2.0   Feb 27 1999 14:26:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   11 Aug 1998 11:29:04   T13EX16
      *APA00032 - PEW R2.0
      *
      *   Rev 1.1   26 Jun 1997 14:41:08   T13EX21
      *DAISY Release 1.0.3
      *
      *   Rev 1.0   26 Jun 1997 11:02:52   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   Micro Focus Advanced BMS Screens (ver 4.0.20)
      *   MapSet Name   BPA0002
      *   Date Created  03/19/98
      *   Time Created  11:53:53
      *   Alignment     NO

      *  Input Data For Map BPA0002
         01 BPA0002I.
            03 FILLER                           PIC X(12).
            03 EGESELLL                         PIC S9(4) COMP.
            03 EGESELLF                         PIC X.
            03 EGESELLA REDEFINES EGESELLF      PIC X.
            03 FILLER                           PIC X(1).
            03 EGESELLI                         PIC X(2).
            03 AGESEBML                         PIC S9(4) COMP.
            03 AGESEBMF                         PIC X.
            03 AGESEBMA REDEFINES AGESEBMF      PIC X.
            03 FILLER                           PIC X(1).
            03 AGESEBMI                         PIC X(10).
            03 ESEIAKTL                         PIC S9(4) COMP.
            03 ESEIAKTF                         PIC X.
            03 ESEIAKTA REDEFINES ESEIAKTF      PIC X.
            03 FILLER                           PIC X(1).
            03 ESEIAKTI                         PIC X(3).
            03 ASEISTRL                         PIC S9(4) COMP.
            03 ASEISTRF                         PIC X.
            03 ASEISTRA REDEFINES ASEISTRF      PIC X.
            03 FILLER                           PIC X(1).
            03 ASEISTRI                         PIC X(1).
            03 ASEIGESL                         PIC S9(4) COMP.
            03 ASEIGESF                         PIC X.
            03 ASEIGESA REDEFINES ASEIGESF      PIC X.
            03 FILLER                           PIC X(1).
            03 ASEIGESI                         PIC X(3).
            03 EVBNRL                           PIC S9(4) COMP.
            03 EVBNRF                           PIC X.
            03 EVBNRA REDEFINES EVBNRF          PIC X.
            03 FILLER                           PIC X(1).
            03 EVBNRI                           PIC X(7).
            03 AVBNRPL                          PIC S9(4) COMP.
            03 AVBNRPF                          PIC X.
            03 AVBNRPA REDEFINES AVBNRPF        PIC X.
            03 FILLER                           PIC X(1).
            03 AVBNRPI                          PIC X(1).
            03 AVBNLL                           PIC S9(4) COMP.
            03 AVBNLF                           PIC X.
            03 AVBNLA REDEFINES AVBNLF          PIC X.
            03 FILLER                           PIC X(1).
            03 AVBNLI                           PIC X(40).
            03 ADIRL                            PIC S9(4) COMP.
            03 ADIRF                            PIC X.
            03 ADIRA REDEFINES ADIRF            PIC X.
            03 FILLER                           PIC X(1).
            03 ADIRI                            PIC X(3).
            03 APWAEBKL                         PIC S9(4) COMP.
            03 APWAEBKF                         PIC X.
            03 APWAEBKA REDEFINES APWAEBKF      PIC X.
            03 FILLER                           PIC X(1).
            03 APWAEBKI                         PIC X(4).
            03 EFCD OCCURS 11.
               05 EFCL                          PIC S9(4) COMP.
               05 EFCF                          PIC X.
               05 EFCA REDEFINES EFCF           PIC X.
               05 FILLER                        PIC X(1).
               05 EFCI                          PIC X(1).
      *     03 APIND OCCURS 11.
               05 APINL                         PIC S9(4) COMP.
               05 APINF                         PIC X.
               05 APINA REDEFINES APINF         PIC X.
               05 FILLER                        PIC X(1).
               05 APINI                         PIC X(15).
      *     03 AKDNKD OCCURS 11.
               05 AKDNKL                        PIC S9(4) COMP.
               05 AKDNKF                        PIC X.
               05 AKDNKA REDEFINES AKDNKF       PIC X.
               05 FILLER                        PIC X(1).
               05 AKDNKI                        PIC X(20).
               05 APRODSLL                      PIC S9(4) COMP.
               05 APRODSLF                      PIC X.
               05 APRODSLA REDEFINES APRODSLF   PIC X.
               05 FILLER                        PIC X(1).
               05 APRODSLI                      PIC X(13).
               05 AEINHL                        PIC S9(4) COMP.
               05 AEINHF                        PIC X.
               05 AEINHA REDEFINES AEINHF       PIC X.
               05 FILLER                        PIC X(1).
               05 AEINHI                        PIC X(8).
               05 APROVBL                       PIC S9(4) COMP.
               05 APROVBF                       PIC X.
               05 APROVBA REDEFINES APROVBF     PIC X.
               05 FILLER                        PIC X(1).
               05 APROVBI                       PIC X(11).
            03 ATEXTGSL                         PIC S9(4) COMP.
            03 ATEXTGSF                         PIC X.
            03 ATEXTGSA REDEFINES ATEXTGSF      PIC X.
            03 FILLER                           PIC X(1).
            03 ATEXTGSI                         PIC X(11).
            03 ASPROVBL                         PIC S9(4) COMP.
            03 ASPROVBF                         PIC X.
            03 ASPROVBA REDEFINES ASPROVBF      PIC X.
            03 FILLER                           PIC X(1).
            03 ASPROVBI                         PIC X(15).
            03 AFCANZL                          PIC S9(4) COMP.
            03 AFCANZF                          PIC X.
            03 AFCANZA REDEFINES AFCANZF        PIC X.
            03 FILLER                           PIC X(1).
            03 AFCANZI                          PIC X(79).

      *  Output Data For Map BPA0002
         01 BPA0002O REDEFINES BPA0002I.
            03 FILLER                           PIC X(12).
            03 FILLER                           PIC X(3).
            03 EGESELLH                         PIC X.
            03 EGESELLO                         PIC X(2).
            03 FILLER                           PIC X(3).
            03 AGESEBMH                         PIC X.
            03 AGESEBMO                         PIC X(10).
            03 FILLER                           PIC X(3).
            03 ESEIAKTH                         PIC X.
            03 ESEIAKTO                         PIC X(3).
            03 FILLER                           PIC X(3).
            03 ASEISTRH                         PIC X.
            03 ASEISTRO                         PIC X(1).
            03 FILLER                           PIC X(3).
            03 ASEIGESH                         PIC X.
            03 ASEIGESO                         PIC X(3).
            03 FILLER                           PIC X(3).
            03 EVBNRH                           PIC X.
            03 EVBNRO                           PIC X(7).
            03 FILLER                           PIC X(3).
            03 AVBNRPH                          PIC X.
            03 AVBNRPO                          PIC X(1).
            03 FILLER                           PIC X(3).
            03 AVBNLH                           PIC X.
            03 AVBNLO                           PIC X(40).
            03 FILLER                           PIC X(3).
            03 ADIRH                            PIC X.
            03 ADIRO                            PIC X(3).
            03 FILLER                           PIC X(3).
            03 APWAEBKH                         PIC X.
            03 APWAEBKO                         PIC X(4).
            03 DFHMS1 OCCURS 11.
               05 FILLER                        PIC X(3).
               05 EFCH                          PIC X.
               05 EFCO                          PIC X(1).
      *     03 DFHMS2 OCCURS 11.
               05 FILLER                        PIC X(3).
               05 APINH                         PIC X.
               05 APINO                         PIC X(15).
      *     03 DFHMS3 OCCURS 11.
               05 FILLER                        PIC X(3).
               05 AKDNKH                        PIC X.
               05 AKDNKO                        PIC X(20).
               05 FILLER                        PIC X(3).
               05 APRODSLH                      PIC X.
               05 APRODSLO                      PIC X(13).
               05 FILLER                        PIC X(3).
               05 AEINHH                        PIC X.
               05 AEINHO                        PIC X(8).
               05 FILLER                        PIC X(3).
               05 APROVBH                       PIC X.
               05 APROVBO                       PIC X(11).
            03 FILLER                           PIC X(3).
            03 ATEXTGSH                         PIC X.
            03 ATEXTGSO                         PIC X(11).
            03 FILLER                           PIC X(3).
            03 ASPROVBH                         PIC X.
            03 ASPROVBO                         PIC X(15).
            03 FILLER                           PIC X(3).
            03 AFCANZH                          PIC X.
            03 AFCANZO                          PIC X(79).
