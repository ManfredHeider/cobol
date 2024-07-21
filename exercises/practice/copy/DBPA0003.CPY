      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0003.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DBPA0003.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:26:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   11 Aug 1998 12:00:26   T13EX16
      *APA00032 - PEW R2.0
      *
      *   Rev 1.1   26 Jun 1997 14:41:36   T13EX21
      *DAISY Release 1.0.3
      *
      *   Rev 1.0   26 Jun 1997 11:03:04   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   Micro Focus Advanced BMS Screens (ver 4.0.22)
      *   MapSet Name   BPA0003
      *   Date Created  08/11/98
      *   Time Created  11:49:04
      *   Alignment     NO

      *  Input Data For Map BPA0003
         01 BPA0003I.
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
            03 EVGRPL                           PIC S9(4) COMP.
            03 EVGRPF                           PIC X.
            03 EVGRPA REDEFINES EVGRPF          PIC X.
            03 FILLER                           PIC X(1).
            03 EVGRPI                           PIC XXX.
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
            03 EVSTATL                          PIC S9(4) COMP.
            03 EVSTATF                          PIC X.
            03 EVSTATA REDEFINES EVSTATF        PIC X.
            03 FILLER                           PIC X(1).
            03 EVSTATI                          PIC X(4).
            03 AVSTEXTL                         PIC S9(4) COMP.
            03 AVSTEXTF                         PIC X.
            03 AVSTEXTA REDEFINES AVSTEXTF      PIC X.
            03 FILLER                           PIC X(1).
            03 AVSTEXTI                         PIC X(40).
            03 APWAEBKL                         PIC S9(4) COMP.
            03 APWAEBKF                         PIC X.
            03 APWAEBKA REDEFINES APWAEBKF      PIC X.
            03 FILLER                           PIC X(1).
            03 APWAEBKI                         PIC X(4).
            03 EFCD OCCURS 10.
               05 EFCL                          PIC S9(4) COMP.
               05 EFCF                          PIC X.
               05 EFCA REDEFINES EFCF           PIC X.
               05 FILLER                        PIC X(1).
               05 EFCI                          PIC X(1).
            03 APINVNRD OCCURS 10.
               05 APINVNRL                      PIC S9(4) COMP.
               05 APINVNRF                      PIC X.
               05 APINVNRA REDEFINES APINVNRF   PIC X.
               05 FILLER                        PIC X(1).
               05 APINVNRI                      PIC X(15).
            03 AKDNKD OCCURS 10.
               05 AKDNKL                        PIC S9(4) COMP.
               05 AKDNKF                        PIC X.
               05 AKDNKA REDEFINES AKDNKF       PIC X.
               05 FILLER                        PIC X(1).
               05 AKDNKI                        PIC X(20).
            03 AVSTATD OCCURS 10.
               05 AVSTATL                       PIC S9(4) COMP.
               05 AVSTATF                       PIC X.
               05 AVSTATA REDEFINES AVSTATF     PIC X.
               05 FILLER                        PIC X(1).
               05 AVSTATI                       PIC X(4).
            03 AWVORDAD OCCURS 10.
               05 AWVORDAL                      PIC S9(4) COMP.
               05 AWVORDAF                      PIC X.
               05 AWVORDAA REDEFINES AWVORDAF   PIC X.
               05 FILLER                        PIC X(1).
               05 AWVORDAI                      PIC X(10).
            03 AEINHD OCCURS 10.
               05 AEINHL                        PIC S9(4) COMP.
               05 AEINHF                        PIC X.
               05 AEINHA REDEFINES AEINHF       PIC X.
               05 FILLER                        PIC X(1).
               05 AEINHI                        PIC X(8).
            03 APROVBD OCCURS 10.
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

      *  Output Data For Map BPA0003
         01 BPA0003O REDEFINES BPA0003I.
            03 FILLER                           PIC X(12).
            03 FILLER                           PIC X(3).
            03 EGESELLH                         PIC X.
            03 EGESELLO                         PIC X(2).
            03 FILLER                           PIC X(3).
            03 AGESEBMH                         PIC X.
            03 AGESEBMO                         PIC X(10).
            03 FILLER                           PIC X(3).
            03 EVGRPH                           PIC X.
            03 EVGRPO                           PIC XXX.
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
            03 EVSTATH                          PIC X.
            03 EVSTATO                          PIC X(4).
            03 FILLER                           PIC X(3).
            03 AVSTEXTH                         PIC X.
            03 AVSTEXTO                         PIC X(40).
            03 FILLER                           PIC X(3).
            03 APWAEBKH                         PIC X.
            03 APWAEBKO                         PIC X(4).
            03 DFHMS1 OCCURS 10.
               05 FILLER                        PIC X(3).
               05 EFCH                          PIC X.
               05 EFCO                          PIC X(1).
            03 DFHMS2 OCCURS 10.
               05 FILLER                        PIC X(3).
               05 APINVNRH                      PIC X.
               05 APINVNRO                      PIC X(15).
            03 DFHMS3 OCCURS 10.
               05 FILLER                        PIC X(3).
               05 AKDNKH                        PIC X.
               05 AKDNKO                        PIC X(20).
            03 DFHMS4 OCCURS 10.
               05 FILLER                        PIC X(3).
               05 AVSTATH                       PIC X.
               05 AVSTATO                       PIC X(4).
            03 DFHMS5 OCCURS 10.
               05 FILLER                        PIC X(3).
               05 AWVORDAH                      PIC X.
               05 AWVORDAO                      PIC X(10).
            03 DFHMS6 OCCURS 10.
               05 FILLER                        PIC X(3).
               05 AEINHH                        PIC X.
               05 AEINHO                        PIC X(8).
            03 DFHMS7 OCCURS 10.
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
