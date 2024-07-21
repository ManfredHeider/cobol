      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAN0003.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DBAN0003.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:05:26   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.1   07 Aug 1998 10:50:42   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   06 Apr 1998 08:54:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   Micro Focus Advanced BMS Screens (ver 4.0.22)
      *   MapSet Name   BAN0003
      *   Date Created  06/17/98
      *   Time Created  16:24:57
      *   Alignment     NO

      *  Input Data For Map BAN0003
         01 BAN0003I.
            03 FILLER                           PIC X(12).
            03 AGESL                            PIC S9(4) COMP.
            03 AGESF                            PIC X.
            03 AGESA REDEFINES AGESF            PIC X.
            03 FILLER                           PIC X(1).
            03 AGESI                            PIC X(2).
            03 AGESEBML                         PIC S9(4) COMP.
            03 AGESEBMF                         PIC X.
            03 AGESEBMA REDEFINES AGESEBMF      PIC X.
            03 FILLER                           PIC X(1).
            03 AGESEBMI                         PIC X(10).
            03 APINL                            PIC S9(4) COMP.
            03 APINF                            PIC X.
            03 APINA REDEFINES APINF            PIC X.
            03 FILLER                           PIC X(1).
            03 APINI                            PIC X(7).
            03 AEDATL                           PIC S9(4) COMP.
            03 AEDATF                           PIC X.
            03 AEDATA REDEFINES AEDATF          PIC X.
            03 FILLER                           PIC X(1).
            03 AEDATI                           PIC X(10).
            03 AAVERML                          PIC S9(4) COMP.
            03 AAVERMF                          PIC X.
            03 AAVERMA REDEFINES AAVERMF        PIC X.
            03 FILLER                           PIC X(1).
            03 AAVERMI                          PIC X(7).
            03 AAVERMPL                         PIC S9(4) COMP.
            03 AAVERMPF                         PIC X.
            03 AAVERMPA REDEFINES AAVERMPF      PIC X.
            03 FILLER                           PIC X(1).
            03 AAVERMPI                         PIC X(1).
            03 AAVERMBL                         PIC S9(4) COMP.
            03 AAVERMBF                         PIC X.
            03 AAVERMBA REDEFINES AAVERMBF      PIC X.
            03 FILLER                           PIC X(1).
            03 AAVERMBI                         PIC X(40).
            03 AVML                             PIC S9(4) COMP.
            03 AVMF                             PIC X.
            03 AVMA REDEFINES AVMF              PIC X.
            03 FILLER                           PIC X(1).
            03 AVMI                             PIC X(7).
            03 AVMPZL                           PIC S9(4) COMP.
            03 AVMPZF                           PIC X.
            03 AVMPZA REDEFINES AVMPZF          PIC X.
            03 FILLER                           PIC X(1).
            03 AVMPZI                           PIC X(1).
            03 AVMBEZL                          PIC S9(4) COMP.
            03 AVMBEZF                          PIC X.
            03 AVMBEZA REDEFINES AVMBEZF        PIC X.
            03 FILLER                           PIC X(1).
            03 AVMBEZI                          PIC X(40).
            03 GRP0001I OCCURS 9.
               05 APRODL                        PIC S9(4) COMP.
               05 APRODF                        PIC X.
               05 APRODA REDEFINES APRODF       PIC X.
               05 FILLER                        PIC X(1).
               05 APRODI                        PIC X(4).
            03 GRP0002I OCCURS 10.
               05 EFCL                          PIC S9(4) COMP.
               05 EFCF                          PIC X.
               05 EFCA REDEFINES EFCF           PIC X.
               05 FILLER                        PIC X(1).
               05 EFCI                          PIC X(1).
               05 ASTERNL                       PIC S9(4) COMP.
               05 ASTERNF                       PIC X.
               05 ASTERNA REDEFINES ASTERNF     PIC X.
               05 FILLER                        PIC X(1).
               05 ASTERNI                       PIC X(1).
               05 EVVNRL                        PIC S9(4) COMP.
               05 EVVNRF                        PIC X.
               05 EVVNRA REDEFINES EVVNRF       PIC X.
               05 FILLER                        PIC X(1).
               05 EVVNRI                        PIC X(15).
               05 EBEITRHL                      PIC S9(4) COMP.
               05 EBEITRHF                      PIC X.
               05 EBEITRHA REDEFINES EBEITRHF   PIC X.
               05 FILLER                        PIC X(1).
               05 EBEITRHI                      PIC X(10).
               05 EWKZHL                        PIC S9(4) COMP.
               05 EWKZHF                        PIC X.
               05 EWKZHA REDEFINES EWKZHF       PIC X.
               05 FILLER                        PIC X(1).
               05 EWKZHI                        PIC X(1).
               05 EBEITRUL                      PIC S9(4) COMP.
               05 EBEITRUF                      PIC X.
               05 EBEITRUA REDEFINES EBEITRUF   PIC X.
               05 FILLER                        PIC X(1).
               05 EBEITRUI                      PIC X(10).
               05 EWKZUL                        PIC S9(4) COMP.
               05 EWKZUF                        PIC X.
               05 EWKZUA REDEFINES EWKZUF       PIC X.
               05 FILLER                        PIC X(1).
               05 EWKZUI                        PIC X(1).
               05 EBEITRSL                      PIC S9(4) COMP.
               05 EBEITRSF                      PIC X.
               05 EBEITRSA REDEFINES EBEITRSF   PIC X.
               05 FILLER                        PIC X(1).
               05 EBEITRSI                      PIC X(10).
               05 EWKZSL                        PIC S9(4) COMP.
               05 EWKZSF                        PIC X.
               05 EWKZSA REDEFINES EWKZSF       PIC X.
               05 FILLER                        PIC X(1).
               05 EWKZSI                        PIC X(1).
               05 EKDBETRL                      PIC S9(4) COMP.
               05 EKDBETRF                      PIC X.
               05 EKDBETRA REDEFINES EKDBETRF   PIC X.
               05 FILLER                        PIC X(1).
               05 EKDBETRI                      PIC X(7).
               05 AAUTOL                        PIC S9(4) COMP.
               05 AAUTOF                        PIC X.
               05 AAUTOA REDEFINES AAUTOF       PIC X.
               05 FILLER                        PIC X(1).
               05 AAUTOI                        PIC X(1).
               05 AHINWL                        PIC S9(4) COMP.
               05 AHINWF                        PIC X.
               05 AHINWA REDEFINES AHINWF       PIC X.
               05 FILLER                        PIC X(1).
               05 AHINWI                        PIC X(4).

      *  Output Data For Map BAN0003
         01 BAN0003O REDEFINES BAN0003I.
            03 FILLER                           PIC X(12).
            03 FILLER                           PIC X(3).
            03 AGESH                            PIC X.
            03 AGESO                            PIC X(2).
            03 FILLER                           PIC X(3).
            03 AGESEBMH                         PIC X.
            03 AGESEBMO                         PIC X(10).
            03 FILLER                           PIC X(3).
            03 APINH                            PIC X.
            03 APINO                            PIC X(7).
            03 FILLER                           PIC X(3).
            03 AEDATH                           PIC X.
            03 AEDATO                           PIC X(10).
            03 FILLER                           PIC X(3).
            03 AAVERMH                          PIC X.
            03 AAVERMO                          PIC X(7).
            03 FILLER                           PIC X(3).
            03 AAVERMPH                         PIC X.
            03 AAVERMPO                         PIC X(1).
            03 FILLER                           PIC X(3).
            03 AAVERMBH                         PIC X.
            03 AAVERMBO                         PIC X(40).
            03 FILLER                           PIC X(3).
            03 AVMH                             PIC X.
            03 AVMO                             PIC X(7).
            03 FILLER                           PIC X(3).
            03 AVMPZH                           PIC X.
            03 AVMPZO                           PIC X(1).
            03 FILLER                           PIC X(3).
            03 AVMBEZH                          PIC X.
            03 AVMBEZO                          PIC X(40).
            03 GRP0001O OCCURS 9.
               05 FILLER                        PIC X(3).
               05 APRODH                        PIC X.
               05 APRODO                        PIC X(4).
            03 GRP0002O OCCURS 10.
               05 FILLER                        PIC X(3).
               05 EFCH                          PIC X.
               05 EFCO                          PIC X(1).
               05 FILLER                        PIC X(3).
               05 ASTERNH                       PIC X.
               05 ASTERNO                       PIC X(1).
               05 FILLER                        PIC X(3).
               05 EVVNRH                        PIC X.
               05 EVVNRO                        PIC X(15).
               05 FILLER                        PIC X(3).
               05 EBEITRHH                      PIC X.
               05 EBEITRHO                      PIC X(10).
               05 FILLER                        PIC X(3).
               05 EWKZHH                        PIC X.
               05 EWKZHO                        PIC X(1).
               05 FILLER                        PIC X(3).
               05 EBEITRUH                      PIC X.
               05 EBEITRUO                      PIC X(10).
               05 FILLER                        PIC X(3).
               05 EWKZUH                        PIC X.
               05 EWKZUO                        PIC X(1).
               05 FILLER                        PIC X(3).
               05 EBEITRSH                      PIC X.
               05 EBEITRSO                      PIC X(10).
               05 FILLER                        PIC X(3).
               05 EWKZSH                        PIC X.
               05 EWKZSO                        PIC X(1).
               05 FILLER                        PIC X(3).
               05 EKDBETRH                      PIC X.
               05 EKDBETRO                      PIC X(7).
               05 FILLER                        PIC X(3).
               05 AAUTOH                        PIC X.
               05 AAUTOO                        PIC X(1).
               05 FILLER                        PIC X(3).
               05 AHINWH                        PIC X.
               05 AHINWO                        PIC X(4).
