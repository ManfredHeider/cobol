      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   dbpa0038.cpy  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\dbpa0038.cpv  $
      *
      *   Rev 2.3   04 Oct 1999 15:49:20   T01JM2S
      *APA00057 - KORREKTUR PVCS-REVISIONEN
      *
      *   Rev 2.2   04 Oct 1999 15:04:56   T01JM2S
      *APA00057 - EINBAU MODUS-PROTOKOLL IN PK10
      *
      *   Rev 2.1   Apr 27 1999 13:21:26   T01JM2S
      *NT-Umstellung
      *
      *   Rev 2.0   Feb 27 1999 14:27:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   22 Jun 1998 08:54:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   Micro Focus BMS Screen Painter (ver MFBMW 1.1.16)
      *   MapSet Name   BPA0038
      *   Date Created  10/01/1999
      *   Time Created  14:12:31

      *  Input Data For Map BPA0038
         01 BPA0038I.
            03 FILLER                         PIC X(12).
            03 EPARTL                         PIC S9(4) COMP.
            03 EPARTF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPARTI                         PIC X(3).
            03 APARTBML                       PIC S9(4) COMP.
            03 APARTBMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APARTBMI                       PIC X(10).
            03 EPOBJL                         PIC S9(4) COMP.
            03 EPOBJF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPOBJI                         PIC X(3).
            03 APOBJBML                       PIC S9(4) COMP.
            03 APOBJBMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APOBJBMI                       PIC X(10).
            03 ADOPPELL                       PIC S9(4) COMP.
            03 ADOPPELF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ADOPPELI                       PIC X(1).
            03 EPAUSL                         PIC S9(4) COMP.
            03 EPAUSF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPAUSI                         PIC X(3).
            03 APAUSBML                       PIC S9(4) COMP.
            03 APAUSBMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APAUSBMI                       PIC X(10).
            03 EAERTTL                        PIC S9(4) COMP.
            03 EAERTTF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EAERTTI                        PIC X(2).
            03 EAERMML                        PIC S9(4) COMP.
            03 EAERMMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EAERMMI                        PIC X(2).
            03 EAERJJL                        PIC S9(4) COMP.
            03 EAERJJF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EAERJJI                        PIC X(4).
            03 EBERTTL                        PIC S9(4) COMP.
            03 EBERTTF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBERTTI                        PIC X(2).
            03 EBERMML                        PIC S9(4) COMP.
            03 EBERMMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBERMMI                        PIC X(2).
            03 EBERJJL                        PIC S9(4) COMP.
            03 EBERJJF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBERJJI                        PIC X(4).
            03 AMODTXTL                       PIC S9(4) COMP.
            03 AMODTXTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AMODTXTI                       PIC X(79).
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
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 EFCZD                          OCCURS 9 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
            03 APOBJBKD                       OCCURS 9 TIMES.
               05 APOBJBKL                       PIC S9(4) COMP.
               05 APOBJBKF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APOBJBKI                       PIC X(4).
            03 APAUSD                         OCCURS 9 TIMES.
               05 APAUSL                         PIC S9(4) COMP.
               05 APAUSF                         PIC X.
               05 FILLER                         PIC X(1).
               05 APAUSI                         PIC X(3).
            03 APAUTBMD                       OCCURS 9 TIMES.
               05 APAUTBML                       PIC S9(4) COMP.
               05 APAUTBMF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APAUTBMI                       PIC X(10).
            03 AERDATD                        OCCURS 9 TIMES.
               05 AERDATL                        PIC S9(4) COMP.
               05 AERDATF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AERDATI                        PIC X(10).
            03 APARTBLD                       OCCURS 9 TIMES.
               05 APARTBLL                       PIC S9(4) COMP.
               05 APARTBLF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APARTBLI                       PIC X(40).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BPA0038
         01 BPA0038O REDEFINES BPA0038I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 EPARTA                         PIC X.
            03 EPARTH                         PIC X.
            03 EPARTO                         PIC X(3).
            03 FILLER                         PIC X(2).
            03 APARTBMA                       PIC X.
            03 APARTBMH                       PIC X.
            03 APARTBMO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 EPOBJA                         PIC X.
            03 EPOBJH                         PIC X.
            03 EPOBJO                         PIC X(3).
            03 FILLER                         PIC X(2).
            03 APOBJBMA                       PIC X.
            03 APOBJBMH                       PIC X.
            03 APOBJBMO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 ADOPPELA                       PIC X.
            03 ADOPPELH                       PIC X.
            03 ADOPPELO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 EPAUSA                         PIC X.
            03 EPAUSH                         PIC X.
            03 EPAUSO                         PIC X(3).
            03 FILLER                         PIC X(2).
            03 APAUSBMA                       PIC X.
            03 APAUSBMH                       PIC X.
            03 APAUSBMO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 EAERTTA                        PIC X.
            03 EAERTTH                        PIC X.
            03 EAERTTO                        PIC X(2).
            03 FILLER                         PIC X(2).
            03 EAERMMA                        PIC X.
            03 EAERMMH                        PIC X.
            03 EAERMMO                        PIC X(2).
            03 FILLER                         PIC X(2).
            03 EAERJJA                        PIC X.
            03 EAERJJH                        PIC X.
            03 EAERJJO                        PIC X(4).
            03 FILLER                         PIC X(2).
            03 EBERTTA                        PIC X.
            03 EBERTTH                        PIC X.
            03 EBERTTO                        PIC X(2).
            03 FILLER                         PIC X(2).
            03 EBERMMA                        PIC X.
            03 EBERMMH                        PIC X.
            03 EBERMMO                        PIC X(2).
            03 FILLER                         PIC X(2).
            03 EBERJJA                        PIC X.
            03 EBERJJH                        PIC X.
            03 EBERJJO                        PIC X(4).
            03 FILLER                         PIC X(2).
            03 AMODTXTA                       PIC X.
            03 AMODTXTH                       PIC X.
            03 AMODTXTO                       PIC X(79).
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
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 DFHMS1 OCCURS 9.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
            03 DFHMS2 OCCURS 9.
               05 FILLER                         PIC X(2).
               05 APOBJBKA                       PIC X.
               05 APOBJBKH                       PIC X.
               05 APOBJBKO                       PIC X(4).
            03 DFHMS3 OCCURS 9.
               05 FILLER                         PIC X(2).
               05 APAUSA                         PIC X.
               05 APAUSH                         PIC X.
               05 APAUSO                         PIC X(3).
            03 DFHMS4 OCCURS 9.
               05 FILLER                         PIC X(2).
               05 APAUTBMA                       PIC X.
               05 APAUTBMH                       PIC X.
               05 APAUTBMO                       PIC X(10).
            03 DFHMS5 OCCURS 9.
               05 FILLER                         PIC X(2).
               05 AERDATA                        PIC X.
               05 AERDATH                        PIC X.
               05 AERDATO                        PIC X(10).
            03 DFHMS6 OCCURS 9.
               05 FILLER                         PIC X(2).
               05 APARTBLA                       PIC X.
               05 APARTBLH                       PIC X.
               05 APARTBLO                       PIC X(40).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).

