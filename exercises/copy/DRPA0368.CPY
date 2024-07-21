      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0368.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0368.CPv  $
      *
      *   Rev 2.2   Sep 21 2018 12:14:08   t13ex44
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.1   May 28 1999 18:31:02   T13EX26
      *Prod-Übergabe 28.0598 hpa
      *
      *   Rev 2.0   May 26 1999 16:57:24   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   09 Jul 1997 16:04:32   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      ****************************************************************
      *    COPY-MEMBER: DRPA0368  FUNKTION                           *
      *                           "EINHEITEN ZUORDNEN "              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HILTON PARENT                                   *
      *    DATUM:    09.07.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * DAISY 1 !09.07.97! 01  ! NEUERSTELLUNG             ! PARENT  *
      *         !        !     !                           !         *
      * APA0012 !13.02.98!01.02! RC-NOK IN RC-VSG GEÄNDERT ! JÄGER   *
      * APA00012!25.09.98!01.03!EINH-AKT ENTFERNT          ! MUND    *
      * APA02548!26.07.18!2.2  ! NACHF. DIREKTION FAMILIE  ! SCHEIDEL*
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRPA0368.
           05 PI-DRPA0368-EINH           PIC S9(006)V99 PACKED-DECIMAL.
           05 PI-DRPA0368-EINH-VWF       PIC S9(006)V99 PACKED-DECIMAL.
           05 PI-DRPA0368-EINH-VTF       PIC S9(006)V99 PACKED-DECIMAL.
           05 PI-DRPA0368-EINH-AVERM     PIC S9(006)V99 PACKED-DECIMAL.
           05 PI-DRPA0368-EINH-VM        PIC S9(006)V99 PACKED-DECIMAL.
           05 PI-DRPA0368-EINH-KD-BETR   PIC S9(006)V99 PACKED-DECIMAL.
           05 PI-DRPA0368-EINH-REST-STRK PIC S9(006)V99 PACKED-DECIMAL.
260718     05 PI-DRPA0368-EINH-NFOE      PIC S9(006)V99 PACKED-DECIMAL.
250998*    05 PI-DRPA0368-EINH-AKT       PIC S9(006)V99 PACKED-DECIMAL.
           05 PI-DRPA0368-VBNR-EMP-TABELLE.
             10 PI-DRPA0368-VBNR-EMP-ZEILE OCCURS 20.
                15  PI-DRPA0368-EMP-TYP PIC S9(004) BINARY.
           05 PI-DRPA0368-VBNR-EMP-TAB-MAX PIC S9(004) BINARY.
      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRPA0368.
           05  PO-DRPA0368-RC                    PIC  X(03).
             88  PO-DRPA0368-RC-OK               VALUE 'OK '.
130298       88  PO-DRPA0368-RC-VSG              VALUE 'VSG'.
           05 PO-DRPA0368-EINH-TABELLE.
             10 PO-DRPA0368-EINH-ZEILE OCCURS 20.
               15 PO-DRPA0368-EINH-VB  PIC S9(06)V99 PACKED-DECIMAL.
           05 PO-DRPA0368-VM-GESCH-KZ  PIC X.
              88  PO-DRPA0368-VM-GESCHAEFT      VALUE 'J'.
              88  PO-DRPA0368-KEIN-VM-GESCHAEFT VALUE 'N'.
250998*    05 PO-DRPA0368-EINH-AKT     PIC S9(006)V99 PACKED-DECIMAL.
      *
      ******** ENDE COPY-MEMBER (DRPA0368) *******************

