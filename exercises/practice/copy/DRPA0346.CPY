      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0346.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0346.CPv  $
      *
      *   Rev 2.1   Mar 23 2000 16:20:44   T13EX22
      *APA00067: PEG Anpassungen für Badenia
      *
      *   Rev 2.0   Feb 27 1999 14:28:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:54:08   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   09 Jul 1997 16:01:08   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0346                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<BEITRAG NORMIEREN   >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    02.07.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.07.97! 01 ! NEU                       ! WOLFF    *
      * APA00067!15.03.00! 02 ! PPFL-SUMME IN PI-DRPA0539 ! MUND     *
      *         !        !    ! ERGÄNZT                   !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0346                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0346.
          05 PI-DRPA0346-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA0346-DATEN.
             10 PI-DRPA0346-PMPP-PG-GSMT-PROV       PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0346-PMPP-PG-BEITRAG         PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
150300       10 PI-DRPA0346-PMPP-PG-SUMME           PIC S9(009)V9(2)
150300                                                  PACKED-DECIMAL.
             10 PI-DRPA0346-KOND-GSP.
                COPY DRPA0396.
      *
       01 PO-DRPA0346.
          05 PO-DRPA0346-RC                         PIC  X(003).
             88 PO-DRPA0346-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0346-DATEN.
             10 PO-DRPA0346-PMPP-PPFL-BEITRAG       PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
150300       10 PO-DRPA0346-PMPP-PPFL-SUMME         PIC S9(009)V9(2)
150300                                                  PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************
