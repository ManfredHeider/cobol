      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0275.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0275.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:40:14   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:29:24   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0275                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PROD. ABL. AUS PGV  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    02.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.06.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0275                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0275.
          05 PI-DRPA0275-DATEN.
             10 PI-DRPA0275-PMPP-PGV          PIC S9(004) BINARY.
             10 PI-DRPA0275-PMPE-VERTR-BEGINN PIC  X(010).
             10 PI-DRPA0275-PMPE-FAELLIG-VON  PIC  X(010).
      *
       01 PO-DRPA0275.
          05 PO-DRPA0275-RC                   PIC  X(003).
             88 PO-DRPA0275-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0275-DATEN.
             10 PO-DRPA0275-PRODART           PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
