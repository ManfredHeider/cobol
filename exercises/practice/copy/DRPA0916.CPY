      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0916.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0916.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:47:42   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   10 Mar 1998 15:35:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0916                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<GSMT-PROV PRÜF. KOND>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    30.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!30.03.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0916                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0916.
          05 PI-DRPA0916-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA0916-DATEN.
             10 PI-DRPA0916-KOND-GSP.
                COPY DRPA0396.
             10 PI-DRPA0916-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA0916-PMPE-GSMT-PROV-DAT      PIC  X(010).
             10 PI-DRPA0916-PMPP-PPFL-BEITRAG       PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0916-PMPP-PPFL-SUMME         PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0916-PMPP-PG-GSMT-PROV       PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA0916.
          05 PO-DRPA0916-RC                         PIC  X(003).
             88 PO-DRPA0916-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0916-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
      *
      ******* ENDE COPY-MEMBER **************************************
