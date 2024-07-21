      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0724.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drki0724.cpv  $
      *
      *   Rev 2.2   Apr 19 2000 13:23:06   T13EX41
      *AKI00065
      *- vergrößerung Schlüsselfelder
      *
      *   Rev 2.1   Mar 10 1999 12:06:40   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 02 1999 11:25:52   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   06 Oct 1998 15:33:00   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0724                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0724                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BÖHM / HORNUNG                                   *
      *    DATUM:    04-Jun-1998                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00065 !30.03.00!    !-VBBVERSBEDINGUNGID IST    !C. HEINECK*
      *         !        !    ! JETZT INTEGER             !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKIX889: ERGEBNISMENGE _XXX_ BEREITSTELLEN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI0724.
           05  PI-DRKI0724-UFKT                PIC  9(02).
           05  PI-DRKI0724-KEYS.
               10  PI-DRKI0724-VBBVERTRAGSID       PIC X(13).
               10  PI-DRKI0724-VBBVERSTARLFDNR     PIC S9(4) COMP.
               10  PI-DRKI0724-VBBVERSBEDINGLFDNR  PIC S9(4) COMP.
      *
       01  PO-DRKI0724.
           05  PO-DRKI0724-RC                  PIC  X(003).
               88 PO-DRKI0724-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0724-RC-EOF                       VALUE 'EOF'.
               88 PO-DRKI0724-RC-LEER                      VALUE 'LER'.
           05  PO-DRKI0724-ERGEBNISZEILE.
000330         10 PO-DRKI0724-VBBVERSBEDINGUNGID   PIC S9(9) COMP.

      ******* ENDE COPY-MEMBER **************************************

