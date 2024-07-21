      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0327.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0327.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:53:54   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   09 Jul 1998 09:58:52   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0327                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. ABR-DAT ZU DAT >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    22.10.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.10.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0327
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01 PI-DRVB0327.
          05 PI-DRVB0327-DATEN.
             10 PI-DRVB0327-KNR-STEUER-INFO         PIC S9(004) BINARY.
             10 PI-DRVB0327-STICHTAG                PIC  X(010).
      *
       01 PO-DRVB0327.
          05 PO-DRVB0327-RC                         PIC  X(003).
             88 PO-DRVB0327-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0327-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRVB0327-DATEN.
             10 PO-DRVB0327-LET-MA-ABR-DAT          PIC  X(010).
             10 PO-DRVB0327-NAE-MA-ABR-DAT          PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
